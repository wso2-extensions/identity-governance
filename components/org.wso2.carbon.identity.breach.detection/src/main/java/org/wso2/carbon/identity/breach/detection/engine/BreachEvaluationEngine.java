/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.breach.detection.engine;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.breach.detection.mgt.EnforcementStatus;
import org.wso2.carbon.identity.breach.detection.metrics.BreachMetrics;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicy;
import org.wso2.carbon.identity.breach.detection.policy.FailurePolicy;
import org.wso2.carbon.identity.breach.source.BreachContext;
import org.wso2.carbon.identity.breach.source.BreachSource;
import org.wso2.carbon.identity.breach.source.BreachSourceException;
import org.wso2.carbon.identity.breach.source.BreachVerdict;
import org.wso2.carbon.identity.breach.source.Capability;
import org.wso2.carbon.identity.breach.source.Outcome;
import org.wso2.carbon.identity.breach.source.UnavailableCause;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Orders the sources a tenant enabled, bounds each call, short-circuits on the first match, contains failures
 * to the source that caused them, and resolves the whole thing into one decision.
 * <p>
 * It knows nothing about any concrete source. Ordering comes from the priority each source declares, so an
 * in-process offline list is consulted before a network round trip - which means the passwords an operator most
 * wants blocked never leave the deployment and never consume third-party quota.
 */
public class BreachEvaluationEngine {

    private static final Log LOG = LogFactory.getLog(BreachEvaluationEngine.class);

    private final SourceRegistry registry;
    private final BreachMetrics metrics;
    private final ThreadPoolExecutor executor;
    private final int timeoutMs;

    public BreachEvaluationEngine(SourceRegistry registry, BreachMetrics metrics, int workerThreads,
                                  int timeoutMs) {

        this.registry = registry;
        this.metrics = metrics;
        this.timeoutMs = timeoutMs;
        AtomicInteger counter = new AtomicInteger();
        this.executor = new ThreadPoolExecutor(1, Math.max(1, workerThreads), 60L, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(Math.max(1, workerThreads) * 10),
                runnable -> {
                    Thread thread = new Thread(runnable, "breach-source-" + counter.incrementAndGet());
                    thread.setDaemon(true);
                    return thread;
                },
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * Evaluate a candidate against the sources this organization's policy names.
     *
     * @param context the candidate and its surrounding operation.
     * @param policy  the organization's effective policy.
     * @return the decision, the reported status, and every contributing verdict.
     */
    public EvaluationResult evaluate(BreachContext context, BreachPolicy policy) {

        if (policy == null || !policy.isEnabled()) {
            return EvaluationResult.accept(EnforcementStatus.OFF);
        }
        List<PlannedSource> plan = plan(policy);
        if (plan.isEmpty()) {
            // Enabled but naming nothing is not enforcing, and must never be reported as if it were.
            LOG.warn("Breached password detection is enabled for tenant '" + context.getTenantDomain()
                    + "' but names no source. No password is being checked.");
            return EvaluationResult.accept(EnforcementStatus.NOT_ENFORCING);
        }

        List<BreachVerdict> verdicts = new ArrayList<>(plan.size());
        List<AtomicBoolean> outstanding = new ArrayList<>(plan.size());
        String breachedBy = null;

        for (PlannedSource planned : plan) {
            BreachVerdict verdict = verdictFor(planned, context, outstanding);
            verdicts.add(verdict);
            if (verdict.getOutcome() == Outcome.FOUND) {
                breachedBy = verdict.getSourceId();
                // A match ends it. Nothing after this needs asking, and no network call is worth making.
                break;
            }
        }

        clearWhenSafe(context, outstanding);
        return resolve(context, policy, verdicts, breachedBy);
    }

    /**
     * Stop accepting work. Called when the component deactivates.
     */
    public void shutdown() {

        executor.shutdownNow();
    }

    private List<PlannedSource> plan(BreachPolicy policy) {

        List<PlannedSource> installed = new ArrayList<>();
        List<PlannedSource> missing = new ArrayList<>();
        for (String sourceId : policy.getSourceIds()) {
            Optional<BreachSource> bound = registry.get(sourceId);
            if (bound.isPresent()) {
                installed.add(new PlannedSource(bound.get().getId(), bound.get()));
            } else {
                missing.add(new PlannedSource(sourceId, null));
            }
        }
        installed.sort(Comparator.comparingInt((PlannedSource p) -> p.source.getPriority())
                .thenComparing(p -> p.sourceId));
        installed.addAll(missing);
        return installed;
    }

    private BreachVerdict verdictFor(PlannedSource planned, BreachContext context,
                                     List<AtomicBoolean> outstanding) {

        long started = System.nanoTime();
        BreachVerdict verdict = call(planned, context, outstanding);
        metrics.record(context.getTenantDomain(), planned.sourceId, verdict.getOutcome(),
                verdict.getCause().orElse(null), System.nanoTime() - started);
        return verdict;
    }

    private BreachVerdict call(PlannedSource planned, BreachContext context,
                               List<AtomicBoolean> outstanding) {

        if (planned.source == null) {
            // Policy names it; nothing is bound behind it. Treated as unavailable, never silently skipped.
            return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.SOURCE_NOT_REGISTERED,
                    "No connector is installed for source '" + planned.sourceId + "'.");
        }
        try {
            if (!planned.source.isConfigured(context.getTenantDomain())) {
                return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.MISCONFIGURED,
                        "The source is installed but not configured.");
            }
        } catch (Throwable t) {
            return contain(planned.sourceId, t);
        }

        boolean offline = planned.source.getCapabilities().contains(Capability.OFFLINE);
        if (offline) {
            // In-process and answering in microseconds. A thread hand-off would cost more than the lookup.
            return invoke(planned, context);
        }
        // Tracked by a flag the task itself sets, not by the future's state: cancelling a future marks it
        // done while the worker may still be reading the characters.
        AtomicBoolean finished = new AtomicBoolean();
        Future<BreachVerdict> future;
        try {
            future = executor.submit(() -> {
                try {
                    return invoke(planned, context);
                } finally {
                    finished.set(true);
                }
            });
        } catch (RejectedExecutionException e) {
            return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.INTERNAL,
                    "Evaluation capacity is exhausted.");
        }
        outstanding.add(finished);
        try {
            BreachVerdict verdict = future.get(timeoutMs, TimeUnit.MILLISECONDS);
            outstanding.remove(finished);
            return verdict;
        } catch (TimeoutException e) {
            future.cancel(true);
            return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.TIMEOUT,
                    "The source did not answer within " + timeoutMs + " ms.");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            future.cancel(true);
            return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.INTERNAL,
                    "The evaluation was interrupted.");
        } catch (Exception e) {
            outstanding.remove(finished);
            return contain(planned.sourceId, e.getCause() == null ? e : e.getCause());
        }
    }

    private BreachVerdict invoke(PlannedSource planned, BreachContext context) {

        try {
            BreachVerdict verdict = planned.source.evaluate(context);
            if (verdict == null) {
                return BreachVerdict.unavailable(planned.sourceId, UnavailableCause.INTERNAL,
                        "The source returned no verdict.");
            }
            return verdict;
        } catch (BreachSourceException e) {
            return BreachVerdict.unavailable(planned.sourceId, e.getUnavailableCause(), e.getMessage());
        } catch (Throwable t) {
            return contain(planned.sourceId, t);
        }
    }

    private BreachVerdict contain(String sourceId, Throwable t) {

        // Contained to this source: one connector's defect must not take the others down with it.
        LOG.error("Breach source '" + sourceId + "' failed while evaluating a password. The source is treated "
                + "as unavailable and the remaining sources are unaffected.", t);
        return BreachVerdict.unavailable(sourceId, UnavailableCause.INTERNAL,
                "The source raised an unexpected error.");
    }

    private void clearWhenSafe(BreachContext context, List<AtomicBoolean> outstanding) {

        for (AtomicBoolean finished : outstanding) {
            if (!finished.get()) {
                // A timed-out source may still be reading the characters. Leave them to the collector rather
                // than corrupting a call in flight.
                LOG.debug("A breach source call is still outstanding; leaving the credential to be collected.");
                return;
            }
        }
        context.getCredential().clear();
    }

    private EvaluationResult resolve(BreachContext context, BreachPolicy policy, List<BreachVerdict> verdicts,
                                     String breachedBy) {

        if (breachedBy != null) {
            return EvaluationResult.of(Decision.REFUSE_BREACHED, EnforcementStatus.ENFORCING, verdicts,
                    breachedBy);
        }

        int answered = 0;
        String denyingSource = null;
        for (BreachVerdict verdict : verdicts) {
            if (verdict.getOutcome() == Outcome.NOT_FOUND) {
                answered++;
            } else if (verdict.getOutcome() == Outcome.UNAVAILABLE
                    && policy.getFailurePolicy(verdict.getSourceId()) == FailurePolicy.DENY
                    && denyingSource == null) {
                denyingSource = verdict.getSourceId();
            }
        }
        int unavailable = verdicts.size() - answered;

        EnforcementStatus status;
        if (unavailable == 0) {
            status = EnforcementStatus.ENFORCING;
        } else if (answered > 0) {
            status = EnforcementStatus.DEGRADED;
        } else {
            status = EnforcementStatus.NOT_ENFORCING;
        }

        if (status == EnforcementStatus.NOT_ENFORCING) {
            // The signal that matters most: everything looks healthy from outside while nothing is checked.
            LOG.error("Breached password detection is not enforcing for tenant '" + context.getTenantDomain()
                    + "': no enabled source could return a verdict. Verdicts: " + verdicts);
        } else if (status == EnforcementStatus.DEGRADED && LOG.isWarnEnabled()) {
            LOG.warn("Breached password detection is degraded for tenant '" + context.getTenantDomain()
                    + "': " + unavailable + " of " + verdicts.size() + " sources could not answer.");
        }

        if (denyingSource != null) {
            return EvaluationResult.of(Decision.REFUSE_UNVERIFIED, status, verdicts, denyingSource);
        }
        return EvaluationResult.of(Decision.ACCEPT, status, verdicts, null);
    }

    /**
     * One entry in the ordered evaluation plan. A null source means policy names an id nothing has registered.
     */
    private static final class PlannedSource {

        private final String sourceId;
        private final BreachSource source;

        private PlannedSource(String sourceId, BreachSource source) {

            this.sourceId = sourceId;
            this.source = source;
        }
    }
}
