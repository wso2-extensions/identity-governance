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

package org.wso2.carbon.identity.breach.detection.metrics;

import org.wso2.carbon.identity.breach.detection.mgt.SourceStats;
import org.wso2.carbon.identity.breach.source.Outcome;
import org.wso2.carbon.identity.breach.source.UnavailableCause;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Counters per source and per organization: evaluations, verdicts, unavailable broken out by cause, and
 * latency.
 * <p>
 * Nothing here takes a candidate password, any part of one, or its digest. Occurrence counts stay on the
 * verdict and never reach a metric label either - a label cardinality of "every breached password" would be a
 * password store with extra steps.
 */
public class BreachMetrics {

    private final ConcurrentHashMap<String, Counters> counters = new ConcurrentHashMap<>();

    /**
     * Record one source's verdict.
     *
     * @param tenantDomain organization the evaluation ran in.
     * @param sourceId     the reporting source.
     * @param outcome      what it concluded.
     * @param cause        why it could not conclude, when applicable.
     * @param latencyNanos how long the call took.
     */
    public void record(String tenantDomain, String sourceId, Outcome outcome, UnavailableCause cause,
                       long latencyNanos) {

        Counters c = counters.computeIfAbsent(key(tenantDomain, sourceId), k -> new Counters());
        c.record(outcome, cause, latencyNanos);
    }

    /**
     * @param tenantDomain organization.
     * @param sourceId     source.
     * @return a snapshot, never null.
     */
    public SourceStats snapshot(String tenantDomain, String sourceId) {

        Counters c = counters.get(key(tenantDomain, sourceId));
        return c == null ? Counters.EMPTY.toStats() : c.toStats();
    }

    private static String key(String tenantDomain, String sourceId) {

        return tenantDomain + '|' + sourceId;
    }

    /**
     * Counters for one (organization, source) pair. Today's counters roll over on the day boundary so the
     * administrator surface can report "checks today" without keeping a time series.
     */
    private static final class Counters {

        private static final Counters EMPTY = new Counters();

        private final AtomicLong evaluations = new AtomicLong();
        private final AtomicLong found = new AtomicLong();
        private final AtomicLong notFound = new AtomicLong();
        private final AtomicLong unavailable = new AtomicLong();
        private final AtomicLong latencyTotalNanos = new AtomicLong();
        private final AtomicLong latencyMaxNanos = new AtomicLong();
        private final ConcurrentHashMap<UnavailableCause, AtomicLong> byCause = new ConcurrentHashMap<>();

        private final AtomicLong dayStamp = new AtomicLong(currentDay());
        private final AtomicLong todayEvaluations = new AtomicLong();
        private final AtomicLong todayFound = new AtomicLong();
        private final AtomicLong todayUnavailable = new AtomicLong();

        void record(Outcome outcome, UnavailableCause cause, long latencyNanos) {

            rollOverIfNeeded();
            evaluations.incrementAndGet();
            todayEvaluations.incrementAndGet();
            latencyTotalNanos.addAndGet(latencyNanos);
            latencyMaxNanos.accumulateAndGet(latencyNanos, Math::max);
            if (outcome == Outcome.FOUND) {
                found.incrementAndGet();
                todayFound.incrementAndGet();
            } else if (outcome == Outcome.NOT_FOUND) {
                notFound.incrementAndGet();
            } else {
                unavailable.incrementAndGet();
                todayUnavailable.incrementAndGet();
                byCause.computeIfAbsent(cause == null ? UnavailableCause.INTERNAL : cause,
                        k -> new AtomicLong()).incrementAndGet();
            }
        }

        private void rollOverIfNeeded() {

            long today = currentDay();
            long stamped = dayStamp.get();
            if (stamped != today && dayStamp.compareAndSet(stamped, today)) {
                todayEvaluations.set(0);
                todayFound.set(0);
                todayUnavailable.set(0);
            }
        }

        SourceStats toStats() {

            Map<String, Long> causes = new LinkedHashMap<>();
            byCause.forEach((cause, count) -> causes.put(cause.name(), count.get()));
            long total = evaluations.get();
            long averageMs = total == 0 ? 0
                    : TimeUnit.NANOSECONDS.toMillis(latencyTotalNanos.get() / total);
            return new SourceStats(total, found.get(), notFound.get(), unavailable.get(), causes,
                    todayEvaluations.get(), todayFound.get(), todayUnavailable.get(),
                    TimeUnit.NANOSECONDS.toMillis(latencyMaxNanos.get()), averageMs);
        }

        private static long currentDay() {

            return System.currentTimeMillis() / TimeUnit.DAYS.toMillis(1);
        }
    }
}
