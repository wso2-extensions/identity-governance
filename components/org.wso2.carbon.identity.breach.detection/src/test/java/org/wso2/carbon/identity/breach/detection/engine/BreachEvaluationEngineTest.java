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

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.breach.detection.mgt.EnforcementStatus;
import org.wso2.carbon.identity.breach.detection.metrics.BreachMetrics;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicy;
import org.wso2.carbon.identity.breach.detection.policy.FailurePolicy;
import org.wso2.carbon.identity.breach.source.BreachContext;
import org.wso2.carbon.identity.breach.source.BreachVerdict;
import org.wso2.carbon.identity.breach.source.Credential;
import org.wso2.carbon.identity.breach.source.Operation;
import org.wso2.carbon.identity.breach.source.Subject;
import org.wso2.carbon.identity.breach.source.UnavailableCause;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * The verdict resolution table, row by row.
 * <p>
 * This is the behavioural contract an administrator is shown and an operator alerts on, so every row is pinned
 * here rather than inferred from the implementation.
 */
public class BreachEvaluationEngineTest {

    private static final String TENANT = "carbon.super";

    private SourceRegistry registry;
    private BreachEvaluationEngine engine;

    @BeforeMethod
    public void setUp() {

        registry = new SourceRegistry();
        engine = new BreachEvaluationEngine(registry, new BreachMetrics(), 4, 500);
    }

    @Test
    public void aDisabledPolicyEvaluatesNothing() {

        StubBreachSource local = StubBreachSource.offline("localList", 100, c -> BreachVerdict.found("localList"));
        registry.bind(local);
        EvaluationResult result = engine.evaluate(context(), BreachPolicy.disabled());
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.OFF);
        assertEquals(local.getCalls(), 0);
    }

    @Test
    public void enabledWithNoNamedSourceIsNotEnforcing() {

        EvaluationResult result = engine.evaluate(context(), policy());
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.NOT_ENFORCING);
    }

    @Test
    public void anyFoundRefusesAsBreached() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        registry.bind(StubBreachSource.remote("hibp", 500, c -> BreachVerdict.found("hibp", 612953)));
        EvaluationResult result = engine.evaluate(context(), policy("localList", "hibp"));
        assertEquals(result.getDecision(), Decision.REFUSE_BREACHED);
        assertEquals(result.getStatus(), EnforcementStatus.ENFORCING);
        assertEquals(result.getDecidingSourceId(), "hibp");
    }

    @Test
    public void allNotFoundAccepts() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        registry.bind(StubBreachSource.remote("hibp", 500, c -> BreachVerdict.notFound("hibp")));
        EvaluationResult result = engine.evaluate(context(), policy("localList", "hibp"));
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.ENFORCING);
    }

    @Test
    public void someUnavailableWithAllowAcceptsAndReportsDegraded() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        registry.bind(StubBreachSource.remote("hibp", 500,
                c -> BreachVerdict.unavailable("hibp", UnavailableCause.TRANSPORT, "down")));
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("localList", "hibp"), failure("hibp", FailurePolicy.ALLOW)));
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.DEGRADED);
    }

    @Test
    public void someUnavailableWithDenyRefusesAsUnverified() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        registry.bind(StubBreachSource.remote("hibp", 500,
                c -> BreachVerdict.unavailable("hibp", UnavailableCause.TRANSPORT, "down")));
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("localList", "hibp"), failure("hibp", FailurePolicy.DENY)));
        assertEquals(result.getDecision(), Decision.REFUSE_UNVERIFIED);
        assertEquals(result.getStatus(), EnforcementStatus.DEGRADED);
        assertEquals(result.getDecidingSourceId(), "hibp");
    }

    @Test
    public void everySourceUnavailableWithAllowAcceptsButIsNotEnforcing() {

        registry.bind(StubBreachSource.remote("hibp", 500,
                c -> BreachVerdict.unavailable("hibp", UnavailableCause.TIMEOUT, "slow")));
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("hibp"), failure("hibp", FailurePolicy.ALLOW)));
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.NOT_ENFORCING);
    }

    @Test
    public void aSourceNamedInPolicyWithNothingBoundIsUnavailableNotSkipped() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("localList", "spycloud"), failure("spycloud", FailurePolicy.ALLOW)));
        assertEquals(result.getDecision(), Decision.ACCEPT);
        assertEquals(result.getStatus(), EnforcementStatus.DEGRADED);
        assertTrue(result.getVerdicts().stream().anyMatch(
                v -> v.getCause().orElse(null) == UnavailableCause.SOURCE_NOT_REGISTERED));
    }

    @Test
    public void aSourceNamedInPolicyWithNothingBoundCanRefuse() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("localList", "spycloud"), failure("spycloud", FailurePolicy.DENY)));
        assertEquals(result.getDecision(), Decision.REFUSE_UNVERIFIED);
        assertEquals(result.getDecidingSourceId(), "spycloud");
    }

    @Test
    public void aSourceThatIsNotSetUpIsUnavailableRatherThanClean() {

        registry.bind(StubBreachSource.remote("hibp", 500, c -> BreachVerdict.notFound("hibp"))
                .notConfigured());
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("hibp"), failure("hibp", FailurePolicy.DENY)));
        assertEquals(result.getDecision(), Decision.REFUSE_UNVERIFIED);
        assertTrue(result.getVerdicts().stream().anyMatch(
                v -> v.getCause().orElse(null) == UnavailableCause.MISCONFIGURED));
    }

    @Test
    public void theCheapSourceRunsFirstAndAMatchStopsTheRest() {

        StubBreachSource local = StubBreachSource.offline("localList", 100,
                c -> BreachVerdict.found("localList"));
        StubBreachSource remote = StubBreachSource.remote("hibp", 500, c -> BreachVerdict.notFound("hibp"));
        // Named in the expensive-first order on purpose: ordering must come from the declared priority.
        registry.bind(remote);
        registry.bind(local);
        EvaluationResult result = engine.evaluate(context(), policy("hibp", "localList"));
        assertEquals(result.getDecision(), Decision.REFUSE_BREACHED);
        assertEquals(local.getCalls(), 1);
        assertEquals(remote.getCalls(), 0, "A match must end the evaluation before any network call.");
    }

    @Test
    public void anErrorInsideOneSourceIsContainedToThatSource() {

        StubBreachSource broken = StubBreachSource.offline("broken", 100, c -> BreachVerdict.notFound("broken"))
                .throwing(new IllegalStateException("connector defect"));
        StubBreachSource healthy = StubBreachSource.offline("healthy", 200,
                c -> BreachVerdict.found("healthy"));
        registry.bind(broken);
        registry.bind(healthy);
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("broken", "healthy"), failure("broken", FailurePolicy.ALLOW)));
        assertEquals(result.getDecision(), Decision.REFUSE_BREACHED);
        assertEquals(healthy.getCalls(), 1);
    }

    @Test
    public void aRemoteSourceThatOverrunsItsBudgetIsUnavailableNotClean() {

        registry.bind(StubBreachSource.remote("slow", 500, c -> BreachVerdict.notFound("slow")).slow(2000));
        long started = System.currentTimeMillis();
        EvaluationResult result = engine.evaluate(context(),
                policy(Arrays.asList("slow"), failure("slow", FailurePolicy.DENY)));
        long elapsed = System.currentTimeMillis() - started;
        assertEquals(result.getDecision(), Decision.REFUSE_UNVERIFIED);
        assertTrue(result.getVerdicts().get(0).getCause().orElse(null) == UnavailableCause.TIMEOUT);
        assertTrue(elapsed < 1500, "The call must be bounded by the timeout, not by the source. Took " + elapsed);
    }

    @Test
    public void theCredentialIsClearedOnceEverySourceHasAnswered() {

        registry.bind(StubBreachSource.offline("localList", 100, c -> BreachVerdict.notFound("localList")));
        BreachContext context = context();
        engine.evaluate(context, policy("localList"));
        assertTrue(context.getCredential().isCleared());
    }

    @Test
    public void aTimedOutCallLeavesTheCredentialAloneRatherThanCorruptingIt() {

        registry.bind(StubBreachSource.remote("slow", 500, c -> BreachVerdict.notFound("slow")).slow(2000));
        BreachContext context = context();
        engine.evaluate(context, policy("slow"));
        // The worker may still be reading it; clearing underneath a call in flight is worse than waiting.
        assertTrue(!context.getCredential().isCleared());
    }

    private BreachContext context() {

        return BreachContext.builder()
                .credential(new Credential("Password@1".toCharArray()))
                .subject(Subject.builder("alice").build())
                .tenantDomain(TENANT)
                .operation(Operation.REGISTER)
                .build();
    }

    private BreachPolicy policy(String... sourceIds) {

        return policy(Arrays.asList(sourceIds), new LinkedHashMap<>());
    }

    private BreachPolicy policy(List<String> sourceIds, Map<String, FailurePolicy> failurePolicies) {

        return new BreachPolicy(true, new ArrayList<>(sourceIds), failurePolicies);
    }

    private Map<String, FailurePolicy> failure(String sourceId, FailurePolicy policy) {

        Map<String, FailurePolicy> map = new LinkedHashMap<>();
        map.put(sourceId, policy);
        return map;
    }
}
