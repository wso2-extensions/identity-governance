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

import org.wso2.carbon.identity.breach.detection.mgt.EnforcementStatus;
import org.wso2.carbon.identity.breach.source.BreachVerdict;

import java.util.Collections;
import java.util.List;

/**
 * The outcome of one evaluation: what to do, what the capability was actually doing at the time, and every
 * verdict that contributed.
 */
public final class EvaluationResult {

    private final Decision decision;
    private final EnforcementStatus status;
    private final List<BreachVerdict> verdicts;
    private final String decidingSourceId;

    private EvaluationResult(Decision decision, EnforcementStatus status, List<BreachVerdict> verdicts,
                             String decidingSourceId) {

        this.decision = decision;
        this.status = status;
        this.verdicts = Collections.unmodifiableList(verdicts);
        this.decidingSourceId = decidingSourceId;
    }

    static EvaluationResult of(Decision decision, EnforcementStatus status, List<BreachVerdict> verdicts,
                               String decidingSourceId) {

        return new EvaluationResult(decision, status, verdicts, decidingSourceId);
    }

    static EvaluationResult accept(EnforcementStatus status) {

        return new EvaluationResult(Decision.ACCEPT, status, Collections.emptyList(), null);
    }

    public Decision getDecision() {

        return decision;
    }

    public EnforcementStatus getStatus() {

        return status;
    }

    public List<BreachVerdict> getVerdicts() {

        return verdicts;
    }

    /**
     * @return the source that caused a refusal, or {@code null} when the password was accepted.
     */
    public String getDecidingSourceId() {

        return decidingSourceId;
    }
}
