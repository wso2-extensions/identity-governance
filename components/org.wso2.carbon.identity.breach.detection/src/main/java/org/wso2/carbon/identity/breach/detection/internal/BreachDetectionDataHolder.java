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

package org.wso2.carbon.identity.breach.detection.internal;

import org.wso2.carbon.identity.breach.detection.engine.BreachEvaluationEngine;
import org.wso2.carbon.identity.breach.detection.engine.SourceRegistry;
import org.wso2.carbon.identity.breach.detection.metrics.BreachMetrics;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicyResolver;
import org.wso2.carbon.identity.breach.detection.source.LocalBlocklistSource;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Wiring, held in one place so the listener and the management service reach the same instances.
 */
public class BreachDetectionDataHolder {

    private static final BreachDetectionDataHolder INSTANCE = new BreachDetectionDataHolder();

    private final SourceRegistry sourceRegistry = new SourceRegistry();
    private final BreachMetrics metrics = new BreachMetrics();

    private IdentityGovernanceService identityGovernanceService;
    private RealmService realmService;
    private BreachEvaluationEngine evaluationEngine;
    private BreachPolicyResolver policyResolver;
    private LocalBlocklistSource localBlocklistSource;

    private BreachDetectionDataHolder() {

    }

    public static BreachDetectionDataHolder getInstance() {

        return INSTANCE;
    }

    public SourceRegistry getSourceRegistry() {

        return sourceRegistry;
    }

    public BreachMetrics getMetrics() {

        return metrics;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {

        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    public RealmService getRealmService() {

        return realmService;
    }

    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }

    public BreachEvaluationEngine getEvaluationEngine() {

        return evaluationEngine;
    }

    public void setEvaluationEngine(BreachEvaluationEngine evaluationEngine) {

        this.evaluationEngine = evaluationEngine;
    }

    public BreachPolicyResolver getPolicyResolver() {

        return policyResolver;
    }

    public void setPolicyResolver(BreachPolicyResolver policyResolver) {

        this.policyResolver = policyResolver;
    }

    public LocalBlocklistSource getLocalBlocklistSource() {

        return localBlocklistSource;
    }

    public void setLocalBlocklistSource(LocalBlocklistSource localBlocklistSource) {

        this.localBlocklistSource = localBlocklistSource;
    }
}
