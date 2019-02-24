/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.internal;

import org.wso2.carbon.identity.claim.verification.core.verifier.ClaimVerifier;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.List;

/**
 * Data holder for the claim verification service.
 */
public class ClaimVerificationServiceDataHolder {

    private static volatile ClaimVerificationServiceDataHolder instance = new ClaimVerificationServiceDataHolder();
    private List<ClaimVerifier> claimVerifiers = new ArrayList<>();
    private IdentityEventService identityEventService;
    private RealmService realmService;
    private IdentityGovernanceService identityGovernanceService;

    /**
     * Get a claim verification service data holder.
     *
     * @return ClaimVerificationServiceDataHolder instance.
     */
    public static ClaimVerificationServiceDataHolder getInstance() {

        return instance;
    }

    /**
     * Set a claim verifier list.
     *
     * @param claimVerifiers ClaimVerifier list.
     */
    public void setClaimVerifiers(List<ClaimVerifier> claimVerifiers) {

        this.claimVerifiers = claimVerifiers;
    }

    /**
     * Get available claim verifiers.
     *
     * @return List of ClaimVerifier.
     */
    public List<ClaimVerifier> getClaimVerifiers() {

        if (claimVerifiers.size() == 0) {
            throw new RuntimeException(" No claim verifiers available.");
        }
        return claimVerifiers;
    }

    /**
     * Set the identity event service.
     *
     * @param identityEventService IdentityEventService.
     */
    public void setIdentityEventService(IdentityEventService identityEventService) {

        this.identityEventService = identityEventService;
    }

    /**
     * Get the identity event service.
     *
     * @return IdentityEventService.
     */
    public IdentityEventService getIdentityEventService() {

        if (identityEventService == null) {
            throw new RuntimeException("IdentityEventService not available. Component is not started properly.");
        }
        return identityEventService;
    }

    /**
     * Set the real service.
     *
     * @param realmService RealmService.
     */
    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }

    /**
     * Get the real service.
     *
     * @return RealmService.
     */
    public RealmService getRealmService() {

        if (realmService == null) {
            throw new RuntimeException("RealmService not available. Component is not started properly.");
        }
        return realmService;
    }

    /**
     * Set the identity governance service.
     *
     * @param identityGovernanceService IdentityGovernanceService.
     */
    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    /**
     * Get the identity governance service.
     *
     * @return IdentityGovernanceService.
     */
    public IdentityGovernanceService getIdentityGovernanceService() {

        if (identityGovernanceService == null) {
            throw new RuntimeException("IdentityGovernanceService not available. Component is not started properly.");
        }
        return identityGovernanceService;
    }
}
