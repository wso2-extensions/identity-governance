/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.recovery.internal;

import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.identity.consent.mgt.services.ConsentUtilityService;
import org.wso2.carbon.identity.core.persistence.registry.RegistryResourceMgtService;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.registry.core.service.RegistryService;
import org.wso2.carbon.user.core.service.RealmService;

public class IdentityRecoveryServiceDataHolder {

    private static IdentityRecoveryServiceDataHolder instance = new IdentityRecoveryServiceDataHolder();
    private RealmService realmService;
    private RegistryService registryService;
    private IdentityEventService identityEventService;
    private IdentityGovernanceService identityGovernanceService;
    private IdpManager idpManager;
    private RegistryResourceMgtService resourceMgtService;
    private AccountLockService accountLockService;
    private ConsentManager consentManager;
    private ConsentUtilityService consentUtilityService;
    public static IdentityRecoveryServiceDataHolder getInstance() {
        return instance;
    }

    public IdentityEventService getIdentityEventService() {
        return identityEventService;
    }

    public void setIdentityEventService(IdentityEventService identityEventService) {
        this.identityEventService = identityEventService;
    }

    public IdpManager getIdpManager() {
        return idpManager;
    }

    public void setIdpManager(IdpManager idpManager) {
        this.idpManager = idpManager;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {
        if(identityGovernanceService == null) {
            throw new RuntimeException("IdentityGovernanceService not available. Component is not started properly.");
        }
        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {
        this.identityGovernanceService = identityGovernanceService;
    }

    public RegistryResourceMgtService getResourceMgtService() {
        return resourceMgtService;
    }

    public void setResourceMgtService(RegistryResourceMgtService resourceMgtService) {
        this.resourceMgtService = resourceMgtService;
    }

    public RealmService getRealmService() {
        return realmService;
    }

    public void setRealmService(RealmService realmService) {
        this.realmService = realmService;
    }

    public RegistryService getRegistryService() {
        return registryService;
    }

    public void setRegistryService(RegistryService registryService) {
        this.registryService = registryService;
    }

    public AccountLockService getAccountLockService() {
        return accountLockService;
    }

    /**
     * Sets consent Manager OSGI service
     *
     * @param consentManager Consent Manager
     */
    public void setConsentManager(ConsentManager consentManager) {

        this.consentManager = consentManager;
    }

    /**
     * Get Consent Manager OSGI service.
     *
     * @return ConsentManager
     */
    public ConsentManager getConsentManager() {

        return consentManager;
    }

    public void setAccountLockService(AccountLockService accountLockService) {
        this.accountLockService = accountLockService;
    }

    /**
     * Get consent utility service
     *
     * @return Consent utility service.
     */
    public ConsentUtilityService getConsentUtilityService() {

        return consentUtilityService;
    }

    /**
     * Set consent utility service
     *
     * @param consentUtilityService
     */
    public void setConsentUtilityService(ConsentUtilityService consentUtilityService) {

        this.consentUtilityService = consentUtilityService;
    }
}
