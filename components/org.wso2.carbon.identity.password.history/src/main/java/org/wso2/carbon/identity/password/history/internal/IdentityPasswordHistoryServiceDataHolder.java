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

package org.wso2.carbon.identity.password.history.internal;

import org.osgi.framework.BundleContext;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.registry.core.service.RegistryService;
import org.wso2.carbon.user.core.service.RealmService;

public class IdentityPasswordHistoryServiceDataHolder {

    private static IdentityPasswordHistoryServiceDataHolder instance = new IdentityPasswordHistoryServiceDataHolder();
    private IdentityEventService eventMgtService;
    private IdentityGovernanceService identityGovernanceService;
    private IdpManager idpManager;
    private BundleContext bundleContext;
    private RealmService realmService;
    private RegistryService registryService;


    private IdentityPasswordHistoryServiceDataHolder() {
    }

    public static IdentityPasswordHistoryServiceDataHolder getInstance() {

        return instance;
    }

    public IdentityEventService getEventMgtService() {
        return eventMgtService;
    }

    public void setEventMgtService(IdentityEventService eventMgtService) {
        this.eventMgtService = eventMgtService;
    }

    public IdpManager getIdpManager() {
        return idpManager;
    }

    public void setIdpManager(IdpManager idpManager) {
        this.idpManager = idpManager;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {
        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {
        this.identityGovernanceService = identityGovernanceService;
    }

    public BundleContext getBundleContext() {
        return bundleContext;
    }

    public void setBundleContext(BundleContext bundleContext) {
        this.bundleContext = bundleContext;
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
}
