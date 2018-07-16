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

package org.wso2.carbon.identity.governance.internal;

import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceServiceImpl;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.List;

public class IdentityMgtServiceDataHolder {

    private static IdentityMgtServiceDataHolder instance = new IdentityMgtServiceDataHolder();
    private IdentityEventService identityEventService;
    private IdpManager idpManager;
    private static volatile List<IdentityConnectorConfig> identityConnectorConfigList = new ArrayList<>();
    private RealmService realmService;
    private IdentityGovernanceServiceImpl identityGovernanceService;

    public static IdentityMgtServiceDataHolder getInstance() {

        return instance;
    }

    public IdentityEventService getIdentityEventService() {
        return identityEventService;
    }

    public void setIdentityEventService(IdentityEventService eventMgtService) {
        this.identityEventService = eventMgtService;
    }

    protected void addIdentityGovernanceConnector(
            IdentityConnectorConfig connector) {

        identityConnectorConfigList.add(connector);
    }

    protected void unsetIdentityGovernanceConnector(
            IdentityConnectorConfig connector) {

        identityConnectorConfigList.remove(connector);
    }

    public List<IdentityConnectorConfig> getIdentityGovernanceConnectorList () {
        return identityConnectorConfigList;
    }

    public IdpManager getIdpManager() {
        return idpManager;
    }

    public void setIdpManager(IdpManager idpManager) {
        this.idpManager = idpManager;
    }

    public RealmService getRealmService() {
        return realmService;
    }

    public void setRealmService(RealmService realmService) {
        this.realmService = realmService;
    }

    public IdentityGovernanceServiceImpl getIdentityGovernanceService() {

        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceServiceImpl identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }
}
