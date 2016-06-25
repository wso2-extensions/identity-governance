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
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.ArrayList;
import java.util.List;

public class IdentityMgtServiceDataHolder {

    private static IdentityMgtServiceDataHolder instance = new IdentityMgtServiceDataHolder();
    private IdentityEventService identityEventService;
    private IdpManager idpManager;
    private static volatile List<IdentityGovernanceConnector> identityGovernanceConnectorList = new ArrayList<>();

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
            IdentityGovernanceConnector connector) {

        identityGovernanceConnectorList.add(connector);
    }

    protected void unsetIdentityGovernanceConnector(
            IdentityGovernanceConnector connector) {

        identityGovernanceConnectorList.remove(connector);
    }

    public List<IdentityGovernanceConnector> getIdentityGovernanceConnectorList () {
        return identityGovernanceConnectorList;
    }

    public IdpManager getIdpManager() {
        return idpManager;
    }

    public void setIdpManager(IdpManager idpManager) {
        this.idpManager = idpManager;
    }
}
