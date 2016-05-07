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

package org.wso2.carbon.identity.governance.listener;

import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.stratos.common.beans.TenantInfoBean;
import org.wso2.carbon.stratos.common.exception.StratosException;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;

import java.util.List;

public class TenantCreationEventListener implements TenantMgtListener {

    @Override
    public void onTenantCreate(TenantInfoBean tenantInfoBean) throws StratosException {
        List<IdentityGovernanceConnector> connectorList = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceConnectorList();
        for (IdentityGovernanceConnector identityGovernanceConnector : connectorList) {
            try {
                IdentityGovernanceUtil.saveConnectorDefaultProperties(identityGovernanceConnector, tenantInfoBean
                        .getTenantDomain());
            } catch (IdentityGovernanceException e) {
                throw new StratosException("Error while saving tenant configurations for " + identityGovernanceConnector.getName() +
                        ".", e);
            }
        }

    }

    @Override
    public void onTenantUpdate(TenantInfoBean tenantInfoBean) throws StratosException {

    }

    @Override
    public void onTenantDelete(int i) {

    }

    @Override
    public void onTenantRename(int i, String s, String s2) throws StratosException {

    }

    @Override
    public void onTenantInitialActivation(int i) throws StratosException {

    }

    @Override
    public void onTenantActivation(int i) throws StratosException {

    }

    @Override
    public void onTenantDeactivation(int i) throws StratosException {

    }

    @Override
    public void onSubscriptionPlanChange(int i, String s, String s2) throws StratosException {

    }

    @Override
    public int getListenerOrder() {
        return 31;
    }

    @Override
    public void onPreDelete(int i) throws StratosException {

    }
}

