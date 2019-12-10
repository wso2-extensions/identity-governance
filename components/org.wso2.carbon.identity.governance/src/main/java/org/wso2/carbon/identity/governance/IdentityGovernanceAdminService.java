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

package org.wso2.carbon.identity.governance;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.core.AbstractAdmin;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class IdentityGovernanceAdminService extends AbstractAdmin {

    public ConnectorConfig[] getConnectorList() throws IdentityGovernanceException {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        IdentityGovernanceService identityGovernanceService = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceService();
        return identityGovernanceService.getConnectorListWithConfigs(tenantDomain).toArray(new ConnectorConfig[0]);
    }

    public void updateConfigurations(Property[] configurations) throws IdentityGovernanceException {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        IdentityGovernanceService identityGovernanceService = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceService();
        Map<String, String> confMap = new HashMap<>();
        for (Property configuration : configurations) {
            confMap.put(configuration.getName(), configuration.getValue());
        }
        identityGovernanceService.updateConfiguration(tenantDomain, confMap);

    }

}
