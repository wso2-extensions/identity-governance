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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Identity Governance admin service.
 */
public class IdentityGovernanceAdminService extends AbstractAdmin {

    IdentityGovernanceService identityGovernanceService;

    public ConnectorConfig[] getConnectorList() throws IdentityGovernanceException {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        identityGovernanceService = new IdentityGovernanceServiceImpl();
        List<IdentityConnectorConfig> list = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceConnectorList();
        Property[] properties = identityGovernanceService.getConfiguration(tenantDomain);
        ConnectorConfig[] configs = new ConnectorConfig[list.size()];
        String[] connectorProperties;

        for (int i = 0; i < list.size(); i++) {
            ConnectorConfig config = new ConnectorConfig();
            Map<String, String> propertyFriendlyNames = list.get(i).getPropertyNameMapping();
            Map<String, String> propertyDescriptions = list.get(i).getPropertyDescriptionMapping();
            config.setFriendlyName(list.get(i).getFriendlyName());
            config.setCategory(list.get(i).getCategory());
            config.setSubCategory(list.get(i).getSubCategory());
            config.setOrder(list.get(i).getOrder());
            connectorProperties = list.get(i).getPropertyNames();
            Property[] configProperties = new Property[connectorProperties.length];
            for (int j = 0; j < connectorProperties.length; j++) {
                for (int k = 0; k < properties.length; k++) {
                    if (connectorProperties[j].equals(properties[k].getName())) {
                        configProperties[j] = properties[k];
                        configProperties[j].setDisplayName(propertyFriendlyNames.get(configProperties[j].getName()));
                        configProperties[j].setDescription(propertyDescriptions.get(configProperties[j].getName()));
                        break;
                    }
                }
            }
            config.setProperties(configProperties);
            configs[i] = config;
        }
        return configs;
    }

    public void updateConfigurations(Property[] configurations) throws IdentityGovernanceException {
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        identityGovernanceService = new IdentityGovernanceServiceImpl();
        Map<String, String> confMap = new HashMap<>();
        for (Property configuration : configurations) {
            confMap.put(configuration.getName(), configuration.getValue());
        }
        identityGovernanceService.updateConfiguration(tenantDomain, confMap);
    }
}
