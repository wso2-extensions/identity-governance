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

import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.List;
import java.util.Map;

public interface IdentityGovernanceService {

    void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails) throws
            IdentityGovernanceException;

    Property[] getConfiguration(String tenantDomain) throws IdentityGovernanceException;

    Property[] getConfiguration(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException;

    List<IdentityConnectorConfig> getConnectorList() throws IdentityGovernanceException;

    List<ConnectorConfig> getConnectorListWithConfigs(String tenantDomain) throws IdentityGovernanceException;

    Map<String, List<ConnectorConfig>> getCategorizedConnectorListWithConfigs(String tenantDomain) throws IdentityGovernanceException;

    List<ConnectorConfig> getConnectorListWithConfigsByCategory(String tenantDomain,
                                                                String category) throws IdentityGovernanceException;

    ConnectorConfig getConnectorListWithConfig(String tenantDomain,
                                                                String connectorName) throws IdentityGovernanceException;
}
