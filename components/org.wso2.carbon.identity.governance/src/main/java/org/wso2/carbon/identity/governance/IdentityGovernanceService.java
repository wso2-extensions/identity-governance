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

    /**
     * Store the configurations of a tenant in cache and database
     *
     * @param tenantDomain         Domain name of the tenant
     * @param configurationDetails Configurations belong to the tenant
     */
    void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails) throws
            IdentityGovernanceException;

    /**
     * Get the configurations of a tenant from cache or database
     *
     * @param tenantDomain Domain name of the tenant
     * @return Configurations belong to the tenant
     */
    Property[] getConfiguration(String tenantDomain) throws IdentityGovernanceException;

    /**
     * @param propertyNames property names
     * @param tenantDomain  tenant domain of the connectors
     * @return connector configurations of the tenant
     * @throws IdentityGovernanceException
     */
    Property[] getConfiguration(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException;

    /**
     * Get the governance connector list.
     *
     * @return connector list
     * @throws IdentityGovernanceException
     */
    List<IdentityConnectorConfig> getConnectorList() throws IdentityGovernanceException;

    /**
     * Get the connector list with the current configurations populated.
     *
     * @param tenantDomain tenant domain of the connectors
     * @return connector configurations of the tenant
     * @throws IdentityGovernanceException
     */
    List<ConnectorConfig> getConnectorListWithConfigs(String tenantDomain) throws IdentityGovernanceException;

    /**
     * Get the connector list grouped by the connector category with the current configurations populated.
     *
     * @param tenantDomain tenant domain of the categories
     * @return connector categories configurations of the tenant
     * @throws IdentityGovernanceException
     */
    Map<String, List<ConnectorConfig>> getCategorizedConnectorListWithConfigs(String tenantDomain)
            throws IdentityGovernanceException;

    /**
     * Get the connector list of a category with the current configurations populated.
     *
     * @param tenantDomain tenant domain of the connector category
     * @param category     name of the connector category
     * @return connector category configurations of the tenant
     * @throws IdentityGovernanceException
     */
    List<ConnectorConfig> getConnectorListWithConfigsByCategory(String tenantDomain,
                                                                String category) throws IdentityGovernanceException;

    /**
     * Get the connector with the current configurations populated.
     *
     * @param tenantDomain  tenant domain of the connector configuration
     * @param connectorName name of the connector
     * @return connector configurations of the tenant
     * @throws IdentityGovernanceException
     */
    ConnectorConfig getConnectorWithConfigs(String tenantDomain,
                                            String connectorName) throws IdentityGovernanceException;
}
