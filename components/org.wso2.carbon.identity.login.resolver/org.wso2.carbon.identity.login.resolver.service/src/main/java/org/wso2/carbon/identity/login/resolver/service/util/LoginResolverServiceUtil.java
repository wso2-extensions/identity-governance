/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.login.resolver.service.util;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.login.resolver.service.internal.LoginResolverServiceDataHolder;

/**
 * The utility class that is used within the login resolver service module.
 */
public class LoginResolverServiceUtil {

    /**
     * Private constructor to restrict from creating multiple instances of the LoginResolverServiceUtil class.
     */
    private LoginResolverServiceUtil() {

    }

    /**
     * Retrieves the connector configuration based on the provided key and the tenant domain.
     *
     * @param key          The key which is used to retrieve the connector configuration for.
     * @param tenantDomain The tenant domain which is used to retrieve the connector configuration for.
     * @return The connector configuration that corresponds to the key and the tenant domain. An empty string if there
     * is no matching connector configuration.
     * @throws IdentityEventException If there is an error while retrieving the connector configuration.
     */
    public static String getConnectorConfig(String key, String tenantDomain) throws IdentityEventException {

        try {
            Property[] connectorConfigs;
            IdentityGovernanceService identityGovernanceService = LoginResolverServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            if (identityGovernanceService != null) {
                connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, tenantDomain);
                if (connectorConfigs != null && connectorConfigs.length > 0) {
                    return connectorConfigs[0].getValue();
                }
            }
            return StringUtils.EMPTY;
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException(String.format("An error occurred while getting the connector " +
                    "configurations for the property : %s", key), e);
        }
    }
}
