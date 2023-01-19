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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.identity.login.resolver.service.constants.LoginResolverServiceConstants;
import org.wso2.carbon.identity.login.resolver.service.internal.LoginResolverServiceDataHolder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The utility class that is used within the login resolver service module.
 */
public class LoginResolverServiceUtil {

    private static final Log log = LogFactory.getLog(LoginResolverServiceUtil.class);

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

    /**
     * Retrieves the list of claim URIs which are enabled for the login resolver on given tenant domain.
     *
     * @param tenantDomain The tenant domain of the user.
     * @return The claim list for which the login resolver is enabled.
     */
    public static List<String> getAllowedClaimsForTenant(String tenantDomain) {

        List<String> allowedClaimsList = new ArrayList<>();
        if (StringUtils.isNotBlank(tenantDomain)) {
            try {
                String claimList = LoginResolverServiceUtil.
                        getConnectorConfig(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES, tenantDomain);
                if (StringUtils.isNotBlank(claimList)) {
                    claimList = StringUtils.deleteWhitespace(claimList);
                    allowedClaimsList = Arrays.asList(claimList.split(","));
                }

            } catch (IdentityEventException e) {
                log.error(String.format("An error occurred while retrieving the allowed login claims for the tenant " +
                        "domain: %s.", tenantDomain), e);
            }
        }
        if (allowedClaimsList.isEmpty()) {
            allowedClaimsList.add(LoginResolverServiceConstants.USERNAME_CLAIM_URI);
        }
        return allowedClaimsList;
    }

    /**
     * Extracts the correct login resolver based on the resident IdP configuration related to the login resolver class.
     *
     * @param tenantDomain The tenant domain.
     * @return A login resolver if found, null if else.
     */
    public static LoginResolver getSelectedLoginResolver(String tenantDomain) {

        String selectedLoginResolverClass = StringUtils.EMPTY;
        try {
            selectedLoginResolverClass = getConnectorConfig(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                    tenantDomain);
        } catch (IdentityEventException e) {
            log.error("An error occurred while retrieving login resolver class property.", e);
        }

        List<LoginResolver> loginResolverList = LoginResolverServiceDataHolder.getInstance().getLoginResolverList();
        if (!loginResolverList.isEmpty()) {
            if (StringUtils.isBlank(selectedLoginResolverClass)) {
                selectedLoginResolverClass = LoginResolverServiceConstants.DEFAULT_LOGIN_RESOLVER_CLASS;
            }
            for (LoginResolver loginResolver : loginResolverList) {
                String loginResolverName = loginResolver.getClass().getCanonicalName();
                if (loginResolverName.equals(selectedLoginResolverClass)) {
                    return loginResolver;
                }
            }
        } else {
            if (log.isDebugEnabled()) {
                log.debug("There are no login resolvers registered to be used. This is an un-expected situation " +
                        "because even the default login resolver has not be registered.");
            }
            return null;
        }

        if (log.isDebugEnabled()) {
            log.debug(String.format("A login resolver with the provided login resolver class %s.",
                    selectedLoginResolverClass));
        }
        return null;
    }
}
