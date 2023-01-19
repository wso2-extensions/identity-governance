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

package org.wso2.carbon.identity.login.resolver.service;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolverService;
import org.wso2.carbon.identity.login.resolver.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.login.resolver.service.constants.LoginResolverServiceConstants;
import org.wso2.carbon.identity.login.resolver.service.util.LoginResolverServiceUtil;
import org.wso2.carbon.user.core.common.AuthenticationResult;

import java.util.List;

/**
 * Provides the services which are needed for the login resolver functionalities.
 */
public class LoginResolverServiceImpl implements LoginResolverService {

    private static final Log log = LogFactory.getLog(LoginResolverServiceImpl.class);

    /**
     * This method is used to determine whether the login resolver feature is enabled or not.
     *
     * @param tenantDomain The tenant domain of the user.
     * @return True if the login resolver feature is enabled for given tenant, false if else.
     */
    @Override
    public boolean isEnabled(String tenantDomain) {

        if (StringUtils.isNotBlank(tenantDomain)) {
            try {
                return Boolean.parseBoolean(LoginResolverServiceUtil.getConnectorConfig(LoginResolverServiceConstants
                        .LOGIN_RESOLVER_PROPERTY, tenantDomain));
            } catch (IdentityEventException e) {
                log.error("An error occurred while retrieving login resolver property.", e);
            }
        }
        return false;
    }

    /**
     * Authenticates with the user login identifier. Resolves the user by matching the login identifier and the user
     * credentials.
     *
     * @param loginIdentifier The user login identifier input which is provided by the user during login.
     * @param credential      The credentials of the user.
     * @param tenantDomain    The tenant domain of the user.
     * @return An AuthenticationResult object with SUCCESS authentication status if the user credentials are correct
     * otherwise returns an AuthenticationResult with FAIL authentication status.
     */
    @Override
    public AuthenticationResult authenticateWithIdentifier(String loginIdentifier, Object credential,
                                                           String tenantDomain) {

        AuthenticationResult authenticationResult =
                new AuthenticationResult(AuthenticationResult.AuthenticationStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifier) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedAttributes = LoginResolverServiceUtil.getAllowedClaimsForTenant(tenantDomain);
            LoginResolver loginResolver = LoginResolverServiceUtil.getSelectedLoginResolver(tenantDomain);
            if (loginResolver != null) {
                authenticationResult = loginResolver.authenticateWithIdentifier(loginIdentifier, allowedAttributes,
                        credential, tenantDomain);
            }
        }
        return authenticationResult;
    }

    /**
     * Resolves a user from the given login identifier and then returns the resolved claim URI and the user details if
     * a matching user exists.
     *
     * @param loginIdentifier The user login identifier input which is provided by the user during login.
     * @param tenantDomain    The tenant domain of the user.
     * @return A ResolvedUserResult object with the user details and the user resolved claim details.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifier, String tenantDomain) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifier) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedAttributes = LoginResolverServiceUtil.getAllowedClaimsForTenant(tenantDomain);
            LoginResolver loginResolver = LoginResolverServiceUtil.getSelectedLoginResolver(tenantDomain);
            if (loginResolver != null) {
                resolvedUserResult = loginResolver.resolveUser(loginIdentifier, allowedAttributes, tenantDomain);
            }
        }
        return resolvedUserResult;
    }

    /**
     * Resolves a user from the given login identifier + login hint and then returns the resolved claim URI and the
     * user details if a matching user exists.
     *
     * @param loginIdentifier The user login identifier input which is provided by the user during login.
     * @param tenantDomain    The tenant domain of the user.
     * @param hint            The login attribute claim hint.
     * @return ResolvedUserResult object with user details and resolved claim details.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifier, String tenantDomain, String hint) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifier) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedClaimList = LoginResolverServiceUtil.getAllowedClaimsForTenant(tenantDomain);
            LoginResolver loginResolver = LoginResolverServiceUtil.getSelectedLoginResolver(tenantDomain);
            if (loginResolver != null) {
                resolvedUserResult = loginResolver.resolveUser(loginIdentifier, allowedClaimList, tenantDomain, hint);
            }
        }
        return resolvedUserResult;
    }
}
