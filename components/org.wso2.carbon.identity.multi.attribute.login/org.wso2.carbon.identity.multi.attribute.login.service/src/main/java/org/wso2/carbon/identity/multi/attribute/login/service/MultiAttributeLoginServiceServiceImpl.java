/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.multi.attribute.login.service;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.multi.attribute.login.constants.MultiAttributeLoginConstants;
import org.wso2.carbon.identity.multi.attribute.login.internal.MultiAttributeLoginDataHolder;
import org.wso2.carbon.identity.multi.attribute.login.internal.MultiAttributeLoginServiceComponent;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginService;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.multi.attribute.login.utill.MultiAttributeLoginUtil;
import org.wso2.carbon.user.core.common.AuthenticationResult;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This service provides the services needed to multi attribute login.
 */
public class MultiAttributeLoginServiceServiceImpl implements MultiAttributeLoginService {

    private static final Log log = LogFactory.getLog(MultiAttributeLoginServiceServiceImpl.class);

    /**
     * This method is used to determine whether the multi attribute login feature is enable or disable.
     *
     * @param tenantDomain User tenant domain.
     * @return True if multi attribute login is enabled for given tenant, otherwise return false.
     */
    @Override
    public boolean isEnabled(String tenantDomain) {

        if (StringUtils.isNotBlank(tenantDomain)) {
            try {
                return Boolean.parseBoolean(MultiAttributeLoginUtil.getConnectorConfig(MultiAttributeLoginConstants
                        .MULTI_ATTRIBUTE_LOGIN_PROPERTY, tenantDomain));
            } catch (IdentityEventException e) {
                log.error("Error occurred while retrieving multi attribute login property.", e);
            }
        }
        return false;
    }

    /**
     * This method is used to get list of claim URIs which are enable for multi attribute login on given tenant domain.
     *
     * @param tenantDomain User tenant domain.
     * @return Multi attribute login enabled claim URI list.
     */
    public List<String> getAllowedClaimsForTenant(String tenantDomain) {

        List<String> allowedClaimsList = new ArrayList<>();
        if (StringUtils.isNotBlank(tenantDomain)) {
            try {
                String claimList = MultiAttributeLoginUtil.
                        getConnectorConfig(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES, tenantDomain);
                if (StringUtils.isNotBlank(claimList)) {
                    claimList = StringUtils.deleteWhitespace(claimList);
                    allowedClaimsList = Arrays.asList(claimList.split(","));
                }

            } catch (IdentityEventException e) {
                log.error("Error occurred while retrieving allowed login claims.", e);
            }
        }
        if (allowedClaimsList.isEmpty()) {
            allowedClaimsList.add(MultiAttributeLoginConstants.USERNAME_CLAIM_URI);
        }
        return allowedClaimsList;
    }

    /**
     * This method is used to authenticate user using multi attribute login identifier.
     *
     * @param loginIdentifierValue User entered login identifier.
     * @param credential           User credential.
     * @param tenantDomain         User tenant domain.
     * @return AuthenticationResult.
     */
    @Override
    public AuthenticationResult authenticateWithIdentifier(String loginIdentifierValue, Object credential,
                                                           String tenantDomain) {

        AuthenticationResult authenticationResult =
                new AuthenticationResult(AuthenticationResult.AuthenticationStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifierValue) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedAttributes = getAllowedClaimsForTenant(tenantDomain);
            authenticationResult = MultiAttributeLoginDataHolder.getInstance().getMultiAttributeLoginResolver().
                    authenticateWithIdentifier(loginIdentifierValue, allowedAttributes, credential, tenantDomain);
        }
        return authenticationResult;
    }

    /**
     * This method is used to resolve user from given login identifier.
     *
     * @param loginIdentifierValue User entered login identifier value.
     * @param tenantDomain         User tenant domain.
     * @return ResolvedUserResult object with resolved user and resolved login identifier claim.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifierValue, String tenantDomain) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifierValue) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedAttributes = getAllowedClaimsForTenant(tenantDomain);
            resolvedUserResult = MultiAttributeLoginDataHolder.getInstance().getMultiAttributeLoginResolver().
                    resolveUser(loginIdentifierValue, allowedAttributes, tenantDomain);
        }
        return resolvedUserResult;
    }

    /**
     * This method is used to resolve user from given login identifier and hint.
     *
     * @param loginIdentifierValue Multi attribute login identifier value.
     * @param tenantDomain         User tenant domain.
     * @param hint                 Claim URI of the login attribute as a hint.
     * @return ResolvedUserResult object with resolved user and resolved login identifier claim.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifierValue, String tenantDomain, String hint) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        if (StringUtils.isNotBlank(loginIdentifierValue) && StringUtils.isNotBlank(tenantDomain)) {
            List<String> allowedClaimList = getAllowedClaimsForTenant(tenantDomain);
            resolvedUserResult = MultiAttributeLoginDataHolder.getInstance().getMultiAttributeLoginResolver().
                    resolveUser(loginIdentifierValue, allowedClaimList, tenantDomain, hint);
        }
        return resolvedUserResult;
    }
}
