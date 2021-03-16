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

package org.wso2.carbon.identity.multi.attribute.login.resolver.regex;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.utils.UserResolverUtil;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.common.AuthenticationResult;
import org.wso2.carbon.user.core.common.User;

import java.util.List;
import java.util.regex.Pattern;

/**
 * This class is used to implement MultiAttributeLoginResolver. In this class, users will be resolved using a regex
 * pattern.
 */
public class RegexResolver implements MultiAttributeLoginResolver {

    private static final Log log = LogFactory.getLog(RegexResolver.class);

    @Override
    public ResolvedUserResult resolveUser(String loginAttribute, List<String> allowedAttributes, String tenantDomain) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        try {
            UserRealm userRealm = UserResolverUtil.getUserRealm(tenantDomain);
            UniqueIDUserStoreManager userStoreManager = UserResolverUtil.getUserStoreManager(tenantDomain);
            ClaimManager claimManager = userRealm.getClaimManager();
            for (String claimURI : allowedAttributes) {
                Claim claim = claimManager.getClaim(claimURI);
                if (claim == null) {
                    continue;
                }
                String regex = claim.getRegEx();
                if (StringUtils.isBlank(regex)) {
                    continue;
                }
                Pattern pattern = Pattern.compile(regex);
                if (pattern.matcher(loginAttribute).matches()) {
                    List<User> userList = userStoreManager.getUserListWithID(claimURI, loginAttribute, null);
                    if (userList.size() == 1) {
                        resolvedUserResult.setResolvedStatus(ResolvedUserResult.UserResolvedStatus.SUCCESS);
                        resolvedUserResult.setResolvedClaim(claimURI);
                        resolvedUserResult.setResolvedValue(loginAttribute);
                        resolvedUserResult.setUser(userList.get(0));
                        break;
                    } else if (userList.size() > 1) {
                        resolvedUserResult.setResolvedStatus(ResolvedUserResult.UserResolvedStatus.FAIL);
                        resolvedUserResult.setErrorMessage("Found multiple users for " + claim.getDisplayTag() +
                                " to value " + loginAttribute);
                        break;
                    }
                }
            }
        } catch (UserStoreException e) {
            log.error("Error occurred while resolving user name", e);
        }
        return resolvedUserResult;
    }

    @Override
    public ResolvedUserResult resolveUser(String loginAttribute, List<String> allowedAttributes, String tenantDomain,
                                          String hint) {

        log.warn("User resolver with hint is not yet implemented. Proceeding without the hint.");
        return resolveUser(loginAttribute, allowedAttributes, tenantDomain);
    }

    @Override
    public AuthenticationResult authenticateWithIdentifier(String loginAttributeValue, List<String> allowedAttributes,
                                                           Object credential, String tenantDomain) {

        AuthenticationResult authenticationResult =
                new AuthenticationResult(AuthenticationResult.AuthenticationStatus.FAIL);
        ClaimManager claimManager;
        try {
            UserRealm userRealm = UserResolverUtil.getUserRealm(tenantDomain);
            UniqueIDUserStoreManager userStoreManager = UserResolverUtil.getUserStoreManager(tenantDomain);
            claimManager = userRealm.getClaimManager();
            for (String claimURI : allowedAttributes) {
                Claim claim = claimManager.getClaim(claimURI);
                if (claim == null) {
                    continue;
                }
                String regex = claim.getRegEx();
                if (StringUtils.isBlank(regex)) {
                    continue;
                }
                Pattern pattern = Pattern.compile(regex);
                if (pattern.matcher(loginAttributeValue).matches()) {
                    authenticationResult = userStoreManager.
                            authenticateWithID(claimURI, loginAttributeValue, credential, StringUtils.EMPTY);
                    if (AuthenticationResult.AuthenticationStatus.SUCCESS.
                            equals(authenticationResult.getAuthenticationStatus())) {
                        break;
                    }
                }
            }
        } catch (UserStoreException e) {
            log.error("Error occurred while resolving authenticationResult", e);
        }
        return authenticationResult;
    }
}
