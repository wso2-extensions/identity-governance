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
import org.wso2.carbon.user.core.constants.UserCoreClaimConstants;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
            if (allowedAttributes == null) {
                return resolvedUserResult;
            }
            UserRealm userRealm = UserResolverUtil.getUserRealm(tenantDomain);
            UniqueIDUserStoreManager userStoreManager = UserResolverUtil.getUserStoreManager(tenantDomain);
            ClaimManager claimManager = userRealm.getClaimManager();

            resolveDistinctUsersForClaims(loginAttribute, allowedAttributes, claimManager, userStoreManager,
                    resolvedUserResult);

        } catch (UserStoreException e) {
            log.error("Error occurred while resolving user name", e);
        }
        return resolvedUserResult;
    }

    private void resolveDistinctUsersForClaims(String loginAttribute, List<String> allowedAttributes,
                                               ClaimManager claimManager,
                                               UniqueIDUserStoreManager userStoreManager,
                                               ResolvedUserResult resolvedUserResult)
            throws UserStoreException {

        Set<String> uniqueUserIds = new HashSet<>();
        Map<String, List<User>> distinctUsers = new HashMap<>();

        for (String claimURI : allowedAttributes) {
            Claim claim = claimManager.getClaim(claimURI);
            if (claim == null || StringUtils.isBlank(claim.getRegEx())) {
                continue;
            }

            Pattern pattern = Pattern.compile(claim.getRegEx());
            String domainSeparateAttribute = UserCoreUtil.removeDomainFromName(loginAttribute);

            if (pattern.matcher(domainSeparateAttribute).matches()) {
                List<User> userList = userStoreManager.getUserListWithID(claimURI, loginAttribute, null);
                if (userList.isEmpty()) {
                    continue;
                }
                /*
                This is to make sure that the same user is not added to the list multiple times from different claims.
                */
                List<User> allowedDistinctUsersForClaim = userList.stream()
                        .filter(user -> uniqueUserIds.add(user.getUserID()))
                        .collect(Collectors.toList());

                if (allowedDistinctUsersForClaim.size() == 1) {
                    /*
                    If the disctinctUsers map already contains a record that means multiple users has been resolved
                    for the login identifier from different claims. Hence, terminate the iteration and set the error
                    message.
                    */
                    if (distinctUsers.size() > 0) {
                        resolvedUserResult.setErrorMessage("Found multiple users for " + allowedAttributes +
                                " to value " + loginAttribute);
                        return;
                    } else {
                        distinctUsers.put(claimURI, allowedDistinctUsersForClaim);
                    }
                    /*
                    If the allowedDistinctUsersForClaim size is greater than 1, that means multiple users has been
                    resolved for the same login identifier from the same claim. Hence, terminate the iteration and set
                    the error message.
                    */
                } else if (allowedDistinctUsersForClaim.size() > 1) {
                    resolvedUserResult.setErrorMessage("Found multiple users for " + claim.getDisplayTag() +
                            " to value " + loginAttribute);
                    return;
                }
            }
        }

        // Check the users from username by default if there are users found from claims and regex patterns.
        if (distinctUsers.size() == 0) {
            if (allowedAttributes.contains(UserCoreClaimConstants.USERNAME_CLAIM_URI)) {
                List<User> userList = userStoreManager.getUserListWithID(UserCoreClaimConstants.USERNAME_CLAIM_URI,
                        loginAttribute, null);
                distinctUsers.put(UserCoreClaimConstants.USERNAME_CLAIM_URI, userList);
            }
        }

        /*
        At this point distinctUsers map will contain only one entry if the login identifier is resolved to a single
        user from multiple claims. If the map is empty, that means the login identifier is not resolved to any user.
        */
        if (distinctUsers.size() == 1) {
            Map.Entry<String, List<User>> entry = distinctUsers.entrySet().iterator().next();
            setResolvedUserResult(entry.getValue(), entry.getKey(), loginAttribute, resolvedUserResult,
                    claimManager.getClaim(entry.getKey()));
        }
    }

    private void setResolvedUserResult(List<User> userList, String claimURI,
                                       String loginAttribute, ResolvedUserResult resolvedUserResult, Claim claim)
            throws org.wso2.carbon.user.core.UserStoreException {

        if (userList.size() == 1) {
            resolvedUserResult.setResolvedStatus(ResolvedUserResult.UserResolvedStatus.SUCCESS);
            resolvedUserResult.setResolvedClaim(claimURI);
            resolvedUserResult.setResolvedValue(loginAttribute);
            User user = userList.get(0);
            user.setUsername(user.getDomainQualifiedUsername());
            resolvedUserResult.setUser(user);
        } else if (userList.size() > 1) {
            resolvedUserResult.setErrorMessage("Found multiple users for " + claim.getDisplayTag() +
                    " to value " + loginAttribute);
        }
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
            if (allowedAttributes == null) {
                return authenticationResult;
            }
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
            /*
            If allowed attributes has only username claim, get authenticationResult even if
            the username claim has no configured regex pattern.
             */
            if (allowedAttributes.size() == 1 && allowedAttributes.contains(
                    UserCoreClaimConstants.USERNAME_CLAIM_URI)) {
                authenticationResult =
                        userStoreManager.authenticateWithID(UserCoreClaimConstants.USERNAME_CLAIM_URI,
                                loginAttributeValue, credential, StringUtils.EMPTY);
            }
        } catch (UserStoreException e) {
            log.error("Error occurred while resolving authenticationResult", e);
        }
        return authenticationResult;
    }
}
