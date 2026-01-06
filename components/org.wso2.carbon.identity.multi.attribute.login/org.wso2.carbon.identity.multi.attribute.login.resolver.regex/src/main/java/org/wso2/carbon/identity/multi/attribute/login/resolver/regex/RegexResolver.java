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
import org.osgi.annotation.bundle.Capability;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.utils.UserResolverUtil;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.common.AuthenticationResult;
import org.wso2.carbon.user.core.common.IterativeUserStoreManager;
import org.wso2.carbon.user.core.common.User;
import org.wso2.carbon.user.core.config.UserStorePreferenceOrderSupplier;
import org.wso2.carbon.user.core.constants.UserCoreClaimConstants;
import org.wso2.carbon.user.core.model.UserMgtContext;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.Collections;
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
@Capability(
        namespace = "osgi.service",
        attribute = {
                "objectClass=org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver",
                "service.scope=singleton"
        }
)
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
        List<String> userStorePreferenceOrder  = getUserStorePreferenceOrder();

        // Resolve the user from the regex matching.
        for (String claimURI : allowedAttributes) {
            Claim claim = claimManager.getClaim(claimURI);
            if (claim == null || StringUtils.isBlank(claim.getRegEx())) {
                continue;
            }

            Pattern pattern = Pattern.compile(claim.getRegEx());
            String domainSeparateAttribute = UserCoreUtil.removeDomainFromName(loginAttribute);

            if (pattern.matcher(domainSeparateAttribute).matches()) {
                List<User> userList = getUserList(claimURI, loginAttribute, userStorePreferenceOrder, userStoreManager);

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

        // Check the users from username by default if there is no regex for username claim.
        Claim usernameClaim = claimManager.getClaim(UserCoreClaimConstants.USERNAME_CLAIM_URI);
        if (allowedAttributes.contains(UserCoreClaimConstants.USERNAME_CLAIM_URI)
                && StringUtils.isBlank(usernameClaim.getRegEx())) {
            List<User> userList = getUserList(UserCoreClaimConstants.USERNAME_CLAIM_URI, loginAttribute,
                    userStorePreferenceOrder, userStoreManager);
            if (!userList.isEmpty()) {
                List<User> allowedDistinctUsersForClaim = userList.stream()
                        .filter(user -> uniqueUserIds.add(user.getUserID()))
                        .collect(Collectors.toList());
                if (allowedDistinctUsersForClaim.size() == 1) {
                    distinctUsers.put(UserCoreClaimConstants.USERNAME_CLAIM_URI, allowedDistinctUsersForClaim);
                }
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
        } else {
            resolvedUserResult.setErrorMessage("Found multiple users for " + allowedAttributes +
                    " to value " + loginAttribute);
        }
    }

    /**
     * This method is used to get the user list according to the user store preference order if configured.
     * If the login attribute contains a domain name, resolve users from the corresponding user store.
     *
     * @param claimURI                 Claim URI.
     * @param loginAttribute           Login attribute.
     * @param userStorePreferenceOrder User store preference order.
     * @param userStoreManager         User store manager.
     * @return User list.
     * @throws org.wso2.carbon.user.core.UserStoreException If an error occurred while getting the user list.
     */
    private List<User> getUserList(String claimURI, String loginAttribute, List<String> userStorePreferenceOrder,
                                   UniqueIDUserStoreManager userStoreManager)
            throws org.wso2.carbon.user.core.UserStoreException {

        if (!loginAttribute.contains(UserCoreConstants.DOMAIN_SEPARATOR) && userStorePreferenceOrder != null
                && !userStorePreferenceOrder.isEmpty()) {
            IterativeUserStoreManager iterativeUserStoreManager = generateUserStoreChain(userStorePreferenceOrder,
                    (AbstractUserStoreManager) userStoreManager);
            if (iterativeUserStoreManager != null) {
                return getUserListAccordingToUserStorePreferenceOrder(claimURI, loginAttribute,
                        iterativeUserStoreManager);
            }
        }
        return userStoreManager.getUserListWithID(claimURI, loginAttribute, null);
    }

    /**
     * This method is used to get the user list according to the user store preference order.
     *
     * @param claimURI                 Claim URI.
     * @param loginAttribute           Login attribute.
     * @param userStoreManager         User store manager.
     * @return User list.
     * @throws org.wso2.carbon.user.core.UserStoreException If an error occurred while getting the user list.
     */
    private List<User> getUserListAccordingToUserStorePreferenceOrder(String claimURI, String loginAttribute,
                                                                      IterativeUserStoreManager userStoreManager)
            throws org.wso2.carbon.user.core.UserStoreException {

        List<org.wso2.carbon.user.core.common.User> userList = new ArrayList<>();
        IterativeUserStoreManager currentUserStoreManager = userStoreManager;
        while (currentUserStoreManager != null) {
            String domainName = UserCoreUtil.getDomainName(currentUserStoreManager.getRealmConfiguration());
            String domainAwareUsername = domainName + CarbonConstants.DOMAIN_SEPARATOR + loginAttribute;
            userList.addAll(currentUserStoreManager.getAbstractUserStoreManager().getUserListWithID(claimURI,
                    domainAwareUsername, null));
            currentUserStoreManager = currentUserStoreManager.nextUserStoreManager();
        }

        return userList;
    }

    /**
     * This method is used to get the user store preference order.
     *
     * @return User store preference order.
     * @throws org.wso2.carbon.user.core.UserStoreException If an error occurred while getting the user store
     */
    private List<String> getUserStorePreferenceOrder() throws org.wso2.carbon.user.core.UserStoreException {

        UserMgtContext userMgtContext = UserCoreUtil.getUserMgtContextFromThreadLocal();
        if (userMgtContext != null) {
            UserStorePreferenceOrderSupplier<List<String>>
                    userStorePreferenceSupplier = userMgtContext.getUserStorePreferenceOrderSupplier();
            if (userStorePreferenceSupplier != null) {
                List<String> userStorePreferenceOrder = userStorePreferenceSupplier.get();
                if (userStorePreferenceOrder != null) {
                    return userStorePreferenceOrder;
                }
            }
        }

        return Collections.emptyList();
    }

    /**
     * This method is used to generate a user store chain using the user store preference order.
     *
     * @param userStorePreferenceOrder User store preference order.
     * @param abstractUserStoreManager Abstract user store manager.
     * @return IterativeUserStoreManager.
     * @throws org.wso2.carbon.user.core.UserStoreException If an error occurred while generating the user store chain.
     */
    private IterativeUserStoreManager generateUserStoreChain(List<String> userStorePreferenceOrder,
                                                             AbstractUserStoreManager abstractUserStoreManager)
            throws org.wso2.carbon.user.core.UserStoreException {

        IterativeUserStoreManager initialUserStoreManager = null;
        IterativeUserStoreManager currentUserStoreManager = null;
        for (String domainName : userStorePreferenceOrder) {
            UserStoreManager userStoreManager = abstractUserStoreManager.getSecondaryUserStoreManager(domainName);
            // If the user store manager is instance of AbstractUserStoreManager then generate a user store chain using
            // IterativeUserStoreManager.
            if (userStoreManager instanceof AbstractUserStoreManager) {
                if (initialUserStoreManager == null) {
                    currentUserStoreManager =
                            new IterativeUserStoreManager((AbstractUserStoreManager) userStoreManager);
                    initialUserStoreManager = currentUserStoreManager;
                } else {
                    IterativeUserStoreManager nextUserStoreManager = new IterativeUserStoreManager(
                            (AbstractUserStoreManager) userStoreManager);
                    currentUserStoreManager.setNextUserStoreManager(nextUserStoreManager);
                    currentUserStoreManager = nextUserStoreManager;
                }
            } else {
                return null;
            }
        }
        // Authenticate using the initial user store from the user store preference list.
        return initialUserStoreManager;
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
