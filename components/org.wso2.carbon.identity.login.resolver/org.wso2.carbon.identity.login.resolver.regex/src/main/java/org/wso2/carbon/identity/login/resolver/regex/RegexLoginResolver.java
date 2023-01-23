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

package org.wso2.carbon.identity.login.resolver.regex;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.identity.login.resolver.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.login.resolver.regex.utils.UserResolverUtil;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.common.AuthenticationResult;
import org.wso2.carbon.user.core.common.User;
import org.wso2.carbon.user.core.constants.UserCoreClaimConstants;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.List;
import java.util.regex.Pattern;

/**
 * This class defines the default implementation of the LoginResolver. With the help of this login resolver, users will
 * get resolved using a claim regex pattern defined in the management console.
 */
public class RegexLoginResolver implements LoginResolver {

    private static final Log log = LogFactory.getLog(RegexLoginResolver.class);

    /**
     * Resolves a user from the given login identifier and then returns the resolved claim URI and the user details if
     * a matching user exists.
     *
     * @param loginIdentifier   The user login identifier input which is provided by the user during login.
     * @param allowedAttributes The list of claim URIs which are allowed by admin for resolving the user.
     * @param tenantDomain      The tenant domain of the user.
     * @return A ResolvedUserResult object with the user details and the user resolved claim details.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifier, List<String> allowedAttributes, String tenantDomain) {

        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        try {
            if (allowedAttributes == null) {
                return resolvedUserResult;
            }
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
                String domainSeparateAttribute = UserCoreUtil.removeDomainFromName(loginIdentifier);
                if (pattern.matcher(domainSeparateAttribute).matches()) {
                    setResolvedUserResult(userStoreManager, claimURI, loginIdentifier, resolvedUserResult, claim);
                    break;
                }
            }
            /*
            resolve user if allowed attributes has only username claim,
            but username claim has no configured regex pattern.
             */
            if (allowedAttributes.size() == 1 &&
                    allowedAttributes.contains(UserCoreClaimConstants.USERNAME_CLAIM_URI)) {
                setResolvedUserResult(userStoreManager, UserCoreClaimConstants.USERNAME_CLAIM_URI, loginIdentifier,
                        resolvedUserResult, claimManager.getClaim(UserCoreClaimConstants.USERNAME_CLAIM_URI));
            }
        } catch (UserStoreException e) {
            log.error("An error occurred while resolving user.", e);
        }
        return resolvedUserResult;
    }

    /**
     * Resolves a user from the given login identifier + login hint and then returns the resolved claim URI and the
     * user details if a matching user exists.
     *
     * @param loginIdentifier   The user login identifier input which is provided by the user during login.
     * @param allowedAttributes The list of claim URIs which are allowed by admin for resolving the user.
     * @param tenantDomain      The tenant domain of the user.
     * @param hint              The login attribute claim hint.
     * @return ResolvedUserResult object with user details and resolved claim details.
     */
    @Override
    public ResolvedUserResult resolveUser(String loginIdentifier, List<String> allowedAttributes, String tenantDomain,
                                          String hint) {

        log.warn("User resolver with hint is not yet implemented. Proceeding without the hint.");
        return resolveUser(loginIdentifier, allowedAttributes, tenantDomain);
    }

    /**
     * Authenticates with the user login identifier. Resolves the user by matching the login identifier and the user
     * credentials.
     *
     * @param loginIdentifier   The user login identifier input which is provided by the user during login.
     * @param allowedAttributes The list of claim URIs which are allowed by admin for resolving the user.
     * @param credential        The credentials of the user.
     * @param tenantDomain      The tenant domain of the user.
     * @return An AuthenticationResult object with SUCCESS authentication status if the user credentials are correct
     * otherwise returns an AuthenticationResult with FAIL authentication status.
     */
    @Override
    public AuthenticationResult authenticateWithIdentifier(String loginIdentifier, List<String> allowedAttributes,
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
                if (pattern.matcher(loginIdentifier).matches()) {
                    authenticationResult = userStoreManager.
                            authenticateWithID(claimURI, loginIdentifier, credential, StringUtils.EMPTY);
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
                        userStoreManager.authenticateWithID(UserCoreClaimConstants.USERNAME_CLAIM_URI, loginIdentifier,
                                credential, StringUtils.EMPTY);
            }
        } catch (UserStoreException e) {
            log.error("An error occurred while resolving authentication result.", e);
        }
        return authenticationResult;
    }

    /**
     * Sets the resolved user result object with the user details if a valid user is found.
     *
     * @param userStoreManager   The user store manager.
     * @param claimURI           The claim URI which should be used to extract the user.
     * @param loginIdentifier    The login identifier provided by the user.
     * @param resolvedUserResult The dummy ResolvedUserResult object for which the user details should be added to.
     * @param claim              The claim object.
     * @throws org.wso2.carbon.user.core.UserStoreException If there is an error while extracting a list of users with
     *                                                      the ID.
     */
    private void setResolvedUserResult(UniqueIDUserStoreManager userStoreManager, String claimURI,
                                       String loginIdentifier, ResolvedUserResult resolvedUserResult, Claim claim)
            throws org.wso2.carbon.user.core.UserStoreException {

        List<User> userList = userStoreManager.getUserListWithID(claimURI, loginIdentifier, null);
        if (userList.size() == 1) {
            resolvedUserResult.setResolvedStatus(ResolvedUserResult.UserResolvedStatus.SUCCESS);
            resolvedUserResult.setResolvedClaim(claimURI);
            resolvedUserResult.setResolvedValue(loginIdentifier);
            User user = userList.get(0);
            user.setUsername(user.getDomainQualifiedUsername());
            resolvedUserResult.setUser(user);
        } else if (userList.size() > 1) {
            resolvedUserResult.setErrorMessage("Found multiple users for " + claim.getDisplayTag() +
                    " to value " + loginIdentifier);
        }
    }
}
