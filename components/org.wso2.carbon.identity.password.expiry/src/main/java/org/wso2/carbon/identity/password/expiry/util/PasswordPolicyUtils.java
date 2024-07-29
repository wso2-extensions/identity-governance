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

package org.wso2.carbon.identity.password.expiry.util;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.apache.commons.collections.CollectionUtils;
import org.wso2.carbon.identity.application.authentication.framework.exception.UserIdNotFoundException;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.ServiceURLBuilder;
import org.wso2.carbon.identity.core.URLBuilderException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.password.expiry.models.OperatorEnum;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRule;
import org.wso2.carbon.identity.role.v2.mgt.core.RoleManagementService;
import org.wso2.carbon.identity.role.v2.mgt.core.exception.IdentityRoleManagementException;
import org.wso2.carbon.identity.role.v2.mgt.core.model.RoleBasicInfo;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.APPLICATION_AUDIENCE;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.CONNECTOR_CONFIG_NAME;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.GROUPS_CLAIM;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.ORGANIZATION_AUDIENCE;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.PASSWORD_RESET_PAGE;

/**
 * Utilities for password change enforcing.
 */
public class PasswordPolicyUtils {

    private static final Log log = LogFactory.getLog(PasswordPolicyUtils.class);

    /**
     * Get the property names required by the password expiry policy.
     *
     * @return The password expiry policy.
     */
    public static String[] getPasswordExpiryPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY);
        properties.add(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
        return properties.toArray(new String[0]);
    }

    /**
     * Get password expiry config related to the given key.
     *
     * @param tenantDomain Tenant domain.
     * @param key          Password  expiry config key.
     * @return Value associated with the given config key.
     * @throws IdentityGovernanceException If an error occurred while getting th config value.
     */
    public static String getPasswordExpiryConfig(String tenantDomain, String key) throws IdentityGovernanceException {

        Property[] connectorConfigs;
        IdentityGovernanceService governanceService =
                EnforcePasswordResetComponentDataHolder.getInstance().getIdentityGovernanceService();
        connectorConfigs = governanceService.getConfiguration(new String[]{key}, tenantDomain);
        return connectorConfigs[0].getValue();
    }

    /**
     * Get password expiry rules.
     *
     * @param tenantDomain Tenant domain.
     * @return List of password expiry rules.
     * @throws PostAuthenticationFailedException If an error occurred while getting the password expiry rules.
     */
    public static List<PasswordExpiryRule> getPasswordExpiryRules(String tenantDomain)
            throws PostAuthenticationFailedException {

        List<PasswordExpiryRule> passwordExpiryRules = new ArrayList<>();
        try {
            IdentityGovernanceService governanceService =
                    EnforcePasswordResetComponentDataHolder.getInstance().getIdentityGovernanceService();
            ConnectorConfig connectorConfig =
                    governanceService.getConnectorWithConfigs(tenantDomain, CONNECTOR_CONFIG_NAME);
            if (connectorConfig == null) {
                return passwordExpiryRules;
            }
            Property[] properties = connectorConfig.getProperties();
            if (properties == null) {
                return passwordExpiryRules;
            }

            for (Property property : properties) {
                if (StringUtils.startsWith(property.getName(), PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX)) {
                    try {
                        PasswordExpiryRule passwordExpiryRule = new PasswordExpiryRule(property.getValue());
                        passwordExpiryRules.add(passwordExpiryRule);
                    } catch (Exception e) {
                        log.error("Error while parsing password expiry rule.", e);
                    }
                }
            }
        } catch (IdentityGovernanceException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getMessage());
        }
        return passwordExpiryRules;
    }

    /**
     * This method checks if the password has expired.
     *
     * @param tenantDomain        The tenant domain of the user trying to authenticate.
     * @param tenantAwareUsername The tenant aware username of the user trying to authenticate.
     * @return true if the password had expired.
     * @throws PostAuthenticationFailedException If an error occurred while checking the password expiry.
     */
    public static boolean isPasswordExpired(String tenantDomain, String tenantAwareUsername)
            throws PostAuthenticationFailedException {

        UserRealm userRealm = getUserRealm(tenantDomain);
        UserStoreManager userStoreManager = getUserStoreManager(userRealm);
        String lastPasswordUpdatedTime = getLastPasswordUpdatedTime(tenantAwareUsername, userStoreManager, userRealm);
        long lastPasswordUpdatedTimeInMillis = getLastPasswordUpdatedTimeInMillis(lastPasswordUpdatedTime);
        int daysDifference = getDaysDifference(lastPasswordUpdatedTimeInMillis);

        // Getting the configured number of days before password expiry in days.
        int passwordExpiryInDays = getPasswordExpiryInDays(tenantDomain);
        return (daysDifference > passwordExpiryInDays || lastPasswordUpdatedTime == null);
    }

    /**
     * This method checks if the password has expired.
     *
     * @param tenantDomain      The tenant domain of the user trying to authenticate.
     * @param authenticatedUser The authenticated user object.
     * @return true if the password had expired.
     * @throws PostAuthenticationFailedException If an error occurred while checking the password expiry.
     */
    public static boolean isPasswordExpired(String tenantDomain, AuthenticatedUser authenticatedUser)
            throws PostAuthenticationFailedException {

        try {
            UserRealm userRealm = getUserRealm(tenantDomain);
            UserStoreManager userStoreManager = getUserStoreManager(userRealm);
            String tenantAwareUsername = authenticatedUser.getUserName();
            String userId = authenticatedUser.getUserId();
            String lastPasswordUpdatedTime =
                    getLastPasswordUpdatedTime(tenantAwareUsername, userStoreManager, userRealm);
            long lastPasswordUpdatedTimeInMillis = getLastPasswordUpdatedTimeInMillis(lastPasswordUpdatedTime);
            int daysDifference = getDaysDifference(lastPasswordUpdatedTimeInMillis);

            List<PasswordExpiryRule> passwordExpiryRules = getPasswordExpiryRules(tenantDomain);
            // Apply general policy if no rules given.
            if (CollectionUtils.isEmpty(passwordExpiryRules)) {
                return applyGeneralPasswordExpiry(tenantDomain, daysDifference, lastPasswordUpdatedTime);
            }

            for (PasswordExpiryRule rule : passwordExpiryRules) {
                if (isRuleApplicable(tenantDomain, tenantAwareUsername, userId, rule)) {
                    // Skip the rule if the operator is not equals.
                    if (OperatorEnum.NE.equals(rule.getOperator())) {
                        return false;
                    }
                    int expiryDays =
                            rule.getExpiryDays() > 0 ? rule.getExpiryDays() : getPasswordExpiryInDays(tenantDomain);
                    return daysDifference > expiryDays || lastPasswordUpdatedTime == null;
                }
            }
            // Apply general policy if no specific rule applies.
            return applyGeneralPasswordExpiry(tenantDomain, daysDifference, lastPasswordUpdatedTime);
        } catch (UserIdNotFoundException e) {
            log.error("Error while getting user id for the user.", e);
        }
        return false;
    }

    /**
     * Determines if a password expiry rule is applicable to a user.
     *
     * @param tenantDomain         The tenant domain.
     * @param tenantAwareUsername  The tenant-aware username.
     * @param userId               The user's ID.
     * @param passwordExpiryRule   The password expiry rule to check.
     * @return true if the rule is applicable, false otherwise.
     * @throws PostAuthenticationFailedException If an error occurs during the check.
     */
    public static boolean isRuleApplicable(String tenantDomain, String tenantAwareUsername, String userId,
                                            PasswordExpiryRule passwordExpiryRule)
            throws PostAuthenticationFailedException {

        List<String> ruleValues = passwordExpiryRule.getValues();
        switch (passwordExpiryRule.getAttribute()) {
            case ROLES:
                return doUserRolesMatchRule(tenantDomain, userId, ruleValues);
            case GROUPS:
                return doUserGroupsMatchRule(tenantDomain, tenantAwareUsername, ruleValues);
            default:
                return false;
        }
    }

    /**
     * Checks if the user's roles match the roles specified in the password expiry rule.
     *
     * @param tenantDomain The tenant domain.
     * @param userId       The user ID.
     * @param ruleRoles    The roles specified in the rule.
     * @return true if the user's roles match the rule's roles.
     * @throws PostAuthenticationFailedException If an error occurs while checking the user's roles.
     */
    private static boolean doUserRolesMatchRule(String tenantDomain, String userId, List<String> ruleRoles)
            throws PostAuthenticationFailedException {

        List<RoleBasicInfo> userRoles = getUserRoles(tenantDomain, userId);
        if (CollectionUtils.isEmpty(userRoles)) {
            return false;
        }

        Set<String> userRoleNames = new HashSet<>();
        for (RoleBasicInfo userRole : userRoles) {
            String fullRoleName = (APPLICATION_AUDIENCE.equals(userRole.getAudience())
                    ? userRole.getAudienceName() + "/" : "") + userRole.getName();
            userRoleNames.add(fullRoleName);
        }

        for (String ruleRole : ruleRoles) {
            if (!userRoleNames.contains(ruleRole)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if the user's groups match the rule's groups.
     *
     * @param tenantDomain        The tenant domain.
     * @param tenantAwareUsername The tenant aware username.
     * @param ruleGroups          The groups specified in the rule.
     * @return true if the user's groups match the rule's groups.
     * @throws PostAuthenticationFailedException If an error occurs while checking the user's groups.
     */
    private static boolean doUserGroupsMatchRule(String tenantDomain, String tenantAwareUsername,
                                                 List<String> ruleGroups) throws PostAuthenticationFailedException {

        String groupClaimValue = getUserClaimValue(tenantDomain, tenantAwareUsername, GROUPS_CLAIM);
        if (StringUtils.isBlank(groupClaimValue)) {
            return false;
        }

        Set<String> userGroups = new HashSet<>(Arrays.asList(groupClaimValue.split(",")));
        for (String ruleGroup : ruleGroups) {
            if (!userGroups.contains(ruleGroup.toLowerCase().trim())) {
                return false;
            }
        }
        return true;
    }

    /**
     * Apply general password expiry policy.
     *
     * @param tenantDomain            The tenant domain.
     * @param daysDifference          The number of days since the password was last updated.
     * @param lastPasswordUpdatedTime The last password updated time.
     * @return true if the password has expired.
     * @throws PostAuthenticationFailedException If an error occurs while applying the general password expiry policy.
     */
    private static boolean applyGeneralPasswordExpiry(String tenantDomain, int daysDifference,
                                                      String lastPasswordUpdatedTime)
            throws PostAuthenticationFailedException {

        return (daysDifference > getPasswordExpiryInDays(tenantDomain) || lastPasswordUpdatedTime == null);
    }

    /**
     * Get the roles of a given user.
     *
     * @param tenantDomain The tenant domain.
     * @param userId       The user ID.
     * @return             The roles of the user.
     * @throws PostAuthenticationFailedException If an error occurs while getting the user roles.
     */
    public static List<RoleBasicInfo> getUserRoles(String tenantDomain, String userId)
            throws PostAuthenticationFailedException {

        try {
            RoleManagementService roleManagementService = EnforcePasswordResetComponentDataHolder.getInstance()
                    .getRoleManagementService();
            return roleManagementService.getRoleListOfUser(userId, tenantDomain);
        } catch (IdentityRoleManagementException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_RETRIEVING_USER_ROLES.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_RETRIEVING_USER_ROLES.getMessage());
        }
    }

    /**
     * Get the claim value of a given Claim URI for a given user.
     *
     * @param tenantDomain        The tenant domain.
     * @param tenantAwareUsername The tenant aware username.
     * @param claimURI            The claim URI.
     * @return The claim value.
     * @throws PostAuthenticationFailedException If an error occurs while getting the claim value.
     */
    public static String getUserClaimValue(String tenantDomain, String tenantAwareUsername, String claimURI)
            throws PostAuthenticationFailedException {

        try {
            UserRealm userRealm = getUserRealm(tenantDomain);
            UserStoreManager userStoreManager = getUserStoreManager(userRealm);
            String userStoreDomain = UserCoreUtil.getDomainFromThreadLocal();
            String domainQualifiedUsername = UserCoreUtil.addDomainToName(tenantAwareUsername, userStoreDomain);

            String claimValue = "";
            Map<String, String> claimValueMap =
                    userStoreManager.getUserClaimValues(domainQualifiedUsername, new String[]{claimURI}, null);
            if (claimValueMap != null && claimValueMap.get(claimURI) != null) {
                claimValue = claimValueMap.get(claimURI);
            }
            return claimValue;
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getMessage());
        }
    }

    /**
     * This method retrieves the last password updated time in milliseconds.
     *
     * @param lastPasswordUpdatedTime The last password updated time in string format.
     * @return The last password updated time in milliseconds.
     */
    private static long getLastPasswordUpdatedTimeInMillis(String lastPasswordUpdatedTime) {

        return StringUtils.isEmpty(lastPasswordUpdatedTime) ? 0 : Long.parseLong(lastPasswordUpdatedTime);
    }

    /**
     * This method retrieves the user store manager object from the user realm.
     *
     * @param userRealm The user realm to retrieve the user store manager.
     * @return The user store manager object.
     * @throws PostAuthenticationFailedException If an error occurs while retrieving the user store manager.
     */
    private static UserStoreManager getUserStoreManager(UserRealm userRealm) throws PostAuthenticationFailedException {

        UserStoreManager userStoreManager;
        try {
            userStoreManager = (UserStoreManager) userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getMessage());
        }
        return userStoreManager;
    }

    /**
     * This method retrieves the user realm object for the given tenant domain.
     *
     * @param tenantDomain The tenant domain to retrieve the user realm.
     * @return The user realm object.
     * @throws PostAuthenticationFailedException If an error occurs while retrieving the user realm.
     */
    private static UserRealm getUserRealm(String tenantDomain) throws PostAuthenticationFailedException {

        UserRealm userRealm;
        try {
            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            RealmService realmService = EnforcePasswordResetComponentDataHolder.getInstance()
                    .getRealmService();
            userRealm = realmService.getTenantUserRealm(tenantId);
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GETTING_USER_REALM.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USER_REALM.getMessage());
        }
        return userRealm;
    }

    /**
     * This method retrieves the password expiry in days configured for the given tenant domain.
     *
     * @param tenantDomain The tenant domain to retrieve the password expiry in days.
     * @return The password expiry in days.
     * @throws PostAuthenticationFailedException If an error occurs while retrieving the password expiry configuration.
     */
    public static int getPasswordExpiryInDays(String tenantDomain) throws PostAuthenticationFailedException {

        try {
            String passwordExpiryInDaysConfiguredValue = getPasswordExpiryConfig(tenantDomain,
                    PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
            int passwordExpiryInDays = PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DEFAULT_VALUE;
            return (passwordExpiryInDaysConfiguredValue != null) ? Integer.parseInt(passwordExpiryInDaysConfiguredValue)
                    : passwordExpiryInDays;
        } catch (IdentityGovernanceException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getMessage());
        }
    }

    /**
     * This method retrieves the password expiry in days configured for the given tenant domain.
     *
     * @param passwordChangedTime The last password updated time in milliseconds.
     * @return The number of days since the password was last updated.
     */
    private static int getDaysDifference(long passwordChangedTime) {

        long currentTimeMillis = System.currentTimeMillis();
        int daysDifference = (int) ((currentTimeMillis - passwordChangedTime) / (1000 * 60 * 60 * 24));
        return daysDifference;
    }

    /**
     * This method retrieves the last password updated time from the user store.
     *
     * @param tenantAwareUsername The tenant aware username of the user trying to authenticate.
     * @param userStoreManager    The user store manager to retrieve the last password updated time from.
     * @param userRealm           The user realm to retrieve the claim manager from.
     * @return The last password updated time.
     * @throws PostAuthenticationFailedException
     */
    @SuppressFBWarnings("FORMAT_STRING_MANIPULATION")
    private static String getLastPasswordUpdatedTime(String tenantAwareUsername, UserStoreManager userStoreManager,
                                                     UserRealm userRealm) throws PostAuthenticationFailedException {

        String lastPasswordUpdatedTime;
        String claimURI = PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM;
        try {
            String userStoreDomain = UserCoreUtil.getDomainFromThreadLocal();
            String domainQualifiedUsername = UserCoreUtil.addDomainToName(tenantAwareUsername, userStoreDomain);
            lastPasswordUpdatedTime = getLastPasswordUpdateTime(userStoreManager, claimURI, domainQualifiedUsername);
            if (StringUtils.isEmpty(lastPasswordUpdatedTime)) {
                ClaimManager claimManager = userRealm.getClaimManager();
                claimURI = PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY;
                if (claimManager.getClaim(claimURI) != null) {
                    lastPasswordUpdatedTime =
                            getLastPasswordUpdateTime(userStoreManager, claimURI, tenantAwareUsername);
                }
            }
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_CLAIM_MAPPINGS.getCode(),
                    String.format(PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_CLAIM_MAPPINGS.getMessage(),
                            claimURI));
        }

        return lastPasswordUpdatedTime;
    }

    /**
     * This method retrieves the last password updated time for a given user.
     *
     * @param userStoreManager    User store manager instance used to retrieve user claim.
     * @param claimURI            The URI of the claim to retrieve.
     * @param tenantAwareUsername The username of the user to retrieve the claim value.
     * @return The last password updated time.
     * @throws UserStoreException If an error occurs while retrieving the claim value.
     */
    private static String getLastPasswordUpdateTime(UserStoreManager userStoreManager, String claimURI,
                                                    String tenantAwareUsername) throws UserStoreException {

        String[] claimURIs = new String[]{claimURI};
        Map<String, String> claimValueMap =
                userStoreManager.getUserClaimValues(tenantAwareUsername, claimURIs, null);
        if (claimValueMap != null && claimValueMap.get(claimURI) != null) {
            return claimValueMap.get(claimURI);
        }

        return StringUtils.EMPTY;
    }

    /**
     * This method checks if password expiry is enabled for a given tenant domain.
     *
     * @param tenantDomain The tenant domain to check for password expiry configuration.
     * @return true if password expiry is enabled, false otherwise.
     * @throws PostAuthenticationFailedException If there is an error while reading system configurations.
     */
    public static boolean isPasswordExpiryEnabled(String tenantDomain) throws PostAuthenticationFailedException {

        try {
            return Boolean.parseBoolean(PasswordPolicyUtils.getPasswordExpiryConfig(tenantDomain,
                    PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY));
        } catch (IdentityGovernanceException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS.getMessage(), e);
        }
    }

    /**
     * Get Password Reset page URL.
     *
     * @param tenantDomain Tenant domain.
     * @throws PostAuthenticationFailedException If an error occurred while getting the password reset page url.
     */
    public static String getPasswordResetPageUrl(String tenantDomain) throws PostAuthenticationFailedException {

        String basePath;
        String serverUrl;
        try {
            if (IdentityTenantUtil.isTenantQualifiedUrlsEnabled()) {
                basePath = ServiceURLBuilder.create().addPath(PASSWORD_RESET_PAGE).setTenant(tenantDomain)
                        .build().getAbsolutePublicURL();
            } else {
                serverUrl = ServiceURLBuilder.create().build().getAbsolutePublicURL();
                if (StringUtils.isNotBlank(tenantDomain) && !MultitenantConstants.SUPER_TENANT_DOMAIN_NAME
                        .equalsIgnoreCase(tenantDomain)) {
                    // accountrecoveryendpoint application is expecting a query param, if tenanted paths are disabled.
                    basePath = serverUrl + "/t/" + tenantDomain + PASSWORD_RESET_PAGE + "?tenantDomain=" + tenantDomain;
                } else {
                    basePath = serverUrl + PASSWORD_RESET_PAGE;
                }
            }
            return basePath;
        } catch (URLBuilderException e) {
            throw new PostAuthenticationFailedException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_BUILDING_PASSWORD_RESET_PAGE_URL.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_BUILDING_PASSWORD_RESET_PAGE_URL.getMessage());
        }
    }
}
