/*
 * Copyright (c) 2023-2024, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRuleAttributeEnum;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRuleOperatorEnum;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiryRule;
import org.wso2.carbon.identity.role.v2.mgt.core.RoleManagementService;
import org.wso2.carbon.identity.role.v2.mgt.core.exception.IdentityRoleManagementException;
import org.wso2.carbon.identity.role.v2.mgt.core.model.RoleBasicInfo;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.user.core.common.Group;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.CONNECTOR_CONFIG_NAME;
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
        properties.add(PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES);
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
    @SuppressFBWarnings("CRLF_INJECTION_LOGS")
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
                if (StringUtils.startsWith(property.getName(), PasswordPolicyConstants.PASSWORD_EXPIRY_RULES_PREFIX) &&
                        StringUtils.isNotEmpty(property.getValue())) {
                    try {
                        PasswordExpiryRule passwordExpiryRule = new PasswordExpiryRule(property.getValue());
                        passwordExpiryRules.add(passwordExpiryRule);
                    } catch (Exception e) {
                        // Log and skip the rule if an error occurred while parsing the rule, without failing the
                        // authentication flow.
                        if (log.isDebugEnabled()) {
                            log.debug(String.format("Error parsing password expiry rule: %s. Rule will be skipped.",
                                    property.getValue()));
                        }
                        log.error("Error parsing password expiry rule.", e);
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

        try {
            if (!isPasswordExpiryEnabled(tenantDomain)) return false;

            UserRealm userRealm = getUserRealm(tenantDomain);
            UserStoreManager userStoreManager = getUserStoreManager(userRealm);
            String userId = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(tenantAwareUsername);
            String lastPasswordUpdatedTime =
                    getLastPasswordUpdatedTime(tenantAwareUsername, userStoreManager, userRealm);
            long lastPasswordUpdatedTimeInMillis = getLastPasswordUpdatedTimeInMillis(lastPasswordUpdatedTime);
            int daysDifference = getDaysDifference(lastPasswordUpdatedTimeInMillis);

            List<PasswordExpiryRule> passwordExpiryRules = getPasswordExpiryRules(tenantDomain);
            boolean skipIfNoApplicableRules = isSkipIfNoApplicableRulesEnabled(tenantDomain);

            // Apply default password expiry policy if no rules given.
            if (CollectionUtils.isEmpty(passwordExpiryRules)) {
                return isPasswordExpiredUnderDefaultPolicy(tenantDomain, daysDifference, lastPasswordUpdatedTime,
                        skipIfNoApplicableRules);
            }

            List<PasswordExpiryRule> filteredRules =
                    filterApplicableExpiryRules(passwordExpiryRules, skipIfNoApplicableRules);

            Map<PasswordExpiryRuleAttributeEnum, Set<String>> fetchedUserAttributes =
                    new EnumMap<>(PasswordExpiryRuleAttributeEnum.class);

            for (PasswordExpiryRule rule : filteredRules) {
                if (isRuleApplicable(rule, fetchedUserAttributes, tenantDomain, userId, userStoreManager)) {
                    // Skip the rule if the operator is not equals.
                    if (PasswordExpiryRuleOperatorEnum.NE.equals(rule.getOperator())) {
                        return false;
                    }
                    int expiryDays =
                            rule.getExpiryDays() > 0 ? rule.getExpiryDays() : getPasswordExpiryInDays(tenantDomain);
                    return daysDifference >= expiryDays || StringUtils.isBlank(lastPasswordUpdatedTime);
                }
            }
            // Apply default password expiry policy if no specific rule applies.
            return isPasswordExpiredUnderDefaultPolicy(tenantDomain, daysDifference, lastPasswordUpdatedTime,
                    skipIfNoApplicableRules);
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getMessage());
        }
    }

    /**
     * Check if the given rule is applicable for the user.
     *
     * @param rule                   Password expiry rule.
     * @param fetchedUserAttributes  Fetched user attributes.
     * @param tenantDomain           Tenant domain.
     * @param userId                 User ID.
     * @param userStoreManager       User store manager.
     * @return true if the rule is applicable, false otherwise.
     * @throws PostAuthenticationFailedException If an error occurred while checking the rule applicability.
     */
    private static boolean isRuleApplicable(PasswordExpiryRule rule,
                                            Map<PasswordExpiryRuleAttributeEnum, Set<String>> fetchedUserAttributes,
                                            String tenantDomain, String userId,
                                            UserStoreManager userStoreManager)
            throws PostAuthenticationFailedException {

        PasswordExpiryRuleAttributeEnum ruleAttribute = rule.getAttribute();
        Set<String> userAttributeValues =
                getUserAttributes(ruleAttribute, fetchedUserAttributes, tenantDomain, userId, userStoreManager);
        if (CollectionUtils.isEmpty(userAttributeValues)) {
            return false;
        }
        return userAttributeValues.containsAll(rule.getValues());
    }

    /**
     * Get the user attribute values for the given password expiry rule attribute.
     *
     * @param attribute              Password expiry rule attribute.
     * @param fetchedUserAttributes  Fetched user attributes.
     * @param tenantDomain           Tenant domain.
     * @param userId                 User ID.
     * @param userStoreManager       User store manager.
     * @return  The user attribute values.
     * @throws PostAuthenticationFailedException If an error occurred while getting the user attributes.
     */
    private static Set<String> getUserAttributes(PasswordExpiryRuleAttributeEnum attribute,
                                                 Map<PasswordExpiryRuleAttributeEnum, Set<String>> fetchedUserAttributes,
                                                 String tenantDomain, String userId,
                                                 UserStoreManager userStoreManager)
            throws PostAuthenticationFailedException {

        if (!fetchedUserAttributes.containsKey(attribute)) {
            switch (attribute) {
                case ROLES:
                    // Fetch roles assigned to user via groups.
                    Set<String> userGroupIds;
                    if (fetchedUserAttributes.containsKey(PasswordExpiryRuleAttributeEnum.GROUPS)) {
                        userGroupIds = fetchedUserAttributes.get(PasswordExpiryRuleAttributeEnum.GROUPS);
                    } else {
                        userGroupIds = getUserGroupIds(userId, userStoreManager);
                        fetchedUserAttributes.put(PasswordExpiryRuleAttributeEnum.GROUPS, userGroupIds);
                    }
                    List<String> roleIdsOfGroups = getRoleIdsOfGroups(new ArrayList<>(userGroupIds), tenantDomain);

                    List<RoleBasicInfo> userRoles = getUserRoles(tenantDomain, userId);
                    Set<String> userRoleIds =
                            userRoles.stream().map(RoleBasicInfo::getId).collect(Collectors.toSet());
                    userRoleIds.addAll(roleIdsOfGroups);
                    fetchedUserAttributes.put(PasswordExpiryRuleAttributeEnum.ROLES, userRoleIds);
                    break;
                case GROUPS:
                    Set<String> groupIds = getUserGroupIds(userId, userStoreManager);
                    fetchedUserAttributes.put(PasswordExpiryRuleAttributeEnum.GROUPS, groupIds);
                    break;
            }
        }
        return fetchedUserAttributes.get(attribute);
    }

    /**
     * Check if the password has expired according to the default password expiry policy.
     *
     * @param tenantDomain            The tenant domain.
     * @param daysDifference          The number of days since the password was last updated.
     * @param lastPasswordUpdatedTime The last password updated time.
     * @return true if the password has expired, false otherwise.
     * @throws PostAuthenticationFailedException If an error occurs while checking the password expiry.
     */
    private static boolean isPasswordExpiredUnderDefaultPolicy(String tenantDomain, int daysDifference,
                                                               String lastPasswordUpdatedTime,
                                                               boolean skipIfNoApplicableRules)
            throws PostAuthenticationFailedException {

        if (skipIfNoApplicableRules) return false;
        return StringUtils.isBlank(lastPasswordUpdatedTime) || daysDifference >= getPasswordExpiryInDays(tenantDomain);
    }

    /**
     * This method returns password expiry time for the given user.
     *
     * @param tenantDomain         The tenant domain.
     * @param tenantAwareUsername  The tenant aware username.
     * @return Optional containing the password expiry time in milliseconds, or empty if not applicable.
     * @throws PostAuthenticationFailedException If an error occurred while getting the password expiry time.
     */
    public static Optional<Long> getUserPasswordExpiryTime(String tenantDomain, String tenantAwareUsername)
            throws PostAuthenticationFailedException {

        return getUserPasswordExpiryTime(tenantDomain, tenantAwareUsername, null,
                null, null, null);
    }

    /**
     * This method returns password expiry time for the given user.
     *
     * @param tenantDomain                     The tenant domain.
     * @param tenantAwareUsername              The tenant aware username.
     * @param isPasswordExpiryEnabled          Whether password expiry is enabled.
     * @param isSkipIfNoApplicableRulesEnabled Whether skip if no applicable rules config is enabled.
     * @param passwordExpiryRules              Password expiry rules.
     * @param defaultPasswordExpiryInDays      Default password expiry in days.
     * @return Optional containing the password expiry time in milliseconds, or empty if not applicable.
     * @throws PostAuthenticationFailedException If an error occurred while getting the password expiry time.
     */
    public static Optional<Long> getUserPasswordExpiryTime(String tenantDomain,
                                                           String tenantAwareUsername,
                                                           Boolean isPasswordExpiryEnabled,
                                                           Boolean isSkipIfNoApplicableRulesEnabled,
                                                           List<PasswordExpiryRule> passwordExpiryRules,
                                                           Integer defaultPasswordExpiryInDays)
        throws PostAuthenticationFailedException {

        try {
            if (isPasswordExpiryEnabled == null) {
                isPasswordExpiryEnabled = isPasswordExpiryEnabled(tenantDomain);
            }
            // If the password expiry is not enabled, password expiry time is not applicable.
            if (!isPasswordExpiryEnabled) return Optional.empty();

            if (isSkipIfNoApplicableRulesEnabled == null) {
                isSkipIfNoApplicableRulesEnabled = isSkipIfNoApplicableRulesEnabled(tenantDomain);
            }
            if (defaultPasswordExpiryInDays == null) {
                defaultPasswordExpiryInDays = getPasswordExpiryInDays(tenantDomain);
            }
            if (passwordExpiryRules == null) {
                passwordExpiryRules = getPasswordExpiryRules(tenantDomain);
            }

            UserRealm userRealm = getUserRealm(tenantDomain);
            UserStoreManager userStoreManager = getUserStoreManager(userRealm);
            String userId = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(tenantAwareUsername);
            String lastPasswordUpdatedTime =
                    getLastPasswordUpdatedTime(tenantAwareUsername, userStoreManager, userRealm);

            long lastPasswordUpdatedTimeInMillis = 0L;
            boolean isLastPasswordUpdatedTimeBlank = StringUtils.isBlank(lastPasswordUpdatedTime);
            if (!isLastPasswordUpdatedTimeBlank) {
                lastPasswordUpdatedTimeInMillis = getLastPasswordUpdatedTimeInMillis(lastPasswordUpdatedTime);
            }

            // If no rules are defined, use the default expiry time if "skipIfNoApplicableRules" is disabled.
            if (CollectionUtils.isEmpty(passwordExpiryRules)) {
                if (isSkipIfNoApplicableRulesEnabled) return Optional.empty();
                // If lastPasswordUpdatedTime is blank, set expiry time to now.
                if (isLastPasswordUpdatedTimeBlank) {
                    return Optional.of(System.currentTimeMillis());
                }
                return Optional.of(
                        lastPasswordUpdatedTimeInMillis + getDaysTimeInMillis(defaultPasswordExpiryInDays));
            }

            Map<PasswordExpiryRuleAttributeEnum, Set<String>> userAttributes =
                    new EnumMap<>(PasswordExpiryRuleAttributeEnum.class);

            List<PasswordExpiryRule> filteredRules =
                    filterApplicableExpiryRules(passwordExpiryRules, isSkipIfNoApplicableRulesEnabled);
            for (PasswordExpiryRule rule : filteredRules) {
                if (isRuleApplicable(rule, userAttributes, tenantDomain, userId, userStoreManager)) {
                    // Skip the rule if the operator is not equals.
                    if (PasswordExpiryRuleOperatorEnum.NE.equals(rule.getOperator())) {
                        return Optional.empty();
                    }
                    if (isLastPasswordUpdatedTimeBlank) {
                        return Optional.of(System.currentTimeMillis());
                    }
                    int expiryDays =
                            rule.getExpiryDays() > 0 ? rule.getExpiryDays() : getPasswordExpiryInDays(tenantDomain);
                    return Optional.of(lastPasswordUpdatedTimeInMillis + getDaysTimeInMillis(expiryDays));
                }
            }

            if (isSkipIfNoApplicableRulesEnabled) return Optional.empty();
            if (isLastPasswordUpdatedTimeBlank) {
                return Optional.of(System.currentTimeMillis());
            }
            return Optional.of(
                    lastPasswordUpdatedTimeInMillis + getDaysTimeInMillis(defaultPasswordExpiryInDays));
        } catch (UserStoreException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USER_STORE_DOMAIN.getMessage());
        }
    }

    private static List<PasswordExpiryRule> filterApplicableExpiryRules(List<PasswordExpiryRule> passwordExpiryRules,
                                                                        boolean skipIfNoApplicableRules) {

        // If the default behavior is to skip the password expiry, rules with skip logic are not required.
        return passwordExpiryRules.stream().filter(
                rule -> !skipIfNoApplicableRules || !PasswordExpiryRuleOperatorEnum.NE.equals(rule.getOperator()))
                .collect(Collectors.toList());
    }

    /**
     * This method returns the time in milliseconds for the given number of days.
     *
     * @param days The number of days.
     * @return The time in milliseconds.
     */
    private static long getDaysTimeInMillis(int days) {

        return (long) days * 24 * 60 * 60 * 1000;
    }

    /**
     * Get the roles of a given user.
     *
     * @param tenantDomain The tenant domain.
     * @param userId       The user ID.
     * @return The roles of the user.
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
     * Get the group IDs of the given user.
     *
     * @param userId           The user ID.
     * @param userStoreManager The user store manager.
     * @return The group IDs of the user.
     * @throws PostAuthenticationFailedException If an error occurs while getting the group IDs of the user.
     */
    private static Set<String> getUserGroupIds(String userId, UserStoreManager userStoreManager)
            throws PostAuthenticationFailedException {

        try {
            List<Group> userGroups =
                    ((AbstractUserStoreManager) userStoreManager).getGroupListOfUser(userId,
                            null, null);
            return userGroups.stream().map(Group::getGroupID).collect(Collectors.toSet());
        } catch (UserStoreException e) {
                throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                        ERROR_WHILE_RETRIEVING_USER_GROUPS.getCode(),
                        PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_RETRIEVING_USER_GROUPS.getMessage());
        }
    }

    /**
     * Get the role IDs of the given groups.
     *
     * @param groupIds     The group IDs.
     * @param tenantDomain The tenant domain.
     * @return The role IDs of the groups.
     * @throws PostAuthenticationFailedException If an error occurs while getting the role IDs of the groups.
     */
    private static List<String> getRoleIdsOfGroups(List<String> groupIds, String tenantDomain)
            throws PostAuthenticationFailedException {

        try {
            RoleManagementService roleManagementService = EnforcePasswordResetComponentDataHolder.getInstance()
                    .getRoleManagementService();
            return roleManagementService.getRoleIdListOfGroups(groupIds, tenantDomain);
        } catch (IdentityRoleManagementException e) {
            throw new PostAuthenticationFailedException(PasswordPolicyConstants.ErrorMessages.
                    ERROR_WHILE_RETRIEVING_USER_ROLES.getCode(),
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_RETRIEVING_USER_ROLES.getMessage());
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
        return (int) ((currentTimeMillis - passwordChangedTime) / (1000 * 60 * 60 * 24));
    }

    /**
     * This method retrieves the last password updated time from the user store.
     *
     * @param tenantAwareUsername The tenant aware username of the user trying to authenticate.
     * @param userStoreManager    The user store manager to retrieve the last password updated time from.
     * @param userRealm           The user realm to retrieve the claim manager from.
     * @return The last password updated time.
     * @throws PostAuthenticationFailedException If an error occurs while retrieving the last password updated time.
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
     * This method checks if the "skip if no applicable rules" option is enabled for a given tenant domain.
     *
     * @param tenantDomain The tenant domain to check for the configuration.
     * @return true if "skip if no applicable rules" is enabled, false otherwise.
     * @throws PostAuthenticationFailedException If an error occurs while reading system configurations.
     */
    public static boolean isSkipIfNoApplicableRulesEnabled(String tenantDomain)
            throws PostAuthenticationFailedException {

        try {
            return Boolean.parseBoolean(PasswordPolicyUtils.getPasswordExpiryConfig(tenantDomain,
                    PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES));
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