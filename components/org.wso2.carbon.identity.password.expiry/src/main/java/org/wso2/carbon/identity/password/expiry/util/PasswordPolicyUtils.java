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
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.ServiceURLBuilder;
import org.wso2.carbon.identity.core.URLBuilderException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.PASSWORD_RESET_PAGE;

/**
 * Utilities for password change enforcing.
 */
public class PasswordPolicyUtils {

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

        // Getting the configured number of days before password expiry in days
        int passwordExpiryInDays = getPasswordExpiryInDays(tenantDomain);
        return (daysDifference > passwordExpiryInDays || lastPasswordUpdatedTime == null);
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
    private static int getPasswordExpiryInDays(String tenantDomain) throws PostAuthenticationFailedException {

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
