/*
 *
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.username;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.base.IdentityConstants;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.IdentityClaimManager;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.internal.service.impl.username.UsernameRecoveryManagerImpl;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.claim.Claim;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class NotificationUsernameRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationUsernameRecoveryManager.class);
    private static final String FORWARD_SLASH = "/";

    private static NotificationUsernameRecoveryManager instance = new NotificationUsernameRecoveryManager();
    private static UsernameRecoveryManagerImpl usernameRecoveryManagerImpl = new UsernameRecoveryManagerImpl();

    private NotificationUsernameRecoveryManager() {

    }

    public static NotificationUsernameRecoveryManager getInstance() {
        return instance;
    }


    /**
     * This returns the user supported claims.
     *
     * @param dialect
     * @return
     * @throws IdentityRecoveryException
     */
    public String[] getUserIdentitySupportedClaims(String dialect, String tenantDomain)
            throws IdentityException {
        IdentityClaimManager claimManager;
        Claim[] claims;

        claimManager = IdentityClaimManager.getInstance();
        UserRealm realm = IdentityTenantUtil.getRealm(null, null);
        claims = claimManager.getAllSupportedClaims(dialect, realm);

        if (claims == null || claims.length == 0) {
            log.warn("Could not find any matching claims for requested dialect : " + dialect);
            return new String[0];
        }

        List<String> claimList = new ArrayList<>();

        for (int i = 0; i < claims.length; i++) {
            if (claims[i].getDisplayTag() != null
                    && !IdentityConstants.PPID_DISPLAY_VALUE.equals(claims[i].getDisplayTag())) {
                if (UserCoreConstants.ClaimTypeURIs.ACCOUNT_STATUS.equals(claims[i].getClaimUri())) {
                    continue;
                }
                if (claims[i].isSupportedByDefault() && (!claims[i].isReadOnly())) {
                    claimList.add(claims[i].getClaimUri());
                }
            }
        }

        return claimList.toArray(new String[claimList.size()]);
    }


    /**
     * This returns the user supported claims info.
     *
     * @param dialect
     * @return
     * @throws IdentityRecoveryException
     */
    public Claim[] getIdentitySupportedClaims(String dialect, String tenantDomain)
            throws IdentityException {
        IdentityClaimManager claimManager;
        Claim[] claims;

        claimManager = IdentityClaimManager.getInstance();
        UserRealm realm = IdentityTenantUtil.getRealm(tenantDomain, null);
        claims = claimManager.getAllSupportedClaims(dialect, realm);

        if (claims == null || claims.length == 0) {
            log.warn("Could not find any matching claims for requested dialect : " + dialect);
            return new Claim[0];
        }

        return claims;
    }

    /**
     * Recovery username of the user who matches the given set of claims.
     *
     * @param claims       User claims
     * @param tenantDomain Tenant domain
     * @param notify       Notify user existence
     * @return Username if notifications are externally managed
     * @throws IdentityRecoveryException Error while recovering the username
     */
    public String verifyUsername(UserClaim[] claims, String tenantDomain, Boolean notify) throws
            IdentityRecoveryException {

        // Resolve Tenant domain.
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }
        // Resolve notification internally managed status.
        boolean isNotificationInternallyManaged = isNotificationsInternallyManaged(tenantDomain, notify);
        HashMap<String, String> userClaims = buildUserClaimsMap(claims);
        // Validate the claims.
        if (claims.length < 1) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY, null);
        }
        RecoveryInformationDTO recoveryInformationDTO = initiateUsernameRecovery(userClaims, tenantDomain,
                isNotificationInternallyManaged);
        /*
        recoveryChannelInfoDTO will be NULL for successful username recovery or when notify user existence in not
        enabled and no user is matched to the given claims.
         */
        if (recoveryInformationDTO == null) {
            return null;
        } else {
            return recoveryInformationDTO.getUsername();
        }
    }

    /**
     * Initiate username recovery.
     *
     * @param claims                        User claims
     * @param tenantDomain                  Tenant domain
     * @param manageNotificationsInternally Enable internal notification management
     * @return RecoveryChannelInfoDTO
     * @throws IdentityRecoveryException Error initiating username recovery.
     */
    private RecoveryInformationDTO initiateUsernameRecovery(Map<String, String> claims, String tenantDomain,
                                                            boolean manageNotificationsInternally) throws IdentityRecoveryException {

        try {
            HashMap<String, String> properties = new HashMap<>();
            properties.put(IdentityRecoveryConstants.USE_LEGACY_API_PROPERTY_KEY, Boolean.toString(true));
            properties.put(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY,
                    Boolean.toString(manageNotificationsInternally));
            return usernameRecoveryManagerImpl.initiate(claims, tenantDomain, properties);
        } catch (IdentityRecoveryServerException exception) {
            if (StringUtils.isNotEmpty(exception.getErrorCode())) {
                String errorCode = exception.getErrorCode();
                // Userstore not found error.
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER.getCode()
                        .equals(errorCode)) {
                    String msg = "Error retrieving the user store manager for the tenant";
                    throw new IdentityRecoveryException(msg, exception);
                }
                // Error retrieving claims.
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_RETRIEVING_USER_CLAIM.getCode()
                        .equals(errorCode)) {
                    String msg = "Unable to retrieve the claim for the given tenant";
                    throw new IdentityRecoveryException(msg, exception);
                }
            }
            throw exception;
        } catch (IdentityRecoveryClientException exception) {
            if (StringUtils.isNotEmpty(exception.getErrorCode())) {
                String errorCode = exception.getErrorCode();
                // Multiple users matched error.
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_MATCHING_USERS.getCode()
                        .equals(errorCode)) {
                    if (Boolean.parseBoolean(IdentityUtil
                                    .getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE))) {
                        throw exception;
                    }
                    /* If the notify user is not enabled, return an NULL object so that the user is not notified with
                    an error. */
                    return null;
                }
                // Configurations not enabled error.
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLED.getCode()
                        .equals(errorCode)) {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLE, null);
                }
            }
            throw exception;
        }
    }

    /**
     * Get a map of user claims using a list of UserClaims.
     *
     * @param claims List of UserClaims
     * @return Claims
     */
    private HashMap<String, String> buildUserClaimsMap(UserClaim[] claims) {

        HashMap<String, String> claimsMap = new HashMap<>();
        for (UserClaim userClaim : claims) {
            if (StringUtils.isNotEmpty(userClaim.getClaimURI()) && StringUtils.isNotEmpty(userClaim.getClaimValue())) {
                claimsMap.put(userClaim.getClaimURI(), userClaim.getClaimValue());
            }
        }
        return claimsMap;
    }

    /**
     * Check whether notifications are internally managed.
     *
     * @param tenantDomain Tenant Domain
     * @param notify       Manage notifications internally
     * @return True of the notifications are internally managed
     * @throws IdentityRecoveryException Error while getting server configurations
     */
    private boolean isNotificationsInternallyManaged(String tenantDomain, Boolean notify)
            throws IdentityRecoveryException {

        if (notify == null) {
            return Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            tenantDomain));
        } else {
            return notify;
        }
    }
}
