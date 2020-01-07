/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.internal.service.impl.username;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.UsernameRecoverDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.services.username.UsernameRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Map;

/**
 * Class that implements the UsernameRecoveryManager.
 */
public class UsernameRecoveryManagerImpl implements UsernameRecoveryManager {

    private static final Log log = LogFactory.getLog(UsernameRecoveryManagerImpl.class);

    /**
     * Get the username recovery information with available verified channel details.
     *
     * @param claims       User Claims
     * @param tenantDomain Tenant domain
     * @param properties   Meta properties
     * @return RecoveryChannelInfoDTO {@link RecoveryInformationDTO} object that contains
     * recovery for a verified user
     * @throws IdentityRecoveryException Error while initiating username recovery
     */
    @Override
    public RecoveryInformationDTO initiate(Map<String, String> claims, String tenantDomain,
                                           Map<String, String> properties) throws IdentityRecoveryException {

        validateTenantDomain(tenantDomain);
        validateConfigurations(tenantDomain);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        RecoveryInformationDTO recoveryInformationDTO = new RecoveryInformationDTO();

        boolean useLegacyAPIApproach = useLegacyAPIApproach(properties);
        boolean manageNotificationsInternally = Utils.isNotificationsInternallyManaged(tenantDomain, properties);
        if (useLegacyAPIApproach) {
            // Use legacy API approach to support legacy username recovery.
            String username = userAccountRecoveryManager.getUsernameByClaims(claims, tenantDomain);
            if (StringUtils.isNotEmpty(username)) {
                if (manageNotificationsInternally) {
                    User user = createUser(username, tenantDomain);
                    triggerNotification(user, NotificationChannels.EMAIL_CHANNEL.getChannelType(),
                            IdentityEventConstants.Event.TRIGGER_NOTIFICATION, null);
                    if (log.isDebugEnabled()) {
                        log.debug("Successful username recovery for user: " + username + ". " +
                                "User notified Internally");
                    }
                    return null;
                }
                if (log.isDebugEnabled()) {
                    log.debug("Successful username recovery for user: " + username + ". User notified Externally");
                }
                recoveryInformationDTO.setUsername(username);
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("No user found for the given claims in tenant domain : " + tenantDomain);
                }
                if (Boolean.parseBoolean(
                        IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE))) {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_OR_MORE_THAN_ONE_USER_FOUND,
                            null);
                }
                return null;
            }
            return recoveryInformationDTO;
        }
        // Add notification method in a meta property list.
        Map<String, String> metaProperties = new HashMap<>();
        metaProperties.put(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY,
                Boolean.toString(manageNotificationsInternally));
        recoveryInformationDTO.setRecoveryChannelInfoDTO(userAccountRecoveryManager
                .retrieveUserRecoveryInformation(claims, tenantDomain, RecoveryScenarios.USERNAME_RECOVERY,
                        metaProperties));
        return recoveryInformationDTO;
    }

    /**
     * Verify the recovery code and send recovery information via channel which matches the given channel id.
     *
     * @param recoveryCode RecoveryId of the user
     * @param channelId    Channel Id of the user
     * @param tenantDomain Tenant Domain
     * @param properties   Meta properties in the recovery request
     * @return UsernameRecoverDTO {@link UsernameRecoverDTO} object that contains notified
     * channel details and success status code
     * @throws IdentityRecoveryException Error while notifying user
     */
    @Override
    public UsernameRecoverDTO notify(String recoveryCode, String channelId, String tenantDomain,
                                     Map<String, String> properties) throws IdentityRecoveryException {

        validateTenantDomain(tenantDomain);
        int channelIdCode = validateChannelID(channelId);
        validateConfigurations(tenantDomain);
        UserAccountRecoveryManager recoveryAccountManager = UserAccountRecoveryManager.getInstance();

        // Validate Recovery data.
        UserRecoveryData userRecoveryData = recoveryAccountManager
                .getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION);
        invalidateRecoveryCode(recoveryCode);
        String notificationChannel = extractNotificationChannelDetails(userRecoveryData.getRemainingSetIds(),
                channelIdCode);

        // If the notifications are externally managed we do not need to send notifications internally.
        if (!IdentityRecoveryConstants.EXTERNAL_NOTIFICATION_CHANNEL.equals(notificationChannel)) {
            String eventName = Utils.resolveEventName(notificationChannel);
            validateCallbackURL(properties, userRecoveryData.getUser());
            triggerNotification(userRecoveryData.getUser(), notificationChannel, eventName, properties);
        }
        // Return successful recovery response object.
        return buildUserNameRecoveryResponseDTO(userRecoveryData.getUser(), notificationChannel);
    }

    /**
     * Whether to use legacy APIs or not.
     *
     * @param properties Meta properties
     * @return True to use legacy API approach
     */
    private boolean useLegacyAPIApproach(Map<String, String> properties) {

        if (MapUtils.isNotEmpty(properties)) {
            try {
                return Boolean.parseBoolean(properties.get(IdentityRecoveryConstants.USE_LEGACY_API_PROPERTY_KEY));
            } catch (NumberFormatException e) {
                if (log.isDebugEnabled()) {
                    String message = String.format("Invalid boolean value : %s to enable legacyAPIs", properties
                            .get(IdentityRecoveryConstants.USE_LEGACY_API_PROPERTY_KEY));
                    log.debug(message);
                }
            }
        }
        return false;
    }

    /**
     * Create a User object.
     *
     * @param userName     Username of the user
     * @param tenantDomain Tenant domain
     * @return User
     */
    private User createUser(String userName, String tenantDomain) {

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(IdentityUtil.extractDomainFromName(userName));
        return user;
    }

    /**
     * Validate the channel Id given in the reqest.
     *
     * @param channelId Channel Id in the request.
     * @return Channel Id
     * @throws IdentityRecoveryClientException Invalid client Id
     */
    private int validateChannelID(String channelId) throws IdentityRecoveryClientException {

        int id;
        // Check whether the channel Id is an int.
        try {
            id = Integer.parseInt(channelId);
        } catch (NumberFormatException e) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        // Channel id needs to be larger than 0.
        if (id < 1) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        return id;
    }

    /**
     * Build the response object for successful username recovery.
     *
     * @param user                User
     * @param notificationChannel Notification channel selected by the user.
     * @return UsernameRecoverDTO object
     */
    private UsernameRecoverDTO buildUserNameRecoveryResponseDTO(User user, String notificationChannel) {

        // Username regex. Example: username@tenantDomain.
        String qualifiedUsernameRegexPattern = "%s@%s";
        UsernameRecoverDTO usernameRecoverDTO = new UsernameRecoverDTO();
        usernameRecoverDTO.setNotificationChannel(notificationChannel);
        // Check for notification method.
        if (IdentityRecoveryConstants.EXTERNAL_NOTIFICATION_CHANNEL.equals(notificationChannel)) {
            usernameRecoverDTO.setCode(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_EXTERNALLY_NOTIFIED.getCode());
            usernameRecoverDTO.setMessage(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_EXTERNALLY_NOTIFIED
                            .getMessage());

            // If notifications are externally managed, username needs to be sent with the request.
            // Build username for external notification.
            String username =
                    String.format(qualifiedUsernameRegexPattern, user.getUserName(), user.getTenantDomain());
            usernameRecoverDTO.setUsername(username);
        } else {
            usernameRecoverDTO.setCode(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_INTERNALLY_NOTIFIED.getCode());
            usernameRecoverDTO.setMessage(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_INTERNALLY_NOTIFIED
                            .getMessage());
            usernameRecoverDTO.setUsername(StringUtils.EMPTY);
        }
        return usernameRecoverDTO;
    }

    /**
     * Invalidate the recovery code.
     *
     * @param recoveryCode Recovery code
     */
    private void invalidateRecoveryCode(String recoveryCode) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(recoveryCode);
    }

    /**
     * Trigger notification to send userName recovery information.
     *
     * @param user                User
     * @param notificationChannel Notification channel
     * @param eventName           Event name
     * @param metaProperties      Meta properties to be send with the notification.
     * @throws IdentityRecoveryException Error while triggering notification.
     */
    private void triggerNotification(User user, String notificationChannel, String eventName,
                                     Map<String, String> metaProperties)
            throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);
        if (metaProperties != null) {
            for (String key : metaProperties.keySet()) {
                String value = metaProperties.get(key);
                if (StringUtils.isNotBlank(key) && StringUtils.isNotBlank(value)) {
                    properties.put(key, value);
                }
            }
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE,
                IdentityRecoveryConstants.NOTIFICATION_ACCOUNT_ID_RECOVERY);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    /**
     * Extract the channel that matches the channelId from the channels stored in recovery data.
     *
     * @param recoveryChannels All available recovery channels
     * @param channelId        User preferred channelId
     * @throws IdentityRecoveryException Invalid channelId
     */
    private String extractNotificationChannelDetails(String recoveryChannels, int channelId)
            throws IdentityRecoveryException {

        String[] channels = recoveryChannels.split(IdentityRecoveryConstants.NOTIFY_CHANNEL_LIST_SEPARATOR);
        if (channels.length < channelId) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        String notificationChannel = channels[channelId - 1];
        String[] channelDetails = notificationChannel.split(IdentityRecoveryConstants.CHANNEL_ATTRIBUTE_SEPARATOR);
        return channelDetails[0];
    }

    /**
     * Check whether the configurations are enabled for username recovery.
     *
     * @param tenantDomain String tenant domain
     * @throws IdentityRecoveryException Configurations are not enabled.
     */
    private void validateConfigurations(String tenantDomain) throws IdentityRecoveryException {

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE,
                        tenantDomain));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLED, null);
        }
    }

    /**
     * Validate the callback url.
     *
     * @param properties Map of notification recovery properties
     * @param user       User
     * @throws IdentityRecoveryException Invalid callback url
     */
    private void validateCallbackURL(Map<String, String> properties, User user) throws IdentityRecoveryException {

        String callbackURL;
        callbackURL = getCallbackURL(properties);
        try {
            if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, user.getTenantDomain(),
                    IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX)) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
            }
        } catch (IdentityEventException e) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }
    }

    /**
     * Get the callback url from the map of properties.
     *
     * @param properties Map of properties
     * @return Callback url
     * @throws IdentityRecoveryServerException Error while getting the callback url
     */
    private String getCallbackURL(Map<String, String> properties) throws IdentityRecoveryServerException {

        String callbackURL = null;
        try {
            if (MapUtils.isNotEmpty(properties)) {
                for (String key : properties.keySet()) {
                    if (IdentityRecoveryConstants.CALLBACK.equals(key)) {
                        callbackURL = URLDecoder.decode(properties.get(key), IdentityRecoveryConstants.UTF_8);
                        break;
                    }
                }
                if (StringUtils.isNotBlank(callbackURL)) {
                    URI uri = new URI(callbackURL);
                    callbackURL = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null,
                            null).toString();
                }
            }
        } catch (UnsupportedEncodingException | URISyntaxException e) {
            String error = "Error getting callback url";
            if (log.isDebugEnabled()) {
                log.debug(error, e);
            }
            // Get the new error code to with the scenario prepended the existing error code.
            String mappedErrorCode = Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
            throw Utils.handleServerException(mappedErrorCode,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID.getMessage(),
                    callbackURL);
        }
        return callbackURL;
    }

    /**
     * Validates the tenant domain in the request.
     *
     * @param tenantDomain Tenant domain
     * @throws IdentityRecoveryClientException Empty tenant domain in the request
     */
    private void validateTenantDomain(String tenantDomain) throws IdentityRecoveryClientException {

        if (StringUtils.isBlank(tenantDomain)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_EMPTY_TENANT_DOMAIN.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_EMPTY_TENANT_DOMAIN
                            .getMessage(), null);
        }
    }
}
