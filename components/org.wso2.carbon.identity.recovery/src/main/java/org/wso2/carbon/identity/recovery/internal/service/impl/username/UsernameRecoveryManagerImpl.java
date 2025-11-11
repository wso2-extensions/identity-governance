/*
 * Copyright (c) 2020-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.internal.service.impl.username;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.slf4j.MDC;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.organization.management.service.exception.OrganizationManagementException;
import org.wso2.carbon.identity.recovery.AuditConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.dto.NotificationChannelDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryChannelInfoDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.UsernameRecoverDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.services.username.UsernameRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.user.mgt.listeners.utils.ListenerUtils;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.AUDIT_FAILED;

/**
 * Class that implements the UsernameRecoveryManager.
 */
public class UsernameRecoveryManagerImpl implements UsernameRecoveryManager {

    private static final Log log = LogFactory.getLog(UsernameRecoveryManagerImpl.class);
    private static final String NOTIFICATION_TYPE_INTERNAL = "Internal";
    private static final String NOTIFICATION_TYPE_EXTERNAL = "External";

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

        String appResidentOrgId = PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getApplicationResidentOrganizationId();
        if (StringUtils.isNotBlank(appResidentOrgId)) {
            tenantDomain = getAppResidentTenantDomain(appResidentOrgId);
        }
        validateTenantDomain(tenantDomain);
        validateConfigurations(tenantDomain);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        RecoveryInformationDTO recoveryInformationDTO = new RecoveryInformationDTO();

        boolean useLegacyAPIApproach = useLegacyAPIApproach(properties);
        boolean manageNotificationsInternally = Utils.isNotificationsInternallyManaged(tenantDomain, properties);
        if (useLegacyAPIApproach) {
            // Use legacy API approach to support legacy username recovery.
            ArrayList<org.wso2.carbon.user.core.common.User> resultedUserList = userAccountRecoveryManager
                    .getUserListByClaims(claims, tenantDomain);
            for (org.wso2.carbon.user.core.common.User recoveredUser : resultedUserList) {
                String username = recoveredUser.getDomainQualifiedUsername();
                if (StringUtils.isNotEmpty(username)) {
                    if (manageNotificationsInternally) {
                        User user = createUser(username, tenantDomain);
                        triggerNotification(user, NotificationChannels.EMAIL_CHANNEL.getChannelType(),
                                IdentityEventConstants.Event.TRIGGER_NOTIFICATION, null);
                        if (log.isDebugEnabled()) {
                            log.debug("Successful username recovery for user: " + username + ". " +
                                    "User notified Internally");
                        }
                        auditUserNameRecovery(AuditConstants.ACTION_USERNAME_RECOVERY, claims, NOTIFICATION_TYPE_INTERNAL,
                                username, null, FrameworkConstants.AUDIT_SUCCESS);
                    }
                    if (log.isDebugEnabled()) {
                        log.debug("Successful username recovery for user: " + username + ". User notified Externally");
                    }
                    auditUserNameRecovery(AuditConstants.ACTION_USERNAME_RECOVERY, claims, NOTIFICATION_TYPE_EXTERNAL,
                            username, null, FrameworkConstants.AUDIT_SUCCESS);
                    recoveryInformationDTO.setUsername(username);
                } else {
                    String errorMsg =
                            String.format("No user found for the given claims in tenant domain : %s", tenantDomain);
                    if (log.isDebugEnabled()) {
                        log.debug(errorMsg);
                    }
                    auditUserNameRecovery(AuditConstants.ACTION_USERNAME_RECOVERY, claims, "N/A", username, errorMsg,
                            FrameworkConstants.AUDIT_FAILED);
                    if (Boolean.parseBoolean(
                            IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE))) {
                        throw Utils.handleClientException(
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND, null);
                    }
                }
            }
            return recoveryInformationDTO;
        }
        // Add notification method in a meta property list.
        Map<String, String> metaProperties = new HashMap<>();
        metaProperties.put(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY,
                Boolean.toString(manageNotificationsInternally));

        boolean nonUniqueUsernameEnabled = Boolean.parseBoolean(IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_NON_UNIQUE_USERNAME));

        RecoveryChannelInfoDTO recoveryChannelInfoDTO;
        if (nonUniqueUsernameEnabled) {
            recoveryChannelInfoDTO = userAccountRecoveryManager
                    .retrieveUsersRecoveryInformationForUsername(claims, tenantDomain, metaProperties);
        } else {
            recoveryChannelInfoDTO = userAccountRecoveryManager
                    .retrieveUserRecoveryInformation(claims, tenantDomain, RecoveryScenarios.USERNAME_RECOVERY,
                            metaProperties);
        }

        // Filtering the notification channel list.
        List<NotificationChannelDTO> enabledNotificationChannelDTOs = new ArrayList<>();
        for (NotificationChannelDTO notificationChannelDTO : recoveryChannelInfoDTO.getNotificationChannelDTOs()) {
            if (isRecoveryChannelEnabled(notificationChannelDTO.getType(), tenantDomain)) {
                enabledNotificationChannelDTOs.add(notificationChannelDTO);
            }
        }
        recoveryChannelInfoDTO.setNotificationChannelDTOs(
                enabledNotificationChannelDTOs.toArray(new NotificationChannelDTO[0]));
        String username = recoveryChannelInfoDTO.getUsername();
        String recoveryFlowId = recoveryChannelInfoDTO.getRecoveryFlowId();
        recoveryInformationDTO.setUsername(username);
        recoveryInformationDTO.setRecoveryFlowId(recoveryFlowId);
        // Do not add recovery channel information if Notification based recovery is not enabled.
        recoveryInformationDTO.setRecoveryChannelInfoDTO(recoveryChannelInfoDTO);
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

        String appResidentOrgId = PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getApplicationResidentOrganizationId();
        if (StringUtils.isNotBlank(appResidentOrgId)) {
            tenantDomain = getAppResidentTenantDomain(appResidentOrgId);
        }
        validateTenantDomain(tenantDomain);
        int channelIdCode = validateChannelID(channelId);
        validateConfigurations(tenantDomain);
        UserAccountRecoveryManager recoveryAccountManager = UserAccountRecoveryManager.getInstance();

        // Validate Recovery data.
        UserRecoveryData userRecoveryData = recoveryAccountManager
                .getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION);
        String recoveryFlowId = userRecoveryData.getRecoveryFlowId();
        if (recoveryFlowId != null) {
            invalidateRecoveryFlowId(recoveryFlowId);
        } else {
            invalidateRecoveryCode(recoveryCode);
        }
        String notificationChannel = extractNotificationChannelDetails(userRecoveryData.getRemainingSetIds(),
                channelIdCode);

        // If the notifications are externally managed we do not need to send notifications internally.
        if (!NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
            String eventName;
            if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannel)) {
                eventName = IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + notificationChannel
                        + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX;
            } else {
                eventName = Utils.resolveEventName(notificationChannel);
            }
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

        return Boolean.parseBoolean(properties.get(IdentityRecoveryConstants.USE_LEGACY_API_PROPERTY_KEY));
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
        user.setUserName(UserCoreUtil.removeDomainFromName(userName));
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
        if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
            usernameRecoverDTO.setCode(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_EXTERNALLY_NOTIFIED.getCode());
            usernameRecoverDTO.setMessage(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_USERNAME_EXTERNALLY_NOTIFIED
                            .getMessage());

            // If notifications are externally managed, username needs to be sent with the request.
            // Build username for external notification.
            StringBuilder usernameCombined = new StringBuilder();
            String[] usernames = user.getUserName().split(",");
            for (String username : usernames) {
                if(usernameCombined.length() > 0) {
                    usernameCombined.append(",");
                }
                usernameCombined.append(String.format(qualifiedUsernameRegexPattern, username, user.getTenantDomain()));
            }
            usernameRecoverDTO.setUsername(usernameCombined.toString());
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
     * Invalidate the recovery flow id.
     *
     * @param recoveryFlowId Recovery flow id.
     * @throws IdentityRecoveryException If an error occurred while invalidating recovery data.
     */
    private void invalidateRecoveryFlowId(String recoveryFlowId) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidateWithRecoveryFlowId(recoveryFlowId);
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

        if (!isRecoveryChannelEnabled(notificationChannel, user.getTenantDomain())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }

        String combinedUsernames = user.getUserName();
        String[] usernames = combinedUsernames.split(",");
        String[] userStoreDomains = null;
        if (user.getUserStoreDomain() != null) {
            userStoreDomains = user.getUserStoreDomain().split(",");
        }

        int userIndex = 0;
        for (String username : usernames) {
            HashMap<String, Object> properties = new HashMap<>();
            properties.put(IdentityEventConstants.EventProperty.USER_NAME, username);
            properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());

            String userStoreDomain = user.getUserStoreDomain();
            if (userStoreDomains != null && userStoreDomains.length > userIndex) {
                userStoreDomain = userStoreDomains[userIndex];
            }
            userIndex++;
            properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, userStoreDomain);
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

    private void auditUserNameRecovery(String action, Map<String, String> claims, String notificationType,
                                       String target, String errorMsg, String result) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(ListenerUtils.CLAIMS_FIELD, new JSONObject(claims));
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.USER_NOTIFIED_TYPE_KEY, notificationType);
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(action, target, dataObject, result);
    }

    private boolean isRecoveryChannelEnabled(String notificationChannelType, String tenantDomain)
            throws IdentityRecoveryServerException {

        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(notificationChannelType)) {
            return isEmailBasedRecoveryEnabled(tenantDomain);
        } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannelType)) {
            return isSMSBasedRecoveryEnabled(tenantDomain);
        }
        return false;
    }

    private boolean isEmailBasedRecoveryEnabled(String tenantDomain) throws IdentityRecoveryServerException {

        try {
            return Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_EMAIL_ENABLE,
                            tenantDomain));
        } catch (IdentityRecoveryServerException e) {
            // Prepend scenario to the thrown exception.
            String errorCode = Utils
                    .prependOperationScenarioToErrorCode(IdentityRecoveryConstants.USER_NAME_RECOVERY,
                            e.getErrorCode());
            throw Utils.handleServerException(errorCode, e.getMessage(), null);
        }
    }

    private boolean isSMSBasedRecoveryEnabled(String tenantDomain) throws IdentityRecoveryServerException {

        try {
            return Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_SMS_ENABLE,
                            tenantDomain));
        } catch (IdentityRecoveryServerException e) {
            // Prepend scenario to the thrown exception.
            String errorCode = Utils
                    .prependOperationScenarioToErrorCode(IdentityRecoveryConstants.USER_NAME_RECOVERY,
                            e.getErrorCode());
            throw Utils.handleServerException(errorCode, e.getMessage(), null);
        }
    }


    private String getAppResidentTenantDomain(String appResidentOrgId) throws IdentityRecoveryServerException {

        if (log.isDebugEnabled()) {
            log.debug("Resolving tenant domain for application resident organization ID : " + appResidentOrgId);
        }
        try {
            return IdentityRecoveryServiceDataHolder.getInstance().getOrganizationManager()
                    .resolveTenantDomain(appResidentOrgId);
        } catch (OrganizationManagementException e) {
            throw new IdentityRecoveryServerException("Error while resolving tenant domain for " +
                    "application resident organization ID : " + appResidentOrgId, e);
        }
    }
}
