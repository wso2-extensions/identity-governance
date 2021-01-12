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

package org.wso2.carbon.identity.recovery.password;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.slf4j.MDC;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.recovery.AuditConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;

import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.util.HashMap;

import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.AUDIT_FAILED;

/**
 * Manager class which can be used to recover passwords using a notification.
 */
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);
    private static NotificationPasswordRecoveryManager instance = new NotificationPasswordRecoveryManager();

    private NotificationPasswordRecoveryManager() {

    }

    public static NotificationPasswordRecoveryManager getInstance() {

        return instance;
    }

    /**
     * Send password recovery information to the user.
     *
     * @param user       User
     * @param type       Notification Type
     * @param notify     Manage notifications internally
     * @param properties Meta properties
     * @return NotificationResponseBean
     * @throws IdentityRecoveryException Error while sending recovery information.
     */
    public NotificationResponseBean sendRecoveryNotification(User user, String type, Boolean notify,
                                                             Property[] properties)
            throws IdentityRecoveryException {

        publishEvent(user, String.valueOf(notify), null, null, properties,
                IdentityEventConstants.Event.PRE_SEND_RECOVERY_NOTIFICATION);

        Utils.validateEmailUsername(user.getUserName());

        // Resolve user attributes.
        resolveUserAttributes(user);
        validatePasswordRecoveryConfiguration(user.getTenantDomain());
        validateCallback(properties, user.getTenantDomain());

        // Build a property map from the properties in the request.
        HashMap<String, String> propertyMap = buildPropertyMap(properties);
        String notificationChannel = getNotificationChannelFromProperties(propertyMap);

        // Check whether to manage notifications internally.
        boolean isNotificationInternallyManage = isNotificationsInternallyManaged(user.getTenantDomain(), notify);
        if (!isNotificationInternallyManage) {
            notificationChannel = NotificationChannels.EXTERNAL_CHANNEL.getChannelType();
        }
        // Check whether the user is already verified ( NOTE: This property is set by the new recovery API).
        if (!isUserVerified(propertyMap)) {
            if (!isExistingUser(user)) {

                /* If the user does not exist, Check for NOTIFY_USER_EXISTENCE property. If the property is not
                enabled, notify with an empty NotificationResponseBean.*/
                boolean notifyUserExistence = Boolean.parseBoolean(
                        IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE));
                if (notifyUserExistence) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                            user.getUserName());
                }
                return new NotificationResponseBean(user);
            }
        }
        checkAccountLockedStatus(user);
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);
        String secretKey = Utils.generateSecretKey(notificationChannel, user.getTenantDomain(),
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name());
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);

        // Store the notified channel in the recovery object for future reference.
        recoveryDataDO.setRemainingSetIds(notificationChannel);
        userRecoveryDataStore.store(recoveryDataDO);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        if (isNotificationInternallyManage) {
            // Manage notifications by the identity server.
            String eventName = Utils.resolveEventName(notificationChannel);
            triggerNotification(user, notificationChannel, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET,
                    secretKey, eventName, properties);
        } else {
            // Set password recovery key since the notifications are managed by an external mechanism.
            notificationResponseBean.setKey(secretKey);
        }
        publishEvent(user, String.valueOf(notify), null, null, properties,
                IdentityEventConstants.Event.POST_SEND_RECOVERY_NOTIFICATION);
        return notificationResponseBean;
    }

    /**
     * Check whether the account is locked or disabled.
     *
     * @param user User
     * @throws IdentityRecoveryException If account is in locked or disabled status
     */
    private void checkAccountLockedStatus(User user) throws IdentityRecoveryException {

        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT,
                    user.getUserName());
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT,
                    user.getUserName());
        }
    }

    /**
     * Check for user existence.
     *
     * @param user User
     * @return True if the user exists
     * @throws IdentityRecoveryException Error while checking user existence
     */
    private boolean isExistingUser(User user) throws IdentityRecoveryException {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager;
            userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil
                    .addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                if (log.isDebugEnabled()) {
                    log.debug("No user found for recovery with username: " + user.toFullQualifiedUsername());
                }
                return false;
            }
            return true;
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }
    }

    /**
     * Check whether notifications are internally managed.
     *
     * @param tenantDomain Tenant Domain
     * @param notify       Manage notifications internally
     * @return True of the notifications are internally managed
     * @throws IdentityRecoveryException Error while checking for configurations
     */
    private boolean isNotificationsInternallyManaged(String tenantDomain, Boolean notify)
            throws IdentityRecoveryException {

        if (notify == null) {
            boolean manageNotificationsInternally = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            tenantDomain));
            if (log.isDebugEnabled()) {
                log.debug("Notify parameter not in the request. ManageNotificationsInternally set to " +
                        "server default value: " + manageNotificationsInternally);
            }
            return manageNotificationsInternally;
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Notify parameter in the request. ManageNotificationsInternally set to : " + notify);
            }
            return notify;
        }
    }

    /**
     * Check whether the VERIFIED_USER_PROPERTY_KEY set to TRUE or FALSE in the property map. If the property is not
     * set, return FALSE.
     *
     * @param propertyMap Properties
     * @return TRUE if the user is already verified.
     */
    private boolean isUserVerified(HashMap<String, String> propertyMap) {

        if (MapUtils.isEmpty(propertyMap)) {
            if (log.isDebugEnabled()) {
                log.debug("Empty property map received in the recovery request");
            }
            return false;
        }
        String verified = propertyMap.get(IdentityRecoveryConstants.VERIFIED_USER_PROPERTY_KEY);
        if (StringUtils.isBlank(verified)) {
            if (log.isDebugEnabled()) {
                log.debug("Property : verifiedUser is not in the property map. Hence returning FALSE");
            }
            return false;
        }
        try {
            return Boolean.parseBoolean(verified);
        } catch (NumberFormatException e) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("Value : %s given for property : %s is not a boolean. Therefore, "
                                        + "returning false for the property.", verified,
                                IdentityRecoveryConstants.VERIFIED_USER_PROPERTY_KEY);
                log.debug(message);
            }
        }
        return false;
    }

    /**
     * Get notification channel from a given map properties.
     *
     * @param propertyMap Properties
     * @return Notification channel
     */
    private String getNotificationChannelFromProperties(HashMap<String, String> propertyMap) {

        if (MapUtils.isEmpty(propertyMap)) {
            return NotificationChannels.EMAIL_CHANNEL.getChannelType();
        }
        String notificationChannel = propertyMap.get(IdentityRecoveryConstants.NOTIFICATION_CHANNEL_PROPERTY_KEY);
        return getServerSupportedNotificationChannel(notificationChannel);
    }

    /**
     * Get server supported notification channel.
     *
     * @param channel Notification channel
     * @return Server supported notification channel
     */
    private String getServerSupportedNotificationChannel(String channel) {

        if (StringUtils.isEmpty(channel)) {
            if (log.isDebugEnabled()) {
                String message =
                        "No notification channel in the request properties. Configuring the notification channel" +
                                " to: " + NotificationChannels.EMAIL_CHANNEL.getChannelType();
                log.debug(message);
            }
            return NotificationChannels.EMAIL_CHANNEL.getChannelType();
        }
        // Validate notification channels.
        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else {
            String defaultNotificationChannel = IdentityGovernanceUtil.getDefaultNotificationChannel();
            if (log.isDebugEnabled()) {
                String message = String.format("Not a server supported notification channel : %s. Therefore "
                                + "default notification channel : %s will be used.", channel,
                        defaultNotificationChannel);
                log.debug(message);
            }
            return defaultNotificationChannel;
        }
    }

    /**
     * Build a map of properties.
     *
     * @param properties Property list
     * @return Properties
     */
    private HashMap<String, String> buildPropertyMap(Property[] properties) {

        HashMap<String, String> propertyMap = new HashMap<>();
        if (ArrayUtils.isNotEmpty(properties)) {
            for (Property property : properties) {
                if (StringUtils.isNotEmpty(property.getKey())) {
                    propertyMap.put(property.getKey(), property.getValue());
                }
            }
        }
        return propertyMap;
    }

    /**
     * Validate the callback Url.
     *
     * @param properties   Properties
     * @param tenantDomain Tenant Domain
     * @throws IdentityRecoveryServerException Error validating the callback
     */
    private void validateCallback(Property[] properties, String tenantDomain) throws IdentityRecoveryServerException {

        String callbackURL = null;
        try {
            callbackURL = Utils.getCallbackURL(properties);
            if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, tenantDomain,
                    IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX)) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
            }
        } catch (URISyntaxException | UnsupportedEncodingException | IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }
    }

    /**
     * Verify whether password recovery is enabled for the tenant domain.
     *
     * @param tenantDomain String tenant domain
     * @throws IdentityRecoveryException Error while validating configurations
     */
    private void validatePasswordRecoveryConfiguration(String tenantDomain) throws IdentityRecoveryException {

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                        tenantDomain));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_BASED_PASSWORD_RECOVERY_NOT_ENABLE,
                    null);
        }
    }

    /**
     * Resolve tenant domain and user store domain of the user.
     *
     * @param user User
     */
    private void resolveUserAttributes(User user) {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            if (log.isDebugEnabled()) {
                log.debug("SendRecoveryNotification :Tenant domain is not in the request. set to default for " +
                        "user : " + user.getUserName());
            }
        }
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            if (log.isDebugEnabled()) {
                log.debug("SendRecoveryNotification : User store domain is not in the request. set to " +
                        "default for user : " + user.getUserName());
            }
        }
    }

    /**
     * Update the password of the user.
     *
     * @param code       Password Reset code
     * @param password   New password
     * @param properties Properties
     * @throws IdentityRecoveryException Error while updating the password
     * @throws IdentityEventException    Error while updating the password
     */
    public void updatePassword(String code, String password, Property[] properties)
            throws IdentityRecoveryException, IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(code);
        validateCallback(properties, userRecoveryData.getUser().getTenantDomain());
        publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                IdentityEventConstants.Event.PRE_ADD_NEW_PASSWORD);
        validateTenantDomain(userRecoveryData.getUser());

        // Validate recovery step.
        if (!RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);
        }
        // Get the notification channel.
        String notificationChannel = getServerSupportedNotificationChannel(userRecoveryData.getRemainingSetIds());
        boolean notificationsInternallyManaged = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                        userRecoveryData.getUser().getTenantDomain()));
        boolean isNotificationSendWhenSuccess = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                userRecoveryData.getUser().getTenantDomain()));
        String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(),
                userRecoveryData.getUser().getUserStoreDomain());

        // Update the password.
        updateNewPassword(userRecoveryData.getUser(), password, domainQualifiedName, userRecoveryData,
                notificationsInternallyManaged);
        userRecoveryDataStore.invalidate(userRecoveryData.getUser());
        if (notificationsInternallyManaged && isNotificationSendWhenSuccess
                && !NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
            try {
                String eventName = Utils.resolveEventName(notificationChannel);
                triggerNotification(userRecoveryData.getUser(), notificationChannel,
                        IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS, StringUtils.EMPTY,
                        eventName, properties);
            } catch (IdentityRecoveryException e) {
                String errorMsg = String.format("Error while sending password reset success notification to user : %s",
                        userRecoveryData.getUser().getUserName());
                log.error(errorMsg);
                auditPasswordReset(AuditConstants.ACTION_PASSWORD_RESET, userRecoveryData.getUser(), errorMsg,
                        FrameworkConstants.AUDIT_SUCCESS);
            }
        }
        publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                IdentityEventConstants.Event.POST_ADD_NEW_PASSWORD);
        if (log.isDebugEnabled()) {
            String msg = "Password is updated for  user: " + domainQualifiedName;
            log.debug(msg);
        }
        auditPasswordReset(AuditConstants.ACTION_PASSWORD_RESET, userRecoveryData.getUser(), null,
                FrameworkConstants.AUDIT_SUCCESS);
    }

    /**
     * Update the new password of the user.
     *
     * @param user                            User
     * @param password                        New password
     * @param domainQualifiedName             Domain qualified name
     * @param userRecoveryData                User recovery data
     * @param isNotificationInternallyManaged Whether the notifications are internally managed.
     * @throws IdentityRecoveryException Error while checking for account state claim
     * @throws IdentityRecoveryException Error while updating the password
     */
    private void updateNewPassword(User user, String password, String domainQualifiedName,
                                   UserRecoveryData userRecoveryData,
                                   boolean isNotificationInternallyManaged)
            throws IdentityEventException, IdentityRecoveryException {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(userRecoveryData.getUser().getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(domainQualifiedName, password);

            // Get the claims that related to a password reset.
            HashMap<String, String> userClaims = getAccountStateClaims(userRecoveryData,
                    isNotificationInternallyManaged);
            if (MapUtils.isNotEmpty(userClaims)) {
                // Update the retrieved claims set.
                userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
            }
        } catch (UserStoreException e) {
            checkPasswordValidity(e, user);
            if (log.isDebugEnabled()) {
                log.debug("NotificationPasswordRecoveryManager: Unexpected Error occurred while updating password "
                        + "for the user: " + domainQualifiedName, e);
            }
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }
    }

    /**
     * Get the claims that needs to be updated when a user attempts a password reset.
     *
     * @param userRecoveryData                User recovery data
     * @param isNotificationInternallyManaged Whether the notifications are internally managed
     * @return Claims that are related to account state
     * @throws IdentityEventException Error while checking for account state claim
     */
    private HashMap<String, String> getAccountStateClaims(UserRecoveryData userRecoveryData,
                                                          boolean isNotificationInternallyManaged)
            throws IdentityEventException {

        HashMap<String, String> userClaims = new HashMap<>();
        // If notifications are internally managed we try to set the verified claims since this is an opportunity
        // to verify a user channel.
        if (isNotificationInternallyManaged) {
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds())) {
                userClaims.put(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds())) {
                userClaims.put(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            } else {
                if (log.isDebugEnabled()) {
                    String error = String
                            .format("No notification channels for the user : %s in tenant domain : " + "%s",
                                    userRecoveryData.getUser().getUserStoreDomain() + userRecoveryData.getUser()
                                            .getUserName(), userRecoveryData.getUser().getTenantDomain());
                    log.debug(error);
                }
                userClaims.put(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            }
            if (Utils.isAccountStateClaimExisting(userRecoveryData.getUser().getTenantDomain())) {
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                        IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM, StringUtils.EMPTY);
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
            }
        }
        // If the scenario is initiated by the admin, set the account locked claim to TRUE.
        if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(userRecoveryData.getRecoveryScenario())
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.
                equals(userRecoveryData.getRecoveryScenario())) {
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
            userClaims.remove(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
            userClaims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM);
        }
        return userClaims;
    }

    /**
     * Validate Tenant domain of the user with the domain in the context.
     *
     * @param user User
     * @throws IdentityRecoveryClientException Tenant domain miss match
     */
    private void validateTenantDomain(User user) throws IdentityRecoveryClientException {

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = user.getTenantDomain();
        if (!StringUtils.equals(contextTenantDomain, userTenantDomain)) {
            throw new IdentityRecoveryClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_TENANT_DOMAIN_MISS_MATCH_WITH_CONTEXT
                            .getCode(), "invalid tenant domain: " + userTenantDomain);
        }
    }

    private void checkPasswordValidity(UserStoreException e, User user) throws IdentityRecoveryClientException {

        Throwable cause = e.getCause();
        while (cause != null) {
            if (cause instanceof IdentityEventException) {
                String errorCode = ((IdentityEventException) cause).getErrorCode();
                if (StringUtils.equals(errorCode, "22001")) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_HISTORY_VIOLATE, null, e);
                }
            }

            if (cause instanceof PolicyViolationException) {
                throw IdentityException.error(IdentityRecoveryClientException.class,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
                        cause.getMessage(), e);
            }
            cause = cause.getCause();
        }
        Utils.checkPasswordPatternViolation(e, user);

    }

    /**
     * Trigger notification to send userName recovery information.
     *
     * @param user                User
     * @param notificationChannel Notification channel
     * @param templateName        Notification Template name
     * @param code                Secret key
     * @param eventName           Event name
     * @param metaProperties      Meta properties to be send with the notification.
     * @throws IdentityRecoveryException Error while triggering notification.
     */
    private void triggerNotification(User user, String notificationChannel, String templateName, String code,
                                     String eventName, Property[] metaProperties) throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, templateName);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
            auditPasswordRecovery(AuditConstants.ACTION_PASSWORD_RECOVERY, notificationChannel, user, null,
                    FrameworkConstants.AUDIT_SUCCESS);
        } catch (IdentityEventException e) {
            auditPasswordRecovery(AuditConstants.ACTION_PASSWORD_RECOVERY, notificationChannel, user, e.getMessage(),
                    FrameworkConstants.AUDIT_FAILED);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }

    }

    private void publishEvent(User user, String notify, String code, String password, Property[] metaProperties,
                              String eventName) throws
            IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }

        if (StringUtils.isNotBlank(notify)) {
            properties.put(IdentityRecoveryConstants.NOTIFY, notify);
        }

        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            log.error("Error occurred while publishing event " + eventName + " for user " + user);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PUBLISH_EVENT,
                    eventName, e);
        }

    }

    /**
     * Method to validate confirmation code of password reset flow.
     *
     * @param code         confirmation code
     * @param recoveryStep recovery step
     * @throws IdentityRecoveryException
     */
    public void validateConfirmationCode(String code, String recoveryStep) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(code);
        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = userRecoveryData.getUser().getTenantDomain();
        if (!StringUtils.equals(contextTenantDomain, userTenantDomain)) {
            throw new IdentityRecoveryClientException("Invalid tenant domain: " + userTenantDomain);
        }
        if (StringUtils.isNotBlank(recoveryStep) && !recoveryStep.equals(userRecoveryData.getRecoveryStep().name())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }
        String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(),
                userRecoveryData.getUser().getUserStoreDomain());
        if (log.isDebugEnabled()) {
            log.debug("Valid confirmation code for user: " + domainQualifiedName);
        }

    }

    private void auditPasswordRecovery(String action, String notificationChannel, User user, String errorMsg,
                                       String result) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.NOTIFICATION_CHANNEL, notificationChannel);
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        dataObject.put(AuditConstants.USER_STORE_DOMAIN, user.getUserStoreDomain());
        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(action, user.getUserName(), dataObject, result);
    }

    private void auditPasswordReset(String action, User user, String errorMsg, String result) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        dataObject.put(AuditConstants.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(action, user.getUserName(), dataObject, result);
    }
}
