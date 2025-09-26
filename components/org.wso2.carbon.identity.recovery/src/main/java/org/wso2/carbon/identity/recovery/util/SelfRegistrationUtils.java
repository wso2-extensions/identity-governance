/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerClientException;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.CarbonConstants.DOMAIN_SEPARATOR;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.Consent.SERVICE_PROVIDER_UUID;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_LINK;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.OTP_CODE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.TEMPLATE_TYPE;

/**
 * Utility class for self registration listener and handler.
 */
public class SelfRegistrationUtils {

    private static final Log LOG = LogFactory.getLog(SelfRegistrationUtils.class);

    /**
     * Handles NotificationChannelManagerException thrown in resolving the channel.
     *
     * @param e            NotificationChannelManagerException
     * @param userName     Username
     * @param domainName   Domain name
     * @param tenantDomain Tenant domain name
     * @throws IdentityEventException Error resolving the channel.
     */
    public static void handledNotificationChannelManagerException(NotificationChannelManagerException e,
                                                                  String userName, String domainName,
                                                                  String tenantDomain)
            throws IdentityEventException {

        if (StringUtils.isNotEmpty(e.getErrorCode()) && StringUtils.isNotEmpty(e.getMessage())) {
            if (IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getCode()
                    .equals(e.getErrorCode())) {
                if (LOG.isDebugEnabled()) {
                    String error = String.format("No communication channel for user : %1$s in domain: %2$s",
                                                 domainName + DOMAIN_SEPARATOR + maskIfRequired(userName),
                                                 tenantDomain);
                    LOG.debug(error, e);
                }
            } else {
                if (LOG.isDebugEnabled()) {
                    String error = String.format("Error getting claim values for user : %1$s in domain: %2$s",
                                                 domainName + DOMAIN_SEPARATOR + maskIfRequired(userName),
                                                 tenantDomain);
                    LOG.debug(error, e);
                }
            }
        } else {
            if (LOG.isDebugEnabled()) {
                String error = String.format("Error getting claim values for user : %1$s in domain: %2$s",
                                             domainName + DOMAIN_SEPARATOR + maskIfRequired(userName),
                                             tenantDomain);
                LOG.debug(error, e);
            }
        }
        throw new IdentityEventException(e.getErrorCode(), e.getMessage());
    }

    /**
     * Get the NotificationChannels object which matches the given channel type.
     *
     * @param username                Username.
     * @param notificationChannelName Notification channel name.
     * @return NotificationChannels object.
     * @throws IdentityRecoveryClientException Unsupported channel type.
     */
    public static NotificationChannels getNotificationChannel(String username, String notificationChannelName)
            throws IdentityRecoveryClientException {

        NotificationChannels channel;
        try {
            channel = NotificationChannels.getNotificationChannel(notificationChannelName);
        } catch (NotificationChannelManagerClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Unsupported channel type : " + notificationChannelName);
            }
            throw Utils.handleClientException(ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS, username, e);
        }
        return channel;
    }

    /**
     * Get the user store manager for the given tenant domain and user domain.
     *
     * @param tenantDomain Tenant domain.
     * @param userDomain   User domain.
     * @return UserStoreManager instance.
     */
    public static UserStoreManager getUserStoreManager(String tenantDomain, String userDomain)
            throws UserStoreException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        UserStoreManager userStoreManager;

        UserRealm tenantUserRealm = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(tenantDomain));
        if (IdentityUtil.getPrimaryDomainName().equals(userDomain)) {
            userStoreManager = (UserStoreManager) tenantUserRealm.getUserStoreManager();
        } else {
            userStoreManager =
                    ((UserStoreManager) tenantUserRealm.getUserStoreManager()).getSecondaryUserStoreManager(userDomain);
        }
        return userStoreManager;
    }

    /**
     * Trigger account creation notification.
     *
     * @param username        Username of the signed-up user.
     * @param tenantDomain    Tenant domain of the signed-up user.
     * @param userStoreDomain User store domain of the signed-up user.
     * @throws IdentityRecoveryServerException Error while triggering the notification.
     */
    public static void triggerAccountCreationNotification(String username, String tenantDomain,
                                                          String userStoreDomain)
            throws IdentityRecoveryServerException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        String serviceProviderUUID = (String) IdentityUtil.threadLocalProperties.get()
                .get(SERVICE_PROVIDER_UUID);

        HashMap<String, Object> properties = new HashMap<>();
        if (serviceProviderUUID != null && !serviceProviderUUID.isEmpty()) {

            properties.put(SERVICE_PROVIDER_UUID, serviceProviderUUID);
        }
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, username);
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, userStoreDomain);
        properties.put(TEMPLATE_TYPE,
                       IdentityRecoveryConstants.NOTIFICATION_TYPE_SELF_SIGNUP_NOTIFY);

        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd/MM/yy hh:mm:ss");
        String selfSignUpConfirmationTime = simpleDateFormat.format(new Date(System.currentTimeMillis()));
        properties.put(IdentityEventConstants.EventProperty.SELF_SIGNUP_CONFIRM_TIME, selfSignUpConfirmationTime);

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(ERROR_CODE_TRIGGER_NOTIFICATION,
                                              username, e);
        }
    }

    /**
     * Lock user account on creation.
     *
     * @param isAccountLockOnCreation        isAccountLockOnCreation config value.
     * @param isEnableConfirmationOnCreation isEnableConfirmationOnCreation config value.
     * @param tenantDomain                   Tenant domain.
     * @param userStoreManager               UserStoreManager instance.
     * @param userName                       Username.
     * @throws IdentityEventException Error while locking user account.
     */
    public static void lockUserAccount(boolean isAccountLockOnCreation, boolean isEnableConfirmationOnCreation,
                                       String tenantDomain, UserStoreManager userStoreManager, String userName)
            throws IdentityEventException {

        if (userStoreManager == null) {
            throw new IdentityEventException("Error while lock user account :" + maskIfRequired(userName));
        }

        HashMap<String, String> userClaims = new HashMap<>();
        if (isAccountLockOnCreation) {
            // Need to lock user account.
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                           IdentityMgtConstants.LockedReason.PENDING_SELF_REGISTRATION.toString());
        }
        if (Utils.isAccountStateClaimExisting(tenantDomain)) {
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                           IdentityRecoveryConstants.PENDING_SELF_REGISTRATION);
        }
        try {
            userStoreManager.setUserClaimValues(userName, userClaims, null);
            if (LOG.isDebugEnabled()) {
                if (isAccountLockOnCreation) {
                    LOG.debug("Locked user account: " + maskIfRequired(userName));
                }
                if (isEnableConfirmationOnCreation) {
                    LOG.debug("Send verification notification for user account: " + maskIfRequired(userName));
                }
            }
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityEventException("Error while lock user account :" + maskIfRequired(userName), e);
        }
    }

    /**
     * Resolve the event name according to the notification channel.
     *
     * @param preferredChannel User preferred notification channel
     * @param userName         Username
     * @param domainName       Domain name
     * @param tenantDomain     Tenant domain name
     * @return Resolved event name
     */
    public static String resolveEventName(String preferredChannel, String userName, String domainName,
                                          String tenantDomain) {

        String eventName;
        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(preferredChannel)) {
            eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        } else {
            eventName = IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + preferredChannel
                    + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX;
        }
        if (LOG.isDebugEnabled()) {
            String message = String
                    .format("For user : %1$s in domain : %2$s, notifications were sent from the event : %3$s",
                            domainName + DOMAIN_SEPARATOR + maskIfRequired(userName), tenantDomain, eventName);
            LOG.debug(message);
        }
        return eventName;
    }

    /**
     * Triggers notifications according to the given event name.
     *
     * @param user                User
     * @param notificationChannel Notification channel
     * @param code                Recovery code
     * @param props               Event properties
     * @param eventName           Name of the event
     * @throws IdentityRecoveryException Error triggering notifications
     */
    public static void triggerNotification(User user, String notificationChannel, String code, Property[] props,
                                           String eventName) throws IdentityRecoveryException, IdentityEventException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Sending self user registration notification user: " + maskIfRequired(user.getUserName()));
        }

        String serviceProviderUUID = (String) IdentityUtil.threadLocalProperties.get().get(SERVICE_PROVIDER_UUID);

        HashMap<String, Object> properties = new HashMap<>();
        if (serviceProviderUUID != null && !serviceProviderUUID.isEmpty()) {
            properties.put(SERVICE_PROVIDER_UUID, serviceProviderUUID);
        }
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);

        if (props != null) {
            for (Property prop : props) {
                properties.put(prop.getKey(), prop.getValue());
            }
        }

        boolean isSelfRegistrationEmailOTPEnabled = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE, user.getTenantDomain()));
        String emailTemplateName = NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_LINK;

        if (StringUtils.isNotBlank(code)) {
            if (isSelfRegistrationEmailOTPEnabled) {
                properties.put(OTP_CODE, code);
                emailTemplateName = NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_OTP;
            } else {
                properties.put(CONFIRMATION_CODE, code);
            }
        }
        properties.put(TEMPLATE_TYPE, emailTemplateName);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(ERROR_CODE_TRIGGER_NOTIFICATION, user.getUserName(), e);
        }
    }

    /**
     * Mask the given value if it is required.
     *
     * @param value Value to be masked.
     * @return Masked/unmasked value.
     */
    public static String maskIfRequired(String value) {

        return LoggerUtils.isLogMaskingEnable ? LoggerUtils.getMaskedContent(value) : value;
    }
}
