/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerClientException;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UserSelfRegistrationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationHandler.class);

    public String getName() {
        return "userSelfRegistration";
    }

    public String getFriendlyName() {
        return "User Self Registration";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);

        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

        String[] roleList = (String[]) eventProperties.get(IdentityEventConstants.EventProperty.ROLE_LIST);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(domainName);

        boolean enable = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            //Self signup feature is disabled

            if (log.isDebugEnabled()) {
                log.debug("Self signup feature is disabled in tenant: " + tenantDomain);
            }
            return;
        }

        //Check selfSignupRole is in the request. If it is not there, this handler will not do anything. just retrun
        if (roleList == null) {
            return;
        } else {
            List<String> roles = Arrays.asList(roleList);
            if (!roles.contains(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                return;
            }
        }

        boolean isAccountLockOnCreation = Boolean.parseBoolean(Utils.getConnectorConfig
                (IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION, user.getTenantDomain()));

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getConnectorConfig
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));

        if (IdentityEventConstants.Event.POST_ADD_USER.equals(event.getEventName())) {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

            try {
                // Get the user preferred notification channel.
                String preferredChannel = resolveNotificationChannel(eventProperties, userName, tenantDomain,
                        domainName);

                // If the preferred channel is already verified, no need to send the notifications or lock
                // the account.
                boolean notificationChannelVerified = isNotificationChannelVerified(userName, tenantDomain,
                        preferredChannel, eventProperties);
                if (notificationChannelVerified) {
                    return;
                }
                // If notifications are externally managed, no send notifications.
                if (isNotificationInternallyManage && isAccountLockOnCreation) {
                    userRecoveryDataStore.invalidate(user);

                    // Create a secret key based on the preferred notification channel.
                    String secretKey = generateSecretKey(preferredChannel);

                    // Resolve event name.
                    String eventName = resolveEventName(preferredChannel, userName, domainName, tenantDomain);

                    UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                            RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

                    // Notified channel is stored in remaining setIds for recovery purposes.
                    recoveryDataDO.setRemainingSetIds(preferredChannel);
                    userRecoveryDataStore.store(recoveryDataDO);
                    triggerNotification(user, preferredChannel, secretKey, Utils.getArbitraryProperties(), eventName);
                }
            } catch (IdentityRecoveryException e) {
                throw new IdentityEventException("Error while sending self sign up notification ", e);
            }
            if (isAccountLockOnCreation) {
                HashMap<String, String> userClaims = new HashMap<>();
                //Need to lock user account
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                        IdentityMgtConstants.LockedReason.PENDING_SELF_REGISTRATION.toString());
                if (Utils.isAccountStateClaimExisting(tenantDomain)) {
                    userClaims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                            IdentityRecoveryConstants.PENDING_SELF_REGISTRATION);
                }
                try {
                    userStoreManager.setUserClaimValues(user.getUserName() , userClaims, null);
                    if (log.isDebugEnabled()) {
                        log.debug("Locked user account: " + user.getUserName());
                    }
                } catch (UserStoreException e) {
                    throw new IdentityEventException("Error while lock user account :" + user.getUserName(), e);
                }
            }

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
    private String resolveEventName(String preferredChannel, String userName, String domainName, String tenantDomain) {

        String eventName;
        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(preferredChannel)) {
            eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        } else {
            eventName = IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + preferredChannel
                    + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX;
        }
        if (log.isDebugEnabled()) {
            String message = String
                    .format("For user : %1$s in domain : %2$s, notifications were sent from the event : %3$s",
                            domainName + CarbonConstants.DOMAIN_SEPARATOR + userName, tenantDomain, eventName);
            log.debug(message);
        }
        return eventName;
    }

    /**
     * Resolve the preferred notification channel for the user.
     *
     * @param eventProperties Event properties
     * @param userName        Username
     * @param tenantDomain    Tenant domain of the user
     * @param domainName      Userstore domain name of the user
     * @return Resolved preferred notification channel
     * @throws IdentityEventException Error while resolving the notification channel
     */
    private String resolveNotificationChannel(Map<String, Object> eventProperties, String userName, String tenantDomain,
            String domainName) throws IdentityEventException {

        // If channel resolving logic is not enabled, return the server default notification channel. Do not need to
        // resolve using user preferred channel.
        if (!Boolean.parseBoolean(
                IdentityUtil.getProperty(IdentityMgtConstants.PropertyConfig.RESOLVE_NOTIFICATION_CHANNELS))) {
            return IdentityGovernanceUtil.getDefaultNotificationChannel();
        }
        // Get the user preferred notification channel.
        String preferredChannel = (String) eventProperties.get(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM);
        // Resolve preferred notification channel.
        if (StringUtils.isEmpty(preferredChannel)) {
            NotificationChannelManager notificationChannelManager = Utils.getNotificationChannelManager();
            try {
                preferredChannel = notificationChannelManager
                        .resolveCommunicationChannel(userName, tenantDomain, domainName);
            } catch (NotificationChannelManagerException e) {
                handledNotificationChannelManagerException(e, userName, domainName, tenantDomain);
            }
        }
        if (log.isDebugEnabled()) {
            String message = String
                    .format("Notification channel : %1$s for the user : %2$s in domain : %3$s.",
                            preferredChannel, domainName + CarbonConstants.DOMAIN_SEPARATOR + userName,
                            tenantDomain);
            log.debug(message);
        }
        return preferredChannel;
    }

    /**
     * Handles NotificationChannelManagerException thrown in resolving the channel.
     *
     * @param e            NotificationChannelManagerException
     * @param userName     Username
     * @param domainName   Domain name
     * @param tenantDomain Tenant domain name
     * @throws IdentityEventException Error resolving the channel.
     */
    private void handledNotificationChannelManagerException(NotificationChannelManagerException e, String userName,
            String domainName, String tenantDomain) throws IdentityEventException {

        if (StringUtils.isNotEmpty(e.getErrorCode()) && StringUtils.isNotEmpty(e.getMessage())) {
            if (IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getCode()
                    .equals(e.getErrorCode())) {
                if (log.isDebugEnabled()) {
                    String error = String.format("No communication channel for user : %1$s in domain: %2$s",
                            domainName + CarbonConstants.DOMAIN_SEPARATOR + userName, tenantDomain);
                    log.debug(error, e);
                }
            } else {
                if (log.isDebugEnabled()) {
                    String error = String.format("Error getting claim values for user : %1$s in domain: %2$s",
                            domainName + CarbonConstants.DOMAIN_SEPARATOR + userName, tenantDomain);
                    log.debug(error, e);
                }
            }
        } else {
            if (log.isDebugEnabled()) {
                String error = String.format("Error getting claim values for user : %1$s in domain: %2$s",
                        domainName + CarbonConstants.DOMAIN_SEPARATOR + userName, tenantDomain);
                log.debug(error, e);
            }
        }
        throw new IdentityEventException(e.getErrorCode(), e.getMessage());
    }

    /**
     * Checks whether the notification channel is already verified for the user.
     *
     * @param username            Username
     * @param tenantDomain        Tenant domain
     * @param notificationChannel Notification channel
     * @param eventProperties     Properties related to the event
     * @return True if the channel is already verified.
     */
    private boolean isNotificationChannelVerified(String username, String tenantDomain, String notificationChannel,
            Map<String, Object> eventProperties) throws IdentityRecoveryClientException {

        boolean isEnableAccountLockForVerifiedPreferredChannelEnabled = Boolean.parseBoolean(IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ACCOUNT_LOCK_FOR_VERIFIED_PREFERRED_CHANNEL));
        if (!isEnableAccountLockForVerifiedPreferredChannelEnabled) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("SkipAccountLockOnVerifiedPreferredChannel is enabled for user : %s in domain : %s. "
                                + "Checking whether the user is already verified", username, tenantDomain);
                log.debug(message);
            }
            // Get the notification channel which matches the given channel type.
            NotificationChannels channel = getNotificationChannel(username, notificationChannel);

            // Get the matching claim uri for the channel.
            String verifiedClaimUri = channel.getVerifiedClaimUrl();

            // Get the verified status for given channel.
            boolean notificationChannelVerified = Boolean.parseBoolean((String) eventProperties.get(verifiedClaimUri));
            if (notificationChannelVerified) {
                if (log.isDebugEnabled()) {
                    String message = String
                            .format("Preferred Notification channel : %1$s is verified for the user : %2$s "
                                            + "in domain : %3$s. Therefore, no notifications will be sent.",
                                    notificationChannel, username, tenantDomain);
                    log.debug(message);
                }
            }
            return notificationChannelVerified;
        }
        return false;
    }

    /**
     * Get the NotificationChannels object which matches the given channel type.
     *
     * @param username            Username
     * @param notificationChannel Notification channel
     * @return NotificationChannels object
     * @throws IdentityRecoveryClientException Unsupported channel type
     */
    private NotificationChannels getNotificationChannel(String username, String notificationChannel)
            throws IdentityRecoveryClientException {

        NotificationChannels channel;
        try {
            channel = NotificationChannels.getNotificationChannel(notificationChannel);
        } catch (NotificationChannelManagerClientException e) {
            if (log.isDebugEnabled()) {
                log.debug("Unsupported channel type : " + notificationChannel);
            }
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS, username, e);
        }
        return channel;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {
        return 60;
    }


    protected void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Sending self user registration notification user: " + user.getUserName());
        }

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (props != null && props.length > 0) {
            for (int i = 0; i < props.length; i++) {
                properties.put(props[i].getKey(), props[i].getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
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
    private void triggerNotification(User user, String notificationChannel, String code, Property[] props,
            String eventName) throws IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Sending self user registration notification user: " + user.getUserName());
        }
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);

        if (props != null && props.length > 0) {
            for (Property prop : props) {
                properties.put(prop.getKey(), prop.getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE,
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    /**
     * Generate an OTP for password recovery via mobile Channel
     *
     * @return OTP
     */
    private String generateSMSOTP() {

        char[] chars = IdentityRecoveryConstants.SMS_OTP_GENERATE_CHAR_SET.toCharArray();
        SecureRandom rnd = new SecureRandom();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < IdentityRecoveryConstants.SMS_OTP_CODE_LENGTH; i++) {
            sb.append(chars[rnd.nextInt(chars.length)]);
        }
        return sb.toString();
    }

    /**
     * Generate a secret key according to the given channel. Method will generate an OTP for mobile channel and a
     * UUID for other channels.
     *
     * @param channel Recovery notification channel.
     * @return Secret key
     */
    private String generateSecretKey(String channel) {

        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel)) {
            if (log.isDebugEnabled()) {
                log.debug("OTP was generated for the user for channel : " + channel);
            }
            return generateSMSOTP();
        } else {
            if (log.isDebugEnabled()) {
                log.debug("UUID was generated for the user for channel : " + channel);
            }
            return UUIDGenerator.generateUUID();
        }
    }

}
