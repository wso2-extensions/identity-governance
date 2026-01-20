/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.handledNotificationChannelManagerException;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.lockUserAccount;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.resolveEventName;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.getNotificationChannel;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.triggerAccountCreationNotification;

/**
 * Event handler which handles post user self registration operations.
 */
public class FlowRegistrationCompletionHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(FlowRegistrationCompletionHandler.class);

    public String getName() {

        return "FlowRegistrationCompletionHandler";
    }

    public String getFriendlyName() {

        return "Flow Registration Completion Handler";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        // This handler only handle POST_ADD_USER event.
        if (!IdentityEventConstants.Event.POST_ADD_USER.equals(event.getEventName())) {
            return;
        }

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager =
                (UserStoreManager) eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);

        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

        String[] roleList = (String[]) eventProperties.get(IdentityEventConstants.EventProperty.ROLE_LIST);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(userStoreDomain);
        boolean enable;

        try {
            // Check whether self sign up flow is enabled for the tenant.
            enable = Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), tenantDomain).getIsEnabled();
        } catch (FlowMgtServerException e) {
            throw new IdentityEventException("Error while retrieving self sign up enable property for tenant: " +
                    tenantDomain, e);
        }

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

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {

            boolean isAccountLockOnCreation =  Boolean.parseBoolean(Utils.getFlowCompletionConfig(
                    Constants.FlowTypes.REGISTRATION, tenantDomain,
                    Constants.FlowCompletionConfig.IS_ACCOUNT_LOCK_ON_CREATION_ENABLED));

            boolean isEnableConfirmationOnCreation = Boolean.parseBoolean(Utils.getFlowCompletionConfig(
                    Constants.FlowTypes.REGISTRATION, tenantDomain,
                    Constants.FlowCompletionConfig.IS_EMAIL_VERIFICATION_ENABLED));

            boolean isSelfRegistrationConfirmationNotify = Boolean.parseBoolean(Utils.getFlowCompletionConfig(
                    Constants.FlowTypes.REGISTRATION, tenantDomain,
                    Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED));

            boolean isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                    user.getTenantDomain()));

            // Get the user preferred notification channel.
            String preferredChannel = resolveNotificationChannel(eventProperties, userName, tenantDomain,
                    userStoreDomain);

            NotificationChannels channel = getNotificationChannel(userName, preferredChannel);

            boolean notificationChannelVerified = isNotificationChannelVerified(userName, tenantDomain,
                    preferredChannel, eventProperties);

            // If the notification channel is verified no account lock is happening hence send the account creation
            // notification if the relevant configuration is enabled.
            // If notification channel is not verified, account creation notification is sent only if account
            // confirmation is disabled unless account confirmation notification will be sent.
            if (isNotificationInternallyManaged && isNotifyingClaimAvailable(channel.getClaimUri() , eventProperties)
                    && isSelfRegistrationConfirmationNotify &&
                    (notificationChannelVerified || !isEnableConfirmationOnCreation) ) {
                triggerAccountCreationNotification(user.getUserName(), user.getTenantDomain(),
                        user.getUserStoreDomain());
            }

            // Event is not published if confirmation on creation is enabled as self registration is not complete yet.
            // If the notification channel is verified, then self registration is complete hence publish the event.
            if (notificationChannelVerified || !(isEnableConfirmationOnCreation && isAccountLockOnCreation)) {
                publishEvent(user, userStoreDomain, tenantDomain, eventProperties,
                        IdentityEventConstants.Event.USER_REGISTRATION_SUCCESS);
            }
            // If the preferred channel is already verified, no need to send the notifications or lock
            // the account.
            if (notificationChannelVerified) {
                return;
            }

            // If account lock on creation is enabled, lock the account by persisting the account lock claim.
            // Account locking is applicable only if Confirmation on creation is enabled.
            if (isAccountLockOnCreation && isEnableConfirmationOnCreation) {
                lockUserAccount(true, true, tenantDomain,
                        userStoreManager, userName);
            } else if (isEnableConfirmationOnCreation) {
                lockUserAccount(false, true, tenantDomain,
                        userStoreManager, userName);
            }

            // If notifications are externally managed, no notification needs to be sent.
            if (!isNotificationInternallyManaged) {
                return;
            }

            // If notifications are externally managed, no send notifications.
            if (isEnableConfirmationOnCreation && isNotifyingClaimAvailable(channel.getClaimUri(), eventProperties)) {
                userRecoveryDataStore.invalidate(user);

                // Create a secret key based on the preferred notification channel.
                String secretKey = Utils.generateSecretKey(preferredChannel, RecoveryScenarios.SELF_SIGN_UP.name(),
                        tenantDomain, "SelfRegistration");

                // Resolve event name.
                String eventName = resolveEventName(preferredChannel, userName, userStoreDomain, tenantDomain);

                UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                        RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

                // Notified channel is stored in remaining setIds for recovery purposes.
                recoveryDataDO.setRemainingSetIds(preferredChannel);
                userRecoveryDataStore.store(recoveryDataDO);
                SelfRegistrationUtils.triggerNotification(user, preferredChannel, secretKey,
                                                          Utils.getArbitraryProperties(), eventName);
            }
        } catch (IdentityRecoveryException | FlowMgtServerException e) {
            throw new IdentityEventException("Error while sending self sign up notification ", e);
        }
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

    private boolean isNotifyingClaimAvailable(String claimUri, Map<String, Object> eventProperties) {

        Map<String, String> userClaims;
        if (eventProperties.containsKey("USER_CLAIMS")) {
            userClaims = (Map<String, String>) eventProperties.get("USER_CLAIMS");
        } else {
            return false;
        }

        if (userClaims == null || userClaims.isEmpty()) {
            if (log.isDebugEnabled()) {
                log.debug("User claims are not available in the event properties.");
            }
            return false;
        }

        return userClaims.containsKey(claimUri);
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
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
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    /**
     * Publish identity event.
     *
     * @param user            User.
     * @param userStoreDomain User store domain.
     * @param tenantDomain    Tenant domain.
     * @param eventProperties Event properties.
     * @param eventName       Event name.
     */
    private void publishEvent(User user, String userStoreDomain, String tenantDomain,
                              Map<String, Object> eventProperties, String eventName) {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER, user);
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, userStoreDomain);
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        if (eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS) != null) {
            properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS,
                    eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS));
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            log.error("Error while publishing event: " + eventName, e);
        }
    }
}
