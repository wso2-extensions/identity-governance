/*
 * Copyright (c) 2025-2026, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.identity.recovery.model.RegistrationCompletionData;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral;
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

            // In order for account lock to be effective, both account lock on creation and confirmation on creation
            // should be enabled.
            boolean isEffectiveAccountLockOnCreation = isAccountLockOnCreation && isEnableConfirmationOnCreation;

            // Get the user preferred notification channel.
            String preferredChannel = resolveNotificationChannel(eventProperties, userName, tenantDomain,
                    userStoreDomain);

            NotificationChannels channel = getNotificationChannel(userName, preferredChannel);

            boolean notificationChannelVerified = isNotificationChannelVerified(userName, tenantDomain,
                    preferredChannel, eventProperties);

            boolean isNotificationClaimAvailable = isNotifyingClaimAvailable(channel.getClaimUri(), eventProperties);

            Map<String, String> userClaims =
                    (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS);
            RegistrationCompletionData completion = new RegistrationCompletionData(userName, tenantDomain,
                    userStoreDomain, preferredChannel, notificationChannelVerified, isNotificationClaimAvailable,
                    isEnableConfirmationOnCreation, isEffectiveAccountLockOnCreation, isNotificationInternallyManaged,
                    isSelfRegistrationConfirmationNotify, true, userClaims,
                    RegistrationCompletionDeferral.snapshotNotificationProperties());

            if (RegistrationCompletionDeferral.defer(completion)) {
                // Account state must be prepared during addUser, even when completion notifications are deferred.
                prepareAccountState(completion, userStoreManager);
                return;
            }

            // Preserve the existing ordering for ordinary registration, outside the dispatcher.
            publishRegistrationCompletion(completion);
            prepareAccountState(completion, userStoreManager);
            sendConfirmationNotification(completion);
        } catch (IdentityRecoveryException | FlowMgtServerException e) {
            throw new IdentityEventException("Error while sending self sign up notification ", e);
        }
    }

    /**
     * Completes registration after the dispatcher has successfully provisioned both resources.
     * Account state was already prepared by the post-add-user handler.
     *
     * @param completion Captured registration completion data.
     * @throws IdentityEventException If registration notification fails.
     */
    public void completeRegistration(RegistrationCompletionData completion) throws IdentityEventException {

        try {
            publishRegistrationCompletion(completion);
            sendConfirmationNotification(completion);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending self sign up notification ", e);
        }
    }

    private void prepareAccountState(RegistrationCompletionData completion, UserStoreManager userStoreManager)
            throws IdentityEventException {

        if (completion.channelVerified()) {
            return;
        }
        if (completion.effectiveAccountLock() && completion.notificationClaimAvailable()) {
            lockUserAccount(true, true, completion.tenantDomain(), userStoreManager, completion.username());
        } else if (completion.confirmationEnabled()) {
            lockUserAccount(false, true, completion.tenantDomain(), userStoreManager, completion.username());
        }
    }

    private void publishRegistrationCompletion(RegistrationCompletionData completion) throws IdentityRecoveryException {

        User user = completion.toUser();
        if (completion.notificationsInternallyManaged() && completion.notificationClaimAvailable()
                && completion.completionNotificationEnabled()
                && (completion.channelVerified() || !completion.confirmationEnabled())) {
            triggerAccountCreationNotification(user.getUserName(), user.getTenantDomain(), user.getUserStoreDomain(),
                    completion.toNotificationProperties());
        }
        if (completion.publishSuccessEvent() && (completion.channelVerified() || !completion.effectiveAccountLock())) {
            Map<String, Object> eventProperties = new HashMap<>();
            eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS,
                    completion.userClaims() == null ? null : new HashMap<>(completion.userClaims()));
            publishEvent(user, user.getUserStoreDomain(), user.getTenantDomain(), eventProperties,
                    IdentityEventConstants.Event.USER_REGISTRATION_SUCCESS);
        }
    }

    private void sendConfirmationNotification(RegistrationCompletionData completion)
            throws IdentityRecoveryException, IdentityEventException {

        if (completion.channelVerified() || !completion.notificationsInternallyManaged()
                || !completion.confirmationEnabled() || !completion.notificationClaimAvailable()) {
            return;
        }
        User user = completion.toUser();
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        userRecoveryDataStore.invalidate(user);
        String secretKey = Utils.generateSecretKey(completion.preferredChannel(), RecoveryScenarios.SELF_SIGN_UP.name(),
                completion.tenantDomain(), "SelfRegistration");
        String eventName = resolveEventName(completion.preferredChannel(), completion.username(),
                completion.userStoreDomain(), completion.tenantDomain());
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        recoveryDataDO.setRemainingSetIds(completion.preferredChannel());
        userRecoveryDataStore.store(recoveryDataDO);
        SelfRegistrationUtils.triggerNotification(user, completion.preferredChannel(), secretKey,
                completion.toNotificationProperties(), eventName);
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
        if (eventProperties.containsKey(IdentityEventConstants.EventProperty.USER_CLAIMS)) {
            userClaims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS);
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
