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

package org.wso2.carbon.identity.recovery.listener;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.ApplicationBasicInfo;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.flow.execution.engine.listener.AbstractFlowExecutionListener;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.utils.DiagnosticLog;

import java.util.HashMap;
import java.util.Map;

import static java.util.Locale.ENGLISH;
import static org.wso2.carbon.identity.application.mgt.ApplicationConstants.MY_ACCOUNT_APPLICATION_NAME;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.SELF_REGISTRATION_DEFAULT_USERSTORE_CONFIG;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE;
import static org.wso2.carbon.identity.recovery.RecoveryScenarios.SELF_SIGN_UP;
import static org.wso2.carbon.identity.recovery.RecoverySteps.CONFIRM_SIGN_UP;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.getNotificationChannel;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.getUserStoreManager;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.handledNotificationChannelManagerException;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.lockUserAccount;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.maskIfRequired;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.resolveEventName;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.triggerAccountCreationNotification;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.triggerNotification;
import static org.wso2.carbon.user.core.UserCoreConstants.APPLICATION_DOMAIN;
import static org.wso2.carbon.user.core.UserCoreConstants.INTERNAL_DOMAIN;
import static org.wso2.carbon.user.core.UserCoreConstants.WORKFLOW_DOMAIN;
import static org.wso2.carbon.utils.DiagnosticLog.ResultStatus.FAILED;
import static org.wso2.carbon.utils.DiagnosticLog.ResultStatus.SUCCESS;

/**
 * Listener to honour self registration connector configs after the successful completion of the self registration
 * flow.
 */
public class SelfRegistrationCompletionListener extends AbstractFlowExecutionListener {

    private static final Log LOG = LogFactory.getLog(SelfRegistrationCompletionListener.class);
    private static final String REGISTRATION_COMPLETION_LISTENER = "registration-completion-listener";
    private static final String TRIGGER_NOTIFICATION = "trigger-notification";
    private static final String FLOW_ID = "flowId";
    private static final String USER_NAME = "userName";
    private static final String NOTIFICATION_CHANNEL = "notificationChannel";
    private static final String ACCOUNT_LOCKED = "accountLocked";

    @Override
    public int getDefaultOrderId() {

        return 3;
    }

    @Override
    public int getExecutionOrderId() {

        return 3;
    }

    @Override
    public boolean isEnabled() {

        return true;
    }

    @Override
    public boolean doPostExecute(FlowExecutionStep step, FlowExecutionContext flowExecutionContext) {

        if (Constants.COMPLETE.equals(step.getFlowStatus())
                && Constants.FlowTypes.REGISTRATION.getType().equals(flowExecutionContext.getFlowType())) {

            FlowUser user = flowExecutionContext.getFlowUser();
            String tenantDomain = flowExecutionContext.getTenantDomain();
            String userStoreDomain = resolveUserStoreDomain(user.getUsername());
            Map<String, Object> loggerInputs = new HashMap<>();

            try {
                boolean isAccountLockOnCreation = Boolean.parseBoolean(
                        Utils.getConnectorConfig(ACCOUNT_LOCK_ON_CREATION, tenantDomain));

                boolean isEnableConfirmationOnCreation = Boolean.parseBoolean(
                        Utils.getConnectorConfig(SEND_CONFIRMATION_NOTIFICATION, tenantDomain));

                String preferredChannel = resolveNotificationChannel(user.getUsername(), tenantDomain, userStoreDomain);
                NotificationChannels channel = getNotificationChannel(user.getUsername(), preferredChannel);
                boolean notificationChannelVerified = isNotificationChannelVerified(channel, user.getClaims());
                if (notificationChannelVerified) {
                    if (LOG.isDebugEnabled()) {
                        String message = String
                                .format("Preferred Notification channel: %1$s is verified for the user: %2$s "
                                                + "in domain : %3$s. Therefore, no notifications will be sent.",
                                        preferredChannel, SelfRegistrationUtils.maskIfRequired(user.getUsername()),
                                        tenantDomain);
                        LOG.debug(message);
                    }
                    return true;
                }
                // If account lock on creation is enabled, lock the account by persisting the account lock claim.
                if (isAccountLockOnCreation || isEnableConfirmationOnCreation) {
                    try {
                        UserStoreManager userStoreManager = getUserStoreManager(tenantDomain, userStoreDomain);
                        lockUserAccount(isAccountLockOnCreation, isEnableConfirmationOnCreation, tenantDomain,
                                userStoreManager, user.getUsername());
                        step.setStepType(Constants.StepTypes.VIEW);
                        step.getData().addAdditionalData(ACCOUNT_LOCKED, Boolean.TRUE.toString());
                    } catch (UserStoreException | IdentityEventException e) {
                        LOG.error("Error while locking the user account from the registration completion listener " +
                                "in the flow: " + flowExecutionContext.getContextIdentifier(), e);
                    }
                }

                boolean isNotificationInternallyManage = Boolean.parseBoolean(
                        Utils.getConnectorConfig(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, tenantDomain));

                boolean isSelfRegistrationConfirmationNotify = Boolean.parseBoolean(Utils.getSignUpConfigs
                        (IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                                tenantDomain));

                // Start building the input map for the diagnostic logs.
                loggerInputs.put(FLOW_ID, flowExecutionContext.getContextIdentifier());
                loggerInputs.put(USER_NAME, maskIfRequired(user.getUsername()));
                loggerInputs.put(NOTIFICATION_CHANNEL, preferredChannel);

                // If notify confirmation is enabled and both iAccountLockOnCreation &&
                // EnableConfirmationOnCreation are disabled then send account creation notification.
                if (!isAccountLockOnCreation && !isEnableConfirmationOnCreation && isNotificationInternallyManage
                        && isSelfRegistrationConfirmationNotify
                        && isNotifyingClaimAvailable(channel.getClaimUri(), user.getClaims(), loggerInputs)) {
                    triggerAccountCreationNotification(user.getUsername(), tenantDomain, userStoreDomain);
                    logDiagnostic("Account creation notification sent successfully.", SUCCESS,
                            TRIGGER_NOTIFICATION, loggerInputs);
                }

                // If notifications are externally managed, do not send notifications.
                if ((isAccountLockOnCreation || isEnableConfirmationOnCreation) && isNotificationInternallyManage &&
                        isNotifyingClaimAvailable(channel.getClaimUri(), user.getClaims(), loggerInputs)) {
                    UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                    User applicationUser = getApplicationUser(user, tenantDomain, userStoreDomain);
                    userRecoveryDataStore.invalidate(applicationUser);

                    // Create a secret key based on the preferred notification channel.
                    String secretKey = Utils.generateSecretKey(preferredChannel, SELF_SIGN_UP.name(),
                            tenantDomain, "SelfRegistration");

                    String eventName = resolveEventName(preferredChannel, applicationUser.getUserName(),
                            userStoreDomain, tenantDomain);

                    UserRecoveryData recoveryDataDO = new UserRecoveryData(applicationUser, secretKey, SELF_SIGN_UP,
                            CONFIRM_SIGN_UP);

                    // Notified channel is stored in remaining setIds for recovery purposes.
                    recoveryDataDO.setRemainingSetIds(preferredChannel);
                    userRecoveryDataStore.store(recoveryDataDO);
                    Property[] properties = resolveNotificationProperties(step, flowExecutionContext);
                    triggerNotification(applicationUser, preferredChannel, secretKey, properties, eventName);
                    logDiagnostic("Account verification notification sent successfully.", SUCCESS,
                            TRIGGER_NOTIFICATION, loggerInputs);
                }
            } catch (IdentityEventException | IdentityRecoveryException | IdentityApplicationManagementException e) {
                LOG.error("Error while sending verification notification from the registration completion listener in" +
                        " the flow: " + flowExecutionContext.getContextIdentifier(), e);
                logDiagnostic("Error while triggering verification notification.", FAILED,
                        TRIGGER_NOTIFICATION, loggerInputs);
            }
        }
        return true;
    }

    private User getApplicationUser(FlowUser user, String tenantDomain, String userStoreDomain) {

        User appUser = new User();
        appUser.setUserName(user.getUsername());
        appUser.setTenantDomain(tenantDomain);
        appUser.setUserStoreDomain(userStoreDomain);
        return appUser;
    }

    private String resolveNotificationChannel(String userName, String tenantDomain, String domainName)
            throws IdentityEventException {

        NotificationChannelManager notificationChannelManager = Utils.getNotificationChannelManager();
        String preferredChannel = null;
        try {
            preferredChannel = notificationChannelManager.resolveCommunicationChannel(userName, tenantDomain,
                    domainName);
        } catch (NotificationChannelManagerException e) {
            handledNotificationChannelManagerException(e, userName, domainName, tenantDomain);
        }
        return preferredChannel;
    }

    private boolean isNotificationChannelVerified(NotificationChannels channel, Map<String, String> userClaims) {

        String verifiedClaimUri = channel.getVerifiedClaimUrl();
        return Boolean.parseBoolean(userClaims.get(verifiedClaimUri));
    }

    private boolean isNotifyingClaimAvailable(String claimUri, Map<String, String> userClaims,
                                              Map<String, Object> loggerInputs) {

        boolean isAvailable = userClaims.containsKey(claimUri);
        if (!isAvailable) {
            logDiagnostic("Cannot trigger notification since user claim is not available.", FAILED,
                    TRIGGER_NOTIFICATION, loggerInputs);
        }
        return userClaims.containsKey(claimUri);
    }

    private String resolveUserStoreDomain(String username) {

        int separatorIndex = username.indexOf(UserCoreConstants.DOMAIN_SEPARATOR);
        if (separatorIndex >= 0) {
            String domain = username.substring(0, separatorIndex);
            if (INTERNAL_DOMAIN.equalsIgnoreCase(domain) || WORKFLOW_DOMAIN.equalsIgnoreCase(domain)
                    || APPLICATION_DOMAIN.equalsIgnoreCase(domain)) {
                return domain.substring(0, 1).toUpperCase(ENGLISH) + domain.substring(1).toLowerCase(ENGLISH);
            }
            return domain.toUpperCase(ENGLISH);
        }

        String domainName = IdentityUtil.getProperty(SELF_REGISTRATION_DEFAULT_USERSTORE_CONFIG);
        return domainName != null ? domainName.toUpperCase(ENGLISH) :
                IdentityUtil.getPrimaryDomainName().toUpperCase(ENGLISH);
    }

    private Property[] resolveNotificationProperties(FlowExecutionStep step, FlowExecutionContext context)
            throws IdentityApplicationManagementException {

        String tenantDomain = context.getTenantDomain();
        ApplicationManagementService applicationManagementService =
                IdentityRecoveryServiceDataHolder.getInstance().getApplicationManagementService();

        String applicationId = context.getApplicationId();
        String applicationName;
        ApplicationBasicInfo appInfo;
        try {
            if (StringUtils.isEmpty(applicationId)) {
                applicationName = MY_ACCOUNT_APPLICATION_NAME;
                appInfo = applicationManagementService.getApplicationBasicInfoByName(applicationName, tenantDomain);
                applicationId = (appInfo != null) ? appInfo.getUuid() : null;
            } else {
                appInfo = applicationManagementService.getApplicationBasicInfoByResourceId(applicationId, tenantDomain);
                applicationName = (appInfo != null) ? appInfo.getApplicationName() : null;
            }
        } catch (IdentityApplicationManagementException e) {
            Map<String, Object> loggerInputs = new HashMap<>();
            loggerInputs.put(FLOW_ID, context.getContextIdentifier());
            logDiagnostic("Error while resolving application details.", FAILED, TRIGGER_NOTIFICATION, loggerInputs);
            throw e;
        }

        Property[] properties = new Property[3];

        properties[0] = new Property("spId", applicationId);
        properties[1] = new Property("sp", applicationName);
        properties[2] = new Property("callback", step.getData().getRedirectURL());
        return properties;
    }

    /**
     * This method is used to log the diagnostic information.
     *
     * @param message  Message to be logged.
     * @param status   Status of the log.
     * @param actionId Action ID.
     */
    private void logDiagnostic(String message, DiagnosticLog.ResultStatus status, String actionId,
                               Map<String, Object> inputParams) {

        if (LoggerUtils.isDiagnosticLogsEnabled()) {
            LoggerUtils.triggerDiagnosticLogEvent(
                    new DiagnosticLog.DiagnosticLogBuilder(REGISTRATION_COMPLETION_LISTENER, actionId)
                            .resultMessage(message)
                            .logDetailLevel(DiagnosticLog.LogDetailLevel.APPLICATION)
                            .inputParams(inputParams)
                            .resultStatus(status)
            );
        }
    }
}
