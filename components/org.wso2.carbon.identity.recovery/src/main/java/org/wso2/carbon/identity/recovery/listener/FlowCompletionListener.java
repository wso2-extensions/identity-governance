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

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.listener.AbstractFlowExecutionListener;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.handler.event.account.lock.constants.AccountConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.workflow.mgt.WorkflowManagementService;
import org.wso2.carbon.identity.workflow.mgt.bean.Entity;
import org.wso2.carbon.identity.workflow.mgt.exception.WorkflowException;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.utils.DiagnosticLog;

import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_LISTENER_FAILURE;
import static org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils.handleServerException;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;
import static org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils.maskIfRequired;
import static org.wso2.carbon.identity.recovery.util.Utils.getDomainQualifiedUsername;
import static org.wso2.carbon.identity.recovery.util.Utils.loadUserRecoveryData;
import static org.wso2.carbon.utils.DiagnosticLog.ResultStatus.FAILED;

/**
 * Listener to handle post actions after a flow is completed.
 */
public class FlowCompletionListener extends AbstractFlowExecutionListener {

    private static final Log log = LogFactory.getLog(FlowCompletionListener.class);
    private static final String FLOW_COMPLETION_LISTENER = "flow-completion-listener";
    private static final String WORKFLOW_USER_ENTITY_TYPE = "USER";
    private static final String SELF_REGISTER_USER_EVENT = "SELF_REGISTER_USER";
    private static final String PENDING_APPROVAL = "PENDING_APPROVAL";
    private static final String ACCOUNT_STATUS = "accountStatus";
    private static final String ACCOUNT_LOCKED = "ACCOUNT_LOCKED";
    private static final String TRIGGER_NOTIFICATION = "trigger-notification";
    private static final String FLOW_ID = "flowId";
    private static final String USER_NAME = "userName";

    @Override
    public int getExecutionOrderId() {

        return 3;
    }

    @Override
    public int getDefaultOrderId() {

        return 3;
    }

    @Override
    public boolean isEnabled() {

        return true;
    }

    @Override
    public boolean doPostExecute(FlowExecutionStep step, FlowExecutionContext context) throws FlowEngineException {

        if (!Constants.COMPLETE.equals(step.getFlowStatus())) {
            return true;
        }

        if (Constants.FlowTypes.INVITED_USER_REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {

            return handleInvitedUserRegistrationCompletion(step, context);
        }

        if (Constants.FlowTypes.REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {

            return handleSelfRegistrationCompletion(step, context);
        }

        return true;
    }

    private boolean handleSelfRegistrationCompletion(FlowExecutionStep step, FlowExecutionContext context) {

        FlowUser user = context.getFlowUser();
        String tenantDomain = context.getTenantDomain();
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        Map<String, Object> loggerInputs = new HashMap<>();

        // Start building the input map for the diagnostic logs.
        loggerInputs.put(FLOW_ID, context.getContextIdentifier());
        loggerInputs.put(USER_NAME, maskIfRequired(user.getUsername()));

        Entity entity = new Entity(getDomainQualifiedUsername(user), WORKFLOW_USER_ENTITY_TYPE, tenantId);

        try {
            if (hasPendingWorkFlow(entity, SELF_REGISTER_USER_EVENT)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The user: %s in tenant domain: %s is associated with a pending " +
                                    "workflow. Hence self registration completion listener will not proceed.",
                            SelfRegistrationUtils.maskIfRequired(user.getUsername()), tenantDomain));
                }
                step.setStepType(Constants.StepTypes.VIEW);
                step.getData().addAdditionalData(ACCOUNT_STATUS, PENDING_APPROVAL);
                return true;
            }

            boolean isAccountLockOnCreation = Boolean.parseBoolean(Utils.getFlowCompletionConfig(
                    Constants.FlowTypes.REGISTRATION, tenantDomain,
                    Constants.FlowCompletionConfig.IS_ACCOUNT_LOCK_ON_CREATION_ENABLED));

            boolean isEnableConfirmationOnCreation = Boolean.parseBoolean(Utils.getFlowCompletionConfig(
                    Constants.FlowTypes.REGISTRATION, tenantDomain,
                    Constants.FlowCompletionConfig.IS_EMAIL_VERIFICATION_ENABLED));

            if (isEnableConfirmationOnCreation && isAccountLockOnCreation) {
                step.setStepType(Constants.StepTypes.VIEW);
                step.getData().addAdditionalData(ACCOUNT_STATUS, ACCOUNT_LOCKED);
            } else {
                publishEvent(user.getClaims(), user.getUserStoreDomain(), tenantDomain,
                        IdentityEventConstants.Event.USER_REGISTRATION_SUCCESS);
            }
        } catch (WorkflowException e) {
            log.error(ERROR_CODE_LISTENER_FAILURE.getMessage(), e);
            log.error("Error while handling workflow in" +
                    " the flow: " + context.getContextIdentifier(), e);
            logDiagnostic("Error while retrieving workflow status from flow completion listener in.", FAILED,
                    TRIGGER_NOTIFICATION, loggerInputs);
        } catch (FlowMgtServerException e) {
            log.error(ERROR_CODE_LISTENER_FAILURE.getMessage(), e);
            log.error("Error while retrieving flow completion configs from the registration completion listener in" +
                    " the flow: " + context.getContextIdentifier(), e);
            logDiagnostic("Error while triggering verification notification.", FAILED,
                    TRIGGER_NOTIFICATION, loggerInputs);
        }
        return true;
    }

    private boolean handleInvitedUserRegistrationCompletion(FlowExecutionStep step, FlowExecutionContext context)
            throws FlowEngineException {

        String confirmationCode = getStringProperty(context, CONFIRMATION_CODE_INPUT);
        String notificationChannel = getStringProperty(context, IdentityRecoveryConstants.NOTIFICATION_CHANNEL);
        String recoveryScenario = getStringProperty(context, IdentityRecoveryConstants.RECOVERY_SCENARIO);
        User user = Utils.resolveUserFromContext(context);

        if (user == null || confirmationCode == null) {
            return false;
        }

        NotificationPasswordRecoveryManager manager = NotificationPasswordRecoveryManager.getInstance();
        notificationChannel = manager.getServerSupportedNotificationChannel(notificationChannel);

        try {
            String tenantDomain = user.getTenantDomain();
            boolean internallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, tenantDomain));

            updateAccountStateClaims(tenantDomain, notificationChannel, internallyManaged, user);
            invalidateRecoveryData(confirmationCode);
            handleNotifications(user, recoveryScenario, notificationChannel, confirmationCode, internallyManaged,
                    tenantDomain);
            publishEvent(user, confirmationCode, recoveryScenario);
        } catch (UserStoreException | IdentityRecoveryException | IdentityEventException e) {
            log.error(ERROR_CODE_LISTENER_FAILURE.getMessage(), e);
            throw handleServerException(ERROR_CODE_LISTENER_FAILURE, this.getClass().getSimpleName(),
                    context.getFlowType(), context.getContextIdentifier());
        }

        return true;
    }

    private boolean hasPendingWorkFlow(Entity entity, String type) throws WorkflowException {

        WorkflowManagementService workflowManagementService = IdentityRecoveryServiceDataHolder
                .getInstance().getWorkflowManagementService();

        return workflowManagementService.entityHasPendingWorkflowsOfType(entity, type);
    }
    private String getStringProperty(FlowExecutionContext context, String key) {

        Object value = context.getProperty(key);
        return (value != null) ? value.toString() : null;
    }

    /**
     * Updates the account state claims for the user.
     * @param tenantDomain          Tenant domain of the user.
     * @param notificationChannel   Notification channel used for the user.
     * @param internallyManaged     Indicates if the notification is internally managed.
     * @param user                  User object containing user details.
     * @throws UserStoreException   if an error occurs while updating user claims.
     */
    private void updateAccountStateClaims(String tenantDomain, String notificationChannel,
                                          boolean internallyManaged, User user)
            throws UserStoreException {

        Map<String, String> userClaims = getAccountStateClaims(tenantDomain, notificationChannel, internallyManaged);
        if (MapUtils.isNotEmpty(userClaims)) {
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                    .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
        }
    }

    /**
     * Retrieves the account state claims based on the tenant domain and notification channel.
     *
     * @param tenantDomain          Tenant domain of the user.
     * @param notificationChannel   Notification channel used for the user.
     * @param internallyManaged     Indicates if the notification is internally managed.
     * @return                     A map containing account state claims.
     */
    private HashMap<String, String> getAccountStateClaims(String tenantDomain, String notificationChannel,
                                                          boolean internallyManaged) {

        HashMap<String, String> claims = new HashMap<>();

        if (internallyManaged) {
            String verifiedClaimUrl;
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(notificationChannel)) {
                verifiedClaimUrl = NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl();
            } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannel)) {
                verifiedClaimUrl = NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl();
            } else {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("No valid notification channels for tenant domain: %s. Defaulting to email.", tenantDomain));
                }
                verifiedClaimUrl = NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl();
            }
            claims.put(verifiedClaimUrl, Boolean.TRUE.toString());
        }

        IdentityUtil.threadLocalProperties.get().put(AccountConstants.ADMIN_INITIATED, false);
        claims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI, IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM, StringUtils.EMPTY);
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
        return claims;
    }

    /**
     * Invalidates the recovery data associated with the provided confirmation code.
     * @param confirmationCode  Confirmation code to identify the recovery data.
     */
    private void invalidateRecoveryData(String confirmationCode) {

        try {
            UserRecoveryData data = loadUserRecoveryData(confirmationCode);
            UserRecoveryDataStore store = JDBCRecoveryDataStore.getInstance();
            if (data.getRecoveryFlowId() != null) {
                store.invalidateWithRecoveryFlowId(data.getRecoveryFlowId());
            } else {
                store.invalidate(data.getUser());
            }
        } catch (IdentityRecoveryException e) {
            String errorMsg = String.format("Error while invalidating user recovery data for confirmation code: %s",
                    confirmationCode);
            if (log.isDebugEnabled()) {
                log.debug(errorMsg, e);
            }
            log.warn(errorMsg);
        }
    }

    private void handleNotifications(User user, String recoveryScenario, String channel, String code,
                                     boolean internallyManaged, String tenantDomain)
            throws IdentityRecoveryServerException {

        if (!internallyManaged || NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(channel)) {
            // If the notification is not internally managed and the channel is external, skip sending notifications.
            return;
        }

        // Send account activation notification if the configuration is enabled.
        try {
            if (Boolean.parseBoolean(Utils.getFlowCompletionConfig(Constants.FlowTypes.INVITED_USER_REGISTRATION,
                    tenantDomain, Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED))) {
                String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
                triggerNotification(user, recoveryScenario, channel,
                        IdentityRecoveryConstants.ACCOUNT_ACTIVATION_SUCCESS, code, eventName);
            }
        } catch (IdentityRecoveryException | FlowMgtServerException e) {
            String errorMsg = String.format("Error while sending account activation notification to user: %s",
                    user.getUserName());
            log.error(errorMsg, e);
        }
    }

    private void triggerNotification(User user, String recoveryScenario, String channel, String template,
                                     String code, String eventName) throws IdentityRecoveryException {

        HashMap<String, Object> props = new HashMap<>();
        props.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        props.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        props.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        props.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, channel);
        props.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);
        props.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        props.put(IdentityRecoveryConstants.TEMPLATE_TYPE, template);

        Event event = new Event(eventName, props);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(event);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    private void publishEvent(User user, String code, String recoveryScenario)
            throws IdentityEventException {

        HashMap<String, Object> props = new HashMap<>();
        props.put(IdentityEventConstants.EventProperty.USER, user);
        props.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        props.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        props.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        props.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);
        props.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);

        Event event = new Event(IdentityEventConstants.Event.POST_ADD_NEW_PASSWORD, props);
        IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(event);
    }

    private void publishEvent(Map<String, String> claims, String userStoreDomain, String tenantDomain,
                              String eventName) {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, userStoreDomain);
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            log.error("Error while publishing event: " + eventName, e);
        }
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
                    new DiagnosticLog.DiagnosticLogBuilder(FLOW_COMPLETION_LISTENER, actionId)
                            .resultMessage(message)
                            .logDetailLevel(DiagnosticLog.LogDetailLevel.APPLICATION)
                            .inputParams(inputParams)
                            .resultStatus(status)
            );
        }
    }
}
