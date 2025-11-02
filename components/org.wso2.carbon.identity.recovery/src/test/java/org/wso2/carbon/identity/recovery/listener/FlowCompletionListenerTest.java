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

import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.flow.mgt.model.DataDTO;
import org.wso2.carbon.identity.flow.mgt.model.NodeConfig;
import org.wso2.carbon.identity.flow.mgt.model.ExecutorDTO;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.workflow.mgt.WorkflowManagementService;
import org.wso2.carbon.identity.workflow.mgt.bean.Entity;
import org.wso2.carbon.identity.workflow.mgt.exception.WorkflowException;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.DiagnosticLog;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for {@link FlowCompletionListener}.
 */
public class FlowCompletionListenerTest {

    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;
    private MockedStatic<Utils> mockedUtilsStatic;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryStatic;
    private MockedStatic<NotificationPasswordRecoveryManager> mockedRecoveryManagerStatic;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<SelfRegistrationUtils> mockedSelfRegistrationUtils;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<LoggerUtils> mockedLoggerUtils;

    private FlowCompletionListener listener;
    private WorkflowManagementService workflowManagementService;
    private IdentityEventService identityEventService;
    private UserStoreManager userStoreManager;
    private JDBCRecoveryDataStore jdbcRecoveryDataStore;
    private NotificationPasswordRecoveryManager recoveryManager;
    private NotificationChannelManager notificationChannelManager;

    @BeforeMethod
    public void setUp() throws Exception {

        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedUtilsStatic = mockStatic(Utils.class);
        mockedJDBCRecoveryStatic = mockStatic(JDBCRecoveryDataStore.class);
        mockedRecoveryManagerStatic = mockStatic(NotificationPasswordRecoveryManager.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedSelfRegistrationUtils = mockStatic(SelfRegistrationUtils.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedLoggerUtils = mockStatic(LoggerUtils.class);

        listener = new FlowCompletionListener();

        // Mock service dependencies
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        workflowManagementService = mock(WorkflowManagementService.class);
        identityEventService = mock(IdentityEventService.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        userStoreManager = mock(UserStoreManager.class);
        jdbcRecoveryDataStore = mock(JDBCRecoveryDataStore.class);
        recoveryManager = mock(NotificationPasswordRecoveryManager.class);
        notificationChannelManager = mock(NotificationChannelManager.class);

        // Default static mocks
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getWorkflowManagementService()).thenReturn(workflowManagementService);
        when(dataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(dataHolder.getRealmService()).thenReturn(realmService);

        mockedJDBCRecoveryStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(jdbcRecoveryDataStore);
        mockedRecoveryManagerStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(recoveryManager);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(1);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        // Mock IdentityUtil methods that are used
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(anyString())).thenReturn("PRIMARY");
        mockedIdentityUtil.when(() -> IdentityUtil.getPrimaryDomainName()).thenReturn("PRIMARY");

        // Mock Utils methods for notification channel manager
        mockedUtilsStatic.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);
        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString()))
                .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

        // Mock SelfRegistrationUtils methods
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.getNotificationChannel(anyString(), anyString()))
                .thenReturn(NotificationChannels.EMAIL_CHANNEL);
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_user");
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.handledNotificationChannelManagerException(
                any(NotificationChannelManagerException.class), anyString(), anyString(), anyString()))
                .thenAnswer(invocation -> null);

        mockedLoggerUtils.when(LoggerUtils::isDiagnosticLogsEnabled).thenReturn(true);
        mockedLoggerUtils.when(() -> LoggerUtils.triggerDiagnosticLogEvent(any(DiagnosticLog.DiagnosticLogBuilder.class)))
                .thenAnswer(invocation -> null);
    }

    @AfterMethod
    public void tearDown() {

        mockedDataHolderStatic.close();
        mockedUtilsStatic.close();
        mockedJDBCRecoveryStatic.close();
        mockedRecoveryManagerStatic.close();
        mockedIdentityTenantUtil.close();
        mockedSelfRegistrationUtils.close();
        mockedIdentityUtil.close();
        mockedLoggerUtils.close();
    }

    @Test
    public void testGetExecutionOrderId() {

        assertEquals(listener.getExecutionOrderId(), 3);
    }

    @Test
    public void testGetDefaultOrderId() {

        assertEquals(listener.getDefaultOrderId(), 3);
    }

    @Test
    public void testIsEnabled() {

        assertTrue(listener.isEnabled());
    }

    @Test
    public void testDoPostExecuteNotComplete() throws FlowEngineException {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(step.getFlowStatus()).thenReturn("IN_PROGRESS");

        boolean result = listener.doPostExecute(step, context);
        assertTrue(result);
    }

    @Test
    public void testDoPostExecuteUnknownFlowType() throws FlowEngineException {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(step.getFlowStatus()).thenReturn(Constants.COMPLETE);
        when(context.getFlowType()).thenReturn("UNKNOWN_FLOW");

        boolean result = listener.doPostExecute(step, context);
        assertTrue(result);
    }

    @DataProvider
    public Object[][] selfRegistrationWorkflowData() {

        return new Object[][]{
                {true, "PENDING_APPROVAL"}, // Has pending workflow
                {false, null} // No pending workflow
        };
    }

    @Test(dataProvider = "selfRegistrationWorkflowData")
    public void testHandleSelfRegistrationCompletionWithWorkflow(boolean hasPendingWorkflow,
                                                                  String expectedStatus) throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createSelfRegistrationContext();

        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), anyString()))
                .thenReturn(hasPendingWorkflow);

        mockedUtilsStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class)))
                .thenReturn("testUser");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        if (hasPendingWorkflow) {
            verify(step).setStepType(Constants.StepTypes.VIEW);
            verify(step.getData()).addAdditionalData("accountStatus", expectedStatus);
        }
    }

    @Test
    public void testHandleSelfRegistrationCompletionWithAccountLockAndVerification() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createSelfRegistrationContext();

        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), anyString()))
                .thenReturn(false);

        mockedUtilsStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class)))
                .thenReturn("testUser");

        // Mock flow completion configs
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_ACCOUNT_LOCK_ON_CREATION_ENABLED)))
                .thenReturn("true");
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_EMAIL_VERIFICATION_ENABLED)))
                .thenReturn("true");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(step).setStepType(Constants.StepTypes.VIEW);
        verify(step.getData()).addAdditionalData("accountStatus", "ACCOUNT_LOCKED");
    }

    @Test
    public void testHandleSelfRegistrationCompletionWorkflowException() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createSelfRegistrationContext();

        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), anyString()))
                .thenThrow(new WorkflowException("Workflow error"));

        mockedUtilsStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class)))
                .thenReturn("testUser");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        mockedLoggerUtils.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(
                any(DiagnosticLog.DiagnosticLogBuilder.class)), times(1));
    }

    @Test
    public void testHandleSelfRegistrationCompletionFlowMgtServerException() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createSelfRegistrationContext();

        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), anyString()))
                .thenReturn(false);

        mockedUtilsStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class)))
                .thenReturn("testUser");

        // Mock flow completion config to throw exception
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_ACCOUNT_LOCK_ON_CREATION_ENABLED)))
                .thenThrow(new FlowMgtServerException("Config error"));

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        mockedLoggerUtils.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(
                any(DiagnosticLog.DiagnosticLogBuilder.class)), times(1));
    }

    @DataProvider
    public Object[][] invitedUserRegistrationData() {
        return new Object[][]{
                {"EMAIL", true}, // Email channel
                {"SMS", true},   // SMS channel
                {"", true},      // Empty channel
        };
    }

    @Test(dataProvider = "invitedUserRegistrationData")
    public void testHandleInvitedUserRegistrationCompletion(String channel,
                                                            boolean internallyManaged) throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn(channel);
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn(String.valueOf(internallyManaged));

        // Mock flow completion config
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery data
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(anyString(), any(), eq(null));
        verify(jdbcRecoveryDataStore).invalidate(user);
        verify(identityEventService, times(2)).handleEvent(any(Event.class));
    }

    @Test
    public void testHandleInvitedUserRegistrationCompletionWithNullUser() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();

        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(null);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertFalse(result);
    }

    @Test
    public void testInvalidateRecoveryDataException() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Mock flow completion config
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock loadUserRecoveryData to throw exception
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString()))
                .thenThrow(new IdentityRecoveryException("Load error"));

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify - should complete successfully even with exception
        assertTrue(result);
    }

    @Test
    public void testDoPostExecutePasswordRecoveryFlowType() throws FlowEngineException {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(step.getFlowStatus()).thenReturn(Constants.COMPLETE);
        when(context.getFlowType()).thenReturn(Constants.FlowTypes.PASSWORD_RECOVERY.getType());

        // Mock empty completed nodes to trigger early return
        when(context.getCompletedNodes()).thenReturn(java.util.Collections.emptyList());

        boolean result = listener.doPostExecute(step, context);
        assertTrue(result);
    }

    @DataProvider
    public Object[][] passwordRecoveryExecutorData() {
        return new Object[][]{
                {"EmailOTPExecutor", NotificationChannels.EMAIL_CHANNEL.getChannelType()},
                {"SMSOTPExecutor", NotificationChannels.SMS_CHANNEL.getChannelType()},
                {"MagicLinkExecutor", NotificationChannels.EMAIL_CHANNEL.getChannelType()}
        };
    }

    @Test(dataProvider = "passwordRecoveryExecutorData")
    public void testHandlePasswordRecoveryCompletionWithDifferentExecutors(String executorName,
                                                                            String expectedChannel) throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext(executorName);

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery configs for internally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(identityEventService).handleEvent(any(Event.class));
    }

    @Test
    public void testHandlePasswordRecoveryCompletionWithNotificationsDisabled() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext("EmailOTPExecutor");

        // Mock flow completion config to disable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("false");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Verify that no event was triggered since notifications are disabled
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
    }

    @Test
    public void testHandlePasswordRecoveryCompletionWithExternallyManagedNotifications() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext("EmailOTPExecutor");

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery configs for externally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("false");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Verify that no event was triggered since notifications are externally managed
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
    }

    @Test
    public void testHandlePasswordRecoveryCompletionWithUnknownExecutor() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext("UnknownExecutor");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Verify that no event was triggered since executor is unknown
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
    }

    @Test
    public void testHandlePasswordRecoveryCompletionWithMissingClaims() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContextWithoutClaims("EmailOTPExecutor");

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery configs for internally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Verify that no event was triggered since required claim is not available
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
        // Verify that diagnostic log was triggered for missing claim
        mockedLoggerUtils.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(
                any(DiagnosticLog.DiagnosticLogBuilder.class)), times(1));
    }

    @Test
    public void testHandlePasswordRecoveryCompletionWithClaimsButNotificationsDisabled() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext("EmailOTPExecutor");

        // Mock flow completion config to disable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("false");

        // Mock recovery configs for internally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Verify that no event was triggered since notifications are disabled
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
        // Verify that diagnostic log was NOT triggered since notifications are disabled (claim check doesn't happen)
        mockedLoggerUtils.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(
                any(DiagnosticLog.DiagnosticLogBuilder.class)), times(0));
    }

    // Tests for Internal and External Managed Notifications

    @DataProvider
    public Object[][] internallyManagedNotificationData() {
        return new Object[][]{
                {"EMAIL", true, NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl()},
                {"SMS", true, NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl()},
                {"EXTERNAL", true, null}, // External channel - no verified claim even if internally managed
                {"EMAIL", false, null},   // Externally managed - no verified claim
                {"SMS", false, null},     // Externally managed - no verified claim
        };
    }

    @Test(dataProvider = "internallyManagedNotificationData")
    public void testInvitedUserRegistrationWithInternalExternalManagedNotifications(
            String channel, boolean internallyManaged, String expectedVerifiedClaimUrl) throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn(channel);
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn(String.valueOf(internallyManaged));

        // Mock flow completion config
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery data
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Capture the claims being set
        org.mockito.ArgumentCaptor<Map> claimsCaptor = org.mockito.ArgumentCaptor.forClass(Map.class);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor.capture(), eq(null));
        verify(jdbcRecoveryDataStore).invalidate(user);

        // Verify claims based on internal/external management
        Map<String, String> capturedClaims = claimsCaptor.getValue();
        if (expectedVerifiedClaimUrl != null) {
            // For internally managed notifications (except EXTERNAL channel), verified claim should be set
            assertTrue(capturedClaims.containsKey(expectedVerifiedClaimUrl),
                    "Expected verified claim URL: " + expectedVerifiedClaimUrl);
            assertEquals(capturedClaims.get(expectedVerifiedClaimUrl), Boolean.TRUE.toString());
        } else {
            // For externally managed or EXTERNAL channel, verified claim should NOT be set
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(channel)) {
                assertFalse(capturedClaims.containsKey(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl()),
                        "Email verified claim should not be set for externally managed");
            } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel)) {
                assertFalse(capturedClaims.containsKey(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl()),
                        "SMS verified claim should not be set for externally managed");
            }
        }

        // Verify account state claims are always set
        assertTrue(capturedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);

        // Verify notification event triggering
        if (internallyManaged && !NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(channel)) {
            // For internally managed notifications (except EXTERNAL channel), notification event should be triggered
            verify(identityEventService, times(2)).handleEvent(any(Event.class));
        } else {
            // For externally managed or EXTERNAL channel, only the POST_ADD_NEW_PASSWORD event should be triggered
            verify(identityEventService, times(1)).handleEvent(any(Event.class));
        }
    }

    @DataProvider
    public Object[][] passwordRecoveryManagedNotificationData() {
        return new Object[][]{
                {"EmailOTPExecutor", true},   // Internally managed with email
                {"SMSOTPExecutor", true},     // Internally managed with SMS
                {"EmailOTPExecutor", false},  // Externally managed with email
                {"SMSOTPExecutor", false},    // Externally managed with SMS
        };
    }

    @Test(dataProvider = "passwordRecoveryManagedNotificationData")
    public void testPasswordRecoveryWithInternalExternalManagedNotifications(
            String executorName, boolean internallyManaged) throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext(executorName);

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery configs for internally/externally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn(String.valueOf(internallyManaged));

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);

        // Verify notification event triggering based on internal/external management
        if (internallyManaged) {
            // For internally managed notifications, event should be triggered
            verify(identityEventService).handleEvent(any(Event.class));
        } else {
            // For externally managed notifications, no event should be triggered
            verify(identityEventService, times(0)).handleEvent(any(Event.class));
        }
    }

    @Test
    public void testPasswordRecoveryWithExternalChannelSkipsNotification() throws Exception {

        // Setup - Create a context with EMAIL executor but EXTERNAL channel
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createPasswordRecoveryContext("EmailOTPExecutor");

        // Override the notification channel to EXTERNAL
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL))
                .thenReturn(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.PASSWORD_RECOVERY), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery configs for internally managed (but should still skip due to EXTERNAL channel)
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        // Even though internally managed is true, EXTERNAL channel should prevent notification
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
    }

    @DataProvider
    public Object[][] accountStateClaimsData() {
        return new Object[][]{
                {"EMAIL", true, true},   // Internally managed, Email channel - should have verified claim
                {"SMS", true, true},     // Internally managed, SMS channel - should have verified claim
                {"EMAIL", false, false}, // Externally managed, Email channel - no verified claim
                {"SMS", false, false},   // Externally managed, SMS channel - no verified claim
                {"UNKNOWN", true, true}, // Internally managed, Unknown channel - defaults to email verified claim
        };
    }

    @Test(dataProvider = "accountStateClaimsData")
    public void testAccountStateClaimsForInternalExternalManagement(
            String channel, boolean internallyManaged, boolean shouldHaveVerifiedClaim) throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn(channel);
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn(String.valueOf(internallyManaged));

        // Mock flow completion config - disable notification to focus on claims only
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("false");

        // Mock recovery data
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Capture the claims being set
        org.mockito.ArgumentCaptor<Map> claimsCaptor = org.mockito.ArgumentCaptor.forClass(Map.class);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor.capture(), eq(null));

        Map<String, String> capturedClaims = claimsCaptor.getValue();

        // Verify account state claims are always present
        assertTrue(capturedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
        assertTrue(capturedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM), Boolean.FALSE.toString());

        // Verify verified claim presence based on internal/external management
        if (shouldHaveVerifiedClaim) {
            boolean hasVerifiedClaim = capturedClaims.containsKey(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl())
                    || capturedClaims.containsKey(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl());
            assertTrue(hasVerifiedClaim, "Verified claim should be present for internally managed notifications");
        } else {
            assertFalse(capturedClaims.containsKey(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl()),
                    "Email verified claim should not be present for externally managed notifications");
            assertFalse(capturedClaims.containsKey(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl()),
                    "SMS verified claim should not be present for externally managed notifications");
        }
    }

    @Test
    public void testInvitedUserRegistrationWithExternallyManagedNotificationsDoesNotTriggerEvent() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        
        // Set externally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("false");

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery data
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(anyString(), any(), eq(null));
        verify(jdbcRecoveryDataStore).invalidate(user);
        
        // For externally managed notifications, only POST_ADD_NEW_PASSWORD event should be triggered
        // TRIGGER_NOTIFICATION event should NOT be triggered
        verify(identityEventService, times(1)).handleEvent(any(Event.class));
    }

    @Test
    public void testInvitedUserRegistrationWithInternallyManagedNotificationsTriggersEvent() throws Exception {

        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        
        // Set internally managed
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        // Mock flow completion config to enable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("true");

        // Mock recovery data
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(anyString(), any(), eq(null));
        verify(jdbcRecoveryDataStore).invalidate(user);
        
        // For internally managed notifications, both TRIGGER_NOTIFICATION and POST_ADD_NEW_PASSWORD events should be triggered
        verify(identityEventService, times(2)).handleEvent(any(Event.class));
    }

    // Helper methods
    private FlowExecutionStep createMockStep() {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        DataDTO data = mock(DataDTO.class);
        when(step.getData()).thenReturn(data);
        when(step.getFlowStatus()).thenReturn(Constants.COMPLETE);
        doNothing().when(step).setStepType(any());
        doNothing().when(data).addAdditionalData(anyString(), anyString());
        return step;
    }

    private FlowExecutionContext createSelfRegistrationContext() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = mock(FlowUser.class);
        Map<String, String> userClaims = new HashMap<>();
        userClaims.put(NotificationChannels.EMAIL_CHANNEL.getClaimUri(), "test@example.com");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(context.getContextIdentifier()).thenReturn("test-flow-id");
        when(flowUser.getUsername()).thenReturn("testUser");
        when(flowUser.getClaims()).thenReturn(userClaims);

        return context;
    }

    private FlowExecutionContext createInvitedUserRegistrationContext() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT, "confirmationCode123");
        properties.put(IdentityRecoveryConstants.NOTIFICATION_CHANNEL, "EMAIL");
        properties.put(IdentityRecoveryConstants.RECOVERY_SCENARIO, "INVITED_USER_REGISTRATION");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.INVITED_USER_REGISTRATION.getType());
        when(context.getProperties()).thenReturn(properties);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("confirmationCode123");
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL)).thenReturn("EMAIL");
        when(context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO)).thenReturn("INVITED_USER_REGISTRATION");

        return context;
    }

    private FlowExecutionContext createPasswordRecoveryContext(String executorName) {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = mock(FlowUser.class);

        // Create mock completed nodes with the specified executor - using NodeConfig and ExecutorDTO
        List<NodeConfig> completedNodes = new ArrayList<>();
        NodeConfig node = mock(NodeConfig.class);
        ExecutorDTO executorDTO = mock(ExecutorDTO.class);
        when(executorDTO.getName()).thenReturn(executorName);
        when(node.getExecutorConfig()).thenReturn(executorDTO);
        completedNodes.add(node);

        // Mock FlowUser claims based on executor type
        Map<String, String> userClaims = new HashMap<>();
        if ("EmailOTPExecutor".equals(executorName) || "MagicLinkExecutor".equals(executorName)) {
            userClaims.put(NotificationChannels.EMAIL_CHANNEL.getClaimUri(), "test@example.com");
        } else if ("SMSOTPExecutor".equals(executorName)) {
            userClaims.put(NotificationChannels.SMS_CHANNEL.getClaimUri(), "+1234567890");
        }

        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT, "confirmationCode123");
        properties.put(IdentityRecoveryConstants.NOTIFICATION_CHANNEL, "EMAIL");
        properties.put(IdentityRecoveryConstants.RECOVERY_SCENARIO, "PASSWORD_RECOVERY");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.PASSWORD_RECOVERY.getType());
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(context.getFlowUser()).thenReturn(flowUser);
        when(flowUser.getUsername()).thenReturn("testUser");
        when(flowUser.getClaims()).thenReturn(userClaims);
        when(context.getCompletedNodes()).thenReturn(completedNodes);
        when(context.getProperties()).thenReturn(properties);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("confirmationCode123");
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL)).thenReturn("EMAIL");
        when(context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO)).thenReturn("PASSWORD_RECOVERY");

        return context;
    }

    private FlowExecutionContext createPasswordRecoveryContextWithMultipleExecutors() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = mock(FlowUser.class);

        // Create mock completed nodes with multiple executors - using NodeConfig and ExecutorDTO
        List<NodeConfig> completedNodes = new ArrayList<>();

        // Add EmailOTPExecutor node
        NodeConfig emailNode = mock(NodeConfig.class);
        ExecutorDTO emailExecutorDTO = mock(ExecutorDTO.class);
        when(emailExecutorDTO.getName()).thenReturn("EmailOTPExecutor");
        when(emailNode.getExecutorConfig()).thenReturn(emailExecutorDTO);
        completedNodes.add(emailNode);

        // Add SMSOTPExecutor node
        NodeConfig smsNode = mock(NodeConfig.class);
        ExecutorDTO smsExecutorDTO = mock(ExecutorDTO.class);
        when(smsExecutorDTO.getName()).thenReturn("SMSOTPExecutor");
        when(smsNode.getExecutorConfig()).thenReturn(smsExecutorDTO);
        completedNodes.add(smsNode);

        // Mock FlowUser claims with both email and SMS claims
        Map<String, String> userClaims = new HashMap<>();
        userClaims.put(NotificationChannels.EMAIL_CHANNEL.getClaimUri(), "test@example.com");
        userClaims.put(NotificationChannels.SMS_CHANNEL.getClaimUri(), "+1234567890");

        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT, "confirmationCode123");
        properties.put(IdentityRecoveryConstants.NOTIFICATION_CHANNEL, "EMAIL");
        properties.put(IdentityRecoveryConstants.RECOVERY_SCENARIO, "PASSWORD_RECOVERY");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.PASSWORD_RECOVERY.getType());
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(context.getFlowUser()).thenReturn(flowUser);
        when(flowUser.getUsername()).thenReturn("testUser");
        when(flowUser.getClaims()).thenReturn(userClaims);
        when(context.getCompletedNodes()).thenReturn(completedNodes);
        when(context.getProperties()).thenReturn(properties);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("confirmationCode123");
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL)).thenReturn("EMAIL");
        when(context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO)).thenReturn("PASSWORD_RECOVERY");

        return context;
    }

    private FlowExecutionContext createPasswordRecoveryContextWithoutClaims(String executorName) {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = mock(FlowUser.class);

        // Create mock completed nodes with the specified executor
        List<NodeConfig> completedNodes = new ArrayList<>();
        NodeConfig node = mock(NodeConfig.class);
        ExecutorDTO executorDTO = mock(ExecutorDTO.class);
        when(executorDTO.getName()).thenReturn(executorName);
        when(node.getExecutorConfig()).thenReturn(executorDTO);
        completedNodes.add(node);

        // Mock FlowUser claims as empty (no notification claims available)
        Map<String, String> userClaims = new HashMap<>();

        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT, "confirmationCode123");
        properties.put(IdentityRecoveryConstants.NOTIFICATION_CHANNEL, "EMAIL");
        properties.put(IdentityRecoveryConstants.RECOVERY_SCENARIO, "PASSWORD_RECOVERY");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.PASSWORD_RECOVERY.getType());
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(context.getFlowUser()).thenReturn(flowUser);
        when(flowUser.getUsername()).thenReturn("testUser");
        when(flowUser.getClaims()).thenReturn(userClaims);
        when(context.getCompletedNodes()).thenReturn(completedNodes);
        when(context.getProperties()).thenReturn(properties);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("confirmationCode123");
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL)).thenReturn("EMAIL");
        when(context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO)).thenReturn("PASSWORD_RECOVERY");

        return context;
    }

    private User createTestUser() {

        User user = new User();
        user.setUserName("testUser");
        user.setTenantDomain("carbon.super");
        user.setUserStoreDomain("PRIMARY");
        return user;
    }
}
