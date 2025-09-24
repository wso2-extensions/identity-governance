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

import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.flow.mgt.model.DataDTO;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
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

        mockedLoggerUtils.when(LoggerUtils::isDiagnosticLogsEnabled).thenReturn(true);
        doNothing().when(LoggerUtils.class);
        LoggerUtils.triggerDiagnosticLogEvent(any(DiagnosticLog.DiagnosticLogBuilder.class));
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
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_user");

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
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_user");

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
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_user");

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
        mockedSelfRegistrationUtils.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_user");

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

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

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
    public void testHandleInvitedUserRegistrationCompletionWithNullConfirmationCode() throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        context.getProperties().remove(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT);
        User user = createTestUser();

        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);

        // Execute
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertFalse(result);
    }

    @DataProvider
    public Object[][] notificationChannelData() {
        return new Object[][]{
                {NotificationChannels.EMAIL_CHANNEL.getChannelType(), NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl()},
                {NotificationChannels.SMS_CHANNEL.getChannelType(), NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl()},
                {"UNKNOWN", NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl()} // Default to email
        };
    }

    @Test(dataProvider = "notificationChannelData")
    public void testGetAccountStateClaims(String channel, String expectedClaimUrl) throws Exception {
        // Use reflection to test the private method indirectly through the public flow
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn(channel);
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(
                eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");

        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);

        // Execute
        listener.doPostExecute(step, context);

        // Verify
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor.capture(), eq(null));
        Map<String, String> capturedClaims = claimsCaptor.getValue();

        assertTrue(capturedClaims.containsKey(expectedClaimUrl));
        assertEquals(capturedClaims.get(expectedClaimUrl), "true");
        assertTrue(capturedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI));
        assertTrue(capturedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM));
    }

    @Test
    public void testInvalidateRecoveryDataWithRecoveryFlowId() throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn("true");

        // Mock recovery data with flowId
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn("flowId123");
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

        // Execute
        listener.doPostExecute(step, context);

        // Verify
        verify(jdbcRecoveryDataStore).invalidateWithRecoveryFlowId("flowId123");
        verify(jdbcRecoveryDataStore, never()).invalidate(any(User.class));
    }

    @Test
    public void testInvalidateRecoveryDataException() throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn("true");

        // Mock recovery data loading to throw exception
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString()))
                .thenThrow(new IdentityRecoveryException("Load error"));

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

        // Execute (should not throw exception, should handle gracefully)
        boolean result = listener.doPostExecute(step, context);

        // Verify
        assertTrue(result);
        verify(jdbcRecoveryDataStore, never()).invalidate(any(User.class));
        verify(jdbcRecoveryDataStore, never()).invalidateWithRecoveryFlowId(anyString());
    }

    @Test
    public void testHandleNotificationsExternalChannel() throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString()))
                .thenReturn(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn("true");

        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

        // Execute
        listener.doPostExecute(step, context);

        // Verify - only one event should be triggered (publishEvent), not notification
        verify(identityEventService, times(1)).handleEvent(any(Event.class));
    }

    @Test
    public void testHandleNotificationsDisabled() throws Exception {
        // Setup
        FlowExecutionStep step = createMockStep();
        FlowExecutionContext context = createInvitedUserRegistrationContext();
        User user = createTestUser();

        when(recoveryManager.getServerSupportedNotificationChannel(anyString())).thenReturn("EMAIL");
        mockedUtilsStatic.when(() -> Utils.resolveUserFromContext(context)).thenReturn(user);
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn("true");

        // Mock flow completion config to disable notifications
        mockedUtilsStatic.when(() -> Utils.getFlowCompletionConfig(
                eq(Constants.FlowTypes.INVITED_USER_REGISTRATION), anyString(),
                eq(Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED)))
                .thenReturn("false");

        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(anyString())).thenReturn(recoveryData);

        // Mock IdentityUtil.addDomainToName directly
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("PRIMARY/testUser");

        // Execute
        listener.doPostExecute(step, context);

        // Verify - only one event should be triggered (publishEvent), not notification
        verify(identityEventService, times(1)).handleEvent(any(Event.class));
    }

    // Helper methods
    private FlowExecutionStep createMockStep() {
        FlowExecutionStep step = mock(FlowExecutionStep.class);
        DataDTO data = mock(DataDTO.class);
        when(step.getFlowStatus()).thenReturn(Constants.COMPLETE);
        when(step.getData()).thenReturn(data);
        return step;
    }

    private FlowExecutionContext createSelfRegistrationContext() {
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername("testUser");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.REGISTRATION.getType());
        when(context.getTenantDomain()).thenReturn("carbon.super");
        when(context.getContextIdentifier()).thenReturn("contextId");
        when(context.getFlowUser()).thenReturn(flowUser);

        return context;
    }

    private FlowExecutionContext createInvitedUserRegistrationContext() {
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT, "confirmationCode123");
        properties.put(IdentityRecoveryConstants.NOTIFICATION_CHANNEL, "EMAIL");
        properties.put(IdentityRecoveryConstants.RECOVERY_SCENARIO, "INVITED_USER_REGISTRATION");

        when(context.getFlowType()).thenReturn(Constants.FlowTypes.INVITED_USER_REGISTRATION.getType());
        when(context.getProperty(anyString())).thenAnswer(invocation -> {
            String key = invocation.getArgument(0);
            return properties.get(key);
        });
        when(context.getProperties()).thenReturn(properties);

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
