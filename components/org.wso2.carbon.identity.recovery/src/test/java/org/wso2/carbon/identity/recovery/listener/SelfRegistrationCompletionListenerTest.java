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

import org.apache.commons.logging.Log;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.ApplicationBasicInfo;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
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
import org.wso2.carbon.identity.flow.mgt.model.DataDTO;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.workflow.mgt.WorkflowManagementService;
import org.wso2.carbon.identity.workflow.mgt.bean.Entity;
import org.wso2.carbon.identity.workflow.mgt.exception.WorkflowException;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.DiagnosticLog;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE;

/**
 * Unit tests for {@link SelfRegistrationCompletionListener}.
 */
public class SelfRegistrationCompletionListenerTest {

    private SelfRegistrationCompletionListener selfRegistrationCompletionListener;
    private MockedStatic<Utils> utilsMockedStatic;
    private MockedStatic<SelfRegistrationUtils> selfRegistrationUtilsMockedStatic;
    private MockedStatic<IdentityRecoveryServiceDataHolder> serviceDataHolderMockedStatic;
    private MockedStatic<JDBCRecoveryDataStore> recoveryDataStoreMockedStatic;
    private MockedStatic<IdentityUtil> identityUtilMockedStatic;
    private MockedStatic<IdentityTenantUtil> identityTenantUtilMockedStatic;
    private MockedStatic<LoggerUtils> loggerUtilsMockedStatic;
    private ByteArrayOutputStream logOutput;
    private ArgumentCaptor<DiagnosticLog.DiagnosticLogBuilder> builderCaptor;
    private FlowExecutionContext context;
    private FlowExecutionStep step;
    private static final String notificationChannel = "EMAIL";

    @Mock
    private IdentityRecoveryServiceDataHolder serviceDataHolder;

    @Mock
    private JDBCRecoveryDataStore jdbcRecoveryDataStore;

    @Mock
    private NotificationChannelManager notificationChannelManager;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private ApplicationManagementService applicationManagementService;

    @Mock
    private RealmService realmService;

    @Mock
    private UserRealm userRealm;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private WorkflowManagementService workflowManagementService;

    @BeforeMethod
    public void setUp() throws UserStoreException, NotificationChannelManagerException, WorkflowException {

        selfRegistrationCompletionListener = new SelfRegistrationCompletionListener();
        context = getRegistrationContext();
        step = getRegistrationStep();

        utilsMockedStatic = Mockito.mockStatic(Utils.class);
        selfRegistrationUtilsMockedStatic = Mockito.mockStatic(SelfRegistrationUtils.class);
        serviceDataHolderMockedStatic = Mockito.mockStatic(IdentityRecoveryServiceDataHolder.class);
        recoveryDataStoreMockedStatic = Mockito.mockStatic(JDBCRecoveryDataStore.class);
        identityUtilMockedStatic = Mockito.mockStatic(IdentityUtil.class);
        identityTenantUtilMockedStatic = Mockito.mockStatic(IdentityTenantUtil.class);
        loggerUtilsMockedStatic = Mockito.mockStatic(LoggerUtils.class);

        serviceDataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        jdbcRecoveryDataStore = mock(JDBCRecoveryDataStore.class);
        notificationChannelManager = mock(NotificationChannelManager.class);
        identityEventService = mock(IdentityEventService.class);
        applicationManagementService = mock(ApplicationManagementService.class);
        realmService = mock(RealmService.class);
        userRealm = mock(UserRealm.class);
        userStoreManager = mock(UserStoreManager.class);
        workflowManagementService = mock(WorkflowManagementService.class);

        // Default behavior: no pending workflows.
        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), anyString()))
                .thenReturn(false);

        serviceDataHolderMockedStatic.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        recoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(jdbcRecoveryDataStore);
        utilsMockedStatic.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);

        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(serviceDataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);
        when(serviceDataHolder.getRealmService()).thenReturn(realmService);
        when(serviceDataHolder.getWorkflowManagementService()).thenReturn(workflowManagementService);
        identityUtilMockedStatic.when(IdentityUtil::getPrimaryDomainName).thenReturn("PRIMARY");
        
        // Mock IdentityTenantUtil for tenant ID resolution.
        identityTenantUtilMockedStatic.when(() -> IdentityTenantUtil.getTenantId("tenant.com")).thenReturn(1);

        when(serviceDataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(1)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString()))
                .thenReturn(notificationChannel);

        NotificationChannels emailChannel = NotificationChannels.EMAIL_CHANNEL;
        selfRegistrationUtilsMockedStatic.when(
                        () -> SelfRegistrationUtils.getNotificationChannel(anyString(), anyString()))
                .thenReturn(emailChannel);

        utilsMockedStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class))).thenReturn("testUser");

        // Mock for logging.
        loggerUtilsMockedStatic.when(LoggerUtils::isDiagnosticLogsEnabled).thenReturn(true);
        doNothing().when(LoggerUtils.class);
        LoggerUtils.triggerDiagnosticLogEvent(any(DiagnosticLog.DiagnosticLogBuilder.class));
        Log mockLog = Mockito.mock(Log.class);
        builderCaptor = ArgumentCaptor.forClass(DiagnosticLog.DiagnosticLogBuilder.class);
        when(mockLog.isDebugEnabled()).thenReturn(true);
        logOutput = new ByteArrayOutputStream();
        System.setOut(new PrintStream(logOutput));
    }

    @AfterMethod
    public void tearDown() {

        utilsMockedStatic.close();
        selfRegistrationUtilsMockedStatic.close();
        serviceDataHolderMockedStatic.close();
        recoveryDataStoreMockedStatic.close();
        identityUtilMockedStatic.close();
        identityTenantUtilMockedStatic.close();
        loggerUtilsMockedStatic.close();
    }

    @DataProvider
    public Object[][] setApplicationInContext() {

        return new Object[][]{
                {true},
                {false}
        };
    }

    @Test
    public void testGetDefaultOrderId() {

        Assert.assertEquals(selfRegistrationCompletionListener.getDefaultOrderId(), 3);
    }

    @Test
    public void testGetExecutionOrderId() {

        Assert.assertEquals(selfRegistrationCompletionListener.getExecutionOrderId(), 3);
    }

    @Test
    public void testIsEnabled() {

        assertTrue(selfRegistrationCompletionListener.isEnabled());
    }

    @Test
    public void testDoPostExecuteNotComplete() throws FlowEngineException {

        FlowExecutionStep step = new FlowExecutionStep.Builder()
                .stepType(Constants.StepTypes.VIEW)
                .flowId("flowId")
                .flowStatus("NOT_COMPLETE")
                .data(new DataDTO.Builder().build())
                .build();

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);
        assertTrue(result);
    }

    @Test
    public void testDoPostExecuteWithVerifiedChannel() throws FlowEngineException, IdentityEventException {

        // Simulate a verified notification channel.
        Map<String, String> claims = new HashMap<>();
        claims.put("http://wso2.org/claims/identity/emailVerified", "true");  // Channel is verified
        context.getFlowUser().addClaims(claims);
        context.setFlowType(Constants.FlowTypes.REGISTRATION.getType());

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        // No notifications should be sent for verified channels
        verify(identityEventService, times(0)).handleEvent(any(Event.class));
        // No diagnostic logs for notifications should be triggered
        loggerUtilsMockedStatic.verify(() ->
                        LoggerUtils.triggerDiagnosticLogEvent(
                                any(DiagnosticLog.DiagnosticLogBuilder.class)),
                times(0));
        assertTrue(logOutput.toString().contains(
                "Preferred Notification channel: " + notificationChannel + " is verified for the user"));

        // Reset the claims to simulate the unverified status.
        claims.put("http://wso2.org/claims/identity/emailVerified", "false");
        context.getFlowUser().addClaims(claims);
    }

    @Test(dataProvider = "setApplicationInContext")
    public void testDoPostExecuteAccountLockAndVerification(boolean isAppInContext) throws Exception {

        if (isAppInContext) {
            context.setApplicationId("app-uuid-001");
        } else {
            context.setApplicationId(null);
        }

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        testApp.setUuid("my-app-uuid");
        testApp.setApplicationName("My Account");

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getSignUpConfigs(eq(SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                        anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn("secret-key-123");

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.getUserStoreManager(anyString(),
                                anyString()))
                .thenReturn(userStoreManager);

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.resolveEventName(anyString(), anyString(),
                                anyString(), anyString()))
                .thenReturn("TRIGGER_NOTIFICATION");
        doNothing().when(SelfRegistrationUtils.class);
        SelfRegistrationUtils.triggerNotification(any(User.class), anyString(), anyString(), any(), anyString());

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"), anyString())).thenReturn(
                myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(
                testApp);

        doNothing().when(jdbcRecoveryDataStore).invalidate(any(User.class));
        doNothing().when(jdbcRecoveryDataStore).store(any(UserRecoveryData.class));

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);
        selfRegistrationUtilsMockedStatic.verify(() ->
                        SelfRegistrationUtils.lockUserAccount(eq(true), eq(true),
                                eq("tenant.com"),
                                any(UserStoreManager.class),
                                eq("testUser")),
                times(1));

        selfRegistrationUtilsMockedStatic.verify(() ->
                        SelfRegistrationUtils.triggerNotification(any(User.class),
                                eq("EMAIL"),
                                eq("secret-key-123"),
                                any(),
                                eq("TRIGGER_NOTIFICATION")),
                times(1));
        loggerUtilsMockedStatic.verify(() ->
                        LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()),
                times(1));

        DiagnosticLog.DiagnosticLogBuilder capturedBuilder = builderCaptor.getValue();
        DiagnosticLog log = capturedBuilder.build();

        assertEquals("trigger-notification", log.getActionId());
        assertEquals("SUCCESS", log.getResultStatus());
        assertEquals("Account verification notification sent successfully.", log.getResultMessage());
    }

    @Test
    public void testDoPostExecuteNoNotificationInternallyManage() throws Exception {

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("false");

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.getUserStoreManager(anyString(),
                                anyString()))
                .thenReturn(userStoreManager);

        doNothing().when(SelfRegistrationUtils.class);
        SelfRegistrationUtils.lockUserAccount(anyBoolean(), anyBoolean(), anyString(), any(UserStoreManager.class),
                anyString());

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);
        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.lockUserAccount(eq(true), eq(true),
                        eq("tenant.com"),
                        any(UserStoreManager.class),
                        eq("testUser")),
                times(1));

        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.triggerNotification(any(User.class),
                        anyString(),
                        anyString(), any(),
                        anyString()),
                times(0));
    }

    @Test
    public void testDoPostExecuteAccountCreationNotification() throws Exception {

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(
                        () -> Utils.getSignUpConfigs(eq(SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION), anyString()))
                .thenReturn("true");

        doNothing().when(SelfRegistrationUtils.class);
        SelfRegistrationUtils.triggerAccountCreationNotification(anyString(), anyString(), anyString());

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);
        selfRegistrationUtilsMockedStatic.verify(
                () -> SelfRegistrationUtils.triggerAccountCreationNotification(eq("testUser"), eq("tenant.com"),
                        anyString()),
                times(1));
        loggerUtilsMockedStatic.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()),
                times(1));

        DiagnosticLog.DiagnosticLogBuilder capturedBuilder = builderCaptor.getValue();
        DiagnosticLog log = capturedBuilder.build();

        assertEquals("trigger-notification", log.getActionId());
        assertEquals("SUCCESS", log.getResultStatus());
        assertEquals("Account creation notification sent successfully.", log.getResultMessage());
    }

    @Test
    public void testDoPostExecuteWithIdentityEventException() throws Exception {

        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString()))
                .thenThrow(new NotificationChannelManagerException("Channel error"));

        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.handledNotificationChannelManagerException(
                        any(NotificationChannelManagerException.class),
                        anyString(), anyString(), anyString()))
                .thenThrow(new IdentityEventException("Handled channel error"));

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        loggerUtilsMockedStatic.verify(() ->
                        LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()),
                times(1));

        DiagnosticLog.DiagnosticLogBuilder capturedBuilder = builderCaptor.getValue();
        DiagnosticLog log = capturedBuilder.build();
        assertEquals("trigger-notification", log.getActionId());
        assertEquals("FAILED", log.getResultStatus());
        assertEquals("Error while triggering verification notification.", log.getResultMessage());
    }

    @Test
    public void testDoPostExecuteNotificationTriggerFailure() throws Exception {

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn("secret-key-123");

        doNothing().when(jdbcRecoveryDataStore).invalidate(any(User.class));
        doNothing().when(jdbcRecoveryDataStore).store(any(UserRecoveryData.class));

        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.maskIfRequired(anyString()))
                .thenReturn("masked_username");
        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.resolveEventName(anyString(), anyString(),
                        anyString(), anyString()))
                .thenReturn("TRIGGER_NOTIFICATION");
        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.triggerNotification(any(User.class),
                        anyString(),
                        anyString(), any(),
                        anyString()))
                .thenThrow(new IdentityEventException("Handled channel error."));

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);
        loggerUtilsMockedStatic.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()), times(1));

        DiagnosticLog.DiagnosticLogBuilder capturedBuilder = builderCaptor.getValue();
        DiagnosticLog log = capturedBuilder.build();
        assertEquals("trigger-notification", log.getActionId());
        assertEquals("FAILED", log.getResultStatus());
        assertEquals("Error while triggering verification notification.", log.getResultMessage());
    }

    @Test
    public void testDoPostExecuteAppInfoFailure() throws Exception {

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getSignUpConfigs(eq(SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                        anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn("secret-key-123");

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.getUserStoreManager(anyString(),
                                anyString()))
                .thenReturn(userStoreManager);

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.resolveEventName(anyString(), anyString(),
                                anyString(), anyString()))
                .thenReturn("TRIGGER_NOTIFICATION");
        doNothing().when(SelfRegistrationUtils.class);
        SelfRegistrationUtils.triggerNotification(any(User.class), anyString(), anyString(), any(), anyString());

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"), anyString())).thenThrow(
                new IdentityApplicationManagementException("Application retrieval failed.")
        );

        doNothing().when(jdbcRecoveryDataStore).invalidate(any(User.class));
        doNothing().when(jdbcRecoveryDataStore).store(any(UserRecoveryData.class));

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.triggerNotification(any(User.class),
                        eq("EMAIL"),
                        eq("secret-key-123"),
                        any(),
                        eq("TRIGGER_NOTIFICATION")),
                times(0));
        loggerUtilsMockedStatic.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()), times(2));

        List<DiagnosticLog.DiagnosticLogBuilder> capturedBuilders = builderCaptor.getAllValues();
        assertEquals(2, capturedBuilders.size());

        DiagnosticLog firstLog = capturedBuilders.get(0).build();
        assertEquals("trigger-notification", firstLog.getActionId());
        assertEquals("FAILED", firstLog.getResultStatus());
        assertEquals("Error while resolving application details.", firstLog.getResultMessage());

        DiagnosticLog secondLog = capturedBuilders.get(1).build();
        assertEquals("trigger-notification", secondLog.getActionId());
        assertEquals("FAILED", secondLog.getResultStatus());
        assertEquals("Error while triggering verification notification.", secondLog.getResultMessage());
    }

    @Test
    public void testDoPostExecuteWithPendingWorkflow() throws Exception {

        // Simulate pending workflow for the user.
        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), eq("SELF_REGISTER_USER")))
                .thenReturn(true);

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        // Verify that step type is set to VIEW when workflow is pending.
        assertEquals(Constants.StepTypes.VIEW, step.getStepType());

        // Verify that PENDING_APPROVAL data is added when workflow is pending.
        assertEquals("true", step.getData().getAdditionalData().get("pendingApproval"));

        // Verify that no further processing occurs when workflow is pending.
        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.lockUserAccount(
                        anyBoolean(), anyBoolean(), anyString(), any(UserStoreManager.class), anyString()),
                times(0));

        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.triggerNotification(
                        any(User.class), anyString(), anyString(), any(), anyString()),
                times(0));

        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.triggerAccountCreationNotification(
                        anyString(), anyString(), anyString()),
                times(0));

        // Verify workflow service was called to check for pending workflows.
        verify(workflowManagementService, times(1))
                .entityHasPendingWorkflowsOfType(any(Entity.class), eq("SELF_REGISTER_USER"));
    }

    @Test
    public void testDoPostExecuteWorkflowException() throws Exception {

        // Simulate WorkflowException when checking for pending workflows.
        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), eq("SELF_REGISTER_USER")))
                .thenThrow(new WorkflowException("Workflow service error"));

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        // Verify error is logged.
        loggerUtilsMockedStatic.verify(() -> LoggerUtils.triggerDiagnosticLogEvent(builderCaptor.capture()),
                times(1));

        DiagnosticLog.DiagnosticLogBuilder capturedBuilder = builderCaptor.getValue();
        DiagnosticLog log = capturedBuilder.build();
        assertEquals("trigger-notification", log.getActionId());
        assertEquals("FAILED", log.getResultStatus());
        assertEquals("Error while triggering verification notification.", log.getResultMessage());
    }

    @Test
    public void testDoPostExecuteWithNoPendingWorkflowContinueProcessing() throws Exception {

        // Simulate no pending workflow for the user (default behavior already set in setUp).
        when(workflowManagementService.entityHasPendingWorkflowsOfType(any(Entity.class), eq("SELF_REGISTER_USER")))
                .thenReturn(false);

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("true");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("false");

        selfRegistrationUtilsMockedStatic.when(() ->
                        SelfRegistrationUtils.getUserStoreManager(anyString(), anyString()))
                .thenReturn(userStoreManager);

        doNothing().when(SelfRegistrationUtils.class);
        SelfRegistrationUtils.lockUserAccount(anyBoolean(), anyBoolean(), anyString(),
                any(UserStoreManager.class), anyString());

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        // Verify workflow service was called.
        verify(workflowManagementService, times(1))
                .entityHasPendingWorkflowsOfType(any(Entity.class), eq("SELF_REGISTER_USER"));

        // Verify account locking occurs since no pending workflow.
        selfRegistrationUtilsMockedStatic.verify(() -> SelfRegistrationUtils.lockUserAccount(
                        eq(true), eq(false), eq("tenant.com"),
                        any(UserStoreManager.class), eq("testUser")),
                times(1));
    }


    private FlowExecutionContext getRegistrationContext() {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setTenantDomain("tenant.com");
        context.setContextIdentifier("contextId");
        context.setFlowType(Constants.FlowTypes.REGISTRATION.getType());

        FlowUser user = new FlowUser();
        user.setUsername("testUser");
        Map<String, String> claims = new HashMap<>();
        claims.put("http://wso2.org/claims/emailaddress", "test@example.com");
        claims.put("http://wso2.org/claims/identity/emailVerified", "false");
        user.addClaims(claims);
        context.setFlowUser(user);
        return context;
    }

    private FlowExecutionStep getRegistrationStep() {

        return new FlowExecutionStep.Builder()
                .stepType(Constants.StepTypes.VIEW)
                .flowId("flowId")
                .flowStatus(Constants.COMPLETE)
                .data(new DataDTO.Builder().url("https://example.com/callback").build())
                .build();
    }

    @Test
    public void testDoPostExecuteWorkflowEntityCreation() throws Exception {

        // Mock Utils.getDomainQualifiedUsername to return expected value for Entity creation.
        utilsMockedStatic.when(() -> Utils.getDomainQualifiedUsername(any(FlowUser.class)))
                .thenReturn("testUser");

        ArgumentCaptor<Entity> entityCaptor = ArgumentCaptor.forClass(Entity.class);
        when(workflowManagementService.entityHasPendingWorkflowsOfType(entityCaptor.capture(), eq("SELF_REGISTER_USER")))
                .thenReturn(false);

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SEND_CONFIRMATION_NOTIFICATION), anyString()))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(eq(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                .thenReturn("false");

        boolean result = selfRegistrationCompletionListener.doPostExecute(step, context);

        assertTrue(result);

        // Verify Entity was created correctly.
        Entity capturedEntity = entityCaptor.getValue();
        assertEquals("testUser", capturedEntity.getEntityId());
        assertEquals("USER", capturedEntity.getEntityType());
        assertEquals(1, capturedEntity.getTenantId());
    }
}