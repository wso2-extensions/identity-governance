/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.confirmation;

import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.dto.ResendConfirmationDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.model.UserRecoveryFlowData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.identity.event.event.Event;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

@WithCarbonHome
public class ResendConfirmationManagerTest {

    @InjectMocks
    private ResendConfirmationManager resendConfirmationManager;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private IdentityGovernanceService identityGovernanceService;

    @Mock
    private RealmService realmService;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    private PrivilegedCarbonContext threadLocalCarbonContext;

    @Mock
    private UserAccountRecoveryManager userAccountRecoveryManager;

    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedServiceDataHolder;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<UserAccountRecoveryManager> mockedUserAccountRecoveryManager;

    private static final String TEST_USERNAME = "test-user";
    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final String TEST_USER_STORE_DOMAIN = "TESTING";

    @BeforeMethod
    public void setUp() throws Exception {

        MockitoAnnotations.openMocks(this);
        resendConfirmationManager = ResendConfirmationManager.getInstance();

        mockedServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedUtils = mockStatic(Utils.class);
        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedJDBCRecoveryDataStore =  mockStatic(JDBCRecoveryDataStore.class);
        mockedUserAccountRecoveryManager = mockStatic(UserAccountRecoveryManager.class);

        when(IdentityRecoveryServiceDataHolder.getInstance()).thenReturn(identityRecoveryServiceDataHolder);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(threadLocalCarbonContext);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedUserAccountRecoveryManager.when(UserAccountRecoveryManager::getInstance)
                .thenReturn(userAccountRecoveryManager);

        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(identityRecoveryServiceDataHolder.getIdentityGovernanceService()).thenReturn(identityGovernanceService);
        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(realmService);

        when(threadLocalCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN);
    }

    @AfterMethod
    public void tearDown() {

        mockedServiceDataHolder.close();
        mockedIdentityUtil.close();
        mockedIdentityTenantUtil.close();
        mockedUtils.close();
        mockedPrivilegedCarbonContext.close();
        mockedJDBCRecoveryDataStore.close();
        mockedUserAccountRecoveryManager.close();
    }

    @Test
    public void testResendConfirmationCodeMobileVerificationOnUpdate() throws Exception {

        String verificationPendingMobile = "0777897621";
        String oldCode = "dummy-code";
        String newCode = "new-code";
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);

        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        assertNotNull(responseBean);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        Assert.assertEquals(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                capturedRecoveryData.getRecoveryScenario());
        Assert.assertEquals(verificationPendingMobile,
                capturedRecoveryData.getRemainingSetIds());

        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        Assert.assertEquals(verificationPendingMobile, eventProperties.get(IdentityRecoveryConstants.SEND_TO));
        Assert.assertEquals(newCode, eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE));

        // Reset data.
        reset(userRecoveryDataStore);
        reset(identityEventService);

        // Case 2: MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE recovery scenario.
        UserRecoveryData userRecoveryData2 = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData2.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE)).thenReturn(userRecoveryData2);

        NotificationResponseBean responseBean2 = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        assertNotNull(responseBean2);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor2 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor2.capture());
        UserRecoveryData capturedRecoveryData2 = recoveryDataCaptor2.getValue();
        Assert.assertEquals(RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                capturedRecoveryData2.getRecoveryScenario());
        Assert.assertEquals(verificationPendingMobile,
                capturedRecoveryData2.getRemainingSetIds());
    }

    @Test
    public void testResendConfirmationCodeEmailVerificationOnUpdate() throws Exception {

        String verificationPendingEmail = "testuser@gmail.com";
        String oldCode = "dummy-code";
        String newCode = "new-code";
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL);
        userRecoveryData.setRemainingSetIds(verificationPendingEmail);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.EMAIL_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockUtilsErrors();

        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString(),
                RecoverySteps.VERIFY_EMAIL.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE, properties);

        assertNotNull(responseBean);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        Assert.assertEquals(RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                capturedRecoveryData.getRecoveryScenario());
        Assert.assertEquals(verificationPendingEmail,
                capturedRecoveryData.getRemainingSetIds());

        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        Assert.assertEquals(verificationPendingEmail, eventProperties.get(IdentityRecoveryConstants.SEND_TO));
        Assert.assertEquals(newCode, eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE));

        // Reset.
        reset(userRecoveryDataStore);
        reset(identityEventService);

        // Case 2: EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE recovery scenario.
        UserRecoveryData userRecoveryData2 = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL);
        userRecoveryData2.setRemainingSetIds(verificationPendingEmail);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE)).thenReturn(userRecoveryData2);

        NotificationResponseBean responseBean2 = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                RecoverySteps.VERIFY_EMAIL.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE, properties);
        assertNotNull(responseBean2);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor2 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor2.capture());
        UserRecoveryData capturedRecoveryData2 = recoveryDataCaptor2.getValue();
        Assert.assertEquals(RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                capturedRecoveryData2.getRecoveryScenario());
        Assert.assertEquals(verificationPendingEmail,
                capturedRecoveryData2.getRemainingSetIds());
    }

    @Test
    public void testResendConfirmationCodeErrorScenarios() throws Exception {

        String verificationPendingMobile = "0777897621";
        String oldCode = "dummy-code";
        String newCode = "new-code";
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockUtilsErrors();

        // Case 1: Null user.
        try {
            resendConfirmationManager.resendConfirmationCode(null,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 2: Empty Recovery scenario.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    "",
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 3: Empty Recovery step.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    "",
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 4: Empty Notification type.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    "", properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test
    public void testResendConfirmationCodeWithCode() throws IdentityRecoveryException, IdentityEventException {

        String verificationPendingEmail = "testuser@gmail.com";
        String oldCode = "dummy-code";
        String newCode = "new-code";
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL);
        userRecoveryData.setRemainingSetIds(verificationPendingEmail);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.EMAIL_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockUtilsErrors();

        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                oldCode,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString(),
                RecoverySteps.VERIFY_EMAIL.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE, properties);

        assertNotNull(responseBean);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        Assert.assertEquals(RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                capturedRecoveryData.getRecoveryScenario());
        Assert.assertEquals(verificationPendingEmail,
                capturedRecoveryData.getRemainingSetIds());

        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        Assert.assertEquals(verificationPendingEmail, eventProperties.get(IdentityRecoveryConstants.SEND_TO));
        Assert.assertEquals(newCode, eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE));

        // Case 2: Code not given.
        try {
            resendConfirmationManager.resendConfirmationCode(
                    user,
                    null,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test
    public void testResendConfirmation() throws IdentityRecoveryException {

        String resendCode = "dummy-code";
        String recoveryFlowId = "dummy-flow-id";
        int resendCount = 2;
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.
                RECOVERY_NOTIFICATION_PASSWORD_MAX_RESEND_ATTEMPTS, TEST_TENANT_DOMAIN)).thenReturn("5");
        mockUtilsErrors();

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, resendCode,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.VERIFY_EMAIL);
        userRecoveryData.setRemainingSetIds(NotificationChannels.EMAIL_CHANNEL.getChannelType());
        userRecoveryData.setRecoveryFlowId(recoveryFlowId);

        when(userAccountRecoveryManager.getUserRecoveryData(anyString(), any())).thenReturn(userRecoveryData);

        UserRecoveryFlowData userRecoveryFlowData = mock(UserRecoveryFlowData.class);
        when(userAccountRecoveryManager.loadUserRecoveryFlowData(userRecoveryData))
                .thenReturn(userRecoveryFlowData);
        when(userRecoveryFlowData.getResendCount()).thenReturn(resendCount);

        ResendConfirmationDTO resendConfirmationDTO = resendConfirmationManager.resendConfirmation(
                TEST_TENANT_DOMAIN, resendCode, RecoveryScenarios.SELF_SIGN_UP.toString(),
                RecoverySteps.VERIFY_EMAIL.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM,
                properties);
        assertEquals(resendConfirmationDTO.getSuccessCode(),
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE.getCode());
        verify(userAccountRecoveryManager).updateRecoveryDataResendCount(recoveryFlowId, resendCount + 1);

        // Case 2: Resend count exceeds the maximum limit.
        when(userRecoveryFlowData.getResendCount()).thenReturn(5);
        try {
            resendConfirmationManager.resendConfirmation(
                    TEST_TENANT_DOMAIN, resendCode, RecoveryScenarios.SELF_SIGN_UP.toString(),
                    RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM,
                    properties);
            verify(userAccountRecoveryManager).invalidateRecoveryData(recoveryFlowId);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 2: When recovery scenarios doesn't match.
        when(userRecoveryFlowData.getResendCount()).thenReturn(1);
        try {
            resendConfirmationManager.resendConfirmation(
                    TEST_TENANT_DOMAIN, resendCode, RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM,
                    properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 3: When tenant domain doesn't match.
        try {
            mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.
                    RECOVERY_NOTIFICATION_PASSWORD_MAX_RESEND_ATTEMPTS, "OTHER_DOMAIN"))
                    .thenReturn("5");
            resendConfirmationManager.resendConfirmation(
                    "OTHER_DOMAIN", resendCode, RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM,
                    properties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

    }

    private static User getUser() {

        User user = new User();
        user.setUserName(TEST_USERNAME);
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        user.setUserStoreDomain(TEST_USER_STORE_DOMAIN);
        return user;
    }

    private void mockUtilsErrors() {

        mockedUtils.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                any())).thenReturn(new IdentityRecoveryClientException("test-code", "test-dec"));

        mockedUtils.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                isNull())).thenReturn(new IdentityRecoveryClientException("test-code", "test-dec"));

        mockedUtils.when(() -> Utils.handleClientException(anyString(), anyString(), any()))
                .thenReturn(new IdentityRecoveryClientException("test-code", "test-dec"));

        mockedUtils.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                anyString())).thenReturn(new IdentityRecoveryClientException("test-code", "test-dec"));

    }
}
