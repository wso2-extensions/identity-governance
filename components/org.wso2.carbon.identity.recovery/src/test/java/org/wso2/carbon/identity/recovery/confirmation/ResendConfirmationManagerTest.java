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

import org.apache.commons.lang.StringUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Attribute;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
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
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowConfigConstants.FLOW_TYPE;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowConfigConstants.IS_AUTO_LOGIN_ENABLED;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowConfigConstants.IS_ENABLED;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowConfigConstants.RESOURCE_NAME_PREFIX;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowConfigConstants.RESOURCE_TYPE;

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
    private MockedStatic<FlowMgtConfigUtils> mockedFlowMgtUtils;
    private ConfigurationManager configurationManager;

    private static final String TEST_USERNAME = "test-user";
    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final String TEST_USER_STORE_DOMAIN = "TESTING";

    @BeforeMethod
    public void setUp() throws Exception {

        MockitoAnnotations.openMocks(this);
        resendConfirmationManager = ResendConfirmationManager.getInstance();
        configurationManager = mock(ConfigurationManager.class);
        mockedServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedUtils = mockStatic(Utils.class);
        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedJDBCRecoveryDataStore =  mockStatic(JDBCRecoveryDataStore.class);
        mockedUserAccountRecoveryManager = mockStatic(UserAccountRecoveryManager.class);
        mockedFlowMgtUtils = mockStatic(FlowMgtConfigUtils.class);

        FlowConfigDTO mockFlowConfig = mock(FlowConfigDTO.class);
        when(mockFlowConfig.getIsEnabled()).thenReturn(false);

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
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(anyString(), anyString()))
                .thenReturn(mockFlowConfig);
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
        mockedFlowMgtUtils.close();
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

        // Reset data.
        reset(userRecoveryDataStore);
        reset(identityEventService);

        UserRecoveryData userRecoveryData3 = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData3.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE)).thenReturn(userRecoveryData3);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData3,
                NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);

        NotificationResponseBean responseBean3 = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        assertNotNull(responseBean3);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor3 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor3.capture());
        UserRecoveryData capturedRecoveryData3 = recoveryDataCaptor3.getValue();
        Assert.assertEquals(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                capturedRecoveryData3.getRecoveryScenario());
        Assert.assertEquals(verificationPendingMobile,
                capturedRecoveryData3.getRemainingSetIds());

        ArgumentCaptor<Event> eventCaptor3 = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor3.capture());
        Event capturedEvent3 = eventCaptor3.getValue();
        Map<String, Object> eventProperties3 = capturedEvent3.getEventProperties();
        Assert.assertEquals(verificationPendingMobile, eventProperties3.get(IdentityRecoveryConstants.SEND_TO));
        Assert.assertEquals(newCode, eventProperties3.get(IdentityRecoveryConstants.CONFIRMATION_CODE));

        // Reset data.
        reset(userRecoveryDataStore);
        reset(identityEventService);

        UserRecoveryData userRecoveryData4 = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData4.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE))
                .thenReturn(userRecoveryData4);

        NotificationResponseBean responseBean4 = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
        assertNotNull(responseBean4);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor4 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor4.capture());
        UserRecoveryData capturedRecoveryData4 = recoveryDataCaptor4.getValue();
        Assert.assertEquals(RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                capturedRecoveryData4.getRecoveryScenario());
        Assert.assertEquals(verificationPendingMobile,
                capturedRecoveryData4.getRemainingSetIds());
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

    @DataProvider(name = "recoveryScenariosDataProvider")
    public Object[][] getForcedPasswordResetDataProvider() {

        return new Object[][] {
                {RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE},
                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE}
        };
    }

    @Test(dataProvider = "recoveryScenariosDataProvider")
    public void testResendConfirmationCodeErrorScenarios(RecoveryScenarios recoveryScenario) throws Exception {

        String verificationPendingMobile = "0777897621";
        String oldCode = "dummy-code";
        String newCode = "new-code";
        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                recoveryScenario, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobile);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                recoveryScenario)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockUtilsErrors();

        // Case 1: Null user.
        try {
            resendConfirmationManager.resendConfirmationCode(null,
                    recoveryScenario.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 2: Empty Recovery scenario.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    "",
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 3: Empty Recovery step.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    recoveryScenario.toString(),
                    "",
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, properties);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 4: Empty Notification type.
        try {
            resendConfirmationManager.resendConfirmationCode(user,
                    recoveryScenario.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    "", properties);
            fail();
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

    @Test (description = "Test resend confirmation code for ASK_PASSWORD scenario when recovery data is missing " +
            "and the user is in pending ask password state.")
    public void testResendConfirmationCodeAskPasswordWhenNoRecoveryData() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};
        String newCode = "new-code";

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD)).thenReturn(null);

        mockedUtils.when(() -> Utils.getAccountStateForUserNameWithoutUserDomain(user))
                .thenReturn(IdentityRecoveryConstants.PENDING_ASK_PASSWORD);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN)).thenReturn("true");
        mockUtilsErrors();

        // Call the method to test.
        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.ASK_PASSWORD.toString(),
                RecoverySteps.UPDATE_PASSWORD.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD, 
                properties);

        // Verify the response.
        assertNotNull(responseBean);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), responseBean.getNotificationChannel());

        // Verify UserRecoveryData was stored properly.
        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        
        assertEquals(capturedRecoveryData.getRecoveryScenario(), RecoveryScenarios.ASK_PASSWORD);
        assertEquals(capturedRecoveryData.getRecoveryStep(), RecoverySteps.UPDATE_PASSWORD);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), capturedRecoveryData.getRemainingSetIds());
        assertEquals(capturedRecoveryData.getSecret(), newCode);

        // Verify notification was triggered.
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        assertEquals(eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE), newCode);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.TEMPLATE_TYPE),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD);
    }

    @Test (description = "Test that confirmation code is not re-sent for ASK_PASSWORD scenario when recovery data " +
            "is missing and the user is not in pending ask password state.")
    public void testResendConfirmationCodeAskPasswordWhenNoRecoveryDataAndUserNotInPendingState() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD)).thenReturn(null);

        mockedUtils.when(() -> Utils.getAccountStateForUserNameWithoutUserDomain(user))
                .thenReturn(IdentityRecoveryConstants.PENDING_SELF_REGISTRATION);
        mockUtilsErrors();

        // Call the method to test.
        try {
            resendConfirmationManager.resendConfirmationCode(
                    user,
                    RecoveryScenarios.ASK_PASSWORD.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD,
                    properties);
            fail("Expected IdentityRecoveryClientException was not thrown.");
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test (description = "Test that confirmation code is not re-sent for ASK_PASSWORD scenario when recovery data " +
            "is missing and no account state claim is present.")
    public void testResendConfirmationCodeAskPasswordWhenNoRecoveryDataAndNoAccountStateClaim() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD)).thenReturn(null);

        mockedUtils.when(() -> Utils.getAccountStateForUserNameWithoutUserDomain(user))
                .thenReturn(StringUtils.EMPTY);
        mockUtilsErrors();

        // Call the method to test.
        try {
            resendConfirmationManager.resendConfirmationCode(
                    user,
                    RecoveryScenarios.ASK_PASSWORD.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD,
                    properties);
            fail("Expected IdentityRecoveryClientException was not thrown.");
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test (description = "Test resend confirmation code for ASK_PASSWORD_VIA_EMAIL_OTP scenario when recovery data " +
            "exists and email verification notification is internally managed.")
    public void testResendConfirmationCodeAskPasswordViaEmailOTP() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};
        String oldCode = "dummy-code";
        String newCode = "new-code";

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP, RecoverySteps.SET_PASSWORD);
        userRecoveryData.setRemainingSetIds(NotificationChannels.EMAIL_CHANNEL.getChannelType());

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.EMAIL_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN)).thenReturn("true");

        // Call the method to test.
        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString(),
                RecoverySteps.SET_PASSWORD.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_EMAIL_OTP,
                properties);

        // Verify the response.
        assertNotNull(responseBean);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), responseBean.getNotificationChannel());

        // Verify UserRecoveryData was stored properly.
        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        
        assertEquals(capturedRecoveryData.getRecoveryScenario(), RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP);
        assertEquals(capturedRecoveryData.getRecoveryStep(), RecoverySteps.SET_PASSWORD);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), capturedRecoveryData.getRemainingSetIds());
        assertEquals(capturedRecoveryData.getSecret(), newCode);

        // Verify notification was triggered.
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        assertEquals(eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE), newCode);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.TEMPLATE_TYPE),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_EMAIL_OTP);
    }

    @Test (description = "Test resend confirmation code for ASK_PASSWORD_VIA_SMS_OTP scenario when recovery data " +
            "exists and SMS notification is internally managed.")
    public void testResendConfirmationCodeAskPasswordViaSMSOTP() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};
        String oldCode = "dummy-code";
        String newCode = "new-code";
        String mobileNumber = "+94771234567";

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, oldCode,
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP, RecoverySteps.SET_PASSWORD);
        userRecoveryData.setRemainingSetIds(NotificationChannels.SMS_CHANNEL.getChannelType());

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP)).thenReturn(userRecoveryData);

        mockedUtils.when(() -> Utils.reIssueExistingConfirmationCode(userRecoveryData,
                NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(false);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN)).thenReturn("true");
        // Mock mobile number retrieval for SMS notification
        mockedUtils.when(() -> Utils.getUserClaim(user, IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM))
                .thenReturn(mobileNumber);
        mockedUtils.when(() -> Utils.resolveEventName(NotificationChannels.SMS_CHANNEL.getChannelType()))
                .thenReturn("SMS_OTP");

        // Call the method to test.
        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString(),
                RecoverySteps.SET_PASSWORD.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP,
                properties);

        // Verify the response.
        assertNotNull(responseBean);
        assertEquals(NotificationChannels.SMS_CHANNEL.getChannelType(), responseBean.getNotificationChannel());

        // Verify UserRecoveryData was stored properly.
        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        
        assertEquals(capturedRecoveryData.getRecoveryScenario(), RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP);
        assertEquals(capturedRecoveryData.getRecoveryStep(), RecoverySteps.SET_PASSWORD);
        assertEquals(NotificationChannels.SMS_CHANNEL.getChannelType(), capturedRecoveryData.getRemainingSetIds());
        assertEquals(capturedRecoveryData.getSecret(), newCode);

        // Verify SMS notification event was triggered.
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        assertEquals(eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE), newCode);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.TEMPLATE_TYPE),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.SEND_TO), mobileNumber);
    }

    @Test (description = "Test resend confirmation code for ASK_PASSWORD_VIA_EMAIL_OTP scenario when recovery data " +
            "is missing and the user is in pending ask password state.")
    public void testResendConfirmationCodeAskPasswordViaEmailOTPWhenNoRecoveryData() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};
        String newCode = "new-code";

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP)).thenReturn(null);

        mockedUtils.when(() -> Utils.getAccountStateForUserNameWithoutUserDomain(user))
                .thenReturn(IdentityRecoveryConstants.PENDING_ASK_PASSWORD);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN)).thenReturn("true");

        // Call the method to test.
        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString(),
                RecoverySteps.SET_PASSWORD.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_EMAIL_OTP,
                properties);

        // Verify the response.
        assertNotNull(responseBean);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), responseBean.getNotificationChannel());

        // Verify UserRecoveryData was stored properly.
        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        
        assertEquals(capturedRecoveryData.getRecoveryScenario(), RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP);
        assertEquals(capturedRecoveryData.getRecoveryStep(), RecoverySteps.SET_PASSWORD);
        assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), capturedRecoveryData.getRemainingSetIds());
        assertEquals(capturedRecoveryData.getSecret(), newCode);

        // Verify notification was triggered.
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        assertEquals(eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE), newCode);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.TEMPLATE_TYPE),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_EMAIL_OTP);
    }

    @Test (description = "Test resend confirmation code for ASK_PASSWORD_VIA_SMS_OTP scenario when recovery data " +
            "is missing and the user is in pending ask password state.")
    public void testResendConfirmationCodeAskPasswordViaSMSOTPWhenNoRecoveryData() throws Exception {

        User user = getUser();
        Property[] properties = new Property[]{new Property("testKey", "testValue")};
        String newCode = "new-code";
        String mobileNumber = "+94771234567";

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP)).thenReturn(null);

        mockedUtils.when(() -> Utils.getAccountStateForUserNameWithoutUserDomain(user))
                .thenReturn(IdentityRecoveryConstants.PENDING_ASK_PASSWORD);
        mockedUtils.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(newCode);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN)).thenReturn("true");
        // Mock mobile number retrieval for SMS notification
        mockedUtils.when(() -> Utils.getUserClaim(user, IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM))
                .thenReturn(mobileNumber);
        mockedUtils.when(() -> Utils.resolveEventName(NotificationChannels.SMS_CHANNEL.getChannelType()))
                .thenReturn("SMS_OTP");

        // Call the method to test.
        NotificationResponseBean responseBean = resendConfirmationManager.resendConfirmationCode(
                user,
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString(),
                RecoverySteps.SET_PASSWORD.toString(),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP,
                properties);

        // Verify the response.
        assertNotNull(responseBean);
        assertEquals(NotificationChannels.SMS_CHANNEL.getChannelType(), responseBean.getNotificationChannel());

        // Verify UserRecoveryData was stored properly.
        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        
        assertEquals(capturedRecoveryData.getRecoveryScenario(), RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP);
        assertEquals(capturedRecoveryData.getRecoveryStep(), RecoverySteps.SET_PASSWORD);
        assertEquals(NotificationChannels.SMS_CHANNEL.getChannelType(), capturedRecoveryData.getRemainingSetIds());
        assertEquals(capturedRecoveryData.getSecret(), newCode);

        // Verify SMS notification event was triggered.
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(eventCaptor.capture());
        Event capturedEvent = eventCaptor.getValue();
        Map<String, Object> eventProperties = capturedEvent.getEventProperties();
        assertEquals(eventProperties.get(IdentityRecoveryConstants.CONFIRMATION_CODE), newCode);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.TEMPLATE_TYPE),
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP);
        assertEquals(eventProperties.get(IdentityRecoveryConstants.SEND_TO), mobileNumber);
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
