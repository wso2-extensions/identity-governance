/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.password;

import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.organization.management.service.util.OrganizationManagementUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.identity.event.event.Event;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.PrivilegedActionException;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL;

public class NotificationPasswordRecoveryManagerTest {

    private static final String NOTIFICATION_CHANNEL_EMAIL = "EMAIL";
    private static final String OTP = "PnRuLm";
    private static final String FLOW_CONFIRMATION_CODE = "4273129d-344a-423c-a889-e5d36ea9d960";
    private static final String TENANT_DOMAIN = "carbon.super";
    public static final String VERIFIED_USER_PROPERTY_KEY = "verifiedUser";
    public static final String PASSWORD_RESET_EMAIL_TEMPLATE_NAME = "passwordreset";
    public static final String PASSWORD_RESET_EMAIL_OTP_TEMPLATE_NAME = "passwordresetotp";
    // Event properties constants.
    public static final String TEMPLATE_TYPE_KEY = "TEMPLATE_TYPE";
    public static final String USER_STORE_DOMAIN_KEY = "userstore-domain";
    public static final String TENANT_DOMAIN_KEY = "tenant-domain";
    private static final String USER_STORE_DOMAIN = "PRIMARY";
    private static final String TRUE_STRING = "true";
    private static final int TENANT_ID = 1234;

    private static final String PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED = "USER-ACTION-PRE-UPDATE-PASSWORD-60001";
    private static final String EXTENSION_ERROR_MESSAGE = "invalid_password";
    private static final String EXTENSION_ERROR_DESCRIPTION = "Invalid password format";

    @Mock
    private IdentityGovernanceService identityGovernanceService;

    @Mock
    UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    IdentityEventService identityEventService;

    @Mock
    RealmService realmService;

    @Mock
    UserRealm userRealm;

    @Mock
    UserStoreManager userStoreManager;

    private MockedStatic<IdentityTenantUtil> identityTenantUtilMockedStatic;

    private MockedStatic<Utils> utilsMockedStatic;

    private MockedStatic<OrganizationManagementUtil> organizationManagementUtilMockedStatic;

    private MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMockedStatic;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
        IdentityRecoveryServiceDataHolder.getInstance().setRealmService(realmService);

    }

    @DataProvider(name = "generateNewConfirmationCodeForOTPFlowData")
    public Object[][] generateNewConfirmationCodeForOTPFlowData() {
        return new Object[][] {
                {"true", "true", OTP},
                {"true", "false", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP},
                {"false", "true", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP},
                {"false", "false", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP}
        };
    }

    @Test(dataProvider = "generateNewConfirmationCodeForOTPFlowData")
    public void testGenerateNewConfirmationCodeForOTPFlow(String sendOTPInEmail, String sendOnlyOtpAsConfirmationCode,
                                                          String confirmationCode) throws Exception {

        User user = new User();
        user.setTenantDomain(TENANT_DOMAIN);

        Property property1 = new Property();
        property1.setName(PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL);
        property1.setValue(sendOTPInEmail);
        Property[] properties = new Property[1];
        properties[0] = property1;

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, OTP, RecoveryScenarios
                .NOTIFICATION_BASED_PW_RECOVERY);
        userRecoveryData.setRecoveryFlowId(FLOW_CONFIRMATION_CODE);

        when(identityGovernanceService.getConfiguration(new String[]{PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL},
                TENANT_DOMAIN)).thenReturn(properties);

        try (MockedStatic<IdentityUtil> identityUtil = Mockito.mockStatic(IdentityUtil.class);
             MockedStatic<Utils> mockedUtils = Mockito.mockStatic(Utils.class, Mockito.CALLS_REAL_METHODS);
             MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore =
                     Mockito.mockStatic(JDBCRecoveryDataStore.class)) {

            identityUtil.when(() -> IdentityUtil.getProperty(
                            IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE))
                    .thenReturn(sendOnlyOtpAsConfirmationCode);
            mockedUtils.when(() -> Utils.generateSecretKey(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(),
                    ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).thenReturn(OTP);

            mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
            when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(ArgumentMatchers.any())).
                    thenReturn(userRecoveryData);

            Method generateNewConfirmationCodeMethod = NotificationPasswordRecoveryManager.class.getDeclaredMethod(
                    "generateNewConfirmationCode", User.class, String.class);
            generateNewConfirmationCodeMethod.setAccessible(true);

            NotificationPasswordRecoveryManager notificationPasswordRecoveryManager =
                    NotificationPasswordRecoveryManager.getInstance();
            UserRecoveryData recoveryData = (UserRecoveryData) generateNewConfirmationCodeMethod.invoke(
                    notificationPasswordRecoveryManager, user, NOTIFICATION_CHANNEL_EMAIL);

            assertEquals(recoveryData.getSecret(), confirmationCode);
        }

    }

    @DataProvider(name = "generateRecoveryNotificationEmailTemplateConfigs")
    public Object[][] generateRecoveryNotificationEmailTemplateConfigs() {

        return new Object[][]{
                {true, PASSWORD_RESET_EMAIL_OTP_TEMPLATE_NAME},
                {false, PASSWORD_RESET_EMAIL_TEMPLATE_NAME}
        };
    }

    @Test(dataProvider = "generateRecoveryNotificationEmailTemplateConfigs")
    public void testSendRecoveryNotificationEmailTemplate(boolean isEMailOtpEnabled, String expectedTemplate)
            throws IdentityRecoveryException, IdentityEventException, UserStoreException {

        // Creating the static mocks.
        identityTenantUtilMockedStatic = mockStatic(IdentityTenantUtil.class);
        utilsMockedStatic = mockStatic(Utils.class);
        organizationManagementUtilMockedStatic = mockStatic(OrganizationManagementUtil.class);
        jdbcRecoveryDataStoreMockedStatic = mockStatic(JDBCRecoveryDataStore.class);

        User user = new User();
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain(USER_STORE_DOMAIN);

        org.wso2.carbon.identity.recovery.model.Property property1 =
                new org.wso2.carbon.identity.recovery.model.Property();
        property1.setKey(PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL);
        property1.setValue(TRUE_STRING);

        org.wso2.carbon.identity.recovery.model.Property property2 =
                new org.wso2.carbon.identity.recovery.model.Property();
        property2.setKey(VERIFIED_USER_PROPERTY_KEY);
        property2.setValue(TRUE_STRING);

        org.wso2.carbon.identity.recovery.model.Property[] properties =
                new org.wso2.carbon.identity.recovery.model.Property[2];
        properties[0] = property1;
        properties[1] = property2;

        doNothing().when(identityEventService).handleEvent(any());
        identityTenantUtilMockedStatic.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(NOTIFICATION_BASED_PW_RECOVERY, TENANT_DOMAIN)).
                thenReturn(TRUE_STRING);
        if (isEMailOtpEnabled) {
            utilsMockedStatic.when(() -> Utils.isPasswordRecoveryEmailOtpEnabled(TENANT_DOMAIN)).thenReturn(true);
        }

        organizationManagementUtilMockedStatic.when(() -> OrganizationManagementUtil.isOrganization(TENANT_DOMAIN)).
                thenReturn(false);

        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager =
                NotificationPasswordRecoveryManager.getInstance();

        notificationPasswordRecoveryManager.sendRecoveryNotification(user,
                null, true, properties);

        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService, times(3)).handleEvent(eventCaptor.capture());

        List<Event> capturedEvents = eventCaptor.getAllValues();

        Event recoveryEvent = capturedEvents.get(1);
        Map<String, Object> eventProperties = recoveryEvent.getEventProperties();

        assertEquals(eventProperties.get(TEMPLATE_TYPE_KEY), expectedTemplate);
        assertEquals(eventProperties.get(USER_STORE_DOMAIN_KEY), USER_STORE_DOMAIN);
        assertEquals(eventProperties.get(TENANT_DOMAIN_KEY), TENANT_DOMAIN);

        // Tear down the static mocks.
        identityTenantUtilMockedStatic.close();
        utilsMockedStatic.close();
        organizationManagementUtilMockedStatic.close();
        jdbcRecoveryDataStoreMockedStatic.close();

    }

    @Test
    public void testExceptionThrownAtPreUpdatePasswordValidationFailure() throws Exception {

        NotificationPasswordRecoveryManager manager = NotificationPasswordRecoveryManager.getInstance();

        User user = new User();
        user.setTenantDomain("carbon.super");

        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getUser()).thenReturn(user);

        try (MockedStatic<IdentityTenantUtil> tenantUtilMock = mockStatic(IdentityTenantUtil.class)) {
            tenantUtilMock.when(() -> IdentityTenantUtil.getTenantId("carbon.super")).thenReturn(-1234);

            RealmService realmService = mock(RealmService.class);
            UserRealm userRealm = mock(UserRealm.class);
            UserStoreManager userStoreManager = mock(UserStoreManager.class);

            when(realmService.getTenantUserRealm(-1234)).thenReturn(userRealm);
            IdentityRecoveryServiceDataHolder.getInstance().setRealmService(realmService);

            when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
            doThrow(buildExceptionForPreUpdatePasswordActionFailure()).when(userStoreManager)
                    .updateCredentialByAdmin(anyString(), anyString());

            Method method = NotificationPasswordRecoveryManager.class.getDeclaredMethod("updateNewPassword", User.class,
                    String.class, String.class, UserRecoveryData.class, boolean.class);
            method.setAccessible(true);

            try {
                method.invoke(manager, user, "Password@123!", "user1", recoveryData, false);
                fail("Expected an IdentityRecoveryClientException was not thrown");
            } catch (InvocationTargetException ex) {
                Throwable actual = ex.getCause();

                assertTrue(actual instanceof IdentityRecoveryClientException);
                IdentityRecoveryClientException identityRecoveryClientException =
                        (IdentityRecoveryClientException) actual;

                assertEquals(identityRecoveryClientException.getErrorCode(),
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_FAILURE.getCode());
                assertEquals(identityRecoveryClientException.getMessage(), EXTENSION_ERROR_MESSAGE);
                assertEquals(identityRecoveryClientException.getDescription(), EXTENSION_ERROR_DESCRIPTION);
            }
        }
    }

    @Test
    public void testSendRecoveryNotification_AccountLocked_HideUserExistenceEnabled() throws Exception {

        User user = createTestUser();
        // Added JDBCRecoveryDataStore to the try-with-resources
        try (MockedStatic<IdentityTenantUtil> tenantUtilMock = mockStatic(IdentityTenantUtil.class);
             MockedStatic<IdentityUtil> identityUtilMock = mockStatic(IdentityUtil.class);
             MockedStatic<Utils> utilsMock = mockStatic(Utils.class);
             MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMock = mockStatic(JDBCRecoveryDataStore.class);
             MockedStatic<OrganizationManagementUtil> organizationManagementUtilMock = mockStatic(OrganizationManagementUtil.class)) {

            // Mock JDBC DataStore instance
            jdbcRecoveryDataStoreMock.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
            organizationManagementUtilMock.when(() -> OrganizationManagementUtil.isOrganization(anyString())).thenReturn(false);

            setupCommonMocks(user, tenantUtilMock, identityUtilMock, utilsMock);

            // Mock lock and hidden status
            utilsMock.when(() -> Utils.isAccountLocked(any(User.class))).thenReturn(true);
            utilsMock.when(Utils::isUserExistenceHidden).thenReturn(true);

            NotificationPasswordRecoveryManager notificationPasswordRecoveryManager =
                    NotificationPasswordRecoveryManager.getInstance();
            NotificationResponseBean result = notificationPasswordRecoveryManager.sendRecoveryNotification(
                    user, NOTIFICATION_CHANNEL_EMAIL, true, new org.wso2.carbon.identity.recovery.model.Property[0]);

            assertNotNull(result, "Should return NotificationResponseBean even when account is locked if " +
                    "existence is hidden.");
        }
    }

    @Test
    public void testSendRecoveryNotification_AccountLocked_HideUserExistenceDisabled() throws Exception {

        User user = createTestUser();

        try (MockedStatic<IdentityTenantUtil> tenantUtilMock = mockStatic(IdentityTenantUtil.class);
             MockedStatic<IdentityUtil> identityUtilMock = mockStatic(IdentityUtil.class);
             MockedStatic<Utils> utilsMock = mockStatic(Utils.class);
             MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMock = mockStatic(JDBCRecoveryDataStore.class);
             MockedStatic<OrganizationManagementUtil> organizationManagementUtilMock = mockStatic(OrganizationManagementUtil.class)) {

            // 1. Mock DataStore
            jdbcRecoveryDataStoreMock.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
            organizationManagementUtilMock.when(() -> OrganizationManagementUtil.isOrganization(anyString())).thenReturn(false);

            // 2. Setup Common Mocks (Realm, etc.)
            setupCommonMocks(user, tenantUtilMock, identityUtilMock, utilsMock);

            // 3. Mock Account Status: Locked = TRUE, Hidden = FALSE
            utilsMock.when(() -> Utils.isAccountLocked(any(User.class))).thenReturn(true);
            utilsMock.when(Utils::isUserExistenceHidden).thenReturn(false);

            // 4. FIX: Mock IdentityUtil to return TRUE for the config (Handle both 1-arg and 2-arg calls)
            identityUtilMock.when(() -> IdentityUtil.getProperty(
                    IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_ACCOUNT_STATUS)).thenReturn(TRUE_STRING);
            identityUtilMock.when(() -> IdentityUtil.getProperty(
                    ArgumentMatchers.eq(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_ACCOUNT_STATUS)
            )).thenReturn(TRUE_STRING);

            // 5. FIX: Stub handleClientException to return a real exception object (since Utils is a mock)
            IdentityRecoveryClientException mockException = new IdentityRecoveryClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT.getCode(),
                    "Account Locked");
            utilsMock.when(() -> Utils.handleClientException(
                    ArgumentMatchers.eq(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT),
                    anyString())).thenReturn(mockException);

            NotificationPasswordRecoveryManager notificationPasswordRecoveryManager =
                    NotificationPasswordRecoveryManager.getInstance();

            try {
                notificationPasswordRecoveryManager.sendRecoveryNotification(user, NOTIFICATION_CHANNEL_EMAIL,
                        true, new org.wso2.carbon.identity.recovery.model.Property[0]);
                fail("Expected IdentityRecoveryClientException was not thrown.");
            } catch (IdentityRecoveryClientException e) {
                assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT.getCode());
            }
        }
    }

    private void setupCommonMocks(User user, MockedStatic<IdentityTenantUtil> tenantUtilMock,
                                  MockedStatic<IdentityUtil> identityUtilMock,
                                  MockedStatic<Utils> utilsMock) throws Exception {

        tenantUtilMock.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(TENANT_ID);
        identityUtilMock.when(IdentityUtil::getPrimaryDomainName).thenReturn(USER_STORE_DOMAIN);
        identityUtilMock.when(() -> IdentityUtil.addDomainToName(anyString(), anyString())).thenReturn(USER_STORE_DOMAIN + "/" + "testUser");

        utilsMock.when(() -> Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE_STRING);
        utilsMock.when(() -> Utils.isAccessUrlAvailable(any())).thenReturn(true);
        utilsMock.when(() -> Utils.isAccountDisabled(any(User.class))).thenReturn(false);
        utilsMock.when(() -> Utils.getUserClaim(any(User.class), anyString())).thenReturn("test@gmail.com");

        // --- FIX: Stub the RealmService and UserRealm chain ---
        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.isExistingUser(anyString())).thenReturn(true);
    }

    private User createTestUser() {
        User user = new User();
        user.setUserName("testUser");
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain(USER_STORE_DOMAIN);
        return user;
    }

    private UserStoreException buildExceptionForPreUpdatePasswordActionFailure() {

        UserActionExecutionClientException userActionExecutionClientException =
                new UserActionExecutionClientException(PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED,
                        EXTENSION_ERROR_MESSAGE, EXTENSION_ERROR_DESCRIPTION);
        InvocationTargetException invocationTargetException =
                new InvocationTargetException(userActionExecutionClientException);
        PrivilegedActionException privilegedActionException = new PrivilegedActionException(invocationTargetException);
        return new UserStoreClientException(privilegedActionException);
    }
}
