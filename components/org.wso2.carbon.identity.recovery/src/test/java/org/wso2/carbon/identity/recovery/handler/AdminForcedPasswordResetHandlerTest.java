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
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * Unit tests for AdminForcedPasswordResetHandler.
 */
public class AdminForcedPasswordResetHandlerTest {

    private static final String TEST_USERNAME = "testUser";
    private static final String MASKED_TEST_USERNAME = "t******r";
    private static final String TEST_TENANT_DOMAIN = "carbon.super";
    private static final String TEST_USER_STORE_DOMAIN = "PRIMARY";
    private static final int TEST_TENANT_ID = 1;
    private static final String TEST_DUMMY_CODE = "dummy-code";
    private static final RecoverySteps UPDATE_PASSWORD_RECOVERY_STEP = RecoverySteps.UPDATE_PASSWORD;
    private static final String EXPECTED_ACCOUNT_LOCK_ERROR_MESSAGE_PATTERN =
            "Error while handling account unlock on admin password update for user: t\\*\\*\\*\\*\\*\\*r";

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private ClaimManager claimManager;

    @Mock
    private RealmService realmService;

    @Mock
    private UserRealm userRealm;

    @Mock
    private TenantManager tenantManager;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private IdentityRecoveryServiceDataHolder serviceDataHolder;

    private AdminForcedPasswordResetHandler adminForcedPasswordResetHandler;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() throws Exception {

        MockitoAnnotations.openMocks(this);
        adminForcedPasswordResetHandler = new AdminForcedPasswordResetHandler();

        mockedUtils = mockStatic(Utils.class);
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);

        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(TEST_TENANT_ID);

        when(serviceDataHolder.getRealmService()).thenReturn(realmService);
        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(TEST_TENANT_DOMAIN)).thenReturn(TEST_TENANT_ID);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn(TEST_USER_STORE_DOMAIN);
        mockedUtils.when(() -> Utils.maskIfRequired(any())).thenReturn(MASKED_TEST_USERNAME);

    }

    @AfterMethod
    public void tearDown() {

        mockedUtils.close();
        mockedJDBCRecoveryDataStore.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedIdentityTenantUtil.close();
    }

    @Test(dataProvider = "claimUpdateDataProvider")
    public void testHandleClaimUpdate(boolean isEmailOtpEnabled,
                                      boolean isEmailLinkEnabled, boolean isSmsOtpEnabled,
                                      String expectedTemplateName, String expectedRecoveryScenario)
            throws IdentityEventException {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, TEST_USER_STORE_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM, "true");
        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);

        if (isEmailOtpEnabled) {
            mockedUtils.when(() -> Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_EMAIL_OTP,
                    TEST_TENANT_DOMAIN)).thenReturn(String.valueOf(true));
        }
        if (isEmailLinkEnabled) {
            mockedUtils.when(() -> Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                    TEST_TENANT_DOMAIN)).thenReturn(String.valueOf(true));
        }
        if (isSmsOtpEnabled) {
            mockedUtils.when(() -> Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_SMS_OTP,
                    TEST_TENANT_DOMAIN)).thenReturn(String.valueOf(true));
        }
        adminForcedPasswordResetHandler.handleClaimUpdate(eventProperties, userStoreManager);
        ArgumentCaptor<Event> eventCaptor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService, times(1)).handleEvent(eventCaptor.capture());
        Event event = eventCaptor.getValue();
        if (!isSmsOtpEnabled) {
            assertEquals(event.getEventName(), IdentityEventConstants.Event.TRIGGER_NOTIFICATION,
                    "Event does not match.");
        }

        Map<String, Object> capturedEventProperties = event.getEventProperties();
        assertEquals(capturedEventProperties.get("user-name"), TEST_USERNAME);
        assertEquals(capturedEventProperties.get("tenant-domain"), TEST_TENANT_DOMAIN);
        assertEquals(capturedEventProperties.get("userstore-domain"), TEST_USER_STORE_DOMAIN);

        assertEquals(capturedEventProperties.get("TEMPLATE_TYPE"), expectedTemplateName);
        if (!isSmsOtpEnabled) {
            assertEquals(capturedEventProperties.get("RECOVERY_SCENARIO"), expectedRecoveryScenario);
        }

    }

    @DataProvider
    public Object[][] claimUpdateDataProvider() {

        return new Object[][]{
                // isEmailOtpEnabled, isEmailLinkEnabled, isSmsOtpEnabled, expectedTemplate,
                // expectedRecoveryScenario
                {true, false, false, IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP,
                        RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.toString()},
                {false, true, false, IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET,
                        RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.toString()},
                {false, false, true, IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_SMS_OTP,
                        RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.toString()},
        };
    }

    @Test
    public void testGetNames() {

        assertEquals(adminForcedPasswordResetHandler.getName(), "adminForcedPasswordReset");
        assertEquals(adminForcedPasswordResetHandler.getFriendlyName(), "Admin Forced Password Reset");
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where no recovery data exists" +
            "and account is in unlocked state.")
    public void testHandlePostUpdateCredentialByAdminWithUnlockedAccount() throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, true);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(null);
        
        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were not updated since account is already unlocked.
        verify(userStoreManager, never()).setUserClaimValues(eq(TEST_USERNAME), any(), any());
        // Verify that invalidation was not called.
        verify(userRecoveryDataStore, never()).invalidate(any(User.class));
    }

    @DataProvider(name = "askPasswordDataProvider")
    public Object[][] getAskPasswordDataProvider() {

        return new Object[][] {
                {null, true},
                {null, false},
                {mockRecoveryData(RecoveryScenarios.ASK_PASSWORD), true},
                {mockRecoveryData(RecoveryScenarios.ASK_PASSWORD), false}
        };
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where the account is locked" +
            "due to pending ask password.", dataProvider = "askPasswordDataProvider")
    public void testHandlePostUpdateCredentialByAdminWithPendingAskPasswordLockedAccount
            (UserRecoveryData recoveryData, boolean accountStateExists) throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                IdentityMgtConstants.LockedReason.PENDING_ASK_PASSWORD.toString());

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, accountStateExists);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(recoveryData);

        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were updated.
        verifyClaimUpdates(accountStateExists);
        if (recoveryData == null) {
            // Verify that invalidation was not called.
            verify(userRecoveryDataStore, never()).invalidate(any(User.class));
        } else {
            // Verify that invalidation was called.
            verify(userRecoveryDataStore, times(1)).invalidate(any(User.class));
        }
    }

    @DataProvider(name = "forcedPasswordRestDataProvider")
    public Object[][] getForcedPasswordResetDataProvider() {

        return new Object[][] {
                {null, true},
                {null, false},
                {mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK), true},
                {mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK), false},
                {mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP), true},
                {mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP), false}
        };
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where the account is locked" +
            "due to pending forced password reset.", dataProvider = "forcedPasswordRestDataProvider")
    public void testHandlePostUpdateCredentialByAdminWithPendingForcedPasswordResetLockedAccount
            (UserRecoveryData recoveryData, boolean accountStateExists) throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                IdentityMgtConstants.LockedReason.PENDING_ADMIN_FORCED_USER_PASSWORD_RESET.toString());

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, accountStateExists);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(recoveryData);

        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were updated.
        verifyClaimUpdates(accountStateExists);
        if (recoveryData == null) {
            // Verify that invalidation was not called.
            verify(userRecoveryDataStore, never()).invalidate(any(User.class));
        } else {
            // Verify that invalidation was called.
            verify(userRecoveryDataStore, times(1)).invalidate(any(User.class));
        }
    }

    @DataProvider(name = "accountStateExistsDataProvider")
    public Object[][] getAccountStateExistsDataProvider() {

        return new Object[][] {
                {true},
                {false},
        };
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where the account is locked" +
            "due to maximum attempts exceed.", dataProvider = "accountStateExistsDataProvider")
    public void testHandlePostUpdateCredentialByAdminWithPendingForcedPasswordResetLockedAccount
            (boolean accountStateExists) throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                IdentityMgtConstants.LockedReason.MAX_ATTEMPTS_EXCEEDED.toString());

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, accountStateExists);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(null);

        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were not updated since account is already unlocked.
        verify(userStoreManager, never()).setUserClaimValues(eq(TEST_USERNAME), any(), any());
        // Verify that invalidation was not called.
        verify(userRecoveryDataStore, never()).invalidate(any(User.class));
    }

    @DataProvider(name = "askPasswordRecoveryDataProvider")
    public Object[][] getAskPasswordRecoveryDataProvider() {

        return new Object[][] {
                {null},
                {mockRecoveryData(RecoveryScenarios.ASK_PASSWORD)},    
        };
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where the account is unlocked " +
            "but its state indicates pending ask password",
            dataProvider = "askPasswordRecoveryDataProvider")
    public void testHandlePostUpdateCredentialByAdminWithPendingAskPasswordUnlockedAccount(
            UserRecoveryData recoveryData) throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                IdentityRecoveryConstants.PENDING_ASK_PASSWORD);

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, true);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(recoveryData);

        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were updated to unlock the account and clear the state.
        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME),
                claimsCaptor.capture(), eq(null));

        Map<String, String> updatedClaims = claimsCaptor.getValue();

        assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                    IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED, "Account state should be set to UNLOCKED");
        assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM),
                Boolean.FALSE.toString(), "Account locked claim should be set to false");
        
        if (recoveryData == null) {
            // Verify that invalidation was not called.
            verify(userRecoveryDataStore, never()).invalidate(any(User.class));
        } else {
            // Verify that invalidation was called.
            verify(userRecoveryDataStore, times(1)).invalidate(any(User.class));
        }
    }

    @DataProvider(name = "accountStateDataProvider")
    public Object[][] getAccountStateDataProvider() {

        return new Object[][] {
                {"PENDING_SR"},
                {"PENDING_LR"},
                {"DISABLED"},
                {"UNLOCKED"}
        };
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event where the account is unlocked " +
            "and the account state is not pending ask password.",
            dataProvider = "accountStateDataProvider")
    public void testHandlePostUpdateCredentialByAdminWithUnlockedAccountAndOtherAccountStates(String accountState)
            throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI, accountState);

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockUserClaims(claims, true);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(null);

        // Test the method.
        adminForcedPasswordResetHandler.handleEvent(event);

        // Verify claims were not updated since account is already unlocked.
        verify(userStoreManager, never()).setUserClaimValues(eq(TEST_USERNAME), any(), any());
        // Verify that invalidation was not called.
        verify(userRecoveryDataStore, never()).invalidate(any(User.class));
    }

    @Test(description = "Test handleEvent() with POST_UPDATE_CREDENTIAL_BY_ADMIN event when getUserClaimValues " +
            "throws exception",
            expectedExceptions = {IdentityEventException.class},
            expectedExceptionsMessageRegExp = EXPECTED_ACCOUNT_LOCK_ERROR_MESSAGE_PATTERN)
    public void testHandlePostUpdateCredentialByAdminWithUserStoreException() throws Exception {

        Event event = createEvent(IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN);
        mockedUtils.when(() -> Utils.isAccountStateClaimExisting(TEST_TENANT_DOMAIN)).thenReturn(true);

        UserStoreException userStoreException = new UserStoreException("Error retrieving user claims");
        when(userStoreManager.getUserClaimValues(eq(TEST_USERNAME), any(), any())).thenThrow(userStoreException);
        
        // Test the method, which should throw IdentityEventException.
        adminForcedPasswordResetHandler.handleEvent(event);
    }

    private Event createEvent(String eventName) {
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, TEST_USER_STORE_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        return new Event(eventName, eventProperties);
    }

    private void mockUserClaims(Map<String, String> claims, boolean accountStateExists) throws UserStoreException {

        mockedUtils.when(() -> Utils.isAccountStateClaimExisting(TEST_TENANT_DOMAIN)).thenReturn(accountStateExists);
        when(userStoreManager.getUserClaimValues(eq(TEST_USERNAME), any(), any())).thenReturn(claims);
    }

    private void verifyClaimUpdates(boolean accountStateExists) throws UserStoreException {

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME),
                claimsCaptor.capture(), eq(null));

        Map<String, String> updatedClaims = claimsCaptor.getValue();
        if (accountStateExists) {
            assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                    IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED, "Account state should be set to UNLOCKED");
        } else {
            assertNull(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                    "Account state claim should not be set when not available");
        }
        assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM),
                Boolean.FALSE.toString(), "Account locked claim should be set to false");
        assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM),
                StringUtils.EMPTY, "Account locked reason should be empty");
    }

    private UserRecoveryData mockRecoveryData(RecoveryScenarios recoveryScenario) {
        
        User user = new User();
        user.setUserName(TEST_USERNAME);
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        user.setUserStoreDomain(TEST_USER_STORE_DOMAIN);

        return new UserRecoveryData(user, TEST_DUMMY_CODE, recoveryScenario, UPDATE_PASSWORD_RECOVERY_STEP);
    }
}
