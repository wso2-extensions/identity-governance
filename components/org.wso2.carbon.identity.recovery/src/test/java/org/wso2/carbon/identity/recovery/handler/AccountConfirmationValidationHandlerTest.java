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

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;

/**
 * Unit tests for AccountConfirmationValidationHandler
 */
public class AccountConfirmationValidationHandlerTest {

    private static final String USERNAME = "testuser";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String DOMAIN_NAME = "PRIMARY";
    private static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private MessageContext<?,?> messageContext;

    private MockedStatic<Utils> utilsMockedStatic;
    private MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMockedStatic;
    private MockedStatic<UserSelfRegistrationManager> userSelfRegistrationManagerMockedStatic;
    private MockedStatic<IdentityUtil> identityUtilMockedStatic;

    private AccountConfirmationValidationHandler handler;

    @BeforeMethod
    public void init() {
        openMocks(this);
        handler = new AccountConfirmationValidationHandler();
        
        utilsMockedStatic = mockStatic(Utils.class);
        jdbcRecoveryDataStoreMockedStatic = mockStatic(JDBCRecoveryDataStore.class);
        userSelfRegistrationManagerMockedStatic = mockStatic(UserSelfRegistrationManager.class);
        identityUtilMockedStatic = mockStatic(IdentityUtil.class);
    }

    @AfterMethod
    public void tearDown() {
        utilsMockedStatic.close();
        jdbcRecoveryDataStoreMockedStatic.close();
        userSelfRegistrationManagerMockedStatic.close();
        identityUtilMockedStatic.close();
    }

    @Test
    public void testGetName() {
        assertEquals(handler.getName(), "accountConfirmationValidation");
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals(handler.getFriendlyName(), "Account Confirmation Validation");
    }

    @Test
    public void testGetPriority() {
        assertEquals(handler.getPriority(messageContext), 50);
    }

    @Test
    public void testHandleEvent_SelfSignupDisabled_EmailVerificationDisabled() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, TENANT_DOMAIN))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, TENANT_DOMAIN))
                .thenReturn("false");

        // When
        handler.handleEvent(event);

        // Then - should return early without processing
        verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
    }

    @Test
    public void testHandleEvent_AccountUnlocked() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(false);

        // When
        handler.handleEvent(event);

        // Then - should return early since account is unlocked
        // No exceptions should be thrown
    }

    @Test
    public void testHandleEvent_AccountLocked_UserConfirmed_SelfSignup() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        setupUserConfirmed(true);

        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", RecoveryScenarios.SELF_SIGN_UP);

        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                .thenReturn(recoveryData);

        // When
        handler.handleEvent(event);

        // Then - should not throw exception since user is confirmed
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*not confirmed yet.*")
    public void testHandleEvent_AccountLocked_UserNotConfirmed_SelfSignup() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        setupUserConfirmed(false);

        // When
        handler.handleEvent(event);
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*email is not confirmed yet.*")
    public void testHandleEvent_AccountLocked_UserNotConfirmed_EmailVerification() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(false);

        // When
        handler.handleEvent(event);
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*email is not verified yet.*")
    public void testHandleEvent_EmailVerificationScenario() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(true); // User is confirmed but in email verification scenario
        setupEmailVerificationScenario();

        // When
        handler.handleEvent(event);
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Invalid login attempt.*")
    public void testHandleEvent_InvalidCredentialsScenario_SelfRegisteredUser() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, false);
        
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        setupSelfRegisteredUser();

        // When
        handler.handleEvent(event);
    }

    @Test
    public void testHandleEvent_UserDoesNotExist_AuthPolicyEnabled() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        
        when(userStoreManager.isExistingUser(USERNAME)).thenReturn(false);
        identityUtilMockedStatic.when(() -> IdentityUtil.getProperty("AuthenticationPolicy.CheckAccountExist"))
                .thenReturn("true");

        // When
        handler.handleEvent(event);

        // Then
        verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
    }

    @Test
    public void testHandleEvent_NonPostAuthenticationEvent() throws Exception {
        // Given
        Event event = new Event("SOME_OTHER_EVENT", createEventProperties());
        setupUserStoreManager();
        setupEnabledFeatures(true, false);

        // When
        handler.handleEvent(event);

        // Then - should return early for non-POST_AUTHENTICATION events
        verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
    }

    @Test
    public void testIsUserEmailVerificationScenario_EmailVerification() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(true); // User confirmed but email verification scenario
        
        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", RecoveryScenarios.EMAIL_VERIFICATION);
        
        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                .thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                .thenReturn(recoveryData);

        // When & Then - this should throw an exception since the email is not verified
        try {
            handler.handleEvent(event);
        } catch (IdentityEventException e) {
            // Expected exception for email not verified scenario
        }
        
        // Verify the recovery data store was called
        verify(userRecoveryDataStore, times(1)).loadWithoutCodeExpiryValidation(any(User.class));
    }

    @Test
    public void testIsUserEmailVerificationScenario_SelfSignup() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, false);
        
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        
        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", RecoveryScenarios.SELF_SIGN_UP);
        
        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                .thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                .thenReturn(recoveryData);

        // When & Then - this should throw an exception for invalid credentials
        try {
            handler.handleEvent(event);
        } catch (IdentityEventException e) {
            // Expected exception for invalid credentials scenario
        }
        
        // Verify the recovery data store was called
        verify(userRecoveryDataStore, times(1)).loadWithoutCodeExpiryValidation(any(User.class));
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Error while retrieving account lock claim value.*")
    public void testHandleEvent_UserStoreException() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        
        when(userStoreManager.getUserClaimValues(eq(USERNAME), any(String[].class), anyString()))
                .thenThrow(new UserStoreException("UserStore error"));

        // When
        handler.handleEvent(event);
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Error occurred while checking whether this user is confirmed or not.*")
    public void testHandleEvent_IdentityRecoveryException() throws Exception {
        // Given
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);
        
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        
        userSelfRegistrationManagerMockedStatic.when(UserSelfRegistrationManager::getInstance)
                .thenReturn(userSelfRegistrationManager);
        when(userSelfRegistrationManager.isUserConfirmed(any(User.class)))
                .thenThrow(new IdentityRecoveryException("Recovery error"));

        // When
        handler.handleEvent(event);
    }

    // Helper methods

    private Event createPostAuthenticationEvent() {
        return new Event(IdentityEventConstants.Event.POST_AUTHENTICATION, createEventProperties());
    }

    private Map<String, Object> createEventProperties() {
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);
        return eventProperties;
    }

    private User createUser() {
        User user = new User();
        user.setUserName(USERNAME);
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain(DOMAIN_NAME);
        return user;
    }

    private void setupUserStoreManager() throws UserStoreException {
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn(DOMAIN_NAME);
        when(userStoreManager.isExistingUser(USERNAME)).thenReturn(true);
        identityUtilMockedStatic.when(() -> IdentityUtil.getProperty("AuthenticationPolicy.CheckAccountExist"))
                .thenReturn(null);
    }

    private void setupEnabledFeatures(boolean selfSignup, boolean emailVerification) {
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, TENANT_DOMAIN))
                .thenReturn(String.valueOf(selfSignup));
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, TENANT_DOMAIN))
                .thenReturn(String.valueOf(emailVerification));
    }

    private void setupAccountLocked(boolean isLocked) throws UserStoreException {
        Map<String, String> claims = new HashMap<>();
        claims.put(ACCOUNT_LOCKED_CLAIM, String.valueOf(isLocked));
        when(userStoreManager.getUserClaimValues(eq(USERNAME), any(String[].class), 
                eq(UserCoreConstants.DEFAULT_PROFILE))).thenReturn(claims);
    }

    private void setupUserConfirmed(boolean isConfirmed) throws IdentityRecoveryException {
        userSelfRegistrationManagerMockedStatic.when(UserSelfRegistrationManager::getInstance)
                .thenReturn(userSelfRegistrationManager);
        when(userSelfRegistrationManager.isUserConfirmed(any(User.class))).thenReturn(isConfirmed);
    }

    private void setupEmailVerificationScenario() throws IdentityRecoveryException {
        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", 
                RecoveryScenarios.EMAIL_VERIFICATION);
        
        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                .thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                .thenReturn(recoveryData);
    }

    private void setupSelfRegisteredUser() throws IdentityRecoveryException {
        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", 
                RecoveryScenarios.SELF_SIGN_UP);
        
        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                .thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                .thenReturn(recoveryData);
    }
}
