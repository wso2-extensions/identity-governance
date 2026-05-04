/*
 * Copyright (c) 2025-2026, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
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

    @Mock
    private FlowConfigDTO mockedFlowConfigDTO;

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

        when(mockedFlowConfigDTO.getIsEnabled()).thenReturn(false);
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
        // Mock self-signup and email verification as disabled in the connector config.
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();

        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, TENANT_DOMAIN))
                .thenReturn("false");
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, TENANT_DOMAIN))
                .thenReturn("false");

        // Handle the event and verify the handler returns early without processing user claims.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);

            verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
        }
    }

    @Test
    public void testHandleEvent_AccountUnlocked() throws Exception {
        // Self-signup is enabled and the account is not locked.
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(false);

        // Handle the event and verify no exception is thrown since the account is already unlocked.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test
    public void testHandleEvent_AccountLocked_UserConfirmed_SelfSignup() throws Exception {
        // Self-signup enabled, account locked, and user has already confirmed via self-signup.
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

        // Handle the event and verify no exception is thrown since user confirmation is complete.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*not confirmed yet.*")
    public void testHandleEvent_AccountLocked_UserNotConfirmed_SelfSignup() throws Exception {
        // Self-signup enabled, account locked, and user has not confirmed registration yet.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);

        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        setupUserConfirmed(false);

        // Handle the event — expected to throw IdentityEventException for unconfirmed self-signup user.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*email is not confirmed yet.*")
    public void testHandleEvent_AccountLocked_UserNotConfirmed_EmailVerification() throws Exception {
        // Email verification enabled, account locked, and user email has not been confirmed yet.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);

        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(false);

        // Handle the event — expected to throw IdentityEventException for unconfirmed email verification user.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*email is not verified yet.*")
    public void testHandleEvent_EmailVerificationScenario() throws Exception {
        // Email verification enabled, account locked, user confirmed but recovery data indicates email is not yet verified.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);

        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(true); // User is confirmed but in email verification scenario
        setupEmailVerificationScenario();

        // Handle the event — expected to throw IdentityEventException since email verification is pending.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Invalid login attempt.*")
    public void testHandleEvent_InvalidCredentialsScenario_SelfRegisteredUser() throws Exception {
        // Self-signup enabled, failed authentication attempt, account locked, and user is self-registered.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, false);

        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);
        setupSelfRegisteredUser();

        // Handle the event — expected to throw IdentityEventException for invalid login on an unconfirmed account.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test
    public void testHandleEvent_UserDoesNotExist_AuthPolicyEnabled() throws Exception {
        // Self-signup enabled, account existence check policy active, and user does not exist in the store.
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);

        when(userStoreManager.isExistingUser(USERNAME)).thenReturn(false);
        identityUtilMockedStatic.when(() -> IdentityUtil.getProperty("AuthenticationPolicy.CheckAccountExist"))
                .thenReturn("true");

        // Handle the event and verify the handler skips claim retrieval for non-existent users.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);

            verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
        }
    }

    @Test
    public void testHandleEvent_NonPostAuthenticationEvent() throws Exception {
        // A non-POST_AUTHENTICATION event is fired with self-signup enabled.
        Event event = new Event("SOME_OTHER_EVENT", createEventProperties());
        setupUserStoreManager();
        setupEnabledFeatures(true, false);

        // Handle the event and verify the handler skips processing for unrelated event types.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);

            verify(userStoreManager, never()).getUserClaimValues(anyString(), any(String[].class), anyString());
        }
    }

    @Test
    public void testIsUserEmailVerificationScenario_EmailVerification() throws Exception {
        // Email verification enabled, account locked, user confirmed but recovery data shows an EMAIL_VERIFICATION scenario.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);

        setupUserStoreManager();
        setupEnabledFeatures(false, true);
        setupAccountLocked(true);
        setupUserConfirmed(true); // User confirmed but email verification scenario

        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", RecoveryScenarios.EMAIL_VERIFICATION);

        try (MockedStatic<FlowMgtConfigUtils> flowConfigMockedStatic = mockStatic(FlowMgtConfigUtils.class)){
            jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                    .thenReturn(userRecoveryDataStore);

            when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                    .thenReturn(recoveryData);

            flowConfigMockedStatic.when(() ->
                            FlowMgtConfigUtils.getFlowConfig(anyString(), anyString()))
                    .thenReturn(mockedFlowConfigDTO);

            // Handle the event — expected to throw IdentityEventException since the email is not yet verified.
            try {
                handler.handleEvent(event);
            } catch (IdentityEventException e) {
                // Expected exception.
            }

            // Verify the recovery data store was queried to determine the verification scenario.
            verify(userRecoveryDataStore, times(1))
                    .loadWithoutCodeExpiryValidation(any(User.class));
        }
    }

    @Test
    public void testIsUserEmailVerificationScenario_SelfSignup() throws Exception {
        // Self-signup enabled, failed authentication, account locked, and recovery data shows a SELF_SIGN_UP scenario.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, false);

        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);

        User user = createUser();
        UserRecoveryData recoveryData = new UserRecoveryData(user, "12345", RecoveryScenarios.SELF_SIGN_UP);

        try (MockedStatic<FlowMgtConfigUtils> flowConfigMockedStatic = mockStatic(FlowMgtConfigUtils.class)){
            jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance)
                    .thenReturn(userRecoveryDataStore);
            when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class)))
                    .thenReturn(recoveryData);

            flowConfigMockedStatic.when(() ->
                            FlowMgtConfigUtils.getFlowConfig(anyString(), anyString()))
                    .thenReturn(mockedFlowConfigDTO);

            // Handle the event — expected to throw IdentityEventException for invalid credentials on an unconfirmed account.
            try {
                handler.handleEvent(event);
            } catch (IdentityEventException e) {
                // Expected exception.
            }

            // Verify the recovery data store was queried to determine the registration scenario.
            verify(userRecoveryDataStore, times(1))
                    .loadWithoutCodeExpiryValidation(any(User.class));
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Error while retrieving account lock claim value.*")
    public void testHandleEvent_UserStoreException() throws Exception {
        // Self-signup enabled and user store throws an exception when fetching account lock claims.
        Event event = createPostAuthenticationEvent();
        setupUserStoreManager();
        setupEnabledFeatures(true, false);

        when(userStoreManager.getUserClaimValues(eq(USERNAME), any(String[].class), anyString()))
                .thenThrow(new UserStoreException("UserStore error"));

        // Handle the event — expected to throw IdentityEventException wrapping the UserStoreException.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    @Test(expectedExceptions = IdentityEventException.class,
            expectedExceptionsMessageRegExp = ".*Error occurred while checking whether this user is confirmed or not.*")
    public void testHandleEvent_IdentityRecoveryException() throws Exception {
        // Self-signup enabled, account locked, and isUserConfirmed throws an IdentityRecoveryException.
        Event event = createPostAuthenticationEvent();
        event.getEventProperties().put(IdentityEventConstants.EventProperty.OPERATION_STATUS, true);

        setupUserStoreManager();
        setupEnabledFeatures(true, false);
        setupAccountLocked(true);

        userSelfRegistrationManagerMockedStatic.when(UserSelfRegistrationManager::getInstance)
                .thenReturn(userSelfRegistrationManager);
        when(userSelfRegistrationManager.isUserConfirmed(any(User.class)))
                .thenThrow(new IdentityRecoveryException("Recovery error"));

        // Handle the event — expected to throw IdentityEventException wrapping the IdentityRecoveryException.
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            handler.handleEvent(event);
        }
    }

    // Helper methods.

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
