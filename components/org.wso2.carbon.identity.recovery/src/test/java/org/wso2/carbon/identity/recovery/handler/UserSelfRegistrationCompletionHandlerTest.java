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
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.SelfRegistrationUtils;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;

public class UserSelfRegistrationCompletionHandlerTest {

    private static final String POST_ADD_USER = "POST_ADD_USER";
    private static final String USER_NAME = "testuser";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String DOMAIN_NAME = "PRIMARY";
    private static final String SECRET_KEY = "12345";
    private static final String FALSE_STRING = "false";
    private static final String EMAIL_CLAIM = "http://wso2.org/claims/emailaddress";
    private static final String EMAIL_VERIFIED_CLAIM = "http://wso2.org/claims/identity/emailVerified";

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private NotificationChannelManager notificationChannelManager;

    @Mock
    private NotificationChannels notificationChannels;

    private MockedStatic<Utils> utilsMockedStatic;
    private MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMockedStatic;
    private MockedStatic<IdentityUtil> identityUtilMockedStatic;
    private MockedStatic<IdentityGovernanceUtil> identityGovernanceUtilMockedStatic;
    private MockedStatic<SelfRegistrationUtils> selfRegistrationUtilsMockedStatic;

    private UserSelfRegistrationCompletionHandler handler;

    @BeforeMethod
    public void init() {
        openMocks(this);
        handler = new UserSelfRegistrationCompletionHandler();
        utilsMockedStatic = mockStatic(Utils.class);
        jdbcRecoveryDataStoreMockedStatic = mockStatic(JDBCRecoveryDataStore.class);
        identityUtilMockedStatic = mockStatic(IdentityUtil.class);
        identityGovernanceUtilMockedStatic = mockStatic(IdentityGovernanceUtil.class);
        selfRegistrationUtilsMockedStatic = mockStatic(SelfRegistrationUtils.class);
    }

    @AfterMethod
    public void tearDown() {
        utilsMockedStatic.close();
        jdbcRecoveryDataStoreMockedStatic.close();
        identityUtilMockedStatic.close();
        identityGovernanceUtilMockedStatic.close();
        selfRegistrationUtilsMockedStatic.close();
    }

    @Test
    public void testGetName() {

        assertEquals(handler.getName(), "userSelfRegistrationCompletionHandler");
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(handler.getFriendlyName(), "User Self Registration Completion Handler");
    }

    @Test
    public void testHandleEventWhenSelfSignupDisabled() throws IdentityEventException {
        Map<String, Object> eventProperties = createBaseEventProperties();
        Event event = new Event(POST_ADD_USER, eventProperties);

        // Create a real FlowConfigDTO instance and set it to disabled
        FlowConfigDTO flowConfig = new FlowConfigDTO();
        flowConfig.setIsEnabled(false);
        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenReturn(flowConfig);

        handler.handleEvent(event);

        // Verify no further processing happens when self signup is disabled
        jdbcRecoveryDataStoreMockedStatic.verifyNoInteractions();
    }

    @Test
    public void testHandleEventWhenRoleListIsNull() throws IdentityEventException {
        Map<String, Object> eventProperties = createBaseEventProperties();
        eventProperties.remove(IdentityEventConstants.EventProperty.ROLE_LIST);
        Event event = new Event(POST_ADD_USER, eventProperties);

        FlowConfigDTO flowConfig = new FlowConfigDTO();
        flowConfig.setIsEnabled(true);
        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenReturn(flowConfig);

        handler.handleEvent(event);

        // Verify no further processing happens when role list is null
        jdbcRecoveryDataStoreMockedStatic.verifyNoInteractions();
    }

    @Test
    public void testHandleEventWhenSelfSignupRoleNotPresent() throws IdentityEventException {
        Map<String, Object> eventProperties = createBaseEventProperties();
        eventProperties.put(IdentityEventConstants.EventProperty.ROLE_LIST, new String[]{"OTHER_ROLE"});
        Event event = new Event(POST_ADD_USER, eventProperties);

        FlowConfigDTO flowConfig = new FlowConfigDTO();
        flowConfig.setIsEnabled(true);
        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenReturn(flowConfig);

        handler.handleEvent(event);

        // Verify no further processing happens when self signup role is not present
        jdbcRecoveryDataStoreMockedStatic.verifyNoInteractions();
    }

    @Test
    public void testHandleEventWithFlowMgtServerException() {
        Map<String, Object> eventProperties = createBaseEventProperties();
        Event event = new Event(POST_ADD_USER, eventProperties);

        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenThrow(new FlowMgtServerException("Test exception"));

        try {
            handler.handleEvent(event);
        } catch (IdentityEventException e) {
            assertEquals(e.getMessage(), "Error while retrieving self sign up enable property for tenant: " + TENANT_DOMAIN);
        }
    }

    private Map<String, Object> createBaseEventProperties() {
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, USER_NAME);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.ROLE_LIST,
                new String[]{IdentityRecoveryConstants.SELF_SIGNUP_ROLE});

        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn(DOMAIN_NAME);

        return eventProperties;
    }

    private Map<String, Object> createCompleteEventProperties() {
        Map<String, Object> eventProperties = createBaseEventProperties();

        // Add user claims
        Map<String, String> userClaims = new HashMap<>();
        userClaims.put(EMAIL_CLAIM, "test@example.com");
        eventProperties.put("USER_CLAIMS", userClaims);
        eventProperties.put(EMAIL_VERIFIED_CLAIM, FALSE_STRING);

        return eventProperties;
    }

    private void setupMocksForEnabledFlowWithNotificationChannelVerified() throws Exception {
        FlowConfigDTO flowConfig = new FlowConfigDTO();
        flowConfig.setIsEnabled(true);
        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenReturn(flowConfig);

        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        doNothing().when(userRecoveryDataStore).invalidate(any(User.class));
        doNothing().when(userRecoveryDataStore).store(any(UserRecoveryData.class));

        // Setup notification channel mocks to return verified status as false
        when(notificationChannels.getClaimUri()).thenReturn(EMAIL_CLAIM);
        when(notificationChannels.getVerifiedClaimUrl()).thenReturn(EMAIL_VERIFIED_CLAIM);

        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.getNotificationChannel(anyString(), anyString()))
                .thenReturn(notificationChannels);

        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.resolveEventName(
                anyString(), eq(USER_NAME), eq(DOMAIN_NAME), eq(TENANT_DOMAIN))).thenReturn("EVENT_NAME");

        utilsMockedStatic.when(() -> Utils.generateSecretKey(anyString(), anyString(), anyString(), anyString()))
                .thenReturn(SECRET_KEY);
        utilsMockedStatic.when(() -> Utils.getArbitraryProperties())
                .thenReturn(new org.wso2.carbon.identity.recovery.model.Property[0]);

        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.triggerNotification(
                any(User.class), anyString(), anyString(), any(), anyString())).thenAnswer(invocation -> null);
        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.triggerAccountCreationNotification(
                anyString(), anyString(), anyString())).thenAnswer(invocation -> null);
        selfRegistrationUtilsMockedStatic.when(() -> SelfRegistrationUtils.lockUserAccount(
                any(Boolean.class), any(Boolean.class), anyString(), any(UserStoreManager.class), anyString()))
                .thenAnswer(invocation -> null);
    }

    private void setupFlowCompletionMocks(boolean isAccountLockOnCreation, boolean isEnableConfirmationOnCreation) {
        utilsMockedStatic.when(() -> Utils.getFlowCompletionConfig(
                Constants.FlowTypes.REGISTRATION, TENANT_DOMAIN,
                Constants.FlowCompletionConfig.IS_ACCOUNT_LOCK_ON_CREATION_ENABLED))
                .thenReturn(String.valueOf(isAccountLockOnCreation));
        utilsMockedStatic.when(() -> Utils.getFlowCompletionConfig(
                Constants.FlowTypes.REGISTRATION, TENANT_DOMAIN,
                Constants.FlowCompletionConfig.IS_EMAIL_VERIFICATION_ENABLED))
                .thenReturn(String.valueOf(isEnableConfirmationOnCreation));
    }

    private void setupNotificationMocks(boolean isNotificationInternallyManage,
                                       boolean isSelfRegistrationConfirmationNotify) {
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, TENANT_DOMAIN))
                .thenReturn(String.valueOf(isNotificationInternallyManage));
        utilsMockedStatic.when(() -> Utils.getFlowCompletionConfig(
                Constants.FlowTypes.REGISTRATION, TENANT_DOMAIN,
                Constants.FlowCompletionConfig.IS_FLOW_COMPLETION_NOTIFICATION_ENABLED))
                .thenReturn(String.valueOf(isSelfRegistrationConfirmationNotify));
    }
}
