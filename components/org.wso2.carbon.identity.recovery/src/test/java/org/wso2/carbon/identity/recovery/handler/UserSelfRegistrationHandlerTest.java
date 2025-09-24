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

import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_LINK;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.SELF_SIGNUP_ROLE;

public class UserSelfRegistrationHandlerTest {

    private static final String POST_ADD_USER = "POST_ADD_USER";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String DOMAIN_NAME = "TEST";
    private static final String TENANT_DOMAIN_KEY = "tenant-domain";
    private static final String USER_STORE_KEY = "userStoreManager";
    private static final String USER_STORE_DOMAIN_KEY = "userstore-domain";
    private static final String ROLE_LIST_KEY = "ROLE_LIST";
    private static final String NOTIFICATION_CHANNEL_KEY = "notification-channel";
    private static final String TEMPLATE_TYPE_KEY = "TEMPLATE_TYPE";
    private static final String EMAIL = "EMAIL";
    private static final String EVENT_NAME = "TRIGGER_NOTIFICATION";
    private static final String CONNECTOR_NAME = "SelfRegistration";
    private static final String TRUE_STRING = "true";

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    private IdentityEventService identityEventService;

    private MockedStatic<Utils> utilsMockedStatic;

    private MockedStatic<JDBCRecoveryDataStore> jdbcRecoveryDataStoreMockedStatic;

    private MockedStatic<IdentityRecoveryServiceDataHolder> identityRecoveryServiceDataHolderMockedStatic;

    @BeforeMethod
    public void init() {

        openMocks(this);
        utilsMockedStatic = mockStatic(Utils.class);
        jdbcRecoveryDataStoreMockedStatic = mockStatic(JDBCRecoveryDataStore.class);
        identityRecoveryServiceDataHolderMockedStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
    }

    @AfterMethod
    public void tearDown() {

        utilsMockedStatic.close();
        jdbcRecoveryDataStoreMockedStatic.close();
        identityRecoveryServiceDataHolderMockedStatic.close();
    }

    @Test(dataProvider = "userRegistrationConfig")
    public void testUserRegistrationEmailTemplates(boolean isEmailOtpEnabled, String expectedEmailTemplate)
            throws IdentityEventException, IdentityRecoveryException {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(TENANT_DOMAIN_KEY, TENANT_DOMAIN);
        eventProperties.put(USER_STORE_KEY, userStoreManager);
        eventProperties.put(ROLE_LIST_KEY, new String[]{SELF_SIGNUP_ROLE});
        Event event = new Event(POST_ADD_USER, eventProperties);

        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(anyString())).thenReturn(DOMAIN_NAME);
        jdbcRecoveryDataStoreMockedStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        doNothing().when(userRecoveryDataStore).invalidate(any(User.class));
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(ENABLE_SELF_SIGNUP, TENANT_DOMAIN)).
                thenReturn(TRUE_STRING);
        if (isEmailOtpEnabled) {
            utilsMockedStatic.when(() -> Utils.getConnectorConfig(SELF_REGISTRATION_SEND_OTP_IN_EMAIL, TENANT_DOMAIN)).
                    thenReturn(TRUE_STRING);
        }
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, TENANT_DOMAIN)).
                thenReturn(TRUE_STRING);
        utilsMockedStatic.when(() -> Utils.getConnectorConfig(ACCOUNT_LOCK_ON_CREATION, TENANT_DOMAIN)).
                thenReturn(TRUE_STRING);
        utilsMockedStatic.when(() -> Utils.generateSecretKey(EMAIL, RecoveryScenarios.SELF_SIGN_UP.name(),
                TENANT_DOMAIN, CONNECTOR_NAME)).thenReturn("12345");

        FlowConfigDTO flowConfig = new FlowConfigDTO();
        flowConfig.setIsEnabled(false);
        utilsMockedStatic.when(() -> Utils.getFlowConfig(Constants.FlowTypes.REGISTRATION.getType(), TENANT_DOMAIN))
                .thenReturn(flowConfig);

        identityRecoveryServiceDataHolderMockedStatic.when(IdentityRecoveryServiceDataHolder::getInstance).
                thenReturn(identityRecoveryServiceDataHolder);
        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);

        UserSelfRegistrationHandler userSelfRegistrationHandler = new UserSelfRegistrationHandler();
        userSelfRegistrationHandler.handleEvent(event);

        // Capturing the event value for asserting.
        ArgumentCaptor<Event> captor = ArgumentCaptor.forClass(Event.class);
        verify(identityEventService).handleEvent(captor.capture());
        Event capturedEvent = captor.getValue();
        Map<String, Object> capturedEventEventProperties = capturedEvent.getEventProperties();
        assertEquals(capturedEvent.getEventName(), EVENT_NAME);
        assertEquals(capturedEventEventProperties.get(TENANT_DOMAIN_KEY), TENANT_DOMAIN);
        assertEquals(capturedEventEventProperties.get(USER_STORE_DOMAIN_KEY), DOMAIN_NAME);
        assertEquals(capturedEventEventProperties.get(NOTIFICATION_CHANNEL_KEY), EMAIL);
        assertEquals(capturedEventEventProperties.get(TEMPLATE_TYPE_KEY), expectedEmailTemplate);
    }

    @DataProvider(name = "userRegistrationConfig")
    private Object[][] buildUserRegistrationConfigs() {

        return new Object[][]{
                {false, NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_LINK},
                {true, NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_OTP}
        };
    }

}
