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

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AskPasswordBasedPasswordSetupHandlerTest {

    @InjectMocks
    private AskPasswordBasedPasswordSetupHandler askPasswordBasedPasswordSetupHandler;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityRecoveryServiceDataHolder serviceDataHolder;

    @Mock
    private IdentityEventService identityEventService;

    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;
    private MockedStatic<IdentityUtil> mockedIdentityUtils;
    private MockedStatic<FlowMgtConfigUtils> mockedFlowMgtUtils;

    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final String TEST_USER_STORE_DOMAIN = "TESTING";
    private static final String TEST_USERNAME = "testuser";
    private static final String NEW_EMAIL = "new@abc.com";

    @AfterMethod
    public void close() {

        mockedJDBCRecoveryDataStore.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedUtils.close();
        mockedFrameworkUtils.close();
        mockedIdentityUtils.close();
        mockedFlowMgtUtils.close();
    }

    @BeforeMethod
    public void setUp() throws NoSuchFieldException, IllegalAccessException {

        MockitoAnnotations.openMocks(this);
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedUtils = mockStatic(Utils.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedIdentityUtils = mockStatic(IdentityUtil.class);
        mockedFlowMgtUtils = mockStatic(FlowMgtConfigUtils.class);
        FlowConfigDTO mockFlowConfig = mock(FlowConfigDTO.class);
        when(mockFlowConfig.getIsEnabled()).thenReturn(true);

        askPasswordBasedPasswordSetupHandler = new AskPasswordBasedPasswordSetupHandler();

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");
        mockedIdentityUtils.when(() -> IdentityUtil.addDomainToName(eq(TEST_USERNAME), anyString()))
                .thenReturn(String.format("%s/%s", TEST_USERNAME, TEST_USER_STORE_DOMAIN));
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(anyString(), anyString()))
                .thenReturn(mockFlowConfig);


        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(eq(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))).thenReturn(TEST_USER_STORE_DOMAIN);

    }

    @Test
    public void testHandleEventPreAddUserAskPasswordClaim() throws IdentityEventException {

        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        mockedIdentityUtils.when(() -> IdentityUtil.getProperty(eq(IdentityRecoveryConstants
                        .ConnectorConfig.ASK_PASSWORD_DISABLE_RANDOM_VALUE_FOR_CREDENTIALS)))
                .thenReturn(Boolean.TRUE.toString());
        char[] password = "test1".toCharArray();
        mockedUtils.when(() -> Utils.generateRandomPassword(anyInt()))
                .thenReturn(password);

        StringBuffer credentials = new StringBuffer("test1");
        Map<String, Object> additionalProperties = new HashMap<>();
        additionalProperties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credentials);

        Map<String, String> additionalClaims = new HashMap<>();
        additionalClaims.put(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM, Boolean.TRUE.toString());

        Event event = createEvent(IdentityEventConstants.Event.PRE_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL, additionalProperties, additionalClaims);

        askPasswordBasedPasswordSetupHandler.handleEvent(event);
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.PRE_ADD_USER_WITH_ASK_PASSWORD),
                any()));
    }

    /**
     * Data provider for different OTP scenarios in Ask Password flow.
     * 
     * @return Test data for [isEmailOTP, isSmsOTP, expectedChannel, expectedRecoveryScenario, expectedNotificationType]
     */
    @DataProvider(name = "askPasswordOTPScenarios")
    public Object[][] askPasswordOTPScenarios() {

        return new Object[][]{
                // [isEmailOTP, isSmsOTP, expectedChannel, expectedRecoveryScenario, expectedNotificationType]
                {false, false, NotificationChannels.EMAIL_CHANNEL.getChannelType(), 
                 RecoveryScenarios.ASK_PASSWORD, IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD},
                {true, false, NotificationChannels.EMAIL_CHANNEL.getChannelType(), 
                 RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP, IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_EMAIL_OTP},
                {false, true, NotificationChannels.SMS_CHANNEL.getChannelType(), 
                 RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP, IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP}
        };
    }
    
    @Test(dataProvider = "askPasswordOTPScenarios")
    public void testHandleEventPostAddUserAskPasswordClaimNotificationInternallyManaged(
            boolean isEmailOTP, boolean isSmsOTP, String expectedChannel,
            RecoveryScenarios expectedRecoveryScenario, String expectedNotificationType)
            throws IdentityEventException, IdentityRecoveryException {

        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE, true);
        
        // Mock OTP configurations
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_EMAIL_OTP, isEmailOTP);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_SMS_OTP, isSmsOTP);

        mockedUtils.when(() -> Utils.isAccountStateClaimExisting(anyString())).thenReturn(true);

        Claim temporaryEmailClaim = new Claim();
        temporaryEmailClaim.setClaimUri(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);
        temporaryEmailClaim.setValue(Boolean.TRUE.toString());
        mockedUtils.when(Utils::getAskPasswordTemporaryClaim).thenReturn(temporaryEmailClaim);

        // Mock secret key generation for the expected channel and scenario
        mockedUtils.when(() -> Utils.generateSecretKey(
                        eq(expectedChannel), eq(expectedRecoveryScenario.name()),
                        eq(TEST_TENANT_DOMAIN), eq("EmailVerification")))
                .thenReturn("test_key");

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);

        askPasswordBasedPasswordSetupHandler.handleEvent(event);
        
        verify(userRecoveryDataStore).store(any());
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.POST_ADD_USER_WITH_ASK_PASSWORD),
                any()));

        // Verify the correct secret key generation was called based on the scenario
        mockedUtils.verify(() -> Utils.generateSecretKey(
                eq(expectedChannel), eq(expectedRecoveryScenario.name()),
                eq(TEST_TENANT_DOMAIN), eq("EmailVerification")));

        // Case 2: Utils.generateSecretKey() throws an exception.
        mockedUtils.when(() -> Utils.generateSecretKey(
                        eq(expectedChannel), eq(expectedRecoveryScenario.name()),
                        eq(TEST_TENANT_DOMAIN), eq("EmailVerification")))
                .thenThrow(new IdentityRecoveryServerException("test_error"));
        
        try {
            askPasswordBasedPasswordSetupHandler.handleEvent(event);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        }

        // Reset.
        mockedUtils.when(() -> Utils.generateSecretKey(
                        eq(expectedChannel), eq(expectedRecoveryScenario.name()),
                        eq(TEST_TENANT_DOMAIN), eq("EmailVerification")))
                .thenReturn("test_key");

        // Case 3: Claims are null.
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        Event event3 = new Event(IdentityEventConstants.Event.POST_ADD_USER, eventProperties);
        askPasswordBasedPasswordSetupHandler.handleEvent(event3);
        verify(userRecoveryDataStore, atLeastOnce()).store(any());
    }

    @Test
    public void testHandleEventPostAddUserAskPasswordClaimNotificationExternallyManaged()
            throws IdentityEventException, IdentityRecoveryException {

        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,false);

        Claim temporaryEmailClaim = new Claim();
        temporaryEmailClaim.setClaimUri(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);
        temporaryEmailClaim.setValue(Boolean.TRUE.toString());
        mockedUtils.when(Utils::getAskPasswordTemporaryClaim).thenReturn(temporaryEmailClaim);

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);

        askPasswordBasedPasswordSetupHandler.handleEvent(event);

        verify(userRecoveryDataStore).store(any());
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.POST_ADD_USER_WITH_ASK_PASSWORD),
                any()));
    }

    /**
     * Test method specifically for SMS OTP scenario with mobile number verification.
     * This tests the complete SMS OTP flow including mobile number retrieval.
     */
    @Test
    public void testHandleEventPostAddUserAskPasswordSmsOTPFlow()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        // Mock configurations for SMS OTP flow
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_EMAIL_OTP, false);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_SMS_OTP, true);

        mockedUtils.when(() -> Utils.isAccountStateClaimExisting(anyString())).thenReturn(true);

        Claim temporaryEmailClaim = new Claim();
        temporaryEmailClaim.setClaimUri(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);
        temporaryEmailClaim.setValue(Boolean.TRUE.toString());
        mockedUtils.when(Utils::getAskPasswordTemporaryClaim).thenReturn(temporaryEmailClaim);

        // Mock secret key generation for SMS channel
        mockedUtils.when(() -> Utils.generateSecretKey(
                        eq(NotificationChannels.SMS_CHANNEL.getChannelType()), 
                        eq(RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.name()),
                        eq(TEST_TENANT_DOMAIN), eq("EmailVerification")))
                .thenReturn("123456");

        // Mock mobile number claim
        String testMobileNumber = "+94771234567";
        when(userStoreManager.getUserClaimValue(eq(TEST_USERNAME), 
                eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), eq(null)))
                .thenReturn(testMobileNumber);

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);

        askPasswordBasedPasswordSetupHandler.handleEvent(event);
        
        verify(userRecoveryDataStore).store(any());
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.POST_ADD_USER_WITH_ASK_PASSWORD),
                any()));

        // Verify that the correct channel and recovery scenario were used
        mockedUtils.verify(() -> Utils.generateSecretKey(
                eq(NotificationChannels.SMS_CHANNEL.getChannelType()), 
                eq(RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.name()),
                eq(TEST_TENANT_DOMAIN), eq("EmailVerification")));
                
        // Verify mobile number retrieval was attempted
        verify(userStoreManager).getUserClaimValue(eq(TEST_USERNAME), 
                eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), eq(null));
    }

    @Test
    public void testAskPasswordSetupHandlerDisabled() throws IdentityEventException {

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, "");
        FlowConfigDTO mockFlowConfig = mock(FlowConfigDTO.class);
        when(mockFlowConfig.getIsEnabled()).thenReturn(false);
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(
                        eq(Constants.FlowTypes.INVITED_USER_REGISTRATION.getType()), anyString()))
                .thenReturn(mockFlowConfig);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, false);
        askPasswordBasedPasswordSetupHandler.handleEvent(event);

        // verify that no further actions were taken when the handler is disabled.
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(), any(), any()), org.mockito.Mockito.never());
    }

    @Test
    public void testAskPasswordSetupHandlerDisabledWithNullFlowConfig() throws IdentityEventException {

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, "");
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(
                        eq(Constants.FlowTypes.INVITED_USER_REGISTRATION.getType()), anyString()))
                .thenReturn(null);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, false);
        askPasswordBasedPasswordSetupHandler.handleEvent(event);

        // verify that no further actions were taken when the handler is disabled.
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(), any(), any()), org.mockito.Mockito.never());
    }

    @Test(expectedExceptions = IdentityEventException.class,
          expectedExceptionsMessageRegExp = "Error while checking the invite user registration flow enablement " +
                  "for tenant: .*")
    public void testHandleEventThrowsIdentityEventExceptionWhenFlowMgtServerExceptionOccurs() throws Exception {

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.FALSE,
                null, null, null);
        String testTenantDomain = "test-tenant.com";
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, testTenantDomain);

        // Mock FlowMgtConfigUtils to throw FlowMgtServerException
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(
                        eq(Constants.FlowTypes.INVITED_USER_REGISTRATION.getType()), eq(testTenantDomain)))
                .thenThrow(new org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException(
                        "Flow management server error"));

        // This should throw IdentityEventException with the expected message
        askPasswordBasedPasswordSetupHandler.handleEvent(event);
    }

    private void mockGetConnectorConfig(String connectorConfig, boolean value) {

        mockedUtils.when(() -> Utils.getConnectorConfig(eq(connectorConfig), anyString()))
                .thenReturn(String.valueOf(value));
    }

    private Event createEvent(String eventType, String verifyEmailClaim, String verifiedEmailsClaim,
                              String emailsClaim, String primaryEmail) {

        return createEvent(eventType, verifyEmailClaim, verifiedEmailsClaim, emailsClaim, primaryEmail, null, null);
    }

    private Event createEvent(String eventType, String verifyEmailClaim, String verifiedEmailsClaim,
                              String emailsClaim, String primaryEmail, Map<String, Object> additionalEventProperties,
                              Map<String, String> additionalClaims) {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        if (additionalEventProperties != null) {
            eventProperties.putAll(additionalEventProperties);
        }

        Map<String, String> claims = new HashMap<>();
        if (primaryEmail != null && !primaryEmail.isEmpty()) {
            claims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, primaryEmail);
        }
        if (verifyEmailClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM, verifyEmailClaim);
        }
        if (emailsClaim != null) {
            claims.put(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM, emailsClaim);
        }
        if (verifiedEmailsClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, verifiedEmailsClaim);
        }

        if (additionalClaims != null) {
            claims.putAll(additionalClaims);
        }

        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        return new Event(eventType, eventProperties);
    }

}
