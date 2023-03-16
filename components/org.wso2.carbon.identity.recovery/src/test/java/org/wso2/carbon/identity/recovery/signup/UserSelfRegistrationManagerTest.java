
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

package org.wso2.carbon.identity.recovery.signup;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.ConsentManagerImpl;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.AddReceiptResponse;
import org.wso2.carbon.consent.mgt.core.model.ConsentManagerConfigurationHolder;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerManager;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.user.api.Claim;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_REGISTRATION_OPTION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_REGISTRATION_OPTIONS;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES;

@WithCarbonHome
public class UserSelfRegistrationManagerTest {

    private UserSelfRegistrationManager userSelfRegistrationManager = UserSelfRegistrationManager.getInstance();
    private ReceiptInput resultReceipt;
    private String TEST_TENANT_DOMAIN_NAME = "carbon.super";
    private String TEST_USERSTORE_DOMAIN = "PRIMARY";
    private IdentityProviderManager identityProviderManager;
    private AuthAttributeHandlerManager authAttributeHandlerManager;
    private IdentityGovernanceService identityGovernanceService;
    private OTPGenerator otpGenerator;
    private final String TEST_USER_NAME = "dummyUser";
    private final String TEST_CLAIM_URI = "ttp://wso2.org/claims/emailaddress";
    private final String TEST_CLAIM_VALUE = "dummyuser@wso2.com";
    private static final Log LOG = LogFactory.getLog(UserSelfRegistrationManagerTest.class);

    @Mock
    UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    IdentityEventService identityEventService;

    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityProviderManager> mockedIdentityProviderManager;

    @BeforeMethod
    public void setUp() {

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedJDBCRecoveryDataStore = Mockito.mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityProviderManager = Mockito.mockStatic(IdentityProviderManager.class);
        identityProviderManager = Mockito.mock(IdentityProviderManager.class);
        authAttributeHandlerManager = Mockito.mock(AuthAttributeHandlerManager.class);
        identityGovernanceService = Mockito.mock(IdentityGovernanceService.class);
        otpGenerator = Mockito.mock(OTPGenerator.class);

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
        IdentityRecoveryServiceDataHolder.getInstance().setOtpGenerator(otpGenerator);
        IdentityRecoveryServiceDataHolder.getInstance().setAuthAttributeHandlerManager(authAttributeHandlerManager);

    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedJDBCRecoveryDataStore.close();
        mockedIdentityProviderManager.close();
    }

    @BeforeTest
    void setup() {

        MockitoAnnotations.openMocks(this);
        this.resultReceipt = null;
    }

    String consentData =
            "{\"jurisdiction\":\"someJurisdiction\",\"collectionMethod\":\"Web Form - Self Registration\"," +
                    "\"language\":\"en\",\"piiPrincipalId\":\"DOMAIN/testuser\",\"services\":" +
                    "[{\"tenantDomain\":\"wso2.com\",\"serviceDisplayName\":\"Resident IDP\"," +
                    "\"serviceDescription\":\"Resident IDP\",\"purposes\":[{\"purposeId\":3,\"purposeCategoryId\":[1]," +
                    "\"consentType\":\"EXPLICIT\",\"piiCategory\":[{\"piiCategoryId\":1," +
                    "\"validity\":\"DATE_UNTIL:INDEFINITE\"}],\"primaryPurpose\":true," +
                    "\"termination\":\"DATE_UNTIL:INDEFINITE\",\"thirdPartyDisclosure\":false}],\"tenantId\":1}]," +
                    "\"policyURL\":\"somePolicyUrl\",\"tenantId\":1,\"properties\":{}}";

    /**
     * Testing ResendConfirmationCode for user self registration.
     *
     * @param username                             Username
     * @param userstore                            Userstore domain
     * @param tenantDomain                         Tenant domain
     * @param preferredChannel                     Preferred Notification channel
     * @param errorMsg                             Error scenario
     * @param enableInternalNotificationManagement Manage notifications internally
     * @param expectedChannel                      Expected notification channel
     * @throws Exception If an error occurred while testing.
     */
    @Test(dataProvider = "userDetailsForResendingAccountConfirmation")
    public void testResendConfirmationCode(String username, String userstore, String tenantDomain,
                                           String preferredChannel, String errorMsg,
                                           String enableInternalNotificationManagement, String expectedChannel)
            throws Exception {

        // Build recovery user.
        User user = new User();
        user.setUserName(username);
        user.setUserStoreDomain(userstore);
        user.setTenantDomain(tenantDomain);

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, "1234-4567-890", RecoveryScenarios
                .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        // Storing preferred notification channel in remaining set ids.
        userRecoveryData.setRemainingSetIds(preferredChannel);

        mockConfigurations("true", enableInternalNotificationManagement);
        mockJDBCRecoveryDataStore(userRecoveryData);
        mockEmailTrigger();

        NotificationResponseBean responseBean =
                userSelfRegistrationManager.resendConfirmationCode(user, null);
        assertEquals(responseBean.getNotificationChannel(), expectedChannel, errorMsg);
    }

    /**
     * Contains user data related resending self registration notification.
     *
     * @return Object[][]
     */
    @DataProvider(name = "userDetailsForResendingAccountConfirmation")
    private Object[][] userDetailsForResendingAccountConfirmation() {

        String username = "sominda";
        // Notification channel types.
        String EMAIL = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String SMS = NotificationChannels.SMS_CHANNEL.getChannelType();
        String EXTERNAL = NotificationChannels.EXTERNAL_CHANNEL.getChannelType();

        /* ArrayOrder: Username, Userstore, Tenant domain, Preferred channel, Error message, Manage notifications
        internally, excepted channel */
        return new Object[][]{
                {username, TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME, EMAIL, "User with EMAIL as Preferred " +
                        "Notification Channel : ", "TRUE", EMAIL},
                {username, TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME, EMAIL, "User with EMAIL as Preferred " +
                        "Notification Channel but notifications are externally managed : ", "FALSE", EXTERNAL},
                {username, TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME, SMS, "User with SMS as Preferred " +
                        "Notification Channel : ", "TRUE", SMS},
                {username, TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME, StringUtils.EMPTY,
                        "User no preferred channel specified : ", "TRUE", null}
        };
    }

    /**
     * Mock user self registration configurations for resend account confirmation.
     *
     * @param enableSelfSignUp            Enable user self registration
     * @param enableInternalNotifications Enable notifications internal management.
     * @throws Exception If an error occurred while mocking configurations.
     */
    private void mockConfigurations(String enableSelfSignUp, String enableInternalNotifications) throws Exception {

        org.wso2.carbon.identity.application.common.model.Property signupConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        signupConfig.setName(ENABLE_SELF_SIGNUP);
        signupConfig.setValue(enableSelfSignUp);

        org.wso2.carbon.identity.application.common.model.Property notificationConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        notificationConfig.setName(SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        notificationConfig.setValue(enableInternalNotifications);

        org.wso2.carbon.identity.application.common.model.Property smsOTPConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        smsOTPConfig.setName(SELF_REGISTRATION_SMS_OTP_REGEX);
        smsOTPConfig.setValue("");

        when(identityGovernanceService
                .getConfiguration(new String[]{ENABLE_SELF_SIGNUP}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{signupConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{notificationConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_SMS_OTP_REGEX}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{smsOTPConfig});
        when(otpGenerator.generateOTP(anyBoolean(), anyBoolean(), anyBoolean(), anyInt(), anyString()))
                .thenReturn("1234-4567-890");
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(TEST_USERSTORE_DOMAIN);
    }

    /**
     * Mock JDBCRecoveryDataStore to store user recovery data.
     *
     * @param userRecoveryData User recovery data to be mocked.
     * @throws IdentityRecoveryException If an error occurred while mocking JDBCRecoveryDataStore.
     */
    private void mockJDBCRecoveryDataStore(UserRecoveryData userRecoveryData) throws IdentityRecoveryException {

        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(ArgumentMatchers.anyObject(), ArgumentMatchers.anyObject())).
                thenReturn(userRecoveryData);
        doNothing().when(userRecoveryDataStore).invalidate(ArgumentMatchers.anyString());
        doNothing().when(userRecoveryDataStore).store(ArgumentMatchers.any(UserRecoveryData.class));
    }

    /**
     * Mock email triggering.
     *
     * @throws IdentityEventException If an error occurred while mocking identityEventService.
     */
    private void mockEmailTrigger() throws IdentityEventException {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
        doNothing().when(identityEventService).handleEvent(ArgumentMatchers.any(Event.class));
    }

    @Test
    public void testAddConsent() throws Exception {

        IdentityProvider identityProvider = new IdentityProvider();
        mockedIdentityProviderManager.when(IdentityProviderManager::getInstance).thenReturn(identityProviderManager);
        when(identityProviderManager.getResidentIdP(ArgumentMatchers.anyString())).thenReturn(identityProvider);
        ConsentManager consentManager = new MyConsentManager(new ConsentManagerConfigurationHolder());
        IdentityRecoveryServiceDataHolder.getInstance().setConsentManager(consentManager);
        userSelfRegistrationManager.addUserConsent(consentData, "wso2.com");
        Assert.assertEquals(IdentityRecoveryConstants.Consent.COLLECTION_METHOD_SELF_REGISTRATION,
                resultReceipt.getCollectionMethod());
        Assert.assertEquals("someJurisdiction", resultReceipt.getJurisdiction());
        Assert.assertEquals("en", resultReceipt.getLanguage());
        Assert.assertNotNull(resultReceipt.getServices());
        Assert.assertEquals(1, resultReceipt.getServices().size());
        Assert.assertNotNull(resultReceipt.getServices().get(0).getPurposes());
        Assert.assertEquals(1, resultReceipt.getServices().get(0).getPurposes().size());
        Assert.assertEquals(new Integer(3), resultReceipt.getServices().get(0).getPurposes().get(0).getPurposeId());
        Assert.assertEquals(IdentityRecoveryConstants.Consent.EXPLICIT_CONSENT_TYPE,
                resultReceipt.getServices().get(0).getPurposes().get(0).getConsentType());
        Assert.assertEquals(IdentityRecoveryConstants.Consent.INFINITE_TERMINATION,
                resultReceipt.getServices().get(0).getPurposes().get(0).getTermination());
        Assert.assertEquals(new Integer(3),
                resultReceipt.getServices().get(0).getPurposes().get(0).getPurposeId());
    }

    @Test
    public void testAttributeVerification() throws Exception {

        ValidationResult validationResult = new ValidationResult();
        validationResult.setValid(true);

        when(authAttributeHandlerManager.validateAuthAttributes(anyString(), anyMap())).thenReturn(validationResult);

        User user = new User();
        user.setUserName(TEST_USER_NAME);

        Claim claim = new Claim();
        claim.setClaimUri(TEST_CLAIM_URI);
        claim.setValue(TEST_CLAIM_VALUE);

        Property property = new Property("registrationOption", "MagicLinkAuthAttributeHandler");

        Boolean response = userSelfRegistrationManager.verifyUserAttributes(user, "password", new Claim[]{claim},
                new Property[]{property});

        Assert.assertTrue(response);
    }

    @DataProvider(name = "attributeVerificationFailureData")
    private Object[][] attributeVerificationFailureData() {

        String scenario1 = "Multiple registration options defined in the request.";
        String scenario2 = "Invalid registration option defined in the request.";
        String scenario3 = "Exceptions while obtaining the validation result.";
        String scenario4 = "Attribute requirements not satisfied.";
        String scenario5 = "Validation result being null.";

        Property property1 = new Property("registrationOption", "MagicLinkAuthAttributeHandler");
        Property property2 = new Property("registrationOption", "BasicAuthAuthAttributeHandler");

        // ArrayOrder: scenario, propertiesMap, thrownException, expectedException, validationResult.
        return new Object[][]{
                {scenario1, new Property[]{property1, property2}, null,
                        ERROR_CODE_MULTIPLE_REGISTRATION_OPTIONS.getCode(), null},
                {scenario2, new Property[]{property1},
                        new AuthAttributeHandlerClientException(ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getCode(),
                                ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getMessage()),
                        ERROR_CODE_INVALID_REGISTRATION_OPTION.getCode(), null},
                {scenario3, new Property[]{property1}, new AuthAttributeHandlerException("error-code", "message"),
                        ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getCode(), null},
                {scenario4, new Property[]{property1}, null,
                        ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION.getCode(), new ValidationResult(false)},
                {scenario5, new Property[]{property1}, null,
                        ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getCode(), null}
        };
    }

    @Test(dataProvider = "attributeVerificationFailureData")
    public void testAttributeVerificationFailures(String scenario, Property[] properties, Exception thrownException,
                                                  String expectedErrorCode, ValidationResult validationResult)
            throws Exception {

        LOG.debug("Attribute verification during self registration test scenario: " + scenario);

        if (thrownException != null) {
            when(authAttributeHandlerManager.validateAuthAttributes(anyString(), anyMap())).thenThrow(thrownException);
        } else {
            when(authAttributeHandlerManager.validateAuthAttributes(anyString(), anyMap())).thenReturn(validationResult);
        }

        User user = new User();
        user.setUserName(TEST_USER_NAME);

        Claim claim = new Claim();
        claim.setClaimUri(TEST_CLAIM_URI);
        claim.setValue(TEST_CLAIM_VALUE);

        try {
            userSelfRegistrationManager.verifyUserAttributes(user, "password", new Claim[]{claim}, properties);
        } catch (SelfRegistrationException e) {
            Assert.assertEquals(e.getErrorCode(), expectedErrorCode);
        }
    }

    /**
     * Sample consent manager class.
     */
    class MyConsentManager extends ConsentManagerImpl {

        public MyConsentManager(ConsentManagerConfigurationHolder configHolder) {

            super(configHolder);
        }

        public AddReceiptResponse addConsent(ReceiptInput receiptInput) throws ConsentManagementException {

            resultReceipt = receiptInput;
            return null;
        }
    }
}