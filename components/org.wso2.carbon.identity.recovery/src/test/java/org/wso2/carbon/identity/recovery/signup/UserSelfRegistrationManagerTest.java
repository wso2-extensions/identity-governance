/*
 * Copyright (c) 2018-2025, WSO2 LLC. (http://www.wso2.com).
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
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.signup;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.ConsentManagerImpl;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.AddReceiptResponse;
import org.wso2.carbon.consent.mgt.core.model.ConsentManagerConfigurationHolder;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.ResolvedUser;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerManager;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.consent.mgt.services.ConsentUtilityService;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;
import org.wso2.carbon.identity.input.validation.mgt.exceptions.InputValidationMgtException;
import org.wso2.carbon.identity.input.validation.mgt.model.RulesConfiguration;
import org.wso2.carbon.identity.input.validation.mgt.model.ValidationConfiguration;
import org.wso2.carbon.identity.input.validation.mgt.model.Validator;
import org.wso2.carbon.identity.input.validation.mgt.services.InputValidationManagementService;
import org.wso2.carbon.identity.input.validation.mgt.utils.Constants;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.UserWorkflowManagementService;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.rule.metadata.internal.config.FlowConfig;
import org.wso2.carbon.identity.workflow.mgt.util.WorkflowErrorConstants;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.fail;

import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND;
import static org.wso2.carbon.identity.mgt.constants.SelfRegistrationStatusCodes.ERROR_CODE_DUPLICATE_CLAIM_VALUE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_REGISTRATION_OPTION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_REGISTRATION_OPTIONS;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES;
import static org.wso2.carbon.identity.workflow.mgt.util.WorkflowErrorConstants.ErrorMessages.ERROR_CODE_USER_WF_ALREADY_EXISTS;
import static org.wso2.carbon.identity.workflow.mgt.util.WorkflowErrorConstants.ErrorMessages.ERROR_CODE_USER_WF_USER_ALREADY_EXISTS;

/**
 * Test class for UserSelfRegistrationManager class.
 */
@WithCarbonHome
public class UserSelfRegistrationManagerTest {

    @InjectMocks
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private ConsentManager consentManger;

    @Mock
    private TenantManager tenantManager;

    @Mock
    private ConsentUtilityService consentUtilityService;

    @Mock
    private UserRealm userRealm;

    @Mock
    private IdentityProviderManager identityProviderManager;

    @Mock
    private IdentityProvider identityProvider;

    @Mock
    private AuthAttributeHandlerManager authAttributeHandlerManager;

    @Mock
    private IdentityGovernanceService identityGovernanceService;

    @Mock
    private ReceiptInput resultReceipt;

    @Mock
    private RealmService realmService;

    @Mock
    private OTPGenerator otpGenerator;

    @Mock
    private PrivilegedCarbonContext privilegedCarbonContext;

    @Mock
    private NotificationChannelManager notificationChannelManager;

    @Mock
    private UserWorkflowManagementService userWorkflowManagementService;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private InputValidationManagementService inputValidationManagementService;

    @Mock
    private ValidationConfiguration validationConfiguration;

    @Mock
    private AbstractUserStoreManager abstractUserStoreManager;

    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedServiceDataHolder;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityProviderManager> mockedIdentityProviderManager;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;
    private MockedStatic<MultitenantUtils> mockedMultiTenantUtils;
    private MockedStatic<UserCoreUtil> mockedUserCoreUtil;
    private FlowConfigDTO mockedFlowConfigDTO;

    private final String TEST_TENANT_DOMAIN_NAME = "carbon.super";
    private final int TEST_TENANT_ID = 12;
    private final String TEST_USERSTORE_DOMAIN = "PRIMARY";
    private final String TEST_SECONDARY_USERSTORE_DOMAIN = "SECONDARY";
    private final String TEST_USER_NAME = "dummyUser";
    private final String TEST_USER_ID = "dummyUserId";
    private final String TEST_INVALID_USER_NAME = "IS";
    private final String TEST_CLAIM_URI = "ttp://wso2.org/claims/emailaddress";
    private final String TEST_CLAIM_VALUE = "dummyuser@wso2.com";
    private final String TEST_MOBILE_CLAIM_VALUE = "0775553443";
    private final String TEST_PRIMARY_USER_STORE_DOMAIN = "PRIMARY";
    private final String TEST_RECOVERY_DATA_STORE_SECRET = "secret";
    private final String TEST_CODE = "test-code";

    private static final Log LOG = LogFactory.getLog(UserSelfRegistrationManagerTest.class);

    @BeforeMethod
    public void setUp() throws UserStoreException, IdentityProviderManagementException, InputValidationMgtException {

        MockitoAnnotations.openMocks(this);

        userSelfRegistrationManager = UserSelfRegistrationManager.getInstance();

        mockedServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityProviderManager = mockStatic(IdentityProviderManager.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedMultiTenantUtils = mockStatic(MultitenantUtils.class);
        mockedUserCoreUtil = mockStatic(UserCoreUtil.class);
        mockedFlowConfigDTO = mock(FlowConfigDTO.class);

        mockedIdentityProviderManager.when(IdentityProviderManager::getInstance).thenReturn(identityProviderManager);
        mockedServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(identityRecoveryServiceDataHolder);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(any())).thenReturn(TEST_TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(TEST_PRIMARY_USER_STORE_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(eq(TEST_USER_NAME), anyString()))
                .thenReturn(String.format("%s/%s", TEST_USER_NAME, TEST_USERSTORE_DOMAIN));
        mockedFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");
        when(identityProviderManager.getResidentIdP(anyString())).thenReturn(identityProvider);

        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(identityRecoveryServiceDataHolder.getIdentityGovernanceService()).thenReturn(identityGovernanceService);
        when(identityRecoveryServiceDataHolder.getOtpGenerator()).thenReturn(otpGenerator);
        when(identityRecoveryServiceDataHolder.getAuthAttributeHandlerManager()).thenReturn(authAttributeHandlerManager);
        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(realmService);
        when(identityRecoveryServiceDataHolder.getConsentManager()).thenReturn(consentManger);
        when(identityRecoveryServiceDataHolder.getConsentUtilityService()).thenReturn(consentUtilityService);
        when(identityRecoveryServiceDataHolder.getInputValidationMgtService())
                .thenReturn(inputValidationManagementService);

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(privilegedCarbonContext.getOSGiService(UserWorkflowManagementService.class, null))
                .thenReturn(userWorkflowManagementService);

        when(mockedFlowConfigDTO.getIsEnabled()).thenReturn(false);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedJDBCRecoveryDataStore.close();
        mockedIdentityProviderManager.close();
        mockedIdentityTenantUtil.close();
        mockedPrivilegedCarbonContext.close();
        mockedServiceDataHolder.close();
        mockedFrameworkUtils.close();
        mockedMultiTenantUtils.close();
        mockedUserCoreUtil.close();
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

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET, RecoveryScenarios
                .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        // Storing preferred notification channel in remaining set ids.
        userRecoveryData.setRemainingSetIds(preferredChannel);
        userRecoveryData.setTimeCreated(new Timestamp(System.currentTimeMillis()));

        mockConfigurations("true", enableInternalNotificationManagement);
        mockJDBCRecoveryDataStore(userRecoveryData);
        mockEmailTrigger();
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getUserClaimValue(any(), eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), any()))
                .thenReturn(TEST_MOBILE_CLAIM_VALUE);
        when(userStoreManager.getUserClaimValue(any(), eq(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM), any()))
                .thenReturn(TEST_CLAIM_VALUE);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);

        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {
            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);
            NotificationResponseBean responseBean =
                    userSelfRegistrationManager.resendConfirmationCode(user, new Property[0]);
            assertEquals(responseBean.getNotificationChannel(), expectedChannel, errorMsg);
        }
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

        org.wso2.carbon.identity.application.common.model.Property sendOtpInEmailConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        sendOtpInEmailConfig.setName(SELF_REGISTRATION_SEND_OTP_IN_EMAIL);
        sendOtpInEmailConfig.setValue("false");

        org.wso2.carbon.identity.application.common.model.Property selfRegistrationEmailOTPConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        selfRegistrationEmailOTPConfig.setName(SELF_REGISTRATION_EMAIL_OTP_ENABLE);
        selfRegistrationEmailOTPConfig.setValue(Boolean.FALSE.toString());

        org.wso2.carbon.identity.application.common.model.Property useLowerCaseConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        useLowerCaseConfig.setName(SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP);
        useLowerCaseConfig.setValue("true");

        org.wso2.carbon.identity.application.common.model.Property useNumbersConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        useNumbersConfig.setName(SELF_REGISTRATION_USE_NUMBERS_IN_OTP);
        useNumbersConfig.setValue("true");

        org.wso2.carbon.identity.application.common.model.Property useUpperCaseConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        useUpperCaseConfig.setName(SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP);
        useUpperCaseConfig.setValue("true");

        org.wso2.carbon.identity.application.common.model.Property otpLengthConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        otpLengthConfig.setName(SELF_REGISTRATION_OTP_LENGTH);
        otpLengthConfig.setValue("6");

        org.wso2.carbon.identity.application.common.model.Property smsOTPConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        smsOTPConfig.setName(SELF_REGISTRATION_SMS_OTP_REGEX);
        smsOTPConfig.setValue("");

        org.wso2.carbon.identity.application.common.model.Property selfRegistrationCodeExpiryConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        selfRegistrationCodeExpiryConfig.setName(SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        selfRegistrationCodeExpiryConfig.setValue("1440");

        org.wso2.carbon.identity.application.common.model.Property selfRegistrationSMSCodeExpiryConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        selfRegistrationSMSCodeExpiryConfig.setName(SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        selfRegistrationSMSCodeExpiryConfig.setValue("1");

        org.wso2.carbon.identity.application.common.model.Property accountLockOnCreationConfig =
                new org.wso2.carbon.identity.application.common.model.Property();
        accountLockOnCreationConfig.setName(ACCOUNT_LOCK_ON_CREATION);
        accountLockOnCreationConfig.setValue("false");

        when(identityGovernanceService
                .getConfiguration(new String[]{ENABLE_SELF_SIGNUP}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{signupConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{notificationConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_SEND_OTP_IN_EMAIL}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{sendOtpInEmailConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_EMAIL_OTP_ENABLE}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{
                        selfRegistrationEmailOTPConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP},
                        TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{useLowerCaseConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_USE_NUMBERS_IN_OTP}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{useNumbersConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP},
                        TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{useUpperCaseConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_OTP_LENGTH}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{otpLengthConfig});
        when(identityGovernanceService
                .getConfiguration(new String[]{SELF_REGISTRATION_SMS_OTP_REGEX}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]{smsOTPConfig});
        when(identityGovernanceService
                .getConfiguration(
                        new String[]{SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]
                        {selfRegistrationCodeExpiryConfig});
        when(identityGovernanceService
                .getConfiguration(
                        new String[]{SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]
                        {selfRegistrationSMSCodeExpiryConfig});
        when(identityGovernanceService
                .getConfiguration(
                        new String[]{ACCOUNT_LOCK_ON_CREATION}, TEST_TENANT_DOMAIN_NAME))
                .thenReturn(new org.wso2.carbon.identity.application.common.model.Property[]
                        {accountLockOnCreationConfig});
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
        when(identityProviderManager.getResidentIdP(ArgumentMatchers.anyString())).thenReturn(identityProvider);
        ConsentManager consentManager = new MyConsentManager(new ConsentManagerConfigurationHolder());
        when(identityRecoveryServiceDataHolder.getConsentManager()).thenReturn(consentManager);
        userSelfRegistrationManager.addUserConsent(consentData, "wso2.com");
        Assert.assertEquals(IdentityRecoveryConstants.Consent.COLLECTION_METHOD_SELF_REGISTRATION,
                resultReceipt.getCollectionMethod());
        Assert.assertEquals("someJurisdiction", resultReceipt.getJurisdiction());
        Assert.assertEquals("en", resultReceipt.getLanguage());
        assertNotNull(resultReceipt.getServices());
        Assert.assertEquals(1, resultReceipt.getServices().size());
        assertNotNull(resultReceipt.getServices().get(0).getPurposes());
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

        Boolean response = userSelfRegistrationManager.verifyUserAttributes(user, "password",
                new Claim[]{claim}, new Property[]{property});

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

    @Test
    public void testConfirmVerificationCodeMe()
            throws IdentityRecoveryException, UserStoreException, ClaimMetadataException {

        // Case 1: Multiple email and mobile per user is enabled.
        String verificationPendingMobileNumber = "0700000000";
        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();
        String updatedVerifiedMobileNumbers =
                capturedClaims.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
        String updatedVerificationPendingMobile =
                capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM);
        String updatedPrimaryMobile = capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);

        assertEquals(updatedVerificationPendingMobile, StringUtils.EMPTY);
        assertEquals(updatedPrimaryMobile, verificationPendingMobileNumber);

        // Case 2: Multiple email and mobile per user is disabled.
        mockMultiAttributeEnabled(false);
        ArgumentCaptor<Map<String, String>> claimsCaptor2 = ArgumentCaptor.forClass(Map.class);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor2.capture(), isNull());
        Map<String, String> capturedClaims2 = claimsCaptor2.getValue();
        String mobileNumberClaims =
                capturedClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);
        String updatedVerificationPendingMobile2 =
                capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM);

        assertEquals(updatedVerificationPendingMobile2, StringUtils.EMPTY);
        assertEquals(mobileNumberClaims, verificationPendingMobileNumber);

        // Case 3: Wrong recovery step.
        UserRecoveryData userRecoveryData3 = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VALIDATE_ALL_CHALLENGE_QUESTION);
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);
        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData3);
        try {
            userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());
            fail();
        } catch (IdentityRecoveryException e) {
            assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getCode());
        }
    }

    @Test
    public void testConfirmVerificationCodeMeVerificationOnVerifiedListUpdate()
            throws IdentityRecoveryException, UserStoreException, ClaimMetadataException {

        // Case 1: Recovery Scenario - MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.
        String verificationPendingMobileNumber = "0700000000";
        String existingPrimaryMobileNumber = "077777777";
        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, existingPrimaryMobileNumber);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());
        Map<String, String> capturedClaims = claimsCaptor.getValue();

        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                verificationPendingMobileNumber);

        reset(userStoreManager);

        // Case 2: Recovery Scenario - MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE with Empty primary mobile value.
        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, StringUtils.EMPTY);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        ArgumentCaptor<Map<String, String>> claimsCaptor2 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor2.capture(), isNull());
        Map<String, String> capturedClaims2 = claimsCaptor2.getValue();
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                verificationPendingMobileNumber);
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM),
                verificationPendingMobileNumber);

        reset(userStoreManager);

        // Case 3: Multiple email and mobile per user is disabled.
        mockMultiAttributeEnabled(false);
        ArgumentCaptor<Map<String, String>> claimsCaptor3 = ArgumentCaptor.forClass(Map.class);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor3.capture(), isNull());
        Map<String, String> capturedClaims3 = claimsCaptor3.getValue();
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
        assertEquals(capturedClaims3.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM),
                verificationPendingMobileNumber);
        assertFalse(capturedClaims3.containsKey(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM));

        reset(userStoreManager);
        reset(userRecoveryDataStore);
        // Case 4: When pending mobile number claim value is null.
        UserRecoveryData userRecoveryData2 = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData2.setRemainingSetIds(null);
        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData2);

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());

        ArgumentCaptor<Map<String, String>> claimsCaptor4 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor4.capture(), isNull());
        assertFalse(claimsCaptor4.getValue().containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM));
        assertFalse(claimsCaptor4.getValue().containsKey(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM));
    }

    @Test(expectedExceptions = IdentityRecoveryServerException.class)
    public void testConfirmVerificationCodeMeUserStoreException()
            throws IdentityRecoveryException, UserStoreException, ClaimMetadataException {

        // Case 3: Throws user store exception while getting user claim values.
        String verificationPendingMobileNumber = "0700000000";
        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(true);
        when(userStoreManager.getUserClaimValue(any(), eq(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                any())).thenThrow(new org.wso2.carbon.user.core.UserStoreException());

        userSelfRegistrationManager.confirmVerificationCodeMe(TEST_CODE, new HashMap<>());
    }

    @Test
    public void testGetConfirmedSelfRegisteredUserEmailVerificationOnUpdate()
            throws IdentityRecoveryException, UserStoreException, IdentityGovernanceException, ClaimMetadataException {

        String verifiedChannelType = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/emailaddress";
        String verificationPendingEmail = "pasindu@gmail.com";
        Map<String, String> metaProperties = new HashMap<>();

        // Case 1: Multiple email and mobile per user is enabled - EMAIL_VERIFICATION_ON_UPDATE.
        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL);
        // Setting verification pending email claim value.
        userRecoveryData.setRemainingSetIds(verificationPendingEmail);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);

        org.wso2.carbon.identity.application.common.model.Property property =
                new org.wso2.carbon.identity.application.common.model.Property();
        org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                new org.wso2.carbon.identity.application.common.model.Property[]{property};

        when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();
        String updatedEmailAddressClaim =
                capturedClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);
        String verificationPendingEmailAddress =
                capturedClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM);

        assertTrue(StringUtils.contains(updatedEmailAddressClaim, verificationPendingEmail));
        assertEquals(verificationPendingEmailAddress, StringUtils.EMPTY);
        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));

        reset(userStoreManager);
        // Case 2: Multiple email and mobile per user is disabled.
        mockMultiAttributeEnabled(false);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor2 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor2.capture(), isNull());

        Map<String, String> capturedClaims2 = claimsCaptor2.getValue();
        String emailAddressClaim =
                capturedClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);
        assertEquals(emailAddressClaim, verificationPendingEmail);
        assertFalse(capturedClaims2.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
        assertFalse(capturedClaims2.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));

        // Case 3 : Throws user store exception while getting user store manager.
        when(userRealm.getUserStoreManager()).thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testGetConfirmedSelfRegisteredUserEmailVerificationOnVerifiedListUpdate()
            throws IdentityRecoveryException, UserStoreException, IdentityGovernanceException, ClaimMetadataException {

        String verifiedChannelType = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/emailaddress";
        String verificationPendingEmail = "pasindu@gmail.com";
        Map<String, String> metaProperties = new HashMap<>();

        // Case 1: Multiple email and mobile per user is enabled - EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE.
        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL);
        // Setting verification pending email claim value.
        userRecoveryData.setRemainingSetIds(verificationPendingEmail);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, "primary@test.com");

        org.wso2.carbon.identity.application.common.model.Property property =
                new org.wso2.carbon.identity.application.common.model.Property();
        org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                new org.wso2.carbon.identity.application.common.model.Property[]{property};

        when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();

        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), StringUtils.EMPTY);
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM),
                verificationPendingEmail);
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM),
                verificationPendingEmail);

        reset(userStoreManager);

        // Case 2: Multiple email and mobile per user is enabled - EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE
        // with empty primary value.

        mockMultiAttributeEnabled(true);
        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, StringUtils.EMPTY);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor2 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor2.capture(), isNull());

        Map<String, String> capturedClaims2 = claimsCaptor2.getValue();

        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM),
                verificationPendingEmail);
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), StringUtils.EMPTY);
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM),
                verificationPendingEmail);
        assertEquals(capturedClaims2.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM),
                verificationPendingEmail);

        reset(userStoreManager);
        // Case 2: Multiple email and mobile per user is disabled.
        mockMultiAttributeEnabled(false);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor3 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager).setUserClaimValues(anyString(), claimsCaptor3.capture(), isNull());

        Map<String, String> capturedClaims3 = claimsCaptor3.getValue();
        String emailAddressClaim =
                capturedClaims3.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);
        assertEquals(emailAddressClaim, verificationPendingEmail);
        assertFalse(capturedClaims3.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
        assertFalse(capturedClaims3.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));
    }

    @Test
    public void testGetConfirmedSelfRegisteredUserMobileVerificationOnUpdate()
            throws IdentityRecoveryException, UserStoreException, IdentityGovernanceException {

        String verifiedChannelType = NotificationChannels.SMS_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/mobile";
        String verificationPendingMobileNumber = "077888888";
        Map<String, String> metaProperties = new HashMap<>();

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        // Setting verification pending email claim value.
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);

        org.wso2.carbon.identity.application.common.model.Property property =
                new org.wso2.carbon.identity.application.common.model.Property();
        org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                new org.wso2.carbon.identity.application.common.model.Property[]{property};

        when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiEmailAddressesPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.getConnectorConfig(
                            eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER),
                            anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(IdentityRecoveryConstants.ConnectorConfig
                            .SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                    anyString())).thenReturn("true");
            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
        }

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();
        String verificationPendingMobileNumberClaim =
                capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM);
        String updatedMobileNumberClaimValue =
                capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);

        assertEquals(verificationPendingMobileNumberClaim, StringUtils.EMPTY);
        assertEquals(updatedMobileNumberClaimValue, verificationPendingMobileNumber);
        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));

        // Case 2: External Verified Channel type.
        verifiedChannelType = NotificationChannels.EXTERNAL_CHANNEL.getChannelType();
        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiEmailAddressesPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.getConnectorConfig(
                            eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER),
                            anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(IdentityRecoveryConstants.ConnectorConfig
                            .SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                    anyString())).thenReturn("true");
            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
        }

        ArgumentCaptor<Map<String, String>> claimsCaptor1 = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor1.capture(), isNull());

        Map<String, String> capturedClaims1 = claimsCaptor1.getValue();
        String emailVerifiedClaim =
                capturedClaims1.get(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM);
        assertEquals(emailVerifiedClaim, Boolean.TRUE.toString());

        // Case 3: Throws user store exception while getting user claim values.
        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiEmailAddressesPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.isMultiMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.getConnectorConfig(
                            eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER),
                            anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(IdentityRecoveryConstants.ConnectorConfig
                            .SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                    anyString())).thenReturn("true");

            when(userStoreManager.getUserClaimValue(anyString(), eq(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM), isNull()))
                    .thenThrow(new org.wso2.carbon.user.core.UserStoreException("test exception"));
            mockedUtils.when(() -> Utils.getMultiValuedClaim(eq(userStoreManager), eq(user),
                    eq(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM))).thenCallRealMethod();

            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }

        // Case 4: With wrong verified channel claim value.
        verifiedChannelType = NotificationChannels.SMS_CHANNEL.getChannelType();
        verifiedChannelClaim = "http://wso2.org/claims/invalid";
        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.getConnectorConfig(
                            eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER),
                            anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(IdentityRecoveryConstants.ConnectorConfig
                            .SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                    anyString())).thenReturn("true");
            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }
    }

    @Test
    public void testGetConfirmedSelfRegisteredUserMobileVerificationOnVerifiedListUpdate()
            throws IdentityRecoveryException, UserStoreException, IdentityGovernanceException {

        String verifiedChannelType = NotificationChannels.SMS_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/mobile";
        String verificationPendingMobileNumber = "077888888";
        Map<String, String> metaProperties = new HashMap<>();

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
        // Setting verification pending email claim value.
        userRecoveryData.setRemainingSetIds(verificationPendingMobileNumber);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockGetUserClaimValue(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);
        mockGetUserClaimValue(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM, StringUtils.EMPTY);

        org.wso2.carbon.identity.application.common.model.Property property =
                new org.wso2.carbon.identity.application.common.model.Property();
        org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                new org.wso2.carbon.identity.application.common.model.Property[]{property};

        when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.getConnectorConfig(
                            eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER),
                            anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(IdentityRecoveryConstants.ConnectorConfig
                            .SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION),
                    anyString())).thenReturn("true");
            userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType,
                    verifiedChannelClaim, metaProperties);
        }

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();

        assertEquals(capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
        assertFalse(capturedClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM));
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM),
                verificationPendingMobileNumber);
        assertEquals(capturedClaims.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                verificationPendingMobileNumber);
    }

    @Test
    public void testGetConfirmedSelfRegisteredUserConfirmSignUp()
            throws IdentityRecoveryException, UserStoreException, IdentityGovernanceException, ClaimMetadataException {

        String verifiedChannelType = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/emailaddress";
        String verificationPendingEmail = "pasindu@gmail.com";
        Map<String, String> metaProperties = new HashMap<>();

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        // Setting verification pending email claim value.
        userRecoveryData.setRemainingSetIds(verificationPendingEmail);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getUsername()).thenReturn(TEST_USER_NAME);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        mockMultiAttributeEnabled(false);

        org.wso2.carbon.identity.application.common.model.Property property =
                new org.wso2.carbon.identity.application.common.model.Property();
        org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                new org.wso2.carbon.identity.application.common.model.Property[]{property};

        when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);

        userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE, verifiedChannelType, verifiedChannelClaim,
                metaProperties);

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());
        Map<String, String> capturedClaims = claimsCaptor.getValue();
        String updatedVerifiedMobileNumbers =
                capturedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
        assertEquals(updatedVerifiedMobileNumbers, Boolean.FALSE.toString());
    }

    @Test
    public void testRegisterUser() throws Exception {

        User user = getUser();
        mockConfigurations("true", "true");
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(abstractUserStoreManager.isExistingRole(eq(IdentityRecoveryConstants.SELF_SIGNUP_ROLE))).thenReturn(true);
        when(abstractUserStoreManager.addUserWithID(anyString(), anyString(), any(), any(), isNull()))
            .thenReturn(getRegisteredUser());
        when(privilegedCarbonContext.getOSGiService(any(), isNull())).thenReturn(notificationChannelManager);
        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

        Property property = new Property(IdentityRecoveryConstants.Consent.CONSENT, consentData);
        NotificationResponseBean notificationResponseBean =
                userSelfRegistrationManager.registerUser(user, "test-pwd", new Claim[0],
                        new Property[]{property});

        User registeredUser = notificationResponseBean.getUser();
        assertEquals(user.getUserName(), registeredUser.getUserName());
        assertTrue(registeredUser instanceof ResolvedUser);
        assertEquals(((ResolvedUser) registeredUser).getUserId(), TEST_USER_ID);
        verify(abstractUserStoreManager).addUserWithID(anyString(), anyString(), any(), any(), isNull());
        verify(consentManger).addConsent(any());
        verify(identityEventService, atLeastOnce()).handleEvent(any());
    }

    @Test
    public void testRegisterUserWithDuplicateClaims() throws Exception {

        User user = getUser();
        mockConfigurations("true", "true");
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(abstractUserStoreManager.isExistingRole(eq(IdentityRecoveryConstants.SELF_SIGNUP_ROLE))).thenReturn(true);
        when(abstractUserStoreManager.addUserWithID(anyString(), anyString(), any(), any(), isNull()))
                .thenThrow(new UserStoreClientException("errorMessage",
                        new PolicyViolationException(ERROR_CODE_DUPLICATE_CLAIM_VALUE, "errorMessage")));
        when(privilegedCarbonContext.getOSGiService(any(), isNull())).thenReturn(notificationChannelManager);
        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

        try {
            userSelfRegistrationManager.registerUser(user, "test-pwd", new Claim[0], new Property[0]);
        } catch (IdentityRecoveryClientException e) {
            assertEquals(e.getErrorCode(), ERROR_CODE_DUPLICATE_CLAIM_VALUE);
        }
    }

    @Test
    public void testRegisterUserNotificationExternallyAccountLockedOnCreation() throws Exception {

        User user = new User();
        user.setUserName(TEST_USER_NAME);
        Property property = new Property(IdentityRecoveryConstants.Consent.CONSENT, consentData);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(abstractUserStoreManager.addUserWithID(anyString(), anyString(), any(), any(), isNull()))
                .thenReturn(getRegisteredUser());

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                    .thenReturn("false");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ENABLE_SELF_SIGNUP), anyString()))
                    .thenReturn("true");
            mockedUtils.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);

            when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                    .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

            NotificationResponseBean notificationResponseBean =
                    userSelfRegistrationManager.registerUser(user, "test-pwd", new Claim[0],
                            new Property[]{property});

            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getCode(),
                    notificationResponseBean.getCode());
            User registeredUser = notificationResponseBean.getUser();
            verify(abstractUserStoreManager).addUserWithID(anyString(), anyString(), any(), any(), isNull());
            assertTrue(registeredUser instanceof ResolvedUser);
        assertEquals(((ResolvedUser) registeredUser).getUserId(), TEST_USER_ID);
            verify(consentManger).addConsent(any());
            verify(identityEventService, atLeastOnce()).handleEvent(any());
        }
    }

    @Test
    public void testRegisterUserVerifiedPreferredChannel() throws Exception {

        String emailVerifiedClaimURI = "http://wso2.org/claims/identity/emailVerified";
        User user = getUser();
        Property property = new Property(IdentityRecoveryConstants.Consent.CONSENT, consentData);
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(
                        eq(IdentityRecoveryConstants.ConnectorConfig
                                .ENABLE_ACCOUNT_LOCK_FOR_VERIFIED_PREFERRED_CHANNEL)))
                .thenReturn("false");
        when(consentUtilityService.filterPIIsFromReceipt(any(), any())).thenReturn(
                new HashSet<>(Arrays.asList(emailVerifiedClaimURI)));
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(abstractUserStoreManager.addUserWithID(anyString(), anyString(), any(), any(), isNull()))
                .thenReturn(getRegisteredUser());

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                    .thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                    .thenReturn("false");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ENABLE_SELF_SIGNUP), anyString()))
                    .thenReturn("true");
            mockedUtils.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);

            when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                    .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

            Claim claim = new Claim();
            claim.setClaimUri(emailVerifiedClaimURI);
            claim.setValue("true");
            Claim[] claims = new Claim[]{claim};

            NotificationResponseBean notificationResponseBean =
                    userSelfRegistrationManager.registerUser(user, "test-pwd", claims,
                            new Property[]{property});

            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getCode(),
                    notificationResponseBean.getCode());
            User registeredUser = notificationResponseBean.getUser();
            verify(abstractUserStoreManager).addUserWithID(anyString(), anyString(), any(), any(), isNull());
            assertTrue(registeredUser instanceof ResolvedUser);
            assertEquals(((ResolvedUser) registeredUser).getUserId(), TEST_USER_ID);
            verify(consentManger).addConsent(any());
            verify(identityEventService, atLeastOnce()).handleEvent(any());
        }
    }

    @Test(dataProvider = "WorkflowErrorProvider")
    public void testRegisterUserWorkflowErrors(WorkflowErrorConstants.ErrorMessages errorMessage)
            throws org.wso2.carbon.user.core.UserStoreException, NotificationChannelManagerException {

        User user = getUser();
        Property property = new Property(IdentityRecoveryConstants.Consent.CONSENT, consentData);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
        when(abstractUserStoreManager.addUserWithID(anyString(), anyString(), any(), any(), isNull()))
                .thenThrow(new org.wso2.carbon.user.core.UserStoreException(
                        errorMessage.getMessage(), errorMessage.getCode()));
        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ACCOUNT_LOCK_ON_CREATION), anyString()))
                    .thenReturn(Boolean.TRUE.toString());
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                    .thenReturn(Boolean.TRUE.toString());
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(ENABLE_SELF_SIGNUP), anyString()))
                    .thenReturn(Boolean.TRUE.toString());
            mockedUtils.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);

            when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                    .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

            userSelfRegistrationManager.registerUser(user, "test-pwd", new Claim[0], new Property[] {property});
            fail("Expected an IdentityRecoveryServerException to be thrown.");
        } catch (IdentityRecoveryException e) {
            assertTrue(e.getMessage().contains(errorMessage.getMessage()));
            assertEquals(e.getErrorCode(), errorMessage.getCode());
        }
        verify(abstractUserStoreManager).addUserWithID(anyString(), anyString(), any(), any(), isNull());
    }

    @DataProvider(name = "WorkflowErrorProvider")
    public Object[][] workflowErrorProvider() {

        return new Object[][] {
                {ERROR_CODE_USER_WF_ALREADY_EXISTS},
                {ERROR_CODE_USER_WF_USER_ALREADY_EXISTS}
        };
    }

    @Test
    public void testIsUserConfirmed() throws IdentityRecoveryException {

        User user = new User();
        user.setUserName(TEST_USER_NAME);
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any())).thenReturn(userRecoveryData);

        // SELF_SIGN_UP scenario.
        boolean isUserConfirmed = userSelfRegistrationManager.isUserConfirmed(user);
        assertFalse(isUserConfirmed);
    }

    @Test
    public void testConfirmUserSelfRegistration() throws IdentityRecoveryException, UserStoreException {

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        when(userRecoveryDataStore.load(anyString())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        userSelfRegistrationManager.confirmUserSelfRegistration(TEST_CODE);

        ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
        verify(userStoreManager, atLeastOnce()).setUserClaimValues(anyString(), claimsCaptor.capture(), isNull());

        Map<String, String> capturedClaims = claimsCaptor.getValue();
        String updatedAccountLockedClaim =
                capturedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
        String updatedEmailVerifiedClaim =
                capturedClaims.get(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM);
        assertEquals(updatedAccountLockedClaim, Boolean.FALSE.toString());
        assertEquals(updatedEmailVerifiedClaim, Boolean.TRUE.toString());

        // Case 2: Invalid Tenant.
        User user1 = getUser();
        user1.setTenantDomain("invalid");
        userRecoveryData = new UserRecoveryData(user1, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        when(userRecoveryDataStore.load(anyString())).thenReturn(userRecoveryData);
        try {
            userSelfRegistrationManager.confirmUserSelfRegistration(TEST_CODE);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }

        // Case 3: Invalid recovery step.
        userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.UPDATE_PASSWORD);
        when(userRecoveryDataStore.load(anyString())).thenReturn(userRecoveryData);
        try {
            userSelfRegistrationManager.confirmUserSelfRegistration(TEST_CODE);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test
    public void testIntrospectUserSelfRegistration() throws IdentityRecoveryException {

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);
        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        String verifiedChannelType = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/emailaddress";

        UserRecoveryData resultUserRecoveryData = userSelfRegistrationManager.introspectUserSelfRegistration(TEST_CODE,
                verifiedChannelType, verifiedChannelClaim, new HashMap<>());
        User resultUser = resultUserRecoveryData.getUser();
        assertEquals(resultUser.getUserName(), user.getUserName());

        // Case 2: Provide invalid verifiedChannelType.
        String verifiedChannelType2 = "TEST";
        String verifiedChannelClaim2 = "http://wso2.org/claims/emailaddress";

        try {
            userSelfRegistrationManager.introspectUserSelfRegistration(TEST_CODE,
                    verifiedChannelType2, verifiedChannelClaim2,  new HashMap<>());
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
            assertEquals(((IdentityRecoveryException) e).getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_VERIFICATION_CHANNEL.getCode());
        }
    }

    @Test
    public void testIsValidTenantDomain() throws UserStoreException, IdentityRecoveryException {

        // Case 1: Valid tenant domain.
        when(tenantManager.getTenantId(eq(TEST_TENANT_DOMAIN_NAME))).thenReturn(TEST_TENANT_ID);

        boolean isValid = userSelfRegistrationManager.isValidTenantDomain(TEST_TENANT_DOMAIN_NAME);
        assertTrue(isValid);

        // Case 2: Invalid tenant domain
        String invalidTenantDomain = "INVALID_TENANT";
        when(realmService.getTenantManager().getTenantId(eq(invalidTenantDomain)))
                .thenThrow(new UserStoreException());
        try {
            userSelfRegistrationManager.isValidTenantDomain("INVALID_TENANT");
        } catch (IdentityRecoveryException e) {
            assertTrue(StringUtils.contains(e.getMessage(), invalidTenantDomain));
        }
    }

    @Test
    public void testIsValidUserStoreDomain() throws IdentityRecoveryException, UserStoreException {

        // Case 1: Valid user store domain.
        when(tenantManager.getTenantId(eq(TEST_TENANT_DOMAIN_NAME))).thenReturn(TEST_TENANT_ID);
        when(userStoreManager.getSecondaryUserStoreManager(eq(TEST_USERSTORE_DOMAIN)))
                .thenReturn(userStoreManager);

        boolean isValid = userSelfRegistrationManager.isValidUserStoreDomain(
                TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME);
        assertTrue(isValid);

        // Case 2: Throws UserStoreException.
        when(userRealm.getUserStoreManager()).thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            userSelfRegistrationManager.isValidUserStoreDomain(TEST_USERSTORE_DOMAIN, TEST_TENANT_DOMAIN_NAME);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }
    }

    @Test
    public void testIsUsernameAlreadyTaken() throws UserStoreException, IdentityRecoveryException {

        mockedMultiTenantUtils.when(() ->
                MultitenantUtils.getTenantDomain(anyString())).thenReturn(TEST_TENANT_DOMAIN_NAME);
        mockedMultiTenantUtils.when(() -> MultitenantUtils.getTenantAwareUsername(anyString()))
                .thenReturn(TEST_USER_NAME);

        when(userStoreManager.isExistingUser(anyString())).thenReturn(true);

        boolean isUsernameAlreadyTaken = userSelfRegistrationManager.isUsernameAlreadyTaken(TEST_USER_NAME);
        assertTrue(isUsernameAlreadyTaken);

        // Case 2: Has pending workflow.
        when(userStoreManager.isExistingUser(anyString())).thenReturn(false);
        when(userWorkflowManagementService.isUserExists(anyString(), anyString())).thenReturn(true);

        boolean isUsernameAlreadyTaken2 = userSelfRegistrationManager.isUsernameAlreadyTaken(TEST_USER_NAME);
        assertTrue(isUsernameAlreadyTaken2);

        // Case 3: Throw UserStoreException.
        when(userRealm.getUserStoreManager()).thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            userSelfRegistrationManager.isUsernameAlreadyTaken(TEST_USER_NAME);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }
    }

    @Test
    public void testIsSelfRegistrationEnabled() throws Exception{

        mockConfigurations("true", "true");
        boolean isSelfRegistrationEnabled = userSelfRegistrationManager
                .isSelfRegistrationEnabled(TEST_TENANT_DOMAIN_NAME);
        assertTrue(isSelfRegistrationEnabled);

        // Case 2: Throw Governance Exception.
        when(identityGovernanceService.getConfiguration(any(), anyString()))
                .thenThrow(new IdentityGovernanceException("test error"));
        try {
            userSelfRegistrationManager.isSelfRegistrationEnabled(TEST_TENANT_DOMAIN_NAME);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }
    }

    @Test
    public void testIsMatchUserNameRegex()
            throws IdentityRecoveryException, InputValidationMgtException,
            UserStoreException {

        mockedMultiTenantUtils.when(() -> MultitenantUtils
                .getTenantAwareUsername(anyString())).thenReturn(TEST_USER_NAME);
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(anyString())).thenReturn(TEST_USER_NAME);
        mockedIdentityUtil.when(() -> IdentityUtil.extractDomainFromName(anyString()))
                .thenReturn(TEST_USERSTORE_DOMAIN);
        when(realmConfiguration.isPrimary()).thenReturn(true);
        when(realmConfiguration.getTenantId()).thenReturn(TEST_TENANT_ID);
        mockedIdentityUtil.when(() -> IdentityTenantUtil.getTenantDomain(TEST_TENANT_ID))
                .thenReturn(TEST_TENANT_DOMAIN_NAME);

        when(inputValidationManagementService.getInputValidationConfiguration(anyString()))
                .thenReturn(Arrays.asList(validationConfiguration));
        when(validationConfiguration.getField()).thenReturn("username");
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(Constants.INPUT_VALIDATION_USERNAME_ENABLED_CONFIG))
                .thenReturn("false");
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG_EX))
                .thenReturn(null);
        boolean isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        assertTrue(isMatchUsernameRegex);

        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG_EX))
                .thenReturn("^[\\S]{5,30}$");

        isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        assertTrue(isMatchUsernameRegex);

        /*
         Case 2: INPUT_VALIDATION_USERNAME_ENABLED_CONFIG enabled.
         */
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(Constants.INPUT_VALIDATION_USERNAME_ENABLED_CONFIG))
                .thenReturn("true");

        Map<String, Validator> validators = new HashMap<>();
        Validator validator = mock(Validator.class);
        validators.put("MockValidator", validator);
        when(inputValidationManagementService.getValidators(anyString())).thenReturn(validators);

        RulesConfiguration rulesConfiguration = mock(RulesConfiguration.class);
        when(rulesConfiguration.getValidatorName()).thenReturn("MockValidator");
        when(rulesConfiguration.getProperties()).thenReturn(new HashMap<>());
        when(validationConfiguration.getRegEx()).thenReturn(Arrays.asList(rulesConfiguration));

         isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        assertTrue(isMatchUsernameRegex);

        /*
         Case 3: Throw UserStoreException while getting user store manager.
         */
        when(userRealm.getUserStoreManager()).thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }

        /*
         Case 4: Throw Error while getting tenant id.
         */
        when(tenantManager.getTenantId(anyString())).thenThrow(new UserStoreException());
        try {
            userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryException);
        }
    }

    @Test
    public void testIsMatchUserNameRegexForSecondaryUserStore()
            throws IdentityRecoveryException, InputValidationMgtException,
            UserStoreException {

        mockedMultiTenantUtils.when(() -> MultitenantUtils
                .getTenantAwareUsername(eq(TEST_USER_NAME))).thenReturn(TEST_USER_NAME);
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(eq(TEST_USER_NAME))).thenReturn(TEST_USER_NAME);
        mockedIdentityUtil.when(() -> IdentityUtil.extractDomainFromName(anyString()))
                .thenReturn(TEST_SECONDARY_USERSTORE_DOMAIN);
        when(realmConfiguration.getTenantId()).thenReturn(TEST_TENANT_ID);
        mockedIdentityUtil.when(() -> IdentityTenantUtil.getTenantDomain(TEST_TENANT_ID))
                .thenReturn(TEST_TENANT_DOMAIN_NAME);

        when(inputValidationManagementService.getInputValidationConfiguration(anyString()))
                .thenReturn(Arrays.asList(validationConfiguration));
        when(validationConfiguration.getField()).thenReturn("username");
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(Constants.INPUT_VALIDATION_USERNAME_ENABLED_CONFIG))
                .thenReturn("false");
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG_EX))
                .thenReturn(null);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG))
                .thenReturn("^[\\S]{5,30}$");

        // regex read from fallback key
        boolean isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        assertTrue(isMatchUsernameRegex);

        mockedMultiTenantUtils.when(() -> MultitenantUtils
                .getTenantAwareUsername(eq(TEST_INVALID_USER_NAME))).thenReturn(TEST_INVALID_USER_NAME);
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(eq(TEST_INVALID_USER_NAME)))
                .thenReturn(TEST_INVALID_USER_NAME);
        isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_INVALID_USER_NAME);
        assertFalse(isMatchUsernameRegex);

        // regex read main key
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG_EX))
                .thenReturn("^[\\S]{5,30}$");
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG))
                .thenReturn(null);
        isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_USER_NAME);
        assertTrue(isMatchUsernameRegex);

        mockedMultiTenantUtils.when(() -> MultitenantUtils
                .getTenantAwareUsername(eq(TEST_INVALID_USER_NAME))).thenReturn(TEST_INVALID_USER_NAME);
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(eq(TEST_INVALID_USER_NAME)))
                .thenReturn(TEST_INVALID_USER_NAME);
        isMatchUsernameRegex =
                userSelfRegistrationManager.isMatchUserNameRegex(TEST_TENANT_DOMAIN_NAME, TEST_INVALID_USER_NAME);
        assertFalse(isMatchUsernameRegex);

    }

    @Test
    public void testPreValidatePasswordWithConfirmationKey() throws Exception {

        String confirmationKey = "testConfirmationKey";
        String password = "testPassword";
        User user = getUser();

        UserRecoveryData recoveryData = new UserRecoveryData(user, confirmationKey,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);
        when(userRecoveryDataStore.load(confirmationKey)).thenReturn(recoveryData);

        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(TEST_USERSTORE_DOMAIN)).thenReturn(userStoreManager);

        userSelfRegistrationManager.preValidatePasswordWithConfirmationKey(confirmationKey, password);
        verify(identityEventService).handleEvent(any(Event.class));

        // Case 2: Throws UserStoreException.
        when(userRealm.getUserStoreManager()).thenThrow(new  org.wso2.carbon.user.core.UserStoreException());
        try {
            userSelfRegistrationManager.preValidatePasswordWithConfirmationKey(confirmationKey, password);
        } catch(Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testPreValidatePasswordWithUsername() throws Exception {

        String password = "testPassword";
        String username = TEST_USERSTORE_DOMAIN + CarbonConstants.DOMAIN_SEPARATOR + TEST_USER_NAME;
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(username)).thenReturn(TEST_USER_NAME);

        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getSecondaryUserStoreManager(TEST_USERSTORE_DOMAIN)).thenReturn(userStoreManager);

        userSelfRegistrationManager.preValidatePasswordWithUsername(username, password);
        verify(identityEventService).handleEvent(any(Event.class));
    }

    @Test
    public void testRegisterLiteUser() throws Exception {

        String emailVerifiedClaimURI = "http://wso2.org/claims/identity/emailVerified";
        User user = new User();
        user.setUserName(TEST_USER_NAME);

        Claim claim1 = new Claim();
        claim1.setClaimUri("http://wso2.org/claims/emailaddress");
        claim1.setValue("test@example.com");

        Claim[] claims = new Claim[]{claim1};

        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(
                        eq(IdentityRecoveryConstants.ConnectorConfig
                                .ENABLE_ACCOUNT_LOCK_FOR_VERIFIED_PREFERRED_CHANNEL)))
                .thenReturn("false");
        when(consentUtilityService.filterPIIsFromReceipt(any(), any())).thenReturn(
                new HashSet<>(Arrays.asList(emailVerifiedClaimURI)));

        Property[] properties = new Property[]{
                new Property(IdentityRecoveryConstants.Consent.CONSENT, consentData)
        };
        when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), anyMap()))
                .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(Utils::getNotificationChannelManager).thenReturn(notificationChannelManager);
            mockedUtils.when(() -> Utils.getCallbackURLFromRegistration(any())).thenReturn("https://test.com");
            mockedUtils.when(() -> Utils.validateCallbackURL(anyString(), anyString(), anyString()))
                    .thenReturn(true);
            mockedUtils.when(() -> Utils.generateRandomPassword(anyInt())).thenReturn("test-pwd-123".toCharArray());
            mockedUtils.when(() -> Utils.handleClientException(any(), anyString()))
                    .thenThrow(new IdentityRecoveryClientException("error-code", "message"));
            mockedUtils.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                            anyString(), any()))
                    .thenReturn(new IdentityRecoveryClientException("error-code", "message"));
            mockedUtils.when(() -> Utils.handleServerException(any(), anyString()))
                    .thenThrow(new IdentityRecoveryServerException("error-code", "message"));
            mockedUtils.when(() -> Utils.handleServerException(any(IdentityRecoveryConstants.ErrorMessages.class),
                            anyString(), any()))
                    .thenReturn(new IdentityRecoveryServerException("error-code", "message"));

            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP),
                    anyString())).thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(
                    IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION),
                    anyString())).thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE),
                    anyString())).thenReturn("true");

            /*
             Case 1: Notification internally managed. Account Lock on creation enabled.
             */
            NotificationResponseBean response = userSelfRegistrationManager.registerLiteUser(user, claims, properties);

            verify(userStoreManager).addUser(anyString(), any(), any(), anyMap(), isNull());
            assertNotNull(response);
            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION.getCode(),
                    response.getCode());
            assertEquals(NotificationChannels.EMAIL_CHANNEL.getChannelType(), response.getNotificationChannel());

            /*
             Case 2: Lite sign-up disabled.
             Expected: Client error should be thrown.
             */
            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP),
                    anyString())).thenReturn("false");

            try {
                userSelfRegistrationManager.registerLiteUser(user, claims, properties);
            } catch (Exception e) {
                assertTrue(e instanceof IdentityRecoveryClientException);
            }

            /*
              Case 3: Preferred channel is verified.
             */
            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP),
                    anyString())).thenReturn("true");

            Claim claim2 = new Claim();
            claim2.setClaimUri(emailVerifiedClaimURI);
            claim2.setValue("true");
            claims = new Claim[]{claim1, claim2};

            response = userSelfRegistrationManager.registerLiteUser(user, claims, properties);

            assertNotNull(response);
            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getCode(),
                    response.getCode());

            /*
              Case 4: Notification externally managed. Account Lock on creation enabled.
             */
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(
                            IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION),
                    anyString())).thenReturn("true");
            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE),
                    anyString())).thenReturn("false");
            claims = new Claim[]{claim1};

            response = userSelfRegistrationManager.registerLiteUser(user, claims, properties);
            assertNotNull(response);
            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getCode(),
                    response.getCode());

            /*
              Case 5: Account Lock on creation is disabled.
             */
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(
                            IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION),
                    anyString())).thenReturn("false");
            mockedUtils.when(() -> Utils.getSignUpConfigs(
                    eq(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE),
                    anyString())).thenReturn("true");
            claims = new Claim[]{claim1};

            response = userSelfRegistrationManager.registerLiteUser(user, claims, properties);
            assertNotNull(response);
            assertEquals(IdentityRecoveryConstants.SuccessEvents
                            .SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION.getCode(),
                    response.getCode());

            /*
             Case 6: Invalid callback URL.
             */
            mockedUtils.when(() -> Utils.validateCallbackURL(anyString(), anyString(), anyString()))
                    .thenReturn(false);
            try {
                userSelfRegistrationManager.registerLiteUser(user, claims, properties);
            } catch (Exception e) {
                assertTrue(e instanceof IdentityRecoveryServerException);
            }

            mockedUtils.when(() -> Utils.validateCallbackURL(anyString(), anyString(), anyString()))
                    .thenReturn(true);
            /*
             Case 7: Throw UserStoreException while adding user.
             */
            doThrow(new org.wso2.carbon.user.core.UserStoreException("test-msg"))
                    .when(userStoreManager).addUser(anyString(), any(), any(), anyMap(), isNull());
            try {
                userSelfRegistrationManager.registerLiteUser(user, claims, properties);
            } catch (Exception e) {
                assertTrue(e instanceof IdentityRecoveryServerException);
            }
        }
    }

    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void registerUserInvalidOrganizationEmailDomain()
            throws IdentityRecoveryException, org.wso2.carbon.user.api.UserStoreException {

        try (MockedStatic<Utils> utilsMockedStatic = mockStatic(Utils.class)) {
            utilsMockedStatic.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                    isNull(), any())).thenReturn(IdentityException.error(IdentityRecoveryClientException.class,
                    "err-code", ""));
            utilsMockedStatic.when(() -> Utils.getSignUpConfigs(anyString(), anyString())).thenReturn("true");
            when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
            when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
            // Mock addUser to throw UserStoreException.
            doThrow(new org.wso2.carbon.user.core.UserStoreException("Simulated exception", "ORG-60091"))
                    .when(userStoreManager)
                    .isExistingRole(anyString());
            userSelfRegistrationManager.registerUser(new User(), "", new Claim[]{}, null);
        }
    }

    @DataProvider(name = "accountStateClaimTestData")
    private Object[][] getAccountStateClaimTestData() {
        return new Object[][]{
                // isAccountStateClaimExists, expectedAccountStateClaim
                {true, "UNLOCKED"},
                {false, null}
        };
    }

    @Test(description = "Verify that getConfirmedSelfRegisteredUser() sets the account state claim correctly upon " +
            "successful account confirmation.", dataProvider = "accountStateClaimTestData")
    public void testGetConfirmedSelfRegisteredUserAccountStateClaim(boolean isAccountStateClaimExists,
                                                                    String expectedAccountStateClaim) throws Exception {

        String verifiedChannelType = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String verifiedChannelClaim = "http://wso2.org/claims/emailaddress";
        Map<String, String> metaProperties = new HashMap<>();

        User user = getUser();
        UserRecoveryData userRecoveryData = new UserRecoveryData(user, TEST_RECOVERY_DATA_STORE_SECRET,
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

        when(userRecoveryDataStore.load(eq(TEST_CODE))).thenReturn(userRecoveryData);
        when(userRecoveryDataStore.load(eq(TEST_CODE), anyBoolean())).thenReturn(userRecoveryData);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN_NAME);

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.isAccountStateClaimExisting(anyString()))
                    .thenReturn(isAccountStateClaimExists);
            mockedUtils.when(() -> Utils.getSignUpConfigs(eq(NOTIFICATION_INTERNALLY_MANAGE), anyString()))
                    .thenReturn("true");

            if (isAccountStateClaimExists) {
                Map<String, String> claims = new HashMap<>();
                claims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI, expectedAccountStateClaim);
                when(userStoreManager.getUserClaimValues(anyString(), any(), anyString())).thenReturn(claims);
            }

            org.wso2.carbon.identity.application.common.model.Property property =
                    new org.wso2.carbon.identity.application.common.model.Property();
            org.wso2.carbon.identity.application.common.model.Property[] testProperties =
                    new org.wso2.carbon.identity.application.common.model.Property[]{property};

            when(identityGovernanceService.getConfiguration(any(), anyString())).thenReturn(testProperties);
            when(privilegedCarbonContext.getOSGiService(eq(NotificationChannelManager.class), isNull()))
                    .thenReturn(notificationChannelManager);
            when(notificationChannelManager.resolveCommunicationChannel(anyString(), anyString(), anyString(), any()))
                    .thenReturn(NotificationChannels.EMAIL_CHANNEL.getChannelType());

            User confirmedUser = userSelfRegistrationManager.getConfirmedSelfRegisteredUser(TEST_CODE,
                    verifiedChannelType, verifiedChannelClaim, metaProperties);

            assertEquals(confirmedUser.getUserName(), user.getUserName());

            ArgumentCaptor<Map<String, String>> claimsCaptor = ArgumentCaptor.forClass(Map.class);
            verify(userStoreManager, atLeastOnce()).setUserClaimValues(
                    anyString(),
                    claimsCaptor.capture(),
                    isNull()
            );

            // Get the captured claims and verify them
            Map<String, String> updatedClaims = claimsCaptor.getValue();
            if (isAccountStateClaimExists) {
                assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                        expectedAccountStateClaim, "Account state should match expected value");
            } else {
                assertFalse(updatedClaims.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI),
                        "Account state claim should not be present when claim does not exist");
            }
            assertEquals(updatedClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM),
                    Boolean.FALSE.toString(), "Account locked claim should be set to false");
            assertEquals(updatedClaims.get(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM),
                    Boolean.TRUE.toString(), "Email verified claim should be set to true");
        }
    }

    private User getUser() {

        User user = new User();
        user.setUserName(TEST_USER_NAME);
        user.setUserStoreDomain(TEST_USERSTORE_DOMAIN);
        user.setTenantDomain(TEST_TENANT_DOMAIN_NAME);
        return user;
    }

    private org.wso2.carbon.user.core.common.User getRegisteredUser() {

        org.wso2.carbon.user.core.common.User user = new org.wso2.carbon.user.core.common.User();
        user.setUsername(TEST_USER_NAME);
        user.setUserStoreDomain(TEST_USERSTORE_DOMAIN);
        user.setTenantDomain(TEST_TENANT_DOMAIN_NAME);
        user.setUserID(TEST_USER_ID);
        return user;
    }

    private void mockMultiAttributeEnabled(Boolean isEnabled) throws ClaimMetadataException {

        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(
                eq(IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER)))
                .thenReturn(isEnabled.toString());

        org.wso2.carbon.identity.application.common.model.Property emailVerificationOnUpdate =
                new org.wso2.carbon.identity.application.common.model.Property();
        emailVerificationOnUpdate.setName(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        emailVerificationOnUpdate.setValue(Boolean.toString(isEnabled));
        try {
            when(identityGovernanceService.getConfiguration(
                    eq(new String[]{IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE}),
                    eq(TEST_TENANT_DOMAIN_NAME))).thenReturn(
                            new org.wso2.carbon.identity.application.common.model.Property[]{emailVerificationOnUpdate});
        } catch (IdentityGovernanceException e) {
            throw new RuntimeException(e);
        }

        if (!isEnabled) {
            return;
        }

        // Mock ClaimMetadataManagementService.
        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService())
                .thenReturn(claimMetadataManagementService);

        List<LocalClaim> localClaims = new ArrayList<>();
        Map<String, String> claimProperties = new HashMap<>();
        claimProperties.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());

        LocalClaim mobileNumbersClaim = new LocalClaim(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
        mobileNumbersClaim.setClaimProperties(claimProperties);
        LocalClaim verifiedMobileNumbersClaim = new LocalClaim(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
        verifiedMobileNumbersClaim.setClaimProperties(claimProperties);
        LocalClaim emailAddressesClaim = new LocalClaim(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
        emailAddressesClaim.setClaimProperties(claimProperties);
        LocalClaim verifiedEmailAddressesClaim =
                new LocalClaim(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        verifiedEmailAddressesClaim.setClaimProperties(claimProperties);

        localClaims.add(verifiedMobileNumbersClaim);
        localClaims.add(mobileNumbersClaim);
        localClaims.add(emailAddressesClaim);
        localClaims.add(verifiedEmailAddressesClaim);

        doReturn(localClaims).when(claimMetadataManagementService).getLocalClaims(TEST_TENANT_DOMAIN_NAME);
    }

    private void mockGetUserClaimValue(String claimUri, String claimValue) throws UserStoreException {

        when(userStoreManager.getUserClaimValue(any(), eq(claimUri), any())).thenReturn(claimValue);
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
