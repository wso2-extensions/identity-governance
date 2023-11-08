
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
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

@WithCarbonHome
public class UserSelfRegistrationManagerTest {

    private UserSelfRegistrationManager userSelfRegistrationManager = UserSelfRegistrationManager.getInstance();
    private ReceiptInput resultReceipt;
    private String TEST_TENANT_DOMAIN_NAME = "carbon.super";
    private String TEST_USERSTORE_DOMAIN = "PRIMARY";
    private IdentityProviderManager identityProviderManager;

    @Mock
    UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    IdentityEventService identityEventService;

    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityProviderManager> mockedIdentityProviderManager;

    @BeforeMethod
    public void setUp() {

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedUtils = Mockito.mockStatic(Utils.class);
        mockedJDBCRecoveryDataStore = Mockito.mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityProviderManager = Mockito.mockStatic(IdentityProviderManager.class);
        identityProviderManager = Mockito.mock(IdentityProviderManager.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedUtils.close();
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

        mockedUtils.when(() -> Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                TEST_TENANT_DOMAIN_NAME)).thenReturn(enableSelfSignUp);
        mockedUtils.when(()->Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                TEST_TENANT_DOMAIN_NAME)).thenReturn(enableInternalNotifications);
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
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(), any())).
                thenReturn(userRecoveryData);
        doNothing().when(userRecoveryDataStore).invalidate(ArgumentMatchers.anyString());
        doNothing().when(userRecoveryDataStore).store(any(UserRecoveryData.class));
    }

    /**
     * Mock email triggering.
     *
     * @throws IdentityEventException If an error occurred while mocking identityEventService.
     */
    private void mockEmailTrigger() throws IdentityEventException {

        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
        doNothing().when(identityEventService).handleEvent(any(Event.class));
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