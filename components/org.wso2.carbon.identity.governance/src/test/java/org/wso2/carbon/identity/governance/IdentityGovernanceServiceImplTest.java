/*
 * Copyright (c) 2024-2025, WSO2 LLC. (https://www.wso2.org)
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.governance;

import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.organization.management.service.OrganizationManager;
import org.wso2.carbon.identity.organization.management.service.exception.OrganizationManagementException;
import org.wso2.carbon.identity.organization.management.service.util.OrganizationManagementUtil;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.application.common.model.Property;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.assertThrows;
import static org.testng.AssertJUnit.assertEquals;

@WithCarbonHome
public class IdentityGovernanceServiceImplTest {

    // Constants.
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String TRUE_STRING = "true";
    private static final String FALSE_STRING = "false";
    private static final String USERNAME_RECOVERY_ENABLE = "Recovery.Notification.Username.Enable";
    private static final String USERNAME_RECOVERY_EMAIL_ENABLE = "Recovery.Notification.Username.Email.Enable";
    private static final String USERNAME_RECOVERY_SMS_ENABLE = "Recovery.Notification.Username.SMS.Enable";
    private static final String PASSWORD_RECOVERY_ENABLE = "Recovery.Notification.Password.Enable";
    private static final String PASSWORD_RECOVERY_EMAIL_LINK_ENABLE
            = "Recovery.Notification.Password.emailLink.Enable";
    private static final String PASSWORD_RECOVERY_EMAIL_OTP_ENABLE =
            "Recovery.Notification.Password.OTP.SendOTPInEmail";
    private static final String PASSWORD_RECOVERY_SMS_OTP_ENABLE = "Recovery.Notification.Password.smsOtp.Enable";
    private static final String APP_RESIDENT_ORG_ID = "bc365db3-5644-4207-9fee-86f82544d388";
    private static final String APP_RESIDENT_TENANT_DOMAIN = "app-resident.org";

    @Mock
    IdentityMgtServiceDataHolder identityMgtServiceDataHolder;

    @Mock
    IdpManager idpManager;

    @Mock
    IdentityProvider identityProvider;

    @Mock
    PrivilegedCarbonContext privilegedCarbonContext;

    @Mock
    OrganizationManager organizationManager;

    MockedStatic<IdentityMgtServiceDataHolder> identityMgtServiceDataHolderMockedStatic;
    MockedStatic<OrganizationManagementUtil> organizationManagementUtilMockedStatic;
    MockedStatic<IdentityMgtConstants.INHERITED_MASKED_CONNECTORS> inheritedMaskedConnectorsMockedStatic;
    MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;

    private IdentityGovernanceServiceImpl identityGovernanceService;

    @BeforeMethod
    public void setup() throws IdentityProviderManagementException {

        MockitoAnnotations.openMocks(this);
        identityMgtServiceDataHolderMockedStatic = mockStatic(IdentityMgtServiceDataHolder.class);
        identityMgtServiceDataHolderMockedStatic.when(IdentityMgtServiceDataHolder::
                getInstance).thenReturn(identityMgtServiceDataHolder);
        when(identityMgtServiceDataHolder.getIdpManager()).thenReturn(idpManager);
        when(identityMgtServiceDataHolder.getOrganizationManager()).thenReturn(organizationManager);
        when(idpManager.getResidentIdP(TENANT_DOMAIN)).thenReturn(identityProvider);

        organizationManagementUtilMockedStatic = mockStatic(OrganizationManagementUtil.class);
        organizationManagementUtilMockedStatic.when(() -> OrganizationManagementUtil.isOrganization(TENANT_DOMAIN))
                .thenReturn(false);

        inheritedMaskedConnectorsMockedStatic = mockStatic(IdentityMgtConstants.INHERITED_MASKED_CONNECTORS.class);

        FederatedAuthenticatorConfig[] authenticatorConfigs = new FederatedAuthenticatorConfig[0];
        when(identityProvider.getFederatedAuthenticatorConfigs()).thenReturn(authenticatorConfigs);

        identityGovernanceService = new IdentityGovernanceServiceImpl();

        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
    }

    @AfterMethod
    public void tearDown() {

        identityMgtServiceDataHolderMockedStatic.close();
        organizationManagementUtilMockedStatic.close();
        inheritedMaskedConnectorsMockedStatic.close();
        mockedPrivilegedCarbonContext.close();
    }

    @Test(dataProvider = "updateConfigurations")
    public void testUpdateConfiguration(Map<String, String> configurationDetails,
                                        IdentityProviderProperty[] identityProviderProperties,
                                        Map<String, String> expected) throws IdentityGovernanceException {

        when(identityProvider.getIdpProperties()).thenReturn(identityProviderProperties);

        identityGovernanceService.updateConfiguration(TENANT_DOMAIN, configurationDetails);

        // Capture the arguments passed to setIdpProperties
        ArgumentCaptor<IdentityProviderProperty[]>
                argumentCaptor = ArgumentCaptor.forClass(IdentityProviderProperty[].class);
        verify(identityProvider).setIdpProperties(argumentCaptor.capture());

        // Assert
        IdentityProviderProperty[] capturedProperties = argumentCaptor.getValue();
        for (IdentityProviderProperty capturedProperty : capturedProperties) {
            assertEquals(expected.get(capturedProperty.getName()), capturedProperty.getValue());
        }

    }

    @Test
    public void testDeleteConfiguration() throws Exception {

        List<String> propertyNames = Arrays.asList("prop1", "prop2");
        identityGovernanceService.deleteConfiguration(propertyNames, TENANT_DOMAIN);
        verify(idpManager).deleteResidentIdpProperties(propertyNames, TENANT_DOMAIN);
    }

    @Test
    public void testDeleteConfigurationThrowsIdentityGovernanceException() throws Exception {

        List<String> propertyNames = Arrays.asList("prop1", "prop2");
        doThrow(new IdentityProviderManagementException("error"))
                .when(idpManager).deleteResidentIdpProperties(propertyNames, TENANT_DOMAIN);
        assertThrows(IdentityGovernanceException.class, () ->
                identityGovernanceService.deleteConfiguration(propertyNames, TENANT_DOMAIN));
    }

    @Test
    public void testGetConnectorListWithConfigsForMaskedProperties() throws Exception {

        organizationManagementUtilMockedStatic.when(() -> OrganizationManagementUtil.isOrganization(TENANT_DOMAIN))
                .thenReturn(true);

        IdentityConnectorConfig mockConnector = mock(IdentityConnectorConfig.class);
        when(mockConnector.getName()).thenReturn("TestMaskedConnector");
        when(mockConnector.getFriendlyName()).thenReturn("Test Masked Connector");
        when(mockConnector.getCategory()).thenReturn("Security");
        when(mockConnector.getSubCategory()).thenReturn("Masking");
        when(mockConnector.getOrder()).thenReturn(1);
        when(mockConnector.getPropertyNames()).thenReturn(new String[]{"maskableProp1", "unmaskedProp2"});

        Map<String, String> friendlyNames = new HashMap<>();
        friendlyNames.put("maskableProp1", "Maskable Property 1");
        friendlyNames.put("unmaskedProp2", "Unmasked Property 2");
        when(mockConnector.getPropertyNameMapping()).thenReturn(friendlyNames);

        Map<String, String> descriptions = new HashMap<>();
        descriptions.put("maskableProp1", "This is a sensitive property.");
        descriptions.put("unmaskedProp2", "This is a normal property.");
        when(mockConnector.getPropertyDescriptionMapping()).thenReturn(descriptions);

        when(mockConnector.getMetaData()).thenReturn(Collections.emptyMap());
        when(mockConnector.getConfidentialPropertyValues(TENANT_DOMAIN)).thenReturn(Collections.emptyList());

        List<IdentityConnectorConfig> connectorConfigList = Collections.singletonList(mockConnector);
        when(identityMgtServiceDataHolder.getIdentityGovernanceConnectorList()).thenReturn(connectorConfigList);

        IdentityProviderProperty idpProp1 = new IdentityProviderProperty();
        idpProp1.setName("maskableProp1");
        idpProp1.setValue("sensitiveData");

        IdentityProviderProperty idpProp2 = new IdentityProviderProperty();
        idpProp2.setName("unmaskedProp2");
        idpProp2.setValue("normalData");

        when(identityProvider.getIdpProperties()).thenReturn(new IdentityProviderProperty[]{idpProp1, idpProp2});

        IdentityMgtConstants.INHERITED_MASKED_CONNECTORS mockEnumInstance =
                mock(IdentityMgtConstants.INHERITED_MASKED_CONNECTORS.class);
        when(mockEnumInstance.getMaskableProperties()).thenReturn(Collections.singletonList("maskableProp1"));

        inheritedMaskedConnectorsMockedStatic.when(() -> IdentityMgtConstants.INHERITED_MASKED_CONNECTORS.findByName(anyString()))
                .thenReturn(Optional.empty());
        inheritedMaskedConnectorsMockedStatic.when(() -> IdentityMgtConstants.INHERITED_MASKED_CONNECTORS.findByName("TestMaskedConnector"))
                .thenReturn(Optional.of(mockEnumInstance));

        List<ConnectorConfig> result = identityGovernanceService.getConnectorListWithConfigs(TENANT_DOMAIN);

        assertNotNull(result, "Result list should not be null");
        assertEquals("Result list should contain one connector", 1, result.size());

        ConnectorConfig resultConnector = result.get(0);
        assertEquals("Connector name mismatch", "TestMaskedConnector", resultConnector.getName());

        Property[] resultProps = resultConnector.getProperties();
        assertEquals("Connector should have two properties", 2, resultProps.length);

        boolean maskablePropFound = false;
        boolean unmaskedPropFound = false;

        for (Property p : resultProps) {
            if ("maskableProp1".equals(p.getName())) {
                assertEquals("Maskable property value mismatch", "*************", p.getValue());
                maskablePropFound = true;
            } else if ("unmaskedProp2".equals(p.getName())) {
                assertEquals("Unmasked property value mismatch", "normalData", p.getValue());
                unmaskedPropFound = true;
            }
        }
        assertTrue(maskablePropFound, "Maskable property was not found in the result.");
        assertTrue(unmaskedPropFound, "Unmasked property was not found in the result.");
    }

    @Test
    public void testGetConnectorWithConfigsForSubOrgApps() throws Exception {

        mockPrivilegedCarbonContext();

        IdentityConnectorConfig mockConnector = mock(IdentityConnectorConfig.class);
        when(mockConnector.getName()).thenReturn("TestConnector");
        when(mockConnector.getFriendlyName()).thenReturn("Test Connector");
        when(mockConnector.getCategory()).thenReturn("Security");
        when(mockConnector.getSubCategory()).thenReturn("Masking");
        when(mockConnector.getOrder()).thenReturn(1);
        when(mockConnector.getPropertyNames()).thenReturn(new String[]{"maskableProp1", "unmaskedProp2"});

        when(mockConnector.getMetaData()).thenReturn(Collections.emptyMap());
        when(mockConnector.getConfidentialPropertyValues(TENANT_DOMAIN)).thenReturn(Collections.emptyList());

        List<IdentityConnectorConfig> connectorConfigList = Collections.singletonList(mockConnector);
        when(identityMgtServiceDataHolder.getIdentityGovernanceConnectorList()).thenReturn(connectorConfigList);

        // Only email link is true. Preconditions: all configs are false.
        IdentityProviderProperty[] passwordIdentityProps1 = getPasswordRecoveryIdentityProviderProperties(false,
                false, false, false);

        when(idpManager.getResidentIdP(APP_RESIDENT_TENANT_DOMAIN)).thenReturn(identityProvider);
        when(identityProvider.getIdpProperties()).thenReturn(passwordIdentityProps1);

        when(organizationManager.resolveTenantDomain(APP_RESIDENT_ORG_ID)).thenReturn(APP_RESIDENT_TENANT_DOMAIN);
        ConnectorConfig connectorConfig = identityGovernanceService.getConnectorWithConfigs(
                APP_RESIDENT_TENANT_DOMAIN, "TestConnector");
        assertNotNull(connectorConfig, "Connector config should not be null");
        assertEquals("Connector name mismatch", "TestConnector", connectorConfig.getName());
    }

    @Test(expectedExceptions = IdentityGovernanceException.class, expectedExceptionsMessageRegExp = "Error while " +
            "resolving tenant domain for application resident organization ID : bc365db3-5644-4207-9fee-86f82544d388")
    public void testGetConnectorWithConfigsForSubOrgAppsWithTenantResolvingException() throws Exception {

        mockPrivilegedCarbonContext();
        when(organizationManager.resolveTenantDomain(APP_RESIDENT_ORG_ID)).thenThrow(
                OrganizationManagementException.class);
        ConnectorConfig connectorConfig = identityGovernanceService.getConnectorWithConfigs(
                APP_RESIDENT_TENANT_DOMAIN, "TestConnector");
    }

    @DataProvider(name = "updateConfigurations")
    public Object[][] buildConfigurations() {

        // Only email config is true. Preconditions: all the configs false.
        Map<String, String> usernameConfig1 = new HashMap<>();
        usernameConfig1.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties1 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected1 = getUsernameRecoveryExpectedPropertyValues(true, true, false);

        // Only sms config is true. Preconditions: all the configs false.
        Map<String, String> usernameConfig2 = new HashMap<>();
        usernameConfig2.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties2 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected2 = getUsernameRecoveryExpectedPropertyValues(true, false, true);

        // Only sms is false. Preconditions: sms and username is true.
        Map<String, String> usernameConfig3 = new HashMap<>();
        usernameConfig3.put(USERNAME_RECOVERY_SMS_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties3 = getUsernameRecoveryIdentityProviderProperties(
                true, false, true);
        Map<String, String> expected3 = getUsernameRecoveryExpectedPropertyValues(false, false, false);

        // Only email is false. Preconditions: email and username is true.
        Map<String, String> usernameConfig4 = new HashMap<>();
        usernameConfig4.put(USERNAME_RECOVERY_EMAIL_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties4 = getUsernameRecoveryIdentityProviderProperties(
                true, true, false);
        Map<String, String> expected4 = getUsernameRecoveryExpectedPropertyValues(false, false, false);

        // Only email is true. Preconditions: sms and username is true.
        Map<String, String> usernameConfig5 = new HashMap<>();
        usernameConfig5.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties5 = getUsernameRecoveryIdentityProviderProperties(
                true, false, true);
        Map<String, String> expected5 = getUsernameRecoveryExpectedPropertyValues(true, true, true);

        // Only sms is true. Preconditions: email and username is true.
        Map<String, String> usernameConfig6 = new HashMap<>();
        usernameConfig6.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties6 = getUsernameRecoveryIdentityProviderProperties(
                true, true, false);
        Map<String, String> expected6 = getUsernameRecoveryExpectedPropertyValues(true, true, true);

        // Sms config true and email config false. Preconditions: all the configs false.
        Map<String, String> usernameConfig7 = new HashMap<>();
        usernameConfig7.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);
        usernameConfig7.put(USERNAME_RECOVERY_EMAIL_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties7 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected7 = getUsernameRecoveryExpectedPropertyValues(true, false, true);

        // Email config true and sms config false. Preconditions: all the configs false.
        Map<String, String> usernameConfig8 = new HashMap<>();
        usernameConfig8.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);
        usernameConfig8.put(USERNAME_RECOVERY_SMS_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties8 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected8 = getUsernameRecoveryExpectedPropertyValues(true, true, false);

        // Sms config true and email config true. Preconditions: all the configs false.
        Map<String, String> usernameConfig9 = new HashMap<>();
        usernameConfig9.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);
        usernameConfig9.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties9 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected9 = getUsernameRecoveryExpectedPropertyValues(true, true, true);

        // Only username config true. Preconditions: all the configs false.
        Map<String, String> usernameConfig10 = new HashMap<>();
        usernameConfig10.put(USERNAME_RECOVERY_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties10 = getUsernameRecoveryIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected10 = getUsernameRecoveryExpectedPropertyValues(true, true, true);

        // Only username config false. Preconditions: all the configs true.
        Map<String, String> usernameConfig11 = new HashMap<>();
        usernameConfig11.put(USERNAME_RECOVERY_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties11 = getUsernameRecoveryIdentityProviderProperties(
                true, true, true);
        Map<String, String> expected11 = getUsernameRecoveryExpectedPropertyValues(false, false, false);

        // Only email link is true. Preconditions: all configs are false.
        Map<String, String> passwordConfig1 = getPasswordRecoveryConfigs(null, true, null, null);
        IdentityProviderProperty[] passwordIdentityProps1 = getPasswordRecoveryIdentityProviderProperties(false,
                false, false, false);
        Map<String, String> passwordExpected1 = getPasswordRecoveryExpectedPropertyValues(true, true, false, false);

        // Only email otp is true. Precondition: all configs are false.
        Map<String, String> passwordConfig2 = getPasswordRecoveryConfigs(null, null, true, null);
        IdentityProviderProperty[] passwordIdentityProps2 = getPasswordRecoveryIdentityProviderProperties(false,
                false, false, false);
        Map<String, String> passwordExpected2 = getPasswordRecoveryExpectedPropertyValues(true, false, true, false);

        // Only sms otp is true. Precondition: all configs are false.
        Map<String, String> passwordConfig3 = getPasswordRecoveryConfigs(null, null, null, true);
        IdentityProviderProperty[] passwordIdentityProps3 = getPasswordRecoveryIdentityProviderProperties(false,
                false, false, false);
        Map<String, String> passwordExpected3 = getPasswordRecoveryExpectedPropertyValues(true, false, false, true);

        // Only email link is false. Precondition: password recovery and email link is true.
        Map<String, String> passwordConfigs4 = getPasswordRecoveryConfigs(null, false, null, null);
        IdentityProviderProperty[] passwordIdentityProps4 = getPasswordRecoveryIdentityProviderProperties(true, true,
                false, false);
        Map<String, String> passwordExpected4 = getPasswordRecoveryExpectedPropertyValues(false, false, false, false);

        // Only email otp is false. Precondition: password recovery and email otp is true.
        Map<String, String> passwordConfigs5 = getPasswordRecoveryConfigs(null, null, false, null);
        IdentityProviderProperty[] passwordIdentityProps5 = getPasswordRecoveryIdentityProviderProperties(true, false
                , true, false);
        Map<String, String> passwordExpected5 = getPasswordRecoveryExpectedPropertyValues(false, false, false, false);

        // Only sms otp is false. Precondition: password recovery and sms otp is true.
        Map<String, String> passwordConfigs6 = getPasswordRecoveryConfigs(null, null, null, false);
        IdentityProviderProperty[] passwordIdentityProps6 = getPasswordRecoveryIdentityProviderProperties(true, false
                , false, true);
        Map<String, String> passwordExpected6 = getPasswordRecoveryExpectedPropertyValues(false, false, false, false);

        // Only enabling the password recovery config. Precondition: all configs are false.
        Map<String, String> passwordConfigs7 = getPasswordRecoveryConfigs(true, null, null, null);
        IdentityProviderProperty[] passwordIdentityProps7 = getPasswordRecoveryIdentityProviderProperties(false, false
                , false, false);
        Map<String, String> passwordExpected7 = getPasswordRecoveryExpectedPropertyValues(true, true, false, true);

        // Disabling the password recovery config. Precondition: email link and sms otp are true.
        Map<String, String> passwordConfigs8 = getPasswordRecoveryConfigs(false, null, null, null);
        IdentityProviderProperty[] passwordIdentityProps8 = getPasswordRecoveryIdentityProviderProperties(true, true
                , true, true);
        Map<String, String> passwordExpected8 = getPasswordRecoveryExpectedPropertyValues(false, false, false, false);

        return new Object[][]{
                {usernameConfig1, identityProviderProperties1, expected1},
                {usernameConfig2, identityProviderProperties2, expected2},
                {usernameConfig3, identityProviderProperties3, expected3},
                {usernameConfig4, identityProviderProperties4, expected4},
                {usernameConfig5, identityProviderProperties5, expected5},
                {usernameConfig6, identityProviderProperties6, expected6},
                {usernameConfig7, identityProviderProperties7, expected7},
                {usernameConfig8, identityProviderProperties8, expected8},
                {usernameConfig9, identityProviderProperties9, expected9},
                {usernameConfig10, identityProviderProperties10, expected10},
                {usernameConfig11, identityProviderProperties11, expected11},
                {passwordConfig1, passwordIdentityProps1, passwordExpected1},
                {passwordConfig2, passwordIdentityProps2, passwordExpected2},
                {passwordConfig3, passwordIdentityProps3, passwordExpected3},
                {passwordConfigs4, passwordIdentityProps4, passwordExpected4},
                {passwordConfigs5, passwordIdentityProps5, passwordExpected5},
                {passwordConfigs6, passwordIdentityProps6, passwordExpected6},
                {passwordConfigs7, passwordIdentityProps7, passwordExpected7},
                {passwordConfigs8, passwordIdentityProps8, passwordExpected8},
        };

    }

    private IdentityProviderProperty[] getUsernameRecoveryIdentityProviderProperties(boolean usernameEnable,
                                                                                     boolean usernameEmailEnable,
                                                                                     boolean usernameSmsEnable) {

        IdentityProviderProperty identityProviderProperty1 = new IdentityProviderProperty();
        identityProviderProperty1.setName(USERNAME_RECOVERY_ENABLE);
        identityProviderProperty1.setValue(usernameEnable ? TRUE_STRING : FALSE_STRING);

        IdentityProviderProperty identityProviderProperty2 = new IdentityProviderProperty();
        identityProviderProperty2.setName(USERNAME_RECOVERY_SMS_ENABLE);
        identityProviderProperty2.setValue(usernameSmsEnable ? TRUE_STRING : FALSE_STRING);

        IdentityProviderProperty identityProviderProperty3 = new IdentityProviderProperty();
        identityProviderProperty3.setName(USERNAME_RECOVERY_EMAIL_ENABLE);
        identityProviderProperty3.setValue(usernameEmailEnable ? TRUE_STRING : FALSE_STRING);

        return new IdentityProviderProperty[]{
                identityProviderProperty1,
                identityProviderProperty2,
                identityProviderProperty3
        };
    }

    private HashMap<String, String> getPasswordRecoveryConfigs(
            Boolean passwordRecoveryEnable,
            Boolean passwordRecoveryEmailLinkEnable,
            Boolean passwordRecoveryEmailOtpEnable,
            Boolean passwordRecoverySmsOtpEnable) {

        HashMap<String, String> configs = new HashMap<>();

        if (passwordRecoveryEnable != null) {
            configs.put(PASSWORD_RECOVERY_ENABLE, passwordRecoveryEnable ? TRUE_STRING : FALSE_STRING);
        }

        if (passwordRecoveryEmailLinkEnable != null) {
            configs.put(PASSWORD_RECOVERY_EMAIL_LINK_ENABLE, passwordRecoveryEmailLinkEnable ? TRUE_STRING :
                    FALSE_STRING);
        }

        if (passwordRecoveryEmailOtpEnable != null) {
            configs.put(PASSWORD_RECOVERY_EMAIL_OTP_ENABLE, passwordRecoveryEmailOtpEnable ? TRUE_STRING :
                    FALSE_STRING);
        }

        if (passwordRecoverySmsOtpEnable != null) {
            configs.put(PASSWORD_RECOVERY_SMS_OTP_ENABLE, passwordRecoverySmsOtpEnable ? TRUE_STRING : FALSE_STRING);
        }

        return configs;
    }

    private IdentityProviderProperty[] getPasswordRecoveryIdentityProviderProperties(
            boolean passwordRecoveryEnable,
            boolean passwordRecoveryEmailLinkEnable,
            boolean passwordRecoveryEmailOtpEnable,
            boolean passwordRecoverySmsOtpEnable) {

        IdentityProviderProperty identityProviderProperty1 = new IdentityProviderProperty();
        identityProviderProperty1.setName(PASSWORD_RECOVERY_ENABLE);
        identityProviderProperty1.setValue(passwordRecoveryEnable ? TRUE_STRING : FALSE_STRING);

        IdentityProviderProperty identityProviderProperty2 = new IdentityProviderProperty();
        identityProviderProperty2.setName(PASSWORD_RECOVERY_EMAIL_LINK_ENABLE);
        identityProviderProperty2.setValue(passwordRecoveryEmailLinkEnable ? TRUE_STRING : FALSE_STRING);

        IdentityProviderProperty identityProviderProperty3 = new IdentityProviderProperty();
        identityProviderProperty3.setName(PASSWORD_RECOVERY_EMAIL_OTP_ENABLE);
        identityProviderProperty3.setValue(passwordRecoveryEmailOtpEnable ? TRUE_STRING : FALSE_STRING);

        IdentityProviderProperty identityProviderProperty4 = new IdentityProviderProperty();
        identityProviderProperty4.setName(PASSWORD_RECOVERY_SMS_OTP_ENABLE);
        identityProviderProperty4.setValue(passwordRecoverySmsOtpEnable ? TRUE_STRING : FALSE_STRING);

        return new IdentityProviderProperty[]{
                identityProviderProperty1,
                identityProviderProperty2,
                identityProviderProperty3,
                identityProviderProperty4
        };
    }


    private HashMap<String, String> getUsernameRecoveryExpectedPropertyValues(boolean usernameEnable,
                                                                              boolean usernameEmailEnable,
                                                                              boolean usernameSmsEnable) {

        HashMap<String, String> expected = new HashMap<>();
        expected.put(USERNAME_RECOVERY_ENABLE, usernameEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(USERNAME_RECOVERY_EMAIL_ENABLE, usernameEmailEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(USERNAME_RECOVERY_SMS_ENABLE, usernameSmsEnable ? TRUE_STRING : FALSE_STRING);

        return expected;
    }

    private HashMap<String, String> getPasswordRecoveryExpectedPropertyValues(boolean passwordRecoveryEnable,
                                                                              boolean passwordRecoveryEmailLinkEnable,
                                                                              boolean passwordRecoveryEmailOtpEnable,
                                                                              boolean passwordRecoverySmsOtpEnable) {

        HashMap<String, String> expected = new HashMap<>();
        expected.put(PASSWORD_RECOVERY_ENABLE, passwordRecoveryEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(PASSWORD_RECOVERY_EMAIL_LINK_ENABLE, passwordRecoveryEmailLinkEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(PASSWORD_RECOVERY_EMAIL_OTP_ENABLE, passwordRecoveryEmailOtpEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(PASSWORD_RECOVERY_SMS_OTP_ENABLE, passwordRecoverySmsOtpEnable ? TRUE_STRING : FALSE_STRING);

        return expected;
    }

    private void mockPrivilegedCarbonContext() {

        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);
        when(privilegedCarbonContext.getApplicationResidentOrganizationId()).thenReturn(APP_RESIDENT_ORG_ID);
    }
}
