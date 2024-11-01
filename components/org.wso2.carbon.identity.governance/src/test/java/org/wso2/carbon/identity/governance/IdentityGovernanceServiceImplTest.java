package org.wso2.carbon.identity.governance;

import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.AssertJUnit.assertEquals;

public class IdentityGovernanceServiceImplTest {

    // Constants.
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String TRUE_STRING = "true";
    private static final String FALSE_STRING = "false";
    private static final String USERNAME_RECOVERY_ENABLE = "Recovery.Notification.Username.Enable";
    private static final String USERNAME_RECOVERY_EMAIL_ENABLE = "Recovery.Notification.Username.Email.Enable";
    private static final String USERNAME_RECOVERY_SMS_ENABLE = "Recovery.Notification.Username.SMS.Enable";

    @Mock
    IdentityMgtServiceDataHolder identityMgtServiceDataHolder;

    @Mock
    IdpManager idpManager;

    @Mock
    IdentityProvider identityProvider;

    MockedStatic<IdentityMgtServiceDataHolder> identityMgtServiceDataHolderMockedStatic;

    private IdentityGovernanceServiceImpl identityGovernanceService;

    @BeforeMethod
    public void setup() throws IdentityProviderManagementException {

        MockitoAnnotations.openMocks(this);
        identityMgtServiceDataHolderMockedStatic = mockStatic(IdentityMgtServiceDataHolder.class);
        identityMgtServiceDataHolderMockedStatic.when(IdentityMgtServiceDataHolder::
                getInstance).thenReturn(identityMgtServiceDataHolder);
        when(identityMgtServiceDataHolder.getIdpManager()).thenReturn(idpManager);
        when(idpManager.getResidentIdP(eq(TENANT_DOMAIN))).thenReturn(identityProvider);

        FederatedAuthenticatorConfig[] authenticatorConfigs = new FederatedAuthenticatorConfig[0];
        when(identityProvider.getFederatedAuthenticatorConfigs()).thenReturn(authenticatorConfigs);

        identityGovernanceService = new IdentityGovernanceServiceImpl();
    }

    @AfterMethod
    public void tearDown() {

        identityMgtServiceDataHolderMockedStatic.close();
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

    @DataProvider(name = "updateConfigurations")
    public Object[][] buildConfigurations() {

        // Only email config is true. Preconditions: all the configs false.
        Map<String, String> usernameConfig1 = new HashMap<>();
        usernameConfig1.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties1 = getIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected1 = getExpectedPropertyValues(true, true, false);

        // Only sms config is true. Preconditions: all the configs false.
        Map<String, String> usernameConfig2 = new HashMap<>();
        usernameConfig2.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties2 = getIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected2 = getExpectedPropertyValues(true, false, true);

        // Only sms is false. Preconditions: sms and username is true.
        Map<String, String> usernameConfig3 = new HashMap<>();
        usernameConfig3.put(USERNAME_RECOVERY_SMS_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties3 = getIdentityProviderProperties(
                true, false, true);
        Map<String, String> expected3 = getExpectedPropertyValues(false, false, false);

        // Only email is false. Preconditions: email and username is true.
        Map<String, String> usernameConfig4 = new HashMap<>();
        usernameConfig4.put(USERNAME_RECOVERY_EMAIL_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties4 = getIdentityProviderProperties(
                true, true, false);
        Map<String, String> expected4 = getExpectedPropertyValues(false, false, false);

        // Only email is true. Preconditions: sms and username is true.
        Map<String, String> usernameConfig5 = new HashMap<>();
        usernameConfig5.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties5 = getIdentityProviderProperties(
                true, false, true);
        Map<String, String> expected5 = getExpectedPropertyValues(true, true, true);

        // Only sms is true. Preconditions: email and username is true.
        Map<String, String> usernameConfig6 = new HashMap<>();
        usernameConfig6.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties6 = getIdentityProviderProperties(
                true, true, false);
        Map<String, String> expected6 = getExpectedPropertyValues(true, true, true);

        // Sms config true and email config false. Preconditions: all the configs false.
        Map<String, String> usernameConfig7 = new HashMap<>();
        usernameConfig7.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);
        usernameConfig7.put(USERNAME_RECOVERY_EMAIL_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties7 = getIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected7 = getExpectedPropertyValues(true, false, true);

        // Email config true and sms config false. Preconditions: all the configs false.
        Map<String, String> usernameConfig8 = new HashMap<>();
        usernameConfig8.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);
        usernameConfig8.put(USERNAME_RECOVERY_SMS_ENABLE, FALSE_STRING);

        IdentityProviderProperty[] identityProviderProperties8 = getIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected8 = getExpectedPropertyValues(true, true, false);

        // Sms config true and email config true. Preconditions: all the configs false.
        Map<String, String> usernameConfig9 = new HashMap<>();
        usernameConfig9.put(USERNAME_RECOVERY_SMS_ENABLE, TRUE_STRING);
        usernameConfig9.put(USERNAME_RECOVERY_EMAIL_ENABLE, TRUE_STRING);

        IdentityProviderProperty[] identityProviderProperties9 = getIdentityProviderProperties(
                false, false, false);
        Map<String, String> expected9 = getExpectedPropertyValues(true, true, true);

        return new Object[][]{
                {usernameConfig1, identityProviderProperties1, expected1},
                {usernameConfig2, identityProviderProperties2, expected2},
                {usernameConfig3, identityProviderProperties3, expected3},
                {usernameConfig4, identityProviderProperties4, expected4},
                {usernameConfig5, identityProviderProperties5, expected5},
                {usernameConfig6, identityProviderProperties6, expected6},
                {usernameConfig7, identityProviderProperties7, expected7},
                {usernameConfig8, identityProviderProperties8, expected8},
                {usernameConfig9, identityProviderProperties9, expected9}
        };

    }

    private IdentityProviderProperty[] getIdentityProviderProperties(boolean usernameEnable,
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

    private HashMap<String, String> getExpectedPropertyValues(boolean usernameEnable,
                                                              boolean usernameEmailEnable,
                                                              boolean usernameSmsEnable) {

        HashMap<String, String> expected = new HashMap<>();
        expected.put(USERNAME_RECOVERY_ENABLE, usernameEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(USERNAME_RECOVERY_EMAIL_ENABLE, usernameEmailEnable ? TRUE_STRING : FALSE_STRING);
        expected.put(USERNAME_RECOVERY_SMS_ENABLE, usernameSmsEnable ? TRUE_STRING : FALSE_STRING);

        return expected;
    }
}