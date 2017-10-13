package org.wso2.carbon.identity.recovery.connector;

import org.apache.commons.lang.StringUtils;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.testng.Assert.assertEquals;

public class AdminForcedPasswordResetConfigImplTest {
    private AdminForcedPasswordResetConfigImpl adminForcedPasswordResetConfigIml;

    @BeforeTest
    public void init() {

        adminForcedPasswordResetConfigIml = new AdminForcedPasswordResetConfigImpl();

    }

    @Test
    public void testGetName() throws Exception {
    }

    @Test
    public void testGetFriendlyName() throws Exception {
    }

    @Test
    public void testGetCategory() throws Exception {
    }

    @Test
    public void testGetSubCategory() throws Exception {
    }

    @Test
    public void testGetOrder() throws Exception {
    }

    @Test
    public void testGetPropertyNameMapping() throws Exception {
        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "Enable Password Reset via Recovery Email");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "Enable Password Reset via OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "Enable Password Reset Offline");


        Map<String, String> nameMappingExpected = adminForcedPasswordResetConfigIml.getPropertyNameMapping();
        assertEquals(nameMapping, nameMappingExpected, "Maps are equal");


    }

    @Test
    public void testGetPropertyDescriptionMapping() throws Exception {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "User gets notified with a link to reset password");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "User gets notified with a one time password to try with SSO login");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "An OTP generated and stored in users claims");

        Map<String, String> descriptionMappingExpected = adminForcedPasswordResetConfigIml.getPropertyDescriptionMapping();
        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are equal");
    }

    @Test
    public void testGetPropertyNames() throws Exception {
        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE);

        String[] propertiesList = properties.toArray(new String[0]);
        String[] propertiesExpected = adminForcedPasswordResetConfigIml.getPropertyNames();

        for (int i = 0; i < properties.size(); i++) {
            assertEquals(propertiesList[i], propertiesExpected[i]);
        }
    }

    @DataProvider(name = "provideTenantDomain")
    public Object[][] provideData() {
        String tenantDomain1;
        String tenantDomain2;

        tenantDomain1 = "null";
        tenantDomain2 = "admin";
        return new Object[][]{
                {tenantDomain1}, {tenantDomain2}
        };
    }

    @Test(dataProvider = "provideTenantDomain")
    public void testGetDefaultPropertyValues(String tenatDomain) throws Exception {
        String testEnableAdminPasswordResetWithRecoveryLink = "false";
        String testEnableAdminPasswordResetWithOTP = "false";
        String testEnableAdminPasswordResetOffline = "false";

        String testAdminPasswordRecoveryWithLinkProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK);
        String testAdminPasswordResetWithOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP);
        String testAdminPasswordResetOfflineProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE);

        if (StringUtils.isNotEmpty(testAdminPasswordRecoveryWithLinkProperty)) {
            testEnableAdminPasswordResetWithRecoveryLink = testAdminPasswordRecoveryWithLinkProperty;
        }
        if (StringUtils.isNotEmpty(testAdminPasswordResetWithOTPProperty)) {
            testEnableAdminPasswordResetWithOTP = testAdminPasswordResetWithOTPProperty;
        }
        if (StringUtils.isNotEmpty(testAdminPasswordResetOfflineProperty)) {
            testEnableAdminPasswordResetOffline = testAdminPasswordResetOfflineProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                testEnableAdminPasswordResetWithRecoveryLink);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                testEnableAdminPasswordResetWithOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                testEnableAdminPasswordResetOffline);

        Properties propertiesExpected = adminForcedPasswordResetConfigIml.getDefaultPropertyValues(tenatDomain);
        Map<String, String> defaultPropertiesExpected = new HashMap<String, String>((Map) propertiesExpected);
        assertEquals(defaultProperties, defaultPropertiesExpected, "Maps are equal");


    }

    @Test
    public void testGetDefaultPropertyValues1() throws Exception {
    }


}