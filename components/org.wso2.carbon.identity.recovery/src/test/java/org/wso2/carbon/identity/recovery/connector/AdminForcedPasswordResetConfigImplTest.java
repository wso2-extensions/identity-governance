/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.connector;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * This class does unit test coverage for AdminForcedPasswordResetConfigImpl class.
 */
public class AdminForcedPasswordResetConfigImplTest {

    private AdminForcedPasswordResetConfigImpl adminForcedPasswordResetConfigIml;

    @BeforeTest
    public void init() {

        adminForcedPasswordResetConfigIml = new AdminForcedPasswordResetConfigImpl();
    }

    @Test
    public void testGetName() throws IdentityGovernanceException {

        assertEquals(adminForcedPasswordResetConfigIml.getName(), "admin-forced-password-reset");
    }

    @Test
    public void testGetFriendlyName() throws IdentityGovernanceException {

        assertEquals(adminForcedPasswordResetConfigIml.getFriendlyName(), "Password Reset");
    }

    @Test
    public void testGetCategory() throws IdentityGovernanceException {

        assertEquals(adminForcedPasswordResetConfigIml.getCategory(), "Account Management");
    }

    @Test
    public void testGetSubCategory() throws IdentityGovernanceException {

        assertEquals(adminForcedPasswordResetConfigIml.getSubCategory(), "DEFAULT");
    }

    @Test
    public void testGetOrder() throws IdentityGovernanceException {

        assertEquals(adminForcedPasswordResetConfigIml.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() throws IdentityGovernanceException {

        Map<String, String> nameMappingExpected = new HashMap<>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "Enable password reset via recovery e-mail");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "Enable password reset via OTP");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "Enable password reset offline");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ADMIN_PASSWORD_RESET_EXPIRY_TIME,
                "Admin forced password reset code expiry time");

        Map<String, String> nameMapping = adminForcedPasswordResetConfigIml.getPropertyNameMapping();
        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() throws IdentityGovernanceException {

        Map<String, String> descriptionMappingExpected = new HashMap<>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "User gets notified with a link to reset password");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "User gets notified with a one time password to try with SSO login");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "An OTP generated and stored in users claims");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ADMIN_PASSWORD_RESET_EXPIRY_TIME,
                "Validity time of the admin forced password reset code in minutes");

        Map<String, String> descriptionMapping = adminForcedPasswordResetConfigIml.getPropertyDescriptionMapping();
        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyNames() throws IdentityGovernanceException {

        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ADMIN_PASSWORD_RESET_EXPIRY_TIME);
        String[] propertiesListExpected = propertiesExpected.toArray(new String[0]);

        String[] properties = adminForcedPasswordResetConfigIml.getPropertyNames();

        for (int i = 0; i < properties.length; i++) {
            assertEquals(properties[i], propertiesListExpected[i]);
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {

        String testEnableAdminPasswordResetWithRecoveryLink = "false";
        String testEnableAdminPasswordResetWithOTP = "false";
        String testEnableAdminPasswordResetOffline = "false";
        String testAdminPasswordResetExpiryTime = "1440";

        Map<String, String> propertiesExpected = new HashMap<>();
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                testEnableAdminPasswordResetWithRecoveryLink);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                testEnableAdminPasswordResetWithOTP);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                testEnableAdminPasswordResetOffline);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ADMIN_PASSWORD_RESET_EXPIRY_TIME,
                testAdminPasswordResetExpiryTime);

        String tenantDomain = "admin";
        // Here tenantDomain parameter is not used by method itself
        Properties properties = adminForcedPasswordResetConfigIml.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultProperties = new HashMap<String, String>((Map) properties);

        assertEquals(defaultProperties, propertiesExpected, "Maps are not equal");
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {

        String tenantDomain = "admin";
        String[] propertyNames = new String[]{"property1", "property2", "property3"};

        // Here tenantDomain and propertyNames parameters are not used by method itself
        Map<String, String> defaultPropertyValues =
                adminForcedPasswordResetConfigIml.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertNull(defaultPropertyValues);
    }
}
