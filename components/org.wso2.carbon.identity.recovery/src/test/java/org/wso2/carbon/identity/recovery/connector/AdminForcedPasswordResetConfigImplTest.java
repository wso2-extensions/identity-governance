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

import org.testng.Assert;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.testng.Assert.assertEquals;

/***
 *This class does unit test coverage for AdminForcedPasswordResetConfigImpl class
 */
public class AdminForcedPasswordResetConfigImplTest {

    private AdminForcedPasswordResetConfigImpl adminForcedPasswordResetConfigIml;

    @BeforeTest
    public void init() {
        adminForcedPasswordResetConfigIml = new AdminForcedPasswordResetConfigImpl();
    }

    @Test
    public void testGetName() throws IdentityGovernanceException {
        assertEquals("admin-forced-password-reset", adminForcedPasswordResetConfigIml.getName());
    }

    @Test
    public void testGetFriendlyName() throws IdentityGovernanceException {
        assertEquals("Password Reset", adminForcedPasswordResetConfigIml.getFriendlyName());
    }

    @Test
    public void testGetCategory() throws IdentityGovernanceException {
        assertEquals("Account Management Policies", adminForcedPasswordResetConfigIml.getCategory());
    }

    @Test
    public void testGetSubCategory() throws IdentityGovernanceException {
        assertEquals("DEFAULT", adminForcedPasswordResetConfigIml.getSubCategory());
    }

    @Test
    public void testGetOrder() throws IdentityGovernanceException {
        assertEquals(0, adminForcedPasswordResetConfigIml.getOrder());
    }

    @Test
    public void testGetPropertyNameMapping() throws IdentityGovernanceException {
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
    public void testGetPropertyDescriptionMapping() throws IdentityGovernanceException {
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
    public void testGetPropertyNames() throws IdentityGovernanceException {
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

    @DataProvider(name = "provideDefaultPropertyValues")
    public Object[][] providePropertyValues() {
        String tenantDomain_1;
        String tenantDoamin_2;

        tenantDomain_1 = null;
        tenantDoamin_2 = "admin";

        return new Object[][]{
                {tenantDomain_1}, {tenantDoamin_2}
        };
    }

    /**
     * @param tenatDomain
     * @throws IdentityGovernanceException
     */
    @Test(dataProvider = "provideDefaultPropertyValues")
    public void testGetDefaultPropertyValues(String tenatDomain) throws IdentityGovernanceException {
        String testEnableAdminPasswordResetWithRecoveryLink = "false";
        String testEnableAdminPasswordResetWithOTP = "false";
        String testEnableAdminPasswordResetOffline = "false";

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                testEnableAdminPasswordResetWithRecoveryLink);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                testEnableAdminPasswordResetWithOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                testEnableAdminPasswordResetOffline);

        Properties propertiesExpected = adminForcedPasswordResetConfigIml.getDefaultPropertyValues(tenatDomain);
        Map<String, String> defaultPropertiesExpected = new HashMap<String, String>((Map) propertiesExpected);
        Assert.assertEquals(defaultProperties, defaultPropertiesExpected, "Maps are equal");
    }

    @DataProvider(name = "defaultPropertyValues1")
    public Object[][] provideDefaultPropertyValues1() {
        String tenantDomain1;
        String tenantDomain2;

        tenantDomain1 = "null";
        tenantDomain2 = "admin";

        String[] propertyNames1;
        String[] propertyNames2;

        propertyNames1 = null;
        propertyNames2 = new String[]{"property1", "property2", "property3"};
        return new Object[][]{
                {propertyNames1, tenantDomain1}, {propertyNames2, tenantDomain2}
        };
    }

    /**
     * @param propertyNames
     * @param tenantDomain
     * @throws IdentityGovernanceException
     */
    @Test(dataProvider = "defaultPropertyValues")
    public void testGetDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        Map<String, String> defaultPropertyValuesExpected = adminForcedPasswordResetConfigIml.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(null, defaultPropertyValuesExpected);
    }

}
