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

/**
 * This class does unit test coverage for SelfRegistrationConfigImpl class
 */
public class SelfRegistrationConfigImplTest {

    private SelfRegistrationConfigImpl selfRegistrationConfigImpl;

    @BeforeTest
    public void Init() {
        selfRegistrationConfigImpl = new SelfRegistrationConfigImpl();
    }

    @Test
    public void testGetName() {
        assertEquals("self-sign-up", selfRegistrationConfigImpl.getName());
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals("User Self Registration", selfRegistrationConfigImpl.getFriendlyName());
    }

    @Test
    public void testGetCategory() {
        assertEquals("Account Management Policies", selfRegistrationConfigImpl.getCategory());
    }

    @Test
    public void testGetSubCategory() {
        assertEquals("DEFAULT", selfRegistrationConfigImpl.getSubCategory());
    }

    @Test
    public void testGetOrder() {
        assertEquals(0, selfRegistrationConfigImpl.getOrder());
    }

    @Test
    public void testGetPropertyNameMapping() {
        Map<String, String> nameMappingExpected = selfRegistrationConfigImpl.getPropertyNameMapping();

        Map<String, String> testNameMapping = new HashMap<String, String>();
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable Self User Registration");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Enable Notification Internally Management");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration code expiry time");
        assertEquals(testNameMapping, nameMappingExpected, "Both maps are equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {
        Map<String, String> descriptionMappingExpected = selfRegistrationConfigImpl.getPropertyDescriptionMapping();

        Map<String, String> testDescriptionMapping = new HashMap<>();
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable self user registration");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user registration");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable captcha verification during self registration");
        testDescriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Set the number of minutes the user self registration verification mail would be valid.(Negative " +
                        "value for infinite validity)");

        assertEquals(testDescriptionMapping, descriptionMappingExpected, "Both maps are equal");
    }

    @Test
    public void testGetPropertyNames() {
        String[] propertiesExpected = selfRegistrationConfigImpl.getPropertyNames();

        List<String> testProperties = new ArrayList<>();
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        String[] testPropertiesArray = testProperties.toArray(new String[testProperties.size()]);

        for (int i = 0; i < testPropertiesArray.length; i++) {
            assertEquals(testPropertiesArray[i], propertiesExpected[i]);
        }
    }

    @DataProvider(name = "provideDefaultPropertyValues")
    public Object[][] providePropertyValues() {
        String tenantDomain_1;
        String tenantDoamin_2;

        tenantDomain_1 = null;
        tenantDoamin_2 = "user2";

        return new Object[][]{
                {tenantDomain_1}, {tenantDoamin_2}
        };
    }

    /**
     * @param tenantDomain
     * @throws IdentityGovernanceException
     */
    @Test(dataProvider = "provideDefaultPropertyValues")
    public void testGetDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {
        String testEnableSelfSignUp = "false";
        String testEnableAccountLockOnCreation = "true";
        String testEnableNotificationInternallyManage = "true";
        String testEnableSelfRegistrationReCaptcha = "true";
        String testVerificationCodeExpiryTime = "1440";

        Map<String, String> testDefaultProperties = new HashMap<>();
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, testEnableSelfSignUp);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                testEnableAccountLockOnCreation);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                testEnableSelfRegistrationReCaptcha);
        testDefaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                testVerificationCodeExpiryTime);

        Properties propertiesExpected = selfRegistrationConfigImpl.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultPropertiesExpected = new HashMap<String, String>((Map) propertiesExpected);
        assertEquals(testDefaultProperties, defaultPropertiesExpected, "Maps are equal");
    }

    @DataProvider(name = "defaultPropertyValues")
    public Object[][] provideDefaultPropertyValues1() {
        String tenantDomain1;
        String tenantDomain2;

        tenantDomain1 = null;
        tenantDomain2 = "user2";

        String[] propertyNames1;
        String[] propertyNames2;

        propertyNames1 = null;
        propertyNames2 = new String[]{"property_1", "property_2", "property_3"};
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
        Map<String, String> defaultPropertyValuesExpected = selfRegistrationConfigImpl.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(null, defaultPropertyValuesExpected);
    }

}
