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

import static org.testng.Assert.*;

/**
 * This class does unit test coverage for UserEmailVerificationConfigImpl class
 */
public class UserEmailVerificationConfigImplTest {

    private UserEmailVerificationConfigImpl userEmailVerificationConfig;

    @BeforeTest
    public void init() {
        userEmailVerificationConfig=new UserEmailVerificationConfigImpl();
    }

    @Test
    public void testGetName() {
        assertEquals("user-email-verification",userEmailVerificationConfig.getName());
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals("User Onboarding",userEmailVerificationConfig.getFriendlyName());
    }

    @Test
    public void testGetCategory() {
        assertEquals("Account Management Policies",userEmailVerificationConfig.getCategory());
    }

    @Test
    public void testGetSubCategory() {
        assertEquals("DEFAULT",userEmailVerificationConfig.getSubCategory());
    }

    @Test
    public void testGetOrder() {
        assertEquals(0,userEmailVerificationConfig.getOrder());
    }

    @Test
    public void testGetPropertyNameMapping() {
        Map<String,String> nameMappingExpected=userEmailVerificationConfig.getPropertyNameMapping();

        Map<String, String> testNameMapping = new HashMap<>();
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable User Email Verification");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Enable Notification Internally Management");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Email verification code expiry time");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Ask password code expiry time");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension class");

        assertEquals(testNameMapping,nameMappingExpected,"Both maps are equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {
        Map<String,String> descriptionMappingExpected=userEmailVerificationConfig.getPropertyDescriptionMapping();

        Map<String,String> testDescriptionMapping=new HashMap<String,String>();
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable to trigger a verification notification during user creation");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user creation");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Set the number of minutes the email verification mail would be valid.(Negative value for infinite " +
                        "validity)");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Set the number of minutes the ask password mail would be valid. (Negative value for infinite " +
                        "validity)");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension point in ask password feature)");

        assertEquals(testDescriptionMapping,descriptionMappingExpected,"Both maps are equal");
    }

    @Test
    public void testGetPropertyNames() {
        String[] propertiesExpected=userEmailVerificationConfig.getPropertyNames();

        List<String> testProperties = new ArrayList<>();
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME);
        String[] testPropertiesArray=testProperties.toArray(new String[testProperties.size()]);

        for (int i = 0; i < testPropertiesArray.length; i++) {
            assertEquals(testPropertiesArray[i], propertiesExpected[i]);
        }
    }

    @DataProvider(name = "provideDefaultPropertyValues")
    public Object[][] providePropertyValues() {
        String tenantDomain_1;
        String tenantDoamin_2;

        tenantDomain_1 = null;
        tenantDoamin_2 = "user3";

        return new Object[][]{
                {tenantDomain_1}, {tenantDoamin_2}
        };
    }

    /**
     *
     * @param tenantDomain
     * @throws IdentityGovernanceException
     */
    @Test(dataProvider = "provideDefaultPropertyValues")
    public void testGetDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {
        String testEnableEmailVerification = "false";
        String testEnableEmailAccountLockOnCreation = "true";
        String testEnableNotificationInternallyManage = "true";
        String testEmailVerificationCodeExpiry = "1440";
        String testAskPasswordCodeExpiry = "1440";
        String testAskPasswordTempPassExtension = "org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator";

        Map<String, String> testDefaultProperties = new HashMap<>();
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                testEnableEmailVerification);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                testEmailVerificationCodeExpiry);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                testAskPasswordCodeExpiry);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                testEnableEmailAccountLockOnCreation);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        testDefaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                testAskPasswordTempPassExtension);

        Properties propertiesExpected = userEmailVerificationConfig.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultPropertiesExpected = new HashMap<String, String>((Map) propertiesExpected);
        assertEquals(testDefaultProperties, defaultPropertiesExpected, "Maps are equal");
    }

    @DataProvider(name = "defaultPropertyValues")
    public Object[][] provideDefaultPropertyValues1() {
        String tenantDomain1;
        String tenantDomain2;

        tenantDomain1 = null;
        tenantDomain2 = "user3";

        String[] propertyNames1;
        String[] propertyNames2;

        propertyNames1 = null;
        propertyNames2 = new String[]{"property_1", "property_2", "property_3"};
        return new Object[][]{
                {propertyNames1, tenantDomain1}, {propertyNames2, tenantDomain2}
        };
    }

    /**
     *
     * @param propertyNames
     * @param tenantDomain
     * @throws IdentityGovernanceException
     */
    @Test(dataProvider = "defaultPropertyValues")
    public void testGetDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        Map<String, String> defaultPropertyValuesExpected = userEmailVerificationConfig.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(null, defaultPropertyValuesExpected);
    }

}
