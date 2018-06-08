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

/**
 * This class does unit test coverage for UserEmailVerificationConfigImpl class
 */
public class UserEmailVerificationConfigImplTest {

    private UserEmailVerificationConfigImpl userEmailVerificationConfig;

    @BeforeTest
    public void init() {
        userEmailVerificationConfig = new UserEmailVerificationConfigImpl();
    }

    @Test
    public void testGetName() {
        assertEquals(userEmailVerificationConfig.getName(), "user-email-verification");
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals(userEmailVerificationConfig.getFriendlyName(), "User Onboarding");
    }

    @Test
    public void testGetCategory() {
        assertEquals(userEmailVerificationConfig.getCategory(), "Account Management Policies");
    }

    @Test
    public void testGetSubCategory() {
        assertEquals(userEmailVerificationConfig.getSubCategory(), "DEFAULT");
    }

    @Test
    public void testGetOrder() {
        assertEquals(userEmailVerificationConfig.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {
        Map<String, String> nameMappingExpected = new HashMap<>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable User Email Verification");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Internal Notification Management");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Email verification code expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Ask password code expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension class");

        Map<String, String> nameMapping = userEmailVerificationConfig.getPropertyNameMapping();

        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {
        Map<String, String> descriptionMappingExpected = new HashMap<>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable to trigger a verification notification during user creation");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user creation");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Set the number of minutes the email verification mail would be valid.(Negative value for infinite " +
                        "validity)");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Set the number of minutes the ask password mail would be valid. (Negative value for infinite " +
                        "validity)");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension point in ask password feature)");

        Map<String, String> descriptionMapping = userEmailVerificationConfig.getPropertyDescriptionMapping();

        assertEquals(descriptionMapping, descriptionMappingExpected, "maps are not equal");
    }

    @Test
    public void testGetPropertyNames() {
        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME);
        String[] propertiesArrayExpected = propertiesExpected.toArray(new String[propertiesExpected.size()]);

        String[] properties = userEmailVerificationConfig.getPropertyNames();

        for (int i = 0; i < propertiesArrayExpected.length; i++) {
            assertEquals(properties[i], propertiesArrayExpected[i]);
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {
        String testEnableEmailVerification = "false";
        String testEnableEmailAccountLockOnCreation = "true";
        String testEnableNotificationInternallyManage = "true";
        String testEmailVerificationCodeExpiry = "1440";
        String testAskPasswordCodeExpiry = "1440";
        String testAskPasswordTempPassExtension = "org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator";

        Map<String, String> defaultPropertiesExpected = new HashMap<>();
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                testEnableEmailVerification);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                testEmailVerificationCodeExpiry);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                testAskPasswordCodeExpiry);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                testEnableEmailAccountLockOnCreation);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                testAskPasswordTempPassExtension);

        String tenantDomain = "admin";
        // Here tenantDomain parameter is not used by method itself
        Properties properties = userEmailVerificationConfig.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultProperties = new HashMap<String, String>((Map) properties);
        assertEquals(defaultProperties, defaultPropertiesExpected, "Maps are not equal");
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {
        String tenantDomain = "admin";
        String[] propertyNames = new String[]{"property1", "property2", "property3"};

        // Here tenantDomain and propertyNames parameters are not used by method itself
        Map<String, String> defaultPropertyValues = userEmailVerificationConfig.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(defaultPropertyValues, null);
    }

}
