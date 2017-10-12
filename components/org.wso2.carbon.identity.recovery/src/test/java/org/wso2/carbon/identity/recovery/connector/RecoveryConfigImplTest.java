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
 * This class does unit test coverage for RecoveryConfigImpl class
 */
public class RecoveryConfigImplTest {

    private RecoveryConfigImpl recoveryConfigImpl;

    @BeforeTest
    public void Init() {
        recoveryConfigImpl = new RecoveryConfigImpl();
    }

    @Test
    public void testGetName() {
        assertEquals("account-recovery", recoveryConfigImpl.getName());
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals("Account Recovery", recoveryConfigImpl.getFriendlyName());
    }

    @Test
    public void testGetCategory() {
        assertEquals("Account Management Policies", recoveryConfigImpl.getCategory());
    }

    @Test
    public void testGetSubCategory() {
        assertEquals("DEFAULT", recoveryConfigImpl.getSubCategory());
    }

    @Test
    public void testGetOrder() {
        assertEquals(0, recoveryConfigImpl.getOrder());
    }

    @Test
    public void testGetPropertyNameMapping() {
        Map<String, String> nameMappingExpected = recoveryConfigImpl.getPropertyNameMapping();

        Map<String, String> testNameMapping = new HashMap<>();
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, "Enable " +
                "Notification Based Password Recovery");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, "Enable " +
                "Notification Internally Management");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY, "Enable Security " +
                "Question Based Password Recovery");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER, "Number Of Questions " +
                "Required For Password Recovery");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE, "Enable Username Recovery");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, "Notification Expiry Time");

        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                "Notify when Recovery Success");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                "Notify when Questions Based Recovery Starts");

        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Enable reCaptcha for Security Questions Based Password Recovery");
        testNameMapping.put(IdentityRecoveryConstants.ConnectorConfig
                .RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS, "Max Failed Attempts for ReCaptcha");

        assertEquals(testNameMapping, nameMappingExpected, "Hashmaps are equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {
        Map<String, String> descriptionMappingExpected = recoveryConfigImpl.getPropertyDescriptionMapping();

        Map<String, String> testDescriptionMapping = new HashMap<String, String>();
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        testDescriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Show captcha for challenge question based password recovery");

        assertEquals(testDescriptionMapping, descriptionMappingExpected, "Hashmaps are equal");
    }

    @Test
    public void testGetPropertyNames() {
        String[] propertiesExpected = recoveryConfigImpl.getPropertyNames();

        List<String> testProperties = new ArrayList<>();
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START);
        testProperties.add(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);

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
        tenantDoamin_2 = "user";

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

        String testEnableNotificationBasedPasswordRecovery = "false";
        String testEnableQuestionBasedPasswordRecovery = "false";
        String testMinimumAnswers = "2";
        String testEnableRecoveryQuestionPasswordReCaptcha = "true";
        String testRecoveryQuestionPasswordReCaptchaMaxFailedAttempts = "2";
        String testEnableUsernameRecovery = "false";
        String testEnableNotificationInternallyManage = "true";
        String testExpiryTime = "1440";
        String testNotifySuccess = "false";
        String testNotifyStart = "false";

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                testEnableNotificationBasedPasswordRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                testEnableQuestionBasedPasswordRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER,
                testMinimumAnswers);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                testEnableRecoveryQuestionPasswordReCaptcha);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig
                        .RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS,
                testRecoveryQuestionPasswordReCaptchaMaxFailedAttempts);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE,
                testEnableUsernameRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, testExpiryTime);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                testNotifySuccess);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                testNotifyStart);

        Properties propertiesExpected = recoveryConfigImpl.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultPropertiesExpected = new HashMap<String, String>((Map) propertiesExpected);
        assertEquals(defaultProperties, defaultPropertiesExpected, "Maps are equal");
    }

    @DataProvider(name = "defaultPropertyValues")
    public Object[][] provideDefaultPropertyValues1() {
        String tenantDomain1;
        String tenantDomain2;

        tenantDomain1 = null;
        tenantDomain2 = "admin";

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
        Map<String, String> defaultPropertyValuesExpected = recoveryConfigImpl.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(null, defaultPropertyValuesExpected);
    }

}
