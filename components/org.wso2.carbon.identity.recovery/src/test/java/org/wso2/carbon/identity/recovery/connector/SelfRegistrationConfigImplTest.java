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
        assertEquals(selfRegistrationConfigImpl.getName(), "self-sign-up");
    }

    @Test
    public void testGetFriendlyName() {
        assertEquals(selfRegistrationConfigImpl.getFriendlyName(), "User Self Registration");
    }

    @Test
    public void testGetCategory() {
        assertEquals(selfRegistrationConfigImpl.getCategory(), "Account Management Policies");
    }

    @Test
    public void testGetSubCategory() {
        assertEquals(selfRegistrationConfigImpl.getSubCategory(), "DEFAULT");
    }

    @Test
    public void testGetOrder() {
        assertEquals(selfRegistrationConfigImpl.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {
        Map<String, String> nameMappingExpected = new HashMap<String, String>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable Self User Registration");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Internal Notification Management");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration code expiry time");

        Map<String, String> nameMapping = selfRegistrationConfigImpl.getPropertyNameMapping();

        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {
        Map<String, String> descriptionMappingExpected = new HashMap<>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable self user registration");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user registration");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable captcha verification during self registration");
        descriptionMappingExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Set the number of minutes the user self registration verification mail would be valid.(Negative " +
                        "value for infinite validity)");

        Map<String, String> descriptionMapping = selfRegistrationConfigImpl.getPropertyDescriptionMapping();

        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyNames() {
        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        String[] propertiesArrayExpected = propertiesExpected.toArray(new String[propertiesExpected.size()]);

        String[] properties = selfRegistrationConfigImpl.getPropertyNames();

        for (int i = 0; i < propertiesArrayExpected.length; i++) {
            assertEquals(properties[i], propertiesArrayExpected[i]);
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {
        String testEnableSelfSignUp = "false";
        String testEnableAccountLockOnCreation = "true";
        String testEnableNotificationInternallyManage = "true";
        String testEnableSelfRegistrationReCaptcha = "true";
        String testVerificationCodeExpiryTime = "1440";

        Map<String, String> propertiesExpected = new HashMap<>();
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, testEnableSelfSignUp);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                testEnableAccountLockOnCreation);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                testEnableSelfRegistrationReCaptcha);
        propertiesExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                testVerificationCodeExpiryTime);

        String tenantDomain = "admin";
        // Here tenantDomain parameter is not used by method itself
        Properties properties = selfRegistrationConfigImpl.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultProperties = new HashMap<String, String>((Map) properties);

        assertEquals(defaultProperties, propertiesExpected, "Maps are not equal");
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {
        String tenantDomain = "admin";
        String[] propertyNames = new String[]{"property1", "property2", "property3"};

        // Here tenantDomain and propertyNames parameters are not used by method itself
        Map<String, String> defaultPropertyValues = selfRegistrationConfigImpl.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertEquals(defaultPropertyValues, null);
    }

}
