/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.password.policy.handler;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertFalse;

public class PasswordPolicyValidationHandlerTest {

    private static final String PW_POLICY_DISABLE = "passwordPolicy.disable";
    PasswordPolicyValidationHandler passwordPolicyValidationHandler;

    @BeforeMethod
    public void setUp() throws Exception {
        passwordPolicyValidationHandler = new PasswordPolicyValidationHandler();
    }

    @Test
    public void testGetName() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getName(),
                "passwordPolicy", "Valid getName().");
        assertNotEquals(passwordPolicyValidationHandler.getName(),
                "passwordkshfy", "Invalid getName().");
        assertFalse(Boolean.parseBoolean(passwordPolicyValidationHandler.getName()),
                "Invalid getName().");
    }

    @Test
    public void testGetFriendlyName() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getFriendlyName(),
                "Password Patterns", "Valid getFriendlyName().");
        assertNotEquals(passwordPolicyValidationHandler.getFriendlyName(),
                "passwordHistory", "Invalid getFriendlyName().");
    }

    @Test
    public void testGetCategory() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getCategory(),
                "Password Policies", "Valid getCategory().");
        assertNotEquals(passwordPolicyValidationHandler.getCategory(),
                "PasswordPolicies", "Invalid getCategory().");
    }

    @Test
    public void testGetSubCategory() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getSubCategory(),
                "DEFAULT", "Valid getSubCategory().");
        assertNotEquals(passwordPolicyValidationHandler.getSubCategory(),
                "PasswordPolicies", "Invalid getSubCategory().");
    }

    @Test
    public void testGetOrder() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getOrder(),
                0, "Valid getOrder().");
        assertNotEquals(passwordPolicyValidationHandler.getOrder(),
                2, "Invalid getOrder().");
    }

    @Test
    public void testGetPropertyNameMapping() throws Exception {
        Map<String, String> expectedMappingTrue = new HashMap<>();
        expectedMappingTrue.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable Password Policy Feature");
        expectedMappingTrue.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, "Password Policy Min Length");
        expectedMappingTrue.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, "Password Policy Max Length");
        expectedMappingTrue.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Password Policy Pattern");
        expectedMappingTrue.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, "Password Policy Error Message");
        assertEquals(passwordPolicyValidationHandler.getPropertyNameMapping(), expectedMappingTrue,
                "Valid getPropertyNameMapping() .");

        Map<String, String> expectedMappingFalse = new HashMap<>();
        expectedMappingFalse.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable Password Policy");
        expectedMappingFalse.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, "Password Policy Length");
        expectedMappingFalse.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, "Password Policy  Length");
        expectedMappingFalse.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Password PolicyPattern");
        expectedMappingFalse.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, "Password Policy Error ");
        assertNotEquals(passwordPolicyValidationHandler.getPropertyNameMapping(), expectedMappingFalse,
                "Invalid getPropertyNameMapping() .");
    }

    @Test
    public void testGetPropertyDescriptionMapping() throws Exception {
        Map<String, String> expecteddescriptionMappingTrue = new HashMap<>();
        expecteddescriptionMappingTrue.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable password pattern policy");
        expecteddescriptionMappingTrue.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Allowed password regex pattern");
        expecteddescriptionMappingTrue.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG,
                "Error message for invalid password patterns");
        assertEquals(passwordPolicyValidationHandler.getPropertyDescriptionMapping(), expecteddescriptionMappingTrue,
                "Valid getPropertyDescriptionMapping().");

        Map<String, String> expecteddescriptionMappingFalse = new HashMap<>();
        expecteddescriptionMappingFalse.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable passwordpattern policy");
        expecteddescriptionMappingFalse.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Allow password regex pattern");
        expecteddescriptionMappingFalse.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG,
                "Error message for valid password patterns");
        assertNotEquals(passwordPolicyValidationHandler.getPropertyDescriptionMapping(), expecteddescriptionMappingFalse,
                "Invalid getPropertyDescriptionMapping().");
    }

    @Test
    public void testGetPropertyNames() throws Exception {
        List<String> expectedpropertiesTrue = new ArrayList<>();
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_ENABLE);
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH);
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH);
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_PATTERN);
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_ERROR_MSG);
        assertEquals(passwordPolicyValidationHandler.getPropertyNames().length,
                expectedpropertiesTrue.size(), "Valid getPropertyDescriptionMapping().");

        List<String> expectedpropertiesFalse = new ArrayList<>();
        expectedpropertiesTrue.add(PasswordPolicyConstants.PW_POLICY_ENABLE);
        expectedpropertiesFalse.add(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH);
        expectedpropertiesFalse.add(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH);
        assertNotEquals(passwordPolicyValidationHandler.getPropertyNames().length,
                expectedpropertiesFalse.size(), "Invalid getPropertyDescriptionMapping().");
    }

    @Test
    public void testGetDefaultPropertyValues() throws Exception {
        assertEquals(passwordPolicyValidationHandler.getOrder(), 0, "Valid getOrder().");
        assertNotEquals(passwordPolicyValidationHandler.getOrder(), null, "Invalid getOrder().");
    }
}
