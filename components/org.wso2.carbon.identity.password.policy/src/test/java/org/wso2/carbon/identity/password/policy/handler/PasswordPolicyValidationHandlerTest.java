/*
 * Copyright (c) 2017-2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
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

public class PasswordPolicyValidationHandlerTest {

    PasswordPolicyValidationHandler passwordPolicyValidationHandler;

    @BeforeMethod
    public void setUp() throws Exception {

        passwordPolicyValidationHandler = new PasswordPolicyValidationHandler();
    }


    @Test
    public void testGetName() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getName(), "passwordPolicy",
                "getName() has been changed.");
    }

    @Test
    public void testGetFriendlyName() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getFriendlyName(), "Password Patterns",
                "getFriendlyName() has been changed.");
    }

    @Test
    public void testGetCategory() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getCategory(), "Password Policies",
                "getCategory() has been changed.");
    }

    @Test
    public void testGetSubCategory() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getSubCategory(), "DEFAULT",
                "getSubCategory() has been changed.");
    }

    @Test
    public void testGetOrder() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getOrder(), 0, "getOrder() has been changed.");
    }

    @Test
    public void testGetPropertyNameMapping() throws Exception {

        Map<String, String> expectedNameMapping = new HashMap<>();
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Validate passwords based on a policy pattern");
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, "Minimum number of characters");
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, "Maximum number of characters");
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Password pattern regex");
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, "Error message on pattern violation");
        expectedNameMapping.put(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE,
                "Check if password contains or equals to username");
        assertEquals(passwordPolicyValidationHandler.getPropertyNameMapping(), expectedNameMapping,
                "getPropertyNameMapping() has been changed.");
    }

    @Test
    public void testGetPropertyDescriptionMapping() throws Exception {

        Map<String, String> expectedDescriptionMapping = new HashMap<>();
        expectedDescriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE,
                "Validate user passwords against a policy");
        expectedDescriptionMapping.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH,
                "Minimum number of characters in the password.");
        expectedDescriptionMapping.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH,
                "Maximum number of characters in the password.");
        expectedDescriptionMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN,
                "The regular expression pattern to validate the password.");
        expectedDescriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG,
                "This error message will be displayed when a pattern violation is detected.");
        assertEquals(passwordPolicyValidationHandler.getPropertyDescriptionMapping(), expectedDescriptionMapping,
                "getPropertyDescriptionMapping() has been changed.");
    }

    @Test
    public void testGetPropertyNames() throws Exception {

        List<String> expectedPropertyName = new ArrayList<>();
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_ENABLE);
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH);
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH);
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_PATTERN);
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_ERROR_MSG);
        expectedPropertyName.add(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE);
        assertEquals(passwordPolicyValidationHandler.getPropertyNames().length, expectedPropertyName.size(),
                "getPropertyNames() has been changed.");
    }

    @Test
    public void testGetDefaultPropertyValues() throws Exception {

        assertEquals(passwordPolicyValidationHandler.getOrder(), 0, "getOrder() has been changed.");
    }
}
