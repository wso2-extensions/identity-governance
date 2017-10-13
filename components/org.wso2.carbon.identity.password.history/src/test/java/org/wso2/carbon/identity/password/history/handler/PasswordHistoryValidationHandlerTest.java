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
package org.wso2.carbon.identity.password.history.handler;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertFalse;

public class PasswordHistoryValidationHandlerTest {

    PasswordHistoryValidationHandler passwordHistoryValidationHandler;

    @BeforeTest
    public void setUp() throws IllegalAccessException, InstantiationException {
        passwordHistoryValidationHandler = new PasswordHistoryValidationHandler();
    }

    @Test
    public void testGetName() throws Exception {
        assertEquals(passwordHistoryValidationHandler.getName(),
                "passwordHistory", "Valid getName().");
        assertNotEquals(passwordHistoryValidationHandler.getName(),
                "passwordkshfy", "Invalid getName().");
        assertFalse(Boolean.parseBoolean(passwordHistoryValidationHandler.getName()),
                "Invalid getName().");
    }

    @Test
    public void testGetFriendlyName() throws Exception {
       assertEquals(passwordHistoryValidationHandler.getFriendlyName(),
                "Password History", "Valid getFriendlyName().");
       assertNotEquals(passwordHistoryValidationHandler.getFriendlyName(),
                "passwordHistory", "Invalid getFriendlyName().");
    }

    @Test
    public void testGetCategory() throws Exception {
        assertEquals(passwordHistoryValidationHandler.getCategory(),
                "Password Policies", "Valid getCategory().");
        assertNotEquals(passwordHistoryValidationHandler.getCategory(),
                "PasswordPolicies", "Invalid getCategory().");
    }


    @Test
    public void testGetSubCategory() throws Exception {
        assertEquals(passwordHistoryValidationHandler.getSubCategory(),
                "DEFAULT", "Valid getSubCategory().");
        assertNotEquals(passwordHistoryValidationHandler.getSubCategory(),
                "PasswordPolicies", "Invalid getSubCategory().");
    }

    @Test
    public void testGetOrder() throws Exception {
        assertEquals(passwordHistoryValidationHandler.getOrder(),
                0, "Valid getOrder().");
        assertNotEquals(passwordHistoryValidationHandler.getOrder(),
                2, "Invalid getOrder().");
    }

    @Test
    public void testGetPropertyNameMapping() throws Exception {
        Map<String, String> expectedMappingTrue = new HashMap<>();
        expectedMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "Enable Password History Feature");
        expectedMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Password History validation count");
        assertEquals(passwordHistoryValidationHandler.getPropertyNameMapping(), expectedMappingTrue,
                "Valid getPropertyNameMapping() .");

        Map<String, String> expectedMappingFalse = new HashMap<>();
        expectedMappingFalse.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "Enable Password History");
        expectedMappingFalse.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Password History validation");
        assertNotEquals(passwordHistoryValidationHandler.getPropertyNameMapping(), expectedMappingFalse,
                "Invalid getPropertyNameMapping() .");
    }

    @Test
    public void testGetPropertyDescriptionMapping() throws Exception {
        Map<String, String> expecteddescriptionMappingTrue = new HashMap<>();
        expecteddescriptionMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_ENABLE,
                "Enable to disallow previously used passwords");
        expecteddescriptionMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_COUNT,
                "Restrict reusing last x number of password during password update");
        assertEquals(passwordHistoryValidationHandler.getPropertyDescriptionMapping(),
                expecteddescriptionMappingTrue, "Valid getPropertyDescriptionMapping().");

        Map<String, String> expecteddescriptionMappingFalse = new HashMap<>();
        expecteddescriptionMappingFalse.put(PasswordHistoryConstants.PW_HISTORY_ENABLE,
                "Enable to allow previously used passwords");
        expecteddescriptionMappingFalse.put(PasswordHistoryConstants.PW_HISTORY_COUNT,
                "Restrict reusing last number of password during password update");
        assertNotEquals(passwordHistoryValidationHandler.getPropertyDescriptionMapping(),
                expecteddescriptionMappingFalse, "Invalid getPropertyDescriptionMapping().");
    }

    @Test
    public void testGetPropertyNames() throws Exception {
        List<String> expectedpropertiesTrue = new ArrayList<>();
        expectedpropertiesTrue.add(PasswordHistoryConstants.PW_HISTORY_ENABLE);
        expectedpropertiesTrue.add(PasswordHistoryConstants.PW_HISTORY_COUNT);
        assertEquals(passwordHistoryValidationHandler.getPropertyNames(),
                expectedpropertiesTrue.toArray(new String[expectedpropertiesTrue.size()]),
                "Valid getPropertyDescriptionMapping().");

        List<String> expectedpropertiesFalse = new ArrayList<>();
        expectedpropertiesFalse.add(PasswordHistoryConstants.PW_HISTORY_ENABLE);
        assertNotEquals(passwordHistoryValidationHandler.getPropertyNames(),
                expectedpropertiesFalse.toArray(new String[expectedpropertiesFalse.size()]),
                "Valid getPropertyDescriptionMapping().");
    }
}
