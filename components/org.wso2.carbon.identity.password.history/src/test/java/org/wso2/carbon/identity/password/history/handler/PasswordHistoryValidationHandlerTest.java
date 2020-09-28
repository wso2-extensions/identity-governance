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

        assertEquals(passwordHistoryValidationHandler.getName(), "passwordHistory",
                "getName() has been changed.");
    }

    @Test
    public void testGetFriendlyName() throws Exception {

        assertEquals(passwordHistoryValidationHandler.getFriendlyName(), "Password History",
                "getFriendlyName() has been changed.");
    }

    @Test
    public void testGetCategory() throws Exception {

        assertEquals(passwordHistoryValidationHandler.getCategory(), "Password Policies",
                "getCategory() has been changed.");
    }

    @Test
    public void testGetSubCategory() throws Exception {

        assertEquals(passwordHistoryValidationHandler.getSubCategory(), "DEFAULT",
                "getSubCategory() has been changed.");
    }

    @Test
    public void testGetOrder() throws Exception {

        assertEquals(passwordHistoryValidationHandler.getOrder(), 0, "getOrder() has been changed.");
    }

    @Test
    public void testGetPropertyNameMapping() throws Exception {

        Map<String, String> expectedMapping = new HashMap<>();
        expectedMapping.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "Validate password history");
        expectedMapping.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Password history validation count");
        assertEquals(passwordHistoryValidationHandler.getPropertyNameMapping(), expectedMapping,
                "getPropertyNameMapping() has been changed.");
    }

    @Test
    public void testGetPropertyDescriptionMapping() throws Exception {

        Map<String, String> expectedDescriptionMappingTrue = new HashMap<>();
        expectedDescriptionMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "User will not be allowed to " +
                "use previously used passwords.");
        expectedDescriptionMappingTrue.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Restrict using this number of " +
                "last used passwords during password update.");
        assertEquals(passwordHistoryValidationHandler.getPropertyDescriptionMapping(),
                expectedDescriptionMappingTrue, "getPropertyDescriptionMapping() has been changed.");
    }

    @Test
    public void testGetPropertyNames() throws Exception {

        List<String> expectedPropertiesTrue = new ArrayList<>();
        expectedPropertiesTrue.add(PasswordHistoryConstants.PW_HISTORY_ENABLE);
        expectedPropertiesTrue.add(PasswordHistoryConstants.PW_HISTORY_COUNT);
        assertEquals(passwordHistoryValidationHandler.getPropertyNames(),
                expectedPropertiesTrue.toArray(new String[0]), "getPropertyNames() has been changed.");
    }
}
