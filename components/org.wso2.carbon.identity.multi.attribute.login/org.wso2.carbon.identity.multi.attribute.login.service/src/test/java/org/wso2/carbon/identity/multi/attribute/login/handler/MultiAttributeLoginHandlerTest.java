/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.multi.attribute.login.handler;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.multi.attribute.login.constants.MultiAttributeLoginConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.MockitoAnnotations.initMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotEquals;

public class MultiAttributeLoginHandlerTest {

    MultiAttributeLoginHandler multiAttributeLoginHandler;

    @BeforeMethod
    public void setUp() {

        initMocks(this);
        multiAttributeLoginHandler = new MultiAttributeLoginHandler();
    }

    @Test
    public void testGetName() {

        assertEquals(multiAttributeLoginHandler.getName(),
                MultiAttributeLoginConstants.HANDLER_NAME, "Valid handler name.");
        assertNotEquals(multiAttributeLoginHandler.getName(),
                "multiattrssibute.logins.handler", "Invalid handler name.");
        assertFalse(Boolean.parseBoolean(multiAttributeLoginHandler.getName()),
                "Invalid handler name.");
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(multiAttributeLoginHandler.getFriendlyName(),
                MultiAttributeLoginConstants.HANDLER_FRIENDLY_NAME, "Valid FriendlyName.");
        assertNotEquals(multiAttributeLoginHandler.getFriendlyName(),
                "MultiAttributer Login", "Invalid FriendlyName.");
    }

    @Test
    public void testGetCategory() {

        assertEquals(multiAttributeLoginHandler.getCategory(),
                MultiAttributeLoginConstants.HANDLER_CATEGORY, "Valid Category.");
        assertNotEquals(multiAttributeLoginHandler.getCategory(),
                "LoginPolicies", "Invalid Category");

    }

    @Test
    public void testGetSubCategory() {

        assertEquals(multiAttributeLoginHandler.getSubCategory(),
                "DEFAULT", "Valid SubCategory.");
        assertNotEquals(multiAttributeLoginHandler.getSubCategory(),
                "LoginPolicies", "Invalid SubCategory.");

    }

    @Test
    public void testGetOrder() {

        assertEquals(multiAttributeLoginHandler.getOrder(),
                0, "Valid order.");
        assertNotEquals(multiAttributeLoginHandler.getOrder(),
                2, "Invalid order.");
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> expectedMappingTrue = new HashMap<>();
        expectedMappingTrue.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                MultiAttributeLoginConstants.MultiAttributeLoginNameMapping.MULTI_ATTRIBUTE_LOGIN_PROPERTY_NAME_MAPPING);
        expectedMappingTrue.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.MultiAttributeLoginNameMapping.ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING);
        assertEquals(multiAttributeLoginHandler.getPropertyNameMapping(), expectedMappingTrue,
                "Valid property name mapping.");

        Map<String, String> expectedMappingFalse = new HashMap<>();
        expectedMappingFalse.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                "EnableMultiAttributeLogin");
        expectedMappingFalse.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                "AllowedAttributeClaimList");
        assertNotEquals(multiAttributeLoginHandler.getPropertyNameMapping(), expectedMappingFalse,
                "Invalid property name mapping.");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> expecteddescriptionMappingTrue = new HashMap<>();
        expecteddescriptionMappingTrue.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                MultiAttributeLoginConstants.MultiAttributeLoginDescriptionMapping.MULTI_ATTRIBUTE_LOGIN_PROPERTY_DESCRIPTION_MAPPING);
        expecteddescriptionMappingTrue.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.MultiAttributeLoginDescriptionMapping.ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING);
        assertEquals(multiAttributeLoginHandler.getPropertyDescriptionMapping(), expecteddescriptionMappingTrue,
                "Valid property description mapping.");

        Map<String, String> expecteddescriptionMappingFalse = new HashMap<>();
        expecteddescriptionMappingFalse.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                "Enableusingmultipleattributesasloginidentifier");
        expecteddescriptionMappingFalse.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                "Allowedclaimlistseparatedbycommas.");
        assertNotEquals(multiAttributeLoginHandler.getPropertyDescriptionMapping(), expecteddescriptionMappingFalse,
                "Invalid property description mapping.");
    }

    @Test
    public void testGetPropertyNames() {

        List<String> expectedpropertiesTrue = new ArrayList<>();
        expectedpropertiesTrue.add(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY);
        expectedpropertiesTrue.add(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES);
        assertEquals(multiAttributeLoginHandler.getPropertyNames().length,
                expectedpropertiesTrue.size(), "Valid property description mapping.");

        List<String> expectedpropertiesFalse = new ArrayList<>();
        expectedpropertiesFalse.add(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY);
        assertNotEquals(multiAttributeLoginHandler.getPropertyNames().length,
                expectedpropertiesFalse.size(), "InValid property description mapping.");

    }

    @Test
    public void testTestGetDefaultPropertyValues() {

        assertEquals(multiAttributeLoginHandler.getOrder(), 0, "Valid order.");
        assertNotEquals(multiAttributeLoginHandler.getOrder(), null, "Invalid order.");
    }
}
