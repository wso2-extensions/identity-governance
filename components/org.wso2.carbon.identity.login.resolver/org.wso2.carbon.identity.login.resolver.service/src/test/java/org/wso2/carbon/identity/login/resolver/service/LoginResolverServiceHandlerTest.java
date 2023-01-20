/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.login.resolver.service;

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.login.resolver.service.constants.LoginResolverServiceConstants;
import org.wso2.carbon.identity.login.resolver.service.handler.LoginResolverServiceHandler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotEquals;

public class LoginResolverServiceHandlerTest {

    LoginResolverServiceHandler loginResolverServiceHandler;

    @BeforeMethod
    public void setUp() {

        openMocks(this);
        loginResolverServiceHandler = new LoginResolverServiceHandler();
    }

    @Test
    public void testGetName() {

        assertEquals(loginResolverServiceHandler.getName(),
                LoginResolverServiceConstants.HANDLER_NAME, "Valid handler name.");
        assertNotEquals(loginResolverServiceHandler.getName(), "logins.resolver.handler",
                "Invalid handler name.");
        assertFalse(Boolean.parseBoolean(loginResolverServiceHandler.getName()), "Invalid handler name.");
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(loginResolverServiceHandler.getFriendlyName(),
                LoginResolverServiceConstants.HANDLER_FRIENDLY_NAME, "Valid Friendly Name.");
        assertNotEquals(loginResolverServiceHandler.getFriendlyName(), "LoginResolver",
                "Invalid Friendly Name.");
    }

    @Test
    public void testGetCategory() {

        assertEquals(loginResolverServiceHandler.getCategory(),
                LoginResolverServiceConstants.HANDLER_CATEGORY, "Valid Category.");
        assertNotEquals(loginResolverServiceHandler.getCategory(), "LoginPolicies", "Invalid Category.");

    }

    @Test
    public void testGetSubCategory() {

        assertEquals(loginResolverServiceHandler.getSubCategory(),
                "DEFAULT", "Valid SubCategory.");
        assertNotEquals(loginResolverServiceHandler.getSubCategory(),
                "LoginPolicies", "Invalid SubCategory.");

    }

    @Test
    public void testGetOrder() {

        assertEquals(loginResolverServiceHandler.getOrder(), 0, "Valid order.");
        assertNotEquals(loginResolverServiceHandler.getOrder(), 2, "Invalid order.");
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> expectedMappingTrue = new HashMap<>();
        expectedMappingTrue.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.LOGIN_RESOLVER_PROPERTY_NAME_MAPPING);
        expectedMappingTrue.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.LOGIN_RESOLVER_CLASS_NAME_MAPPING);
        expectedMappingTrue.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING);
        assertEquals(loginResolverServiceHandler.getPropertyNameMapping(), expectedMappingTrue,
                "Valid property name mapping.");

        Map<String, String> expectedMappingFalse = new HashMap<>();
        expectedMappingFalse.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY, "EnableMultiAttributeLogin");
        expectedMappingFalse.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS, "ResolverClass");
        expectedMappingFalse.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES, "AllowedAttributeClaimList");
        assertNotEquals(loginResolverServiceHandler.getPropertyNameMapping(), expectedMappingFalse,
                "Invalid property name mapping.");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> expectedDescriptionMappingTrue = new HashMap<>();
        expectedDescriptionMappingTrue.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        LOGIN_RESOLVER_PROPERTY_DESCRIPTION_MAPPING);
        expectedDescriptionMappingTrue.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        LOGIN_RESOLVER_CLASS_DESCRIPTION_MAPPING);
        expectedDescriptionMappingTrue.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING);
        assertEquals(loginResolverServiceHandler.getPropertyDescriptionMapping(), expectedDescriptionMappingTrue,
                "Valid property description mapping.");

        Map<String, String> expectedDescriptionMappingFalse = new HashMap<>();
        expectedDescriptionMappingFalse.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                "EnableUsingMultipleAttributesAsLoginIdentifier.");
        expectedDescriptionMappingFalse.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                "TheClassWhichDefinesTheLoginResolverMethodology.");
        expectedDescriptionMappingFalse.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                "AllowedClaimListSeparatedByCommas.");
        assertNotEquals(loginResolverServiceHandler.getPropertyDescriptionMapping(), expectedDescriptionMappingFalse,
                "Invalid property description mapping.");
    }

    @Test
    public void testGetPropertyNames() {

        List<String> expectedPropertiesTrue = new ArrayList<>();
        expectedPropertiesTrue.add(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY);
        expectedPropertiesTrue.add(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS);
        expectedPropertiesTrue.add(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES);
        assertEquals(loginResolverServiceHandler.getPropertyNames().length, expectedPropertiesTrue.size(),
                "Valid property description mapping.");

        List<String> expectedPropertiesFalse = new ArrayList<>();
        expectedPropertiesFalse.add(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY);
        assertNotEquals(loginResolverServiceHandler.getPropertyNames().length, expectedPropertiesFalse.size(),
                "InValid property description mapping.");

    }

    @Test
    public void testTestGetDefaultPropertyValues() {

        assertEquals(loginResolverServiceHandler.getOrder(), 0, "Valid order.");
        assertNotEquals(loginResolverServiceHandler.getOrder(), null, "Invalid order.");
    }
}
