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

package org.wso2.carbon.identity.password.expiry;

import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;

import java.util.Map;
import java.util.Properties;

/**
 * Tests for password expiry configs.
 */
public class PasswordExpiryConfigImplTest {

    private PasswordExpiryConfigImpl passwordPolicyConfig;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        passwordPolicyConfig = new PasswordExpiryConfigImpl();
    }

    @Test
    public void testGetName() {

        Assert.assertEquals(passwordPolicyConfig.getName(),
                PasswordPolicyConstants.CONNECTOR_CONFIG_NAME);
    }

    @Test
    public void testGetFriendlyName() {

        Assert.assertEquals(passwordPolicyConfig.getFriendlyName(),
                PasswordPolicyConstants.CONNECTOR_CONFIG_FRIENDLY_NAME);
    }

    @Test
    public void testGetCategory() {

        Assert.assertEquals(passwordPolicyConfig.getCategory(),
                PasswordPolicyConstants.CONNECTOR_CONFIG_CATEGORY);
    }

    @Test
    public void testGetSubCategory() {

        Assert.assertEquals(passwordPolicyConfig.getSubCategory(),
                PasswordPolicyConstants.CONNECTOR_CONFIG_SUB_CATEGORY);
    }

    @Test
    public void testGetOrder() {

        Assert.assertEquals(passwordPolicyConfig.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> propertyNameMapping = passwordPolicyConfig.getPropertyNameMapping();
        Assert.assertEquals(propertyNameMapping.size(), 3);
        Assert.assertEquals(
                propertyNameMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS),
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DISPLAYED_NAME);
        Assert.assertEquals(
                propertyNameMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY),
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DISPLAYED_NAME);
        Assert.assertEquals(
                propertyNameMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES),
                PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES_DISPLAYED_NAME);
    }

    @Test
    public void testGetDefaultPropertyValuesWithPropertyNames() throws IdentityGovernanceException {

        Assert.assertNull(passwordPolicyConfig.getDefaultPropertyValues(
                new String[]{PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS}, "test.com"));
    }

    @Test
    public void testGetPropertyNames() {

        String[] propertyNames = passwordPolicyConfig.getPropertyNames();
        Assert.assertEquals(propertyNames.length, 3);
        Assert.assertEquals(propertyNames[0], PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY);
        Assert.assertEquals(propertyNames[1], PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);
        Assert.assertEquals(propertyNames[2], PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES);
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> propertyDescriptionMapping = passwordPolicyConfig.getPropertyDescriptionMapping();
        Assert.assertEquals(propertyDescriptionMapping.size(), 3);
        Assert.assertEquals(
                propertyDescriptionMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS),
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DESCRIPTION);
        Assert.assertEquals(
                propertyDescriptionMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY),
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DESCRIPTION);
        Assert.assertEquals(
                propertyDescriptionMapping.get(PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES),
                PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES_DESCRIPTION);
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {

        Properties defaultPropertyValues = passwordPolicyConfig.getDefaultPropertyValues("test.com");
        Assert.assertEquals(defaultPropertyValues.size(), 3);
        Assert.assertEquals(
                defaultPropertyValues.get(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS),
                String.valueOf(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DEFAULT_VALUE));
        Assert.assertEquals(
                defaultPropertyValues.get(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY),
                PasswordPolicyConstants.FALSE);
        Assert.assertEquals(
                defaultPropertyValues.get(PasswordPolicyConstants.CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES),
                PasswordPolicyConstants.FALSE);
    }
}
