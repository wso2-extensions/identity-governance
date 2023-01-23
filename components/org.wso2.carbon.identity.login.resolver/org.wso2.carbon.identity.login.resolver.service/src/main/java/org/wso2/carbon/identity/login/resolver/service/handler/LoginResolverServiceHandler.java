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

package org.wso2.carbon.identity.login.resolver.service.handler;

import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.login.resolver.service.constants.LoginResolverServiceConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * This handler is used to manage the login resolver configurations in the management console.
 */
public class LoginResolverServiceHandler implements IdentityConnectorConfig {

    /**
     * Retrieves the name of the handler.
     *
     * @return Name of the handler.
     */
    @Override
    public String getName() {

        return LoginResolverServiceConstants.HANDLER_NAME;
    }

    /**
     * Retrieves the friendly name of the connector configuration.
     *
     * @return Friendly name of the connector configuration.
     */
    @Override
    public String getFriendlyName() {

        return LoginResolverServiceConstants.HANDLER_FRIENDLY_NAME;
    }

    /**
     * Retrieves the category of the connector configuration.
     *
     * @return Category of the connector configuration.
     */
    @Override
    public String getCategory() {

        return LoginResolverServiceConstants.HANDLER_CATEGORY;
    }

    /**
     * Retrieves the sub-category of the connector configuration.
     *
     * @return Sub-category of the connector configuration.
     */
    @Override
    public String getSubCategory() {

        return LoginResolverServiceConstants.HANDLER_SUB_CATEGORY;
    }

    /**
     * Retrieves the order of the connector configuration.
     *
     * @return Order of the connector configuration.
     */
    @Override
    public int getOrder() {

        return 0;
    }

    /**
     * Retrieves the property name mappings of the connector configuration.
     *
     * @return Property name mappings of the connector configuration.
     */
    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.LOGIN_RESOLVER_PROPERTY_NAME_MAPPING);
        nameMapping.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.LOGIN_RESOLVER_CLASS_NAME_MAPPING);
        nameMapping.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.LoginResolverServiceNameMapping.ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING);
        return nameMapping;
    }

    /**
     * Retrieves the property description mappings of the connector configuration.
     *
     * @return Property description mappings of the connector configuration.
     */
    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        LOGIN_RESOLVER_PROPERTY_DESCRIPTION_MAPPING);
        descriptionMapping.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        LOGIN_RESOLVER_CLASS_DESCRIPTION_MAPPING);
        descriptionMapping.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.LoginResolverServiceDescriptionMapping.
                        ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING);
        return descriptionMapping;
    }

    /**
     * Retrieves the property names of the connector configuration.
     *
     * @return Property names of the connector configuration.
     */
    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY);
        properties.add(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS);
        properties.add(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES);
        return properties.toArray(new String[0]);
    }

    /**
     * Retrieves the default property values of the connector configuration.
     *
     * @param tenantDomain The tenant domain.
     * @return Default property values of the connector configuration.
     */
    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY, "false");
        defaultProperties.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.DEFAULT_LOGIN_RESOLVER_CLASS);
        defaultProperties.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.USERNAME_CLAIM_URI);
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    /**
     * Retrieves the default property values of the connector configuration.
     *
     * @param propertyNames The property names.
     * @param tenantDomain  The tenant domain.
     * @return Default property values of the connector configuration.
     */
    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(LoginResolverServiceConstants.LOGIN_RESOLVER_PROPERTY, "false");
        defaultProperties.put(LoginResolverServiceConstants.LOGIN_RESOLVER_CLASS,
                LoginResolverServiceConstants.DEFAULT_LOGIN_RESOLVER_CLASS);
        defaultProperties.put(LoginResolverServiceConstants.ALLOWED_LOGIN_ATTRIBUTES,
                LoginResolverServiceConstants.USERNAME_CLAIM_URI);
        return defaultProperties;
    }
}
