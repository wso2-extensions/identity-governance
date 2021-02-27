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

import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.multi.attribute.login.constants.MultiAttributeLoginConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * This handler is used to manage multi attribute login configurations in carbon console.
 */
public class MultiAttributeLoginHandler implements IdentityConnectorConfig {

    @Override
    public String getName() {

        return MultiAttributeLoginConstants.HANDLER_NAME;
    }

    @Override
    public String getFriendlyName() {

        return MultiAttributeLoginConstants.HANDLER_FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return MultiAttributeLoginConstants.HANDLER_CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return MultiAttributeLoginConstants.HANDLER_SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                MultiAttributeLoginConstants.MultiAttributeLoginNameMapping.MULTI_ATTRIBUTE_LOGIN_PROPERTY_NAME_MAPPING);
        nameMapping.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.MultiAttributeLoginNameMapping.ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING);
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY,
                MultiAttributeLoginConstants.MultiAttributeLoginDescriptionMapping.MULTI_ATTRIBUTE_LOGIN_PROPERTY_DESCRIPTION_MAPPING);
        descriptionMapping.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.MultiAttributeLoginDescriptionMapping.ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING);
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY);
        properties.add(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String s) {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY, "false");
        defaultProperties.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.USERNAME_CLAIM_URI);
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] strings, String s) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(MultiAttributeLoginConstants.MULTI_ATTRIBUTE_LOGIN_PROPERTY, "false");
        defaultProperties.put(MultiAttributeLoginConstants.ALLOWED_LOGIN_ATTRIBUTES,
                MultiAttributeLoginConstants.USERNAME_CLAIM_URI);
        return defaultProperties;
    }
}

