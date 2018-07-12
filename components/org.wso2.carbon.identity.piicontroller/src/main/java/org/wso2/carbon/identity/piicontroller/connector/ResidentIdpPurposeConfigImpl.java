/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */

package org.wso2.carbon.identity.piicontroller.connector;

import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.piicontroller.ConsentConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * This defines purpose related configurations for resident IDP
 */
public class ResidentIdpPurposeConfigImpl implements IdentityConnectorConfig {

    private IdentityGovernanceService identityGovernanceService;
    private final String ADD_PURPOSE_PROPERTY_KEY = "_url_addPurpose";
    private final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurpose";
    public static final String PURPOSE_MANAGEMENT = "Purpose Management";
    public static final String ADD_PURPOSE = "Add Purpose";
    public static final String VIEW_PURPOSES = "View Purposes";
    private String consentPurposeURL = "../consent/add-purpose.jsp?purposeGroup=test&purposeGroupType=grpType" +
            "&callback=../idpmgt/idp-mgt-edit-local.jsp";
    private static String consentListURL = "../consent/list-purposes.jsp?purposeGroup=test&purposeGroupType=grpType" +
            "&callback=../idpmgt/idp-mgt-edit-local.jsp";

    @Override
    public String getName() {

        return PURPOSE_MANAGEMENT;
    }

    /**
     * Returns Friendly name of the governance connector
     *
     * @return Friendly name.
     */
    @Override
    public String getFriendlyName() {

        return PURPOSE_MANAGEMENT;
    }

    /**
     * Returns the category of the governance connector.
     *
     * @return Catetgory.
     */
    @Override
    public String getCategory() {

        return ConsentConstants.PII_CONTROLLER_CONNECTOR_CATEGORY;
    }

    /**
     * Returns the sub-category of the connector.
     *
     * @return sub-category.
     */
    @Override
    public String getSubCategory() {

        return ConsentConstants.PII_CONTROLLER_CONNECTOR_SUB_CATEGORY;
    }

    /**
     * Order of the connector.
     *
     * @return The order of the connector.
     */
    @Override
    public int getOrder() {

        // Order doesn't really matter as long as it is a unique non conflicting number is.
        return 140;
    }

    /**
     * Returns property name mappings which are shown in UI as display name of the governance connector.
     *
     * @return Property name mappings which are shown as display name.
     */
    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> mapping = new HashMap<>();
        mapping.put(ADD_PURPOSE_PROPERTY_KEY, "Add Self-Sign-Up Purposes");
        mapping.put(LIST_PURPOSE_PROPERTY_KEY, "List Self-Sign-Up Purposes");
        return mapping;
    }

    /**
     * Returns property descriptions of the governance connector.
     *
     * @return Property descriptions.
     */
    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> mapping = new HashMap<>();
        mapping.put(ADD_PURPOSE_PROPERTY_KEY, "Add Self-Sign-Up Purposes");
        mapping.put(LIST_PURPOSE_PROPERTY_KEY, "List Self-Sign-Up Purposes");
        return mapping;
    }

    /**
     * Returns property names
     *
     * @return Array of property names which are supported through this governance connector.
     */
    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(ADD_PURPOSE_PROPERTY_KEY);
        properties.add(LIST_PURPOSE_PROPERTY_KEY);
        return properties.toArray(new String[properties.size()]);
    }

    /**
     * @param tenantDomain Tenant domain
     * @return Default values of the properties.
     * @throws IdentityGovernanceException
     */
    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws
            IdentityGovernanceException {

        Properties defaultPropertyValues = new Properties();
        defaultPropertyValues.put(ADD_PURPOSE_PROPERTY_KEY, consentPurposeURL);
        defaultPropertyValues.put(LIST_PURPOSE_PROPERTY_KEY, consentListURL);
        return defaultPropertyValues;
    }

    /**
     * Returns default values of properties.
     *
     * @param propertyNames Names of properties.
     * @param tenantDomain  Tenant domain.
     * @return Default values of properties.
     * @throws IdentityGovernanceException
     */
    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        Map<String, String> mapping = new HashMap<>();
        mapping.put(ADD_PURPOSE_PROPERTY_KEY, consentPurposeURL);
        mapping.put(LIST_PURPOSE_PROPERTY_KEY, consentListURL);
        return mapping;
    }
}
