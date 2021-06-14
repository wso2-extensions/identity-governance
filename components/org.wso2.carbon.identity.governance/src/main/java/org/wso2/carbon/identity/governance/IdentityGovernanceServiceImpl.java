/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.governance;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.util.IdentityApplicationConstants;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Class which contains exposed identity governance services.
 */
public class IdentityGovernanceServiceImpl implements IdentityGovernanceService {

    private static final Log log = LogFactory.getLog(IdentityGovernanceServiceImpl.class);

    public void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails)
            throws IdentityGovernanceException {

        try {
            IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
            IdentityProvider residentIdp = identityProviderManager.getResidentIdP(tenantDomain);

            IdentityProviderProperty[] identityMgtProperties = residentIdp.getIdpProperties();
            List<IdentityProviderProperty> newProperties = new ArrayList<>();
            for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
                IdentityProviderProperty prop = new IdentityProviderProperty();
                String key = identityMgtProperty.getName();
                prop.setName(key);
                if (configurationDetails.containsKey(key)) {
                    prop.setValue(configurationDetails.get(key));
                } else {
                    prop.setValue(identityMgtProperty.getValue());
                }
                newProperties.add(prop);
                configurationDetails.remove(key);
            }
            for (Map.Entry<String, String> entry : configurationDetails.entrySet()) {
                IdentityProviderProperty prop = new IdentityProviderProperty();
                prop.setName(entry.getKey());
                prop.setValue(entry.getValue());
                newProperties.add(prop);
            }

            residentIdp.setIdpProperties(newProperties.toArray(new IdentityProviderProperty[newProperties.size()]));
            FederatedAuthenticatorConfig[] authenticatorConfigs = residentIdp.getFederatedAuthenticatorConfigs();
            List<FederatedAuthenticatorConfig> configsToSave = new ArrayList<>();
            for (FederatedAuthenticatorConfig authenticatorConfig : authenticatorConfigs) {
                if (IdentityApplicationConstants.Authenticator.PassiveSTS.NAME.equals(authenticatorConfig.getName
                        ()) || IdentityApplicationConstants.Authenticator.SAML2SSO.NAME.equals(authenticatorConfig
                        .getName())) {
                    configsToSave.add(authenticatorConfig);
                }
            }
            residentIdp.setFederatedAuthenticatorConfigs(configsToSave.toArray(new
                    FederatedAuthenticatorConfig[configsToSave.size()]));
            identityProviderManager.updateResidentIdP(residentIdp, tenantDomain);
        } catch (IdentityProviderManagementException e) {
            log.error("Error while updating identityManagement Properties of Resident Idp.", e);
        }

    }

    @Override
    public Property[] getConfiguration(String tenantDomain) throws IdentityGovernanceException {

        IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
        IdentityProvider residentIdp = null;
        try {
            residentIdp = identityProviderManager.getResidentIdP(tenantDomain);
        } catch (IdentityProviderManagementException e) {
            String errorMsg = String.format("Error while retrieving resident Idp for %s tenant.", tenantDomain);
            throw new IdentityGovernanceException(errorMsg, e);
        }
        IdentityProviderProperty[] identityMgtProperties = residentIdp.getIdpProperties();
        Property[] configMap = new Property[identityMgtProperties.length];
        int index = 0;
        for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
            if (IdentityEventConstants.PropertyConfig.ALREADY_WRITTEN_PROPERTY_KEY
                    .equals(identityMgtProperty.getName())) {
                continue;
            }
            Property property = new Property();
            property.setName(identityMgtProperty.getName());
            property.setValue(identityMgtProperty.getValue());
            configMap[index] = property;
            index++;
        }

        return configMap;
    }

    @Override
    public Property[] getConfiguration(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        List<Property> requestedProperties = new ArrayList<>();
        Property[] allProperties = getConfiguration(tenantDomain);
        for (String propertyName : propertyNames) {
            for (int i = 0; i < allProperties.length; i++) {
                if (propertyName.equals(allProperties[i].getName())) {
                    requestedProperties.add(allProperties[i]);
                    break;
                }
            }

        }
        return requestedProperties.toArray(new Property[requestedProperties.size()]);

    }

    public List<IdentityConnectorConfig> getConnectorList() throws IdentityGovernanceException {

        return IdentityMgtServiceDataHolder.getInstance().getIdentityGovernanceConnectorList();
    }

    public List<ConnectorConfig> getConnectorListWithConfigs(String tenantDomain) throws IdentityGovernanceException {

        List<IdentityConnectorConfig> list = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceConnectorList();
        Property[] properties = this.getConfiguration(tenantDomain);
        List<ConnectorConfig> configs = new ArrayList<>(list.size());
        String[] connectorProperties;
        for (int i = 0; i < list.size(); i++) {
            ConnectorConfig config = new ConnectorConfig();
            Map<String, String> propertyFriendlyNames = list.get(i).getPropertyNameMapping();
            Map<String, String> propertyDescriptions = list.get(i).getPropertyDescriptionMapping();
            Map<String, Property> metaData = list.get(i).getMetaData();
            List<String> confidentialProperties = list.get(i).getConfidentialPropertyValues(tenantDomain);
            config.setFriendlyName(list.get(i).getFriendlyName());
            config.setName(list.get(i).getName());
            config.setCategory(list.get(i).getCategory());
            config.setSubCategory(list.get(i).getSubCategory());
            config.setOrder(list.get(i).getOrder());
            connectorProperties = list.get(i).getPropertyNames();
            Property[] configProperties = new Property[connectorProperties.length];
            for (int j = 0; j < connectorProperties.length; j++) {
                for (Property property : properties) {
                    if (connectorProperties[j].equals(property.getName())) {
                        configProperties[j] = property;
                        String resourceName = configProperties[j].getName();
                        configProperties[j].setDisplayName(propertyFriendlyNames.get(resourceName));
                        configProperties[j].setDescription(propertyDescriptions.get(resourceName));
                        if (metaData != null && metaData.containsKey(resourceName)) {
                            configProperties[j].setType(metaData.get(resourceName).getType());
                            configProperties[j].setRegex(metaData.get(resourceName).getRegex());
                            configProperties[j].setGroupId(metaData.get(resourceName).getGroupId());
                        }
                        if (confidentialProperties != null &&
                                confidentialProperties.contains(configProperties[j].getName())) {
                            configProperties[j].setConfidential(true);
                        }
                        break;
                    }
                }
            }
            config.setProperties(configProperties);
            configs.add(i, config);
        }
        return configs;
    }

    public Map<String, List<ConnectorConfig>> getCategorizedConnectorListWithConfigs(String tenantDomain)
            throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        Map<String, List<ConnectorConfig>> categorizedConnectorListWithConfigs = new HashMap<>();

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            String category = connectorConfig.getCategory();
            if (categorizedConnectorListWithConfigs.get(category) == null) {
                List<ConnectorConfig> categorizedConnectors = new ArrayList<>();
                categorizedConnectors.add(connectorConfig);
                categorizedConnectorListWithConfigs.put(category, categorizedConnectors);
            } else {
                categorizedConnectorListWithConfigs.get(category).add(connectorConfig);
            }
        }

        return categorizedConnectorListWithConfigs;
    }

    public List<ConnectorConfig> getConnectorListWithConfigsByCategory(String tenantDomain,
                                                                       String category)
            throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        List<ConnectorConfig> categorizedConnectorListWithConfigs = new ArrayList<>();

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            if (connectorConfig.getCategory().equals(category)) {
                categorizedConnectorListWithConfigs.add(connectorConfig);
            }
        }

        return categorizedConnectorListWithConfigs;
    }

    public ConnectorConfig getConnectorWithConfigs(String tenantDomain,
                                                   String connectorName) throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            if (connectorConfig.getName().equals(connectorName)) {
                return connectorConfig;
            }
        }
        return null;
    }

}
