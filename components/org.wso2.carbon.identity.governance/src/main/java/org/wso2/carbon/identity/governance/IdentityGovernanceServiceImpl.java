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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.util.IdentityApplicationConstants;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class IdentityGovernanceServiceImpl implements IdentityGovernanceService {

    private static final Log log = LogFactory.getLog(IdentityGovernanceServiceImpl.class);

    /**
     * Store the configurations of a tenant in cache and database
     *
     * @param tenantDomain             Domain name of the tenant
     * @param configurationDetails Configurations belong to the tenant
     */
    public void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails) throws IdentityGovernanceException {

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
                        ()) || IdentityApplicationConstants.NAME.equals(authenticatorConfig.getName()) ||
                        IdentityApplicationConstants.Authenticator.SAML2SSO.NAME.equals(authenticatorConfig
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

    /**
     * Get the configurations of a tenant from cache or database
     *
     * @param tenantDomain Domain name of the tenant
     * @return Configurations belong to the tenant
     */
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
            if (IdentityEventConstants.PropertyConfig.ALREADY_WRITTEN_PROPERTY_KEY.equals(identityMgtProperty.getName())) {
                continue;
            }
            Property property = new Property();
            property.setName(identityMgtProperty.getName());
            property.setValue(identityMgtProperty.getValue());
            configMap[index] = property;
            index++;
        }

        List<IdentityConnectorConfig> list = getConnectorList();
        String[] connectorProperties;

        ArrayList<Property> propertiesToAdd = new ArrayList<>();
        for (IdentityConnectorConfig aList : list) {
            connectorProperties = aList.getPropertyNames();
            for (String connectorProperty : connectorProperties) {
                boolean propertyExists = false;
                for (Property property : configMap) {
                    if (connectorProperty.equals(property.getName())) {
                        propertyExists = true;
                        break;
                    }
                }
                if (!propertyExists) {
                    Property newProperty = new Property();
                    newProperty.setName(connectorProperty);
                    newProperty.setDescription(aList.getPropertyDescriptionMapping().get(connectorProperty));
                    newProperty.setDisplayName(aList.getPropertyNameMapping().get(connectorProperty));
                    Properties defaultPropertyValues = aList.getDefaultPropertyValues(tenantDomain);
                    newProperty.setValue(String.valueOf(defaultPropertyValues.get(connectorProperty)));
                    updateConfiguration(tenantDomain, Collections.singletonMap(newProperty.getName(), newProperty.getValue()));
                    propertiesToAdd.add(newProperty);
                }
            }
        }

        if (propertiesToAdd.size() > 0) {
            configMap = (Property[]) ArrayUtils.addAll(configMap, propertiesToAdd.toArray(new Property[0]));
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

}
