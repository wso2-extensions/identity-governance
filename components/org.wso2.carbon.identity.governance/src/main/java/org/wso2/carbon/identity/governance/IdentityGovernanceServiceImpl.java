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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.util.IdentityApplicationConstants;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.internal.EventMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    public Map<String, String> getConfiguration(String tenantDomain) throws IdentityGovernanceException {

        IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
        IdentityProvider residentIdp = null;
        try {
            residentIdp = identityProviderManager.getResidentIdP(tenantDomain);
        } catch (IdentityProviderManagementException e) {
            log.error("Error while retrieving resident Idp with identity mgt properties.");
        }
        IdentityProviderProperty[] identityMgtProperties = residentIdp.getIdpProperties();
        Map<String, String> configMap = new HashMap<>();
        for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
            if (EventMgtConstants.PropertyConfig.ALREADY_WRITTEN_PROPERTY_KEY.equals(identityMgtProperty.getName())) {
                continue;
            }
            configMap.put(identityMgtProperty.getName(), identityMgtProperty.getValue());
        }
        return configMap;
    }

    @Override
    public Map<String, String> getConfiguration(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        Map<String, String> requestedProperties = new HashMap<>();
        Map<String, String> allProperties = getConfiguration(tenantDomain);
        for (String propertyName : propertyNames) {
            String propertyValue = allProperties.get(propertyName);
            if (StringUtils.isNotBlank(propertyValue)) {
                requestedProperties.put(propertyName, allProperties.get(propertyName));
            } else {
                throw new IdentityGovernanceException(propertyName + " was an invalid property name.");
            }
        }
        return requestedProperties;

    }

    public List<IdentityGovernanceConnector> getConnectorList() throws IdentityGovernanceException {
        return IdentityMgtServiceDataHolder.getInstance().getIdentityGovernanceConnectorList();
    }

}
