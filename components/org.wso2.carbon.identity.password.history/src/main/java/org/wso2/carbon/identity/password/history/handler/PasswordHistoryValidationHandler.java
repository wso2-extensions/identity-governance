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
package org.wso2.carbon.identity.password.history.handler;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.password.history.Util.Utils;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;
import org.wso2.carbon.identity.password.history.internal.IdentityPasswordHistoryServiceDataHolder;
import org.wso2.carbon.identity.password.history.store.PasswordHistoryDataStore;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.password.history.Util.Utils.isPasswordTrimEnabled;

public class PasswordHistoryValidationHandler extends AbstractEventHandler implements IdentityConnectorConfig {

    private static final Log log = LogFactory.getLog(PasswordHistoryValidationHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties
                .get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants
                .RealmConfig.PROPERTY_DOMAIN_NAME);
        if (StringUtils.isBlank(domainName)) {
            domainName = IdentityUtil.getPrimaryDomainName();
        }
        User user = new User();
        user.setUserName(userName);
        user.setUserStoreDomain(domainName);
        user.setTenantDomain(tenantDomain);

        Property[] identityProperties;
        try {
            identityProperties = IdentityPasswordHistoryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService().getConfiguration(getPropertyNames(), tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException("Error while retrieving account lock handler properties.", e);
        }

        boolean passwordHistoryValidation = false;
        int historyCount = 0;
        String hashingAlgorithm;
        for (Property identityProperty : identityProperties) {
            if (PasswordHistoryConstants.PW_HISTORY_ENABLE.equals(identityProperty.getName())) {
                passwordHistoryValidation = Boolean.parseBoolean(identityProperty.getValue());
            } else if (PasswordHistoryConstants.PW_HISTORY_COUNT.equals(identityProperty.getName())) {
                historyCount = Integer.parseInt(identityProperty.getValue());
            }
        }

        if (!passwordHistoryValidation) {
            if (log.isDebugEnabled()) {
                log.debug("Password History validation is disabled");
            }
            return;
        }

        if (historyCount <= 0) {
            //The history should not validate
            return;
        }

        hashingAlgorithm = configs.getModuleProperties().getProperty(PasswordHistoryConstants.PW_HISTORY_HASHING_ALGORITHM);
        String passwordHistoryDataStoreClass = configs.getModuleProperties().getProperty(
                PasswordHistoryConstants.PW_HISTORY_DATA_STORE);

        if (StringUtils.isBlank(passwordHistoryDataStoreClass)) {
            passwordHistoryDataStoreClass = "org.wso2.carbon.identity.password.history.store.Impl.DefaultPasswordHistoryDataStore";
        }

        PasswordHistoryDataStore passwordHistoryDataStore;
        try {
            Class<?> cls = Class.forName(passwordHistoryDataStoreClass);
            Class[] parameterTypes = new Class[]{String.class, Integer.TYPE};
            Constructor<?> cons = cls.getConstructor(parameterTypes);
            Object[] arguments = {hashingAlgorithm, historyCount};
            passwordHistoryDataStore = (PasswordHistoryDataStore) cons.newInstance(arguments);
        } catch (ClassNotFoundException | InvocationTargetException | SecurityException | NoSuchMethodException |
                InstantiationException | IllegalArgumentException | IllegalAccessException e) {
            throw Utils.handleEventException(
                    PasswordHistoryConstants.ErrorMessages.ERROR_CODE_LOADING_HISTORY_DATA_SOURCE, null, e);
        }

        if (IdentityEventConstants.Event.PRE_UPDATE_CREDENTIAL.equals(event.getEventName()) ||
                IdentityEventConstants.Event.PRE_UPDATE_CREDENTIAL_BY_ADMIN.equals(event.getEventName())) {
            Object credential = event.getEventProperties().get(IdentityEventConstants.EventProperty.CREDENTIAL);
            try {
                if (isPasswordTrimEnabled() && credential != null) {
                    credential = credential.toString().trim();
                }
                boolean validate = passwordHistoryDataStore.validate(user, credential);
                if (!validate) {
                    throw Utils.handleEventException(
                            PasswordHistoryConstants.ErrorMessages.ERROR_CODE_HISTORY_VIOLATE, null);
                }
            } catch (IdentityPasswordHistoryException e) {
                throw Utils.handleEventException(
                        PasswordHistoryConstants.ErrorMessages.ERROR_CODE_VALIDATING_HISTORY, null, e);
            }
        }

        if (IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL.equals(event.getEventName()) || IdentityEventConstants.Event
                .POST_UPDATE_CREDENTIAL_BY_ADMIN.equals(event.getEventName()) || IdentityEventConstants.Event
                .POST_ADD_USER.equals(event.getEventName())) {
            Object credential = event.getEventProperties().get(IdentityEventConstants.EventProperty.CREDENTIAL);
            if (isPasswordTrimEnabled() && credential != null) {
                credential = credential.toString().trim();
            }
            try {
                passwordHistoryDataStore.store(user, credential);
            } catch (IdentityPasswordHistoryException e) {
                throw Utils.handleEventException(PasswordHistoryConstants.ErrorMessages.ERROR_CODE_STORING_HISTORY, null, e);
            }
        }

        if (IdentityEventConstants.Event.POST_DELETE_USER.equals(event.getEventName())) {
            try {
                passwordHistoryDataStore.remove(user);
            } catch (IdentityPasswordHistoryException e) {
                throw Utils.handleEventException(PasswordHistoryConstants.ErrorMessages.ERROR_CODE_DELETE_HISTORY,
                        user.getUserName(), e);
            }
        }
    }

    @Override
    public String getName() {

        return "passwordHistory";
    }

    @Override
    public String getFriendlyName() {

        return "Password History";
    }

    @Override
    public String getCategory() {

        return "Password Policies";
    }

    @Override
    public String getSubCategory() {

        return "DEFAULT";
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "Validate password history");
        nameMapping.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Password history validation count");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, "User will not be allowed to use " +
                "previously used passwords.");
        descriptionMapping.put(PasswordHistoryConstants.PW_HISTORY_COUNT, "Restrict using this number of last used " +
                "passwords during password update.");
        return descriptionMapping;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
        IdentityPasswordHistoryServiceDataHolder.getInstance().getBundleContext().registerService
                (IdentityConnectorConfig.class.getName(), this, null);
    }

    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(PasswordHistoryConstants.PW_HISTORY_ENABLE);
        properties.add(PasswordHistoryConstants.PW_HISTORY_COUNT);
        return properties.toArray(new String[0]);
    }

    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(PasswordHistoryConstants.PW_HISTORY_ENABLE, configs.getModuleProperties()
                .getProperty(PasswordHistoryConstants.PW_HISTORY_ENABLE));
        defaultProperties.put(PasswordHistoryConstants.PW_HISTORY_COUNT, configs.getModuleProperties()
                .getProperty(PasswordHistoryConstants.PW_HISTORY_COUNT));
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        return null;
    }
}
