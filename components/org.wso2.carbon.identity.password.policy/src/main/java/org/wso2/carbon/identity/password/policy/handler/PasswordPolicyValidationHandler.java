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
package org.wso2.carbon.identity.password.policy.handler;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.common.base.handler.InitConfig;
import org.wso2.carbon.identity.event.AbstractEventHandler;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.mgt.policy.PolicyRegistry;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordLengthPolicy;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordNamePolicy;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordPatternPolicy;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.policy.internal.IdentityPasswordPolicyServiceDataHolder;
import org.wso2.carbon.identity.password.policy.util.Utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;


/**
 * Event handler and governance connector to handle password policy
 */

public class PasswordPolicyValidationHandler extends AbstractEventHandler implements IdentityConnectorConfig {

    private static final Log log = LogFactory.getLog(PasswordPolicyValidationHandler.class);

    @Override
    public void handleEvent(Event event) throws EventException {

        Map<String, Object> eventProperties = event.getEventProperties();

        String userName = (String) eventProperties.get(EventConstants.EventProperty.USER_NAME);
        String tenantDomain = (String) eventProperties.get(EventConstants.EventProperty.TENANT_DOMAIN);
        Object credentials = eventProperties.get(EventConstants.EventProperty.CREDENTIAL);

        Property[] identityProperties;
        try {
            identityProperties = IdentityPasswordPolicyServiceDataHolder.getInstance()
                    .getIdentityGovernanceService().getConfiguration(getPropertyNames(), tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new EventException("Error while retrieving password policy properties.", e);
        }

        /*initialize to default values*/
        boolean passwordPolicyValidation = false;
        String pwMinLength = "6";
        String pwMaxLength = "12";
        String pwPattern = "^((?=.*\\\\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#$%&*])).{0,100}$";
        String errorMsg = "Password pattern policy violated. Password should contain a digit[0-9], a lower case " +
                "letter[a-z], an upper case letter[A-Z], one of !@#$%&* characters";

        for (Property identityProperty : identityProperties) {

            if (identityProperty == null) {
                continue;
            }

            String propertyName = identityProperty.getName();
            String propertyValue = identityProperty.getValue();

            if (PasswordPolicyConstants.PW_POLICY_ENABLE.equals(propertyName)) {
                passwordPolicyValidation = BooleanUtils.toBoolean(propertyValue);
                if (!passwordPolicyValidation) {
                    if (log.isDebugEnabled()) {
                        log.debug("Password Policy validation is disabled");
                    }
                    return;
                }
            } else if (PasswordPolicyConstants.PW_POLICY_MIN_LENGTH.equals(propertyName)) {
                if (NumberUtils.isNumber(propertyValue) && Integer.parseInt(propertyValue) > 0) {
                    pwMinLength = propertyValue;
                } else {
                    log.warn("Password Policy MIN Length is not correct hence using default value: " + pwMinLength);
                }
            } else if (PasswordPolicyConstants.PW_POLICY_MAX_LENGTH.equals(propertyName)) {
                if (NumberUtils.isNumber(propertyValue) && Integer.parseInt(propertyValue) > 0) {
                    pwMaxLength = propertyValue;
                } else {
                    log.warn("Password Policy MAX Length is not correct hence using default value: " + pwMaxLength);
                }
            } else if (PasswordPolicyConstants.PW_POLICY_PATTERN.equals(propertyName)) {
                if (StringUtils.isNotBlank(propertyValue)) {
                    pwPattern = propertyValue;
                } else {
                    log.warn("Password Policy Pattern is not correct hence using default value: " + pwPattern);
                }
            } else if (PasswordPolicyConstants.PW_POLICY_ERROR_MSG.equals(propertyName)) {
                if (StringUtils.isNotBlank(propertyValue)) {
                    errorMsg = propertyValue;
                } else {
                    log.warn("Password Policy Error Msg cannot be Empty hence using default Msg: " + errorMsg);
                }
            }
        }

        PolicyRegistry policyRegistry = new PolicyRegistry();

        String pwLengthPolicyCls = moduleConfig.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_LENGTH_CLASS);
        String pwNamePolicyCls = moduleConfig.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_NAME_CLASS);
        String pwPatternPolicyCls = moduleConfig.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_PATTERN_CLASS);
        try {
            if (StringUtils.isNotBlank(pwLengthPolicyCls)) {
                DefaultPasswordLengthPolicy defaultPasswordLengthPolicy = (DefaultPasswordLengthPolicy) Class.
                        forName(pwLengthPolicyCls).newInstance();
                HashMap pwPolicyLengthParams = new HashMap<String, String>();
                pwPolicyLengthParams.put("min.length", pwMinLength);
                pwPolicyLengthParams.put("max.length", pwMaxLength);
                defaultPasswordLengthPolicy.init(pwPolicyLengthParams);
                policyRegistry.addPolicy(defaultPasswordLengthPolicy);
            }

            if (StringUtils.isNotBlank(pwNamePolicyCls)) {
                DefaultPasswordNamePolicy defaultPasswordNamePolicy = (DefaultPasswordNamePolicy) Class.
                        forName(pwNamePolicyCls).newInstance();
                policyRegistry.addPolicy(defaultPasswordNamePolicy);
            }

            if (StringUtils.isNotBlank(pwPatternPolicyCls)) {
                DefaultPasswordPatternPolicy defaultPasswordPatternPolicy = (DefaultPasswordPatternPolicy) Class.
                        forName(pwPatternPolicyCls).newInstance();
                HashMap pwPolicyPatternParams = new HashMap<String, String>();
                pwPolicyPatternParams.put("pattern", pwPattern);
                pwPolicyPatternParams.put("errorMsg", errorMsg);
                defaultPasswordPatternPolicy.init(pwPolicyPatternParams);
                policyRegistry.addPolicy(defaultPasswordPatternPolicy);
            }
        } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
            throw Utils.handleEventException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_CODE_LOADING_PASSWORD_POLICY_CLASSES, null, e);
        } catch (Exception e) {
            throw Utils.handleEventException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_CODE_LOADING_PASSWORD_POLICY_CLASSES, null, e);
        }

        try {
            policyRegistry.enforcePasswordPolicies(credentials.toString(), userName);
        } catch (PolicyViolationException e) {
            throw Utils.handleEventException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_CODE_VALIDATING_PASSWORD_POLICY, e.getMessage(), e);
        }
    }

    @Override
    public String getName() {
        return "passwordPolicy";
    }

    @Override
    public String getFriendlyName() {
        return "Password Patterns";
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
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable Password Policy Feature");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, "Password Policy Min Length");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, "Password Policy Max Length");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Password Policy Pattern");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, "Password Policy Error Message");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Enable password pattern policy");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Allowed password regex pattern");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG,
                               "Error message for invalid password patterns");
        return descriptionMapping;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
        IdentityPasswordPolicyServiceDataHolder.getInstance().getBundleContext().registerService
                (IdentityConnectorConfig.class.getName(), this, null);
    }

    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(PasswordPolicyConstants.PW_POLICY_ENABLE);
        properties.add(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH);
        properties.add(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH);
        properties.add(PasswordPolicyConstants.PW_POLICY_PATTERN);
        properties.add(PasswordPolicyConstants.PW_POLICY_ERROR_MSG);
        return properties.toArray(new String[properties.size()]);
    }

    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {
        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_ENABLE, moduleConfig.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_ENABLE));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, moduleConfig.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, moduleConfig.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_PATTERN, moduleConfig.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_PATTERN));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, moduleConfig.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_ERROR_MSG));
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {
        return null;
    }
}
