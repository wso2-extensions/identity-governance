/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.policy.handler;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.NumberUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.mgt.constants.PasswordPolicyStatusCodes;
import org.wso2.carbon.identity.mgt.policy.PolicyRegistry;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordLengthPolicy;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordNamePolicy;
import org.wso2.carbon.identity.mgt.policy.password.DefaultPasswordPatternPolicy;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.policy.internal.IdentityPasswordPolicyServiceDataHolder;
import org.wso2.carbon.identity.password.policy.util.Utils;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class PasswordPolicyValidationHandler extends AbstractEventHandler implements IdentityConnectorConfig {

    private static final Log log = LogFactory.getLog(PasswordPolicyValidationHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        // Skip password policy validation if skipPasswordPatternValidation thread local is set to true.
        if (UserCoreUtil.getSkipPasswordPatternValidationThreadLocal()) {
            return;
        }

        Map<String, Object> eventProperties = event.getEventProperties();

        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        Object credentials = eventProperties.get(IdentityEventConstants.EventProperty.CREDENTIAL);

        Property[] identityProperties;
        try {
            identityProperties = IdentityPasswordPolicyServiceDataHolder.getInstance()
                    .getIdentityGovernanceService().getConfiguration(getPropertyNames(), tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException("Error while retrieving password policy properties.", e);
        }

        // initialize to default values
        boolean passwordPolicyValidation;
        String pwMinLength = "6";
        String pwMaxLength = "12";
        String pwPattern = "^((?=.*\\\\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#$%&*])).{0,100}$";
        String pwUsernameCheckMode = "equal";
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
            } else if (PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE.equals(propertyName)) {
                if (StringUtils.isNotBlank(propertyValue)) {
                    pwUsernameCheckMode = propertyValue;
                } else {
                    log.warn("Password Policy Username Check Mode cannot be empty hence using default value: "
                            + pwUsernameCheckMode);
                }
            }
        }

        PolicyRegistry policyRegistry = new PolicyRegistry();

        String pwLengthPolicyCls = configs.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_LENGTH_CLASS);
        String pwNamePolicyCls = configs.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_NAME_CLASS);
        String pwPatternPolicyCls = configs.getModuleProperties().
                getProperty(PasswordPolicyConstants.PW_POLICY_PATTERN_CLASS);
        try {
            if (StringUtils.isNotBlank(pwLengthPolicyCls)) {
                DefaultPasswordLengthPolicy defaultPasswordLengthPolicy = (DefaultPasswordLengthPolicy) Class.
                        forName(pwLengthPolicyCls).newInstance();
                HashMap<String, String> pwPolicyLengthParams = new HashMap<>();
                pwPolicyLengthParams.put("min.length", pwMinLength);
                pwPolicyLengthParams.put("max.length", pwMaxLength);
                defaultPasswordLengthPolicy.init(pwPolicyLengthParams);
                policyRegistry.addPolicy(defaultPasswordLengthPolicy);
            }

            if (StringUtils.isNotBlank(pwNamePolicyCls)) {
                DefaultPasswordNamePolicy defaultPasswordNamePolicy = (DefaultPasswordNamePolicy) Class.
                        forName(pwNamePolicyCls).newInstance();
                HashMap<String, String> pwPolicyNameParams = new HashMap<>();
                pwPolicyNameParams.put("username.check.mode", pwUsernameCheckMode);
                defaultPasswordNamePolicy.init(pwPolicyNameParams);
                policyRegistry.addPolicy(defaultPasswordNamePolicy);
            }

            if (StringUtils.isNotBlank(pwPatternPolicyCls)) {
                DefaultPasswordPatternPolicy defaultPasswordPatternPolicy = (DefaultPasswordPatternPolicy) Class.
                        forName(pwPatternPolicyCls).newInstance();
                HashMap<String, String> pwPolicyPatternParams = new HashMap<>();
                pwPolicyPatternParams.put("pattern", pwPattern);
                pwPolicyPatternParams.put("errorMsg", errorMsg);
                defaultPasswordPatternPolicy.init(pwPolicyPatternParams);
                policyRegistry.addPolicy(defaultPasswordPatternPolicy);
            }
        } catch (Exception e) {
            throw Utils.handleEventException(
                    PasswordPolicyConstants.ErrorMessages.ERROR_CODE_LOADING_PASSWORD_POLICY_CLASSES, null, e);
        }

        try {
            policyRegistry.enforcePasswordPolicies(credentials.toString(), userName);
        } catch (PolicyViolationException e) {
            if (PasswordPolicyStatusCodes.ERROR_CODE_PASSWORD_POLICY_VIOLATION
                    .equals(e.getErrorCode())) {
                throw IdentityException.error(IdentityEventException.class, e.getErrorCode(), e.getMessage(), e);
            }
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
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Validate passwords based on a policy pattern");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, "Minimum number of characters");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, "Maximum number of characters");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "Password pattern regex");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, "Error message on pattern violation");
        nameMapping.put(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE,
                "Check if password contains or equals to username");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ENABLE, "Validate user passwords against a policy");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH,
                "Minimum number of characters in the password.");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH,
                "Maximum number of characters in the password.");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_PATTERN, "The regular expression pattern to validate" +
                " the password.");
        descriptionMapping.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG,
                "This error message will be displayed when a pattern violation is detected.");
        return descriptionMapping;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(PasswordPolicyConstants.PW_POLICY_ENABLE);
        properties.add(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH);
        properties.add(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH);
        properties.add(PasswordPolicyConstants.PW_POLICY_PATTERN);
        properties.add(PasswordPolicyConstants.PW_POLICY_ERROR_MSG);
        properties.add(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE);
        return properties.toArray(new String[0]);
    }

    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_ENABLE, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_ENABLE));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_MIN_LENGTH));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_MAX_LENGTH));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_PATTERN, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_PATTERN));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_ERROR_MSG, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_ERROR_MSG));
        defaultProperties.put(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE, configs.getModuleProperties()
                .getProperty(PasswordPolicyConstants.PW_POLICY_USERNAME_CHECK_MODE));
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
