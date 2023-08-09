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

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * This class contains the password expiry config implementation.
 */
public class PasswordExpiryConfigImpl implements IdentityConnectorConfig {

    private static final Log log = LogFactory.getLog(PasswordExpiryConfigImpl.class);

    @Override
    public String getName() {

        return PasswordPolicyConstants.CONNECTOR_CONFIG_NAME;
    }

    @Override
    public String getFriendlyName() {

        return PasswordPolicyConstants.CONNECTOR_CONFIG_FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return PasswordPolicyConstants.CONNECTOR_CONFIG_CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return PasswordPolicyConstants.CONNECTOR_CONFIG_SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    @SuppressFBWarnings("HARD_CODE_PASSWORD")
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY,
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DISPLAYED_NAME);
        nameMapping.put(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS,
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DISPLAYED_NAME);
        return nameMapping;
    }

    @Override
    @SuppressFBWarnings("HARD_CODE_PASSWORD")
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY,
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DESCRIPTION);
        nameMapping.put(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS,
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DESCRIPTION);
        return nameMapping;
    }

    @Override
    public String[] getPropertyNames() {

        return PasswordPolicyUtils.getPasswordExpiryPropertyNames();

    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        // 5 minutes in seconds.
        String enablePasswordExpiry = PasswordPolicyConstants.FALSE;
        String passwordExpiryInDays =
                String.valueOf(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DEFAULT_VALUE);

        String enablePasswordExpiryProperty = IdentityUtil.getProperty(
                PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY);
        String passwordExpiryInDaysProperty = IdentityUtil.getProperty(
                PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS);

        if (StringUtils.isNotBlank(enablePasswordExpiryProperty)) {
            enablePasswordExpiry = enablePasswordExpiryProperty;
        }
        if (StringUtils.isNotBlank(passwordExpiryInDaysProperty)) {
            passwordExpiryInDays = passwordExpiryInDaysProperty;
        }
        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(PasswordPolicyConstants.CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY, enablePasswordExpiry);
        defaultProperties.put(PasswordPolicyConstants.CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS, passwordExpiryInDays);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] strings, String s) throws IdentityGovernanceException {

        return null;
    }
}
