/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.connector;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Class which contains config to enable for email verification on update feature.
 */
public class EmailVerificationOnUpdateConfigImpl implements IdentityConnectorConfig {

    private static String connectorName = "user-claim-update";
    private static final String CATEGORY = "Account Management Policies";
    private static final String FRIENDLY_NAME = "User Claim Update";
    private static final String SUB_CATEGORY = "DEFAULT";

    @Override
    public String getName() {

        return connectorName;
    }

    @Override
    public String getFriendlyName() {

        return FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Enable User Email Verification On Update");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Trigger a verification notification when user's email address is updated.");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        String enableEmailVerificationOnUpdate = "false";
        String emailVerificationOnUpdateProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);

        if (StringUtils.isNotEmpty(emailVerificationOnUpdateProperty)) {
            enableEmailVerificationOnUpdate = emailVerificationOnUpdateProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                enableEmailVerificationOnUpdate);

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
