/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Class which contains Admin forced password reset configs.
 */
public class AdminForcedPasswordResetConfigImpl implements IdentityConnectorConfig {

    private static final String connectorName = "admin-forced-password-reset";

    @Override
    public String getName() {

        return connectorName;
    }

    @Override
    public String getFriendlyName() {

        return "Password Reset";
    }

    @Override
    public String getCategory() {

        return "Account Management";
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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "Enable password reset via recovery e-mail");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "Enable password reset via OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "Enable password reset offline");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                "User gets notified with a link to reset password");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                "User gets notified with a one time password to try with SSO login");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                "An OTP generated and stored in users claims");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE);

        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableAdminPasswordResetWithRecoveryLink = "false";
        String enableAdminPasswordResetWithOTP = "false";
        String enableAdminPasswordResetOffline = "false";

        String adminPasswordRecoveryWithLinkProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK);
        String adminPasswordResetWithOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP);
        String adminPasswordResetOfflineProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE);

        if (StringUtils.isNotEmpty(adminPasswordRecoveryWithLinkProperty)) {
            enableAdminPasswordResetWithRecoveryLink = adminPasswordRecoveryWithLinkProperty;
        }
        if (StringUtils.isNotEmpty(adminPasswordResetWithOTPProperty)) {
            enableAdminPasswordResetWithOTP = adminPasswordResetWithOTPProperty;
        }
        if (StringUtils.isNotEmpty(adminPasswordResetOfflineProperty)) {
            enableAdminPasswordResetOffline = adminPasswordResetOfflineProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                enableAdminPasswordResetWithRecoveryLink);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                enableAdminPasswordResetWithOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                enableAdminPasswordResetOffline);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        return null;
    }

    private Property getPropertyObject(String type) {

        Property property = new Property();
        property.setType(type);
        return property;
    }

    @Override
    public Map<String, Property> getMetaData() {

        Map<String, Property> meta = new HashMap<>();
        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        return meta;
    }

}
