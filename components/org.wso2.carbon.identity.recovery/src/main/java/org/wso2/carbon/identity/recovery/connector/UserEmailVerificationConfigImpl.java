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
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class UserEmailVerificationConfigImpl implements IdentityConnectorConfig {

    private static String connectorName = "user-email-verification";

    @Override
    public String getName() {
        return connectorName;
    }

    @Override
    public String getFriendlyName() {
        return "User Onboarding";
    }

    @Override
    public String getCategory() {
        return "Account Management Policies";
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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable User Email Verification");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Internal Notification Management");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Email verification code expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Ask password code expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension class");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable to trigger a verification notification during user creation");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user creation");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Set the number of minutes the email verification mail would be valid.(Negative value for infinite " +
                        "validity)");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Set the number of minutes the ask password mail would be valid. (Negative value for infinite " +
                        "validity)");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension point in ask password feature)");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR);

        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableEmailVerification = "false";
        String enableEmailAccountLockOnCreation = "true";
        String enableNotificationInternallyManage = "true";
        String emailVerificationCodeExpiry = "1440";
        String askPasswordCodeExpiry = "1440";
        String askPasswordTempPassExtension = "org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator";

        String emailVerificationProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION);
        String emailVerificationCodeExpiryProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME);
        String askPasswordCodeExpiryProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME);
        String askPasswordTempPasswordProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR);
        String lockOnCreationProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        String notificationInternallyManagedProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);

        if (StringUtils.isNotEmpty(emailVerificationProperty)) {
            enableEmailVerification = emailVerificationProperty;
        }
        if (StringUtils.isNotEmpty(lockOnCreationProperty)) {
            enableEmailAccountLockOnCreation = lockOnCreationProperty;
        }
        if (StringUtils.isNotEmpty(notificationInternallyManagedProperty)) {
            enableNotificationInternallyManage = notificationInternallyManagedProperty;
        }
        if (StringUtils.isNotEmpty(emailVerificationCodeExpiryProperty)) {
            emailVerificationCodeExpiry = emailVerificationCodeExpiryProperty;
        }
        if (StringUtils.isNotEmpty(askPasswordCodeExpiryProperty)) {
            askPasswordCodeExpiry = askPasswordCodeExpiryProperty;
        }
        if (StringUtils.isNotBlank(askPasswordTempPasswordProperty)) {
            askPasswordTempPassExtension = askPasswordTempPasswordProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                enableEmailVerification);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                emailVerificationCodeExpiry);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                askPasswordCodeExpiry);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                enableEmailAccountLockOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                askPasswordTempPassExtension);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        return null;
    }

}
