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

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.governance.IdentityGovernanceUtil.getPropertyObject;

/**
 * Class which contains user email verification configs.
 */
public class UserEmailVerificationConfigImpl implements IdentityConnectorConfig {

    private static final String connectorName = "user-email-verification";
    private static final String CATEGORY = "User Onboarding";
    private static final String FRIENDLY_NAME = "Ask Password";
    private static final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurposeJITProvisioning";
    private static final String SYSTEM_PURPOSE_GROUP = "JIT";
    private static final String JIT_PURPOSE_GROUP_TYPE = "SYSTEM";
    private static final String CALLBACK_URL = "/carbon/idpmgt/idp-mgt-edit-local.jsp?category=" + CATEGORY +
            "&subCategory=" + FRIENDLY_NAME;
    private static final String CONSENT_LIST_URL = "/carbon/consent/list-purposes.jsp?purposeGroup=" +
            SYSTEM_PURPOSE_GROUP + "&purposeGroupType=" + JIT_PURPOSE_GROUP_TYPE;

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

        return "DEFAULT";
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION,
                "Enable user email verification");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Enable account lock on creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Manage notifications sending internally");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Email verification code expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Ask password code expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension class");
        nameMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Manage JIT provisioning purposes");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION,
                "A verification notification will be triggered during user creation.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "The user account will be locked during user creation.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Disable if the client application handles notification sending.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                "Set the time span that the verification e-mail would be valid, in minutes. (For infinite validity " +
                        "period, set -1)");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                "Set the time span that the ask password e-mail would be valid, in minutes. (For infinite validity " +
                        "period, set -1)");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                "Temporary password generation extension point in ask password feature.)");
        descriptionMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Click here to manage just in time provisioning purposes.");

        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR);
        properties.add(LIST_PURPOSE_PROPERTY_KEY);

        return properties.toArray(new String[0]);
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
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION);
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
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION,
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
        try {
            defaultProperties.put(LIST_PURPOSE_PROPERTY_KEY, CONSENT_LIST_URL + "&callback=" + URLEncoder.encode
                    (CALLBACK_URL, StandardCharsets.UTF_8.name()));
        } catch (UnsupportedEncodingException e) {
            throw new IdentityGovernanceException("Error while url encoding callback url: " + CALLBACK_URL, e);
        }

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        return null;
    }

    @Override
    public Map<String, Property> getMetaData() {

        Map<String, Property> meta = new HashMap<>();

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_TEMP_PASSWORD_GENERATOR,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(LIST_PURPOSE_PROPERTY_KEY, getPropertyObject(IdentityMgtConstants.DataTypes.URI.getValue()));

        return meta;
    }

}
