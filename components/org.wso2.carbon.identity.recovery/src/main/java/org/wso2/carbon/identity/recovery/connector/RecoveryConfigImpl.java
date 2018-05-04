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

public class RecoveryConfigImpl implements IdentityConnectorConfig {

    private static String connectorName = "account-recovery";

    @Override
    public String getName() {
        return connectorName;
    }

    @Override
    public String getFriendlyName() {
        return "Account Recovery";
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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, "Enable " +
                "Notification Based Password Recovery");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, "Enable " +
                "Internal Notification Management");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY, "Enable Security " +
                "Question Based Password Recovery");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER, "Number Of Questions " +
                "Required For Password Recovery");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE, "Enable Username Recovery");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, "Notification Expiry Time");

        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                "Notify when Recovery Success");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                "Notify when Questions Based Recovery Starts");

        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Enable reCaptcha for Security Questions Based Password Recovery");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig
                .RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS, "Max Failed Attempts for ReCaptcha");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Show captcha for challenge question based password recovery");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);
        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableNotificationBasedPasswordRecovery = "false";
        String enableQuestionBasedPasswordRecovery = "false";
        String minimumAnswers = "2";
        String enableRecoveryQuestionPasswordReCaptcha = "true";
        String recoveryQuestionPasswordReCaptchaMaxFailedAttempts = "2";
        String enableUsernameRecovery = "false";
        String enableNotificationInternallyManage = "true";
        String expiryTime = "1440";
        String notifySuccess = "false";
        String notifyStart = "false";

        String notificationBasedPasswordRecovery = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        String questionBasedPasswordRecovery = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        String miniMumAnswerProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        String recoveryQuestionPasswordReCaptcha = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE);
        String passwordReCaptchaMaxFailedAttempts = IdentityUtil.getProperty(IdentityRecoveryConstants.
                ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS);
        String usernameRecovery = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        String notificationInternallyManged = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        String expiryTimeProperty = IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);
        String notifySuccessProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS);
        String notifyStartProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START);

        if (StringUtils.isNotEmpty(notificationBasedPasswordRecovery)) {
            enableNotificationBasedPasswordRecovery = notificationBasedPasswordRecovery;
        }
        if (StringUtils.isNotEmpty(questionBasedPasswordRecovery)) {
            enableQuestionBasedPasswordRecovery = questionBasedPasswordRecovery;
        }
        if (StringUtils.isNotEmpty(notificationInternallyManged)) {
            enableNotificationInternallyManage = notificationInternallyManged;
        }
        if (StringUtils.isNotEmpty(miniMumAnswerProperty)) {
            minimumAnswers = miniMumAnswerProperty;
        }
        if (StringUtils.isNotEmpty(recoveryQuestionPasswordReCaptcha)) {
            enableRecoveryQuestionPasswordReCaptcha = recoveryQuestionPasswordReCaptcha;
        }
        if (StringUtils.isNotEmpty(passwordReCaptchaMaxFailedAttempts)) {
            recoveryQuestionPasswordReCaptchaMaxFailedAttempts = passwordReCaptchaMaxFailedAttempts;
        }
        if (StringUtils.isNotEmpty(usernameRecovery)) {
            enableUsernameRecovery = usernameRecovery;
        }
        if (StringUtils.isNotEmpty(expiryTimeProperty)) {
            expiryTime = expiryTimeProperty;
        }
        if (StringUtils.isNotEmpty(notifySuccessProperty)) {
            notifySuccess = notifySuccessProperty;
        }
        if (StringUtils.isNotEmpty(notifyStartProperty)) {
            notifyStart = notifyStartProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                enableNotificationBasedPasswordRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                enableQuestionBasedPasswordRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER,
                minimumAnswers);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                enableRecoveryQuestionPasswordReCaptcha);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig
                        .RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS,
                recoveryQuestionPasswordReCaptchaMaxFailedAttempts);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE,
                enableUsernameRecovery);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, expiryTime);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                notifySuccess);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                notifyStart);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        return null;
    }

}
