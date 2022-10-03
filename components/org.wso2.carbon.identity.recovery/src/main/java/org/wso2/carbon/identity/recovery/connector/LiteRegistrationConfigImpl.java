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
 * Class which contains Lite user sign-up configs.
 */
public class LiteRegistrationConfigImpl implements IdentityConnectorConfig {

    private static final String connectorName = "lite-user-sign-up";
    private static final String CATEGORY = "User Onboarding";
    private static final String FRIENDLY_NAME = "Lite User Registration";
    private static final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurposeLiteUserSignUp";
    private static final String SYSTEM_PURPOSE_GROUP = "LITE-SIGNUP";
    private static final String SIGNUP_PURPOSE_GROUP_TYPE = "SYSTEM";
    private static final String CALLBACK_URL = "/carbon/idpmgt/idp-mgt-edit-local.jsp?category=" + CATEGORY +
            "&subCategory=" + FRIENDLY_NAME;
    private static final String consentListURL = "/carbon/consent/list-purposes.jsp?purposeGroup=" +
            SYSTEM_PURPOSE_GROUP + "&purposeGroupType=" + SIGNUP_PURPOSE_GROUP_TYPE;

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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP, "Lite user registration");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION,
                "Lock user account on creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Manage notifications sending internally");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA, "Prompt reCaptcha");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Lite user registration verification link expiry time");
        nameMapping
                .put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                        "Lite user registration SMS OTP expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX,
                "Lite user registration SMS OTP regex");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX,
                "Lite user registration callback URL regex");
        nameMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Manage Lite-Sign-Up purposes");
        nameMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE,
                "Resend confirmation email if the lite user exists");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP,
                "Allow user's to self register to the system without a password.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION,
                "Lock self registered user account until e-mail verification.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Disable if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha verification during self registration.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the verification link.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the SMS OTP.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX,
                "Regex for SMS OTP in format [allowed characters]{length}. Supported character " +
                        "ranges are a-z, A-Z, 0-9. Minimum OTP length is " +
                        IdentityMgtConstants.MINIMUM_SMS_OTP_LENGTH);
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX,
                "This prefix will be used to validate the callback URL.");
        descriptionMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Click here to manage Lite-Sign-Up purposes");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE,
                "Resend confirmation email on lite user registration if the lite user already exists");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        properties
                .add(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX);
        properties.add(LIST_PURPOSE_PROPERTY_KEY);
        properties.add(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableLiteSignUp = "false";
        String enableAccountLockOnCreation = "true";
        String enableNotificationInternallyManage = "true";
        String enableLiteRegistrationReCaptcha = "true";
        String verificationCodeExpiryTime = "1440";
        String verificationSMSOTPExpiryTime = "1";
        String verificationSMSOTPRegex = "[a-zA-Z0-9]{6}";
        String liteRegistrationCallbackRegex = IdentityRecoveryConstants.DEFAULT_CALLBACK_REGEX;
        String resendVerificationOnUserExistence = "false";

        String liteSignUpProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP);
        String accountLockProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION);
        String notificationInternallyMangedProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        String reCaptchaProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA);
        String verificationCodeExpiryTimeProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        String verificationSMSOTPExpiryTimeProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        String verificationSMSOTPRegexProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX);
        String selfRegistrationCallbackRegexProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX);
        String selfRegistrationResendVerificationOnUserExistenceProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE);
        if (StringUtils.isNotEmpty(liteSignUpProperty)) {
            enableLiteSignUp = liteSignUpProperty;
        }
        if (StringUtils.isNotEmpty(accountLockProperty)) {
            enableAccountLockOnCreation = accountLockProperty;
        }
        if (StringUtils.isNotEmpty(notificationInternallyMangedProperty)) {
            enableNotificationInternallyManage = notificationInternallyMangedProperty;
        }
        if (StringUtils.isNotEmpty(reCaptchaProperty)) {
            enableLiteRegistrationReCaptcha = reCaptchaProperty;
        }
        if (StringUtils.isNotEmpty(verificationCodeExpiryTimeProperty)) {
            verificationCodeExpiryTime = verificationCodeExpiryTimeProperty;
        }
        if (StringUtils.isNotEmpty(verificationSMSOTPExpiryTimeProperty)) {
            verificationSMSOTPExpiryTime = verificationSMSOTPExpiryTimeProperty;
        }
        if (StringUtils.isNotEmpty(verificationSMSOTPRegexProperty)) {
            verificationSMSOTPRegex = verificationSMSOTPRegexProperty;
        }
        if (StringUtils.isNotEmpty(selfRegistrationCallbackRegexProperty)) {
            liteRegistrationCallbackRegex = selfRegistrationCallbackRegexProperty;
        }
        if (StringUtils.isNotEmpty(selfRegistrationResendVerificationOnUserExistenceProperty)) {
            resendVerificationOnUserExistence = selfRegistrationResendVerificationOnUserExistenceProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP, enableLiteSignUp);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION,
                enableAccountLockOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA,
                enableLiteRegistrationReCaptcha);
        defaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                verificationCodeExpiryTime);
        defaultProperties
                .put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                        verificationSMSOTPExpiryTime);
        defaultProperties
                .put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX,
                        verificationSMSOTPRegex);
        defaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE,
                resendVerificationOnUserExistence);
        try {
            defaultProperties.put(LIST_PURPOSE_PROPERTY_KEY, consentListURL + "&callback=" + URLEncoder.encode
                    (CALLBACK_URL, StandardCharsets.UTF_8.name()));
        } catch (UnsupportedEncodingException e) {
            throw new IdentityGovernanceException("Error while encoding callback url: " + CALLBACK_URL, e);
        }
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX,
                liteRegistrationCallbackRegex);

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

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RE_CAPTCHA,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_SMS_OTP_REGEX,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(LIST_PURPOSE_PROPERTY_KEY, getPropertyObject(IdentityMgtConstants.DataTypes.URI.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_CALLBACK_REGEX,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));
        meta.put(IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        return meta;
    }

}
