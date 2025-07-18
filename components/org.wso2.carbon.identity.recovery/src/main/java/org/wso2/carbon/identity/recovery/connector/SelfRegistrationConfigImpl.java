/*
 * Copyright (c) 2016-2025, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
 * Class which contains self sign up configs.
 */
public class SelfRegistrationConfigImpl implements IdentityConnectorConfig {

    private static final String CONNECTOR_NAME = "self-sign-up";
    private static final String CATEGORY = "User Onboarding";
    private static final String FRIENDLY_NAME = "Self Registration";
    private static final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurposeSelfSignUp";
    private static final String SYSTEM_PURPOSE_GROUP = "SELF-SIGNUP";
    private static final String SIGNUP_PURPOSE_GROUP_TYPE = "SYSTEM";
    private static final String CALLBACK_URL = "/carbon/idpmgt/idp-mgt-edit-local.jsp?category=" + CATEGORY +
            "&subCategory=" + FRIENDLY_NAME;
    private static final String consentListURL = "/carbon/consent/list-purposes.jsp?purposeGroup=" +
            SYSTEM_PURPOSE_GROUP + "&purposeGroupType=" + SIGNUP_PURPOSE_GROUP_TYPE;

    @Override
    public String getName() {

        return CONNECTOR_NAME;
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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, "User self registration");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account on creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL,
                "Send OTP in e-mail");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE,
                "Enable email OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP,
                "Include uppercase characters in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP,
                "Include lowercase characters in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP,
                "Include numbers in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH,
                "OTP length");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION,
                "Enable Account Confirmation On Creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY,
                "Show username unavailability");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Manage notifications sending internally");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA, "Prompt reCaptcha");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration verification link expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration SMS OTP expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX,
                "User self registration SMS OTP regex");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                "User self registration callback URL regex");
        nameMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Manage Self-Sign-Up purposes");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                "Send sign up confirmation email");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                "Prompt reCaptcha on re-send confirmation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                "Enable Auto Login After Account Confirmation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                "Alias of the key used to sign to cookie");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL,
                "Enable dynamic registration portal");

        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Allow user's to self register to the system.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock self registered user account until e-mail verification.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL,
                "Enable to send OTP in verification e-mail instead of confirmation code.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE,
                "Enable to send email OTP for self registration.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP,
                "Enable to include uppercase characters in SMS and e-mail OTPs.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP,
                "Enable to include lowercase characters in SMS and e-mail OTPs.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP,
                "Enable to include numbers in SMS and e-mail OTPs.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH,
                "Length of the OTP for SMS and e-mail verifications. OTP length must be 4-10.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION,
                "Enable user account confirmation when the user account is not locked on creation");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY,
                "Show a descriptive error message to the user if the username is already taken. However, this may lead to username enumeration");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Disable if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha verification during self registration.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the verification link.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the SMS OTP.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX,
                "Regex for SMS OTP in format [allowed characters]{length}. Supported character " +
                        "ranges are a-z, A-Z, 0-9. Minimum OTP length is " +
                        IdentityMgtConstants.MINIMUM_SMS_OTP_LENGTH);
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                "This prefix will be used to validate the callback URL.");
        descriptionMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Click here to manage Self-Sign-Up purposes");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                "Enable sending notification for self sign up confirmation.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                "Prompt reCaptcha verification for resend confirmation");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                "User will be logged in automatically after completing the Account Confirmation ");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                "Alias of the key used to sign to cookie. The public key has to be imported to the keystore. ");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL,
                "Enable and allow users to register to the system using the dynamic registration portal.");

        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        properties
                .add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX);
        properties.add(LIST_PURPOSE_PROPERTY_KEY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL);

        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableSelfSignUp = "false";
        String enableAccountLockOnCreation = "true";
        String enableSendOTPInEmail = "false";
        String enableSelfRegistrationEmailOTP = "false";
        String useUppercaseCharactersInOTP = StringUtils.EMPTY;
        String useLowercaseCharactersInOTP = StringUtils.EMPTY;
        String useNumbersInOTP = StringUtils.EMPTY;
        String otpLength = "6";
        String enableSendNotificationOnCreation = "false";
        String showUsernameUnavailability = "true";
        String enableNotificationInternallyManage = "true";
        String enableSelfRegistrationReCaptcha = "true";
        String verificationCodeExpiryTime = "1440";
        String verificationSMSOTPExpiryTime = "1";
        String verificationSMSOTPRegex = "[a-zA-Z0-9]{6}";
        String selfRegistrationCallbackRegex = IdentityRecoveryConstants.DEFAULT_CALLBACK_REGEX;
        String enableSelfSignUpConfirmationNotification = "false";
        String enableResendConfirmationRecaptcha = "false";
        String enableSelfRegistrationAutoLogin = "false";
        String selfRegistrationAutoLoginAlias = "wso2carbon";
        String enableDynamicRegistrationPortal = "false";

        String selfSignUpProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        String accountLockProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        String sendOTPInEmailProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL);
        String selfRegistrationEmailOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE);
        String useUppercaseCharactersInOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP);
        String useLowercaseCharactersInOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP);
        String useNumbersInOTPProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP);
        String otpLengthProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH);
        String sendNotificationOnCreationProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION);
        String showUsernameUnavailabilityProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY);
        String notificationInternallyMangedProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        String reCaptchaProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        String verificationCodeExpiryTimeProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        String verificationSMSOTPExpiryTimeProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        String verificationSMSOTPRegexProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX);
        String selfRegistrationCallbackRegexProperty = IdentityUtil.getPropertyWithoutStandardPort(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX);
        String selfSignUpConfirmationNotificationProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION);
        String selfRegistrationResendConfirmationCaptchaProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE);
        String selfRegistrationAutoLogin = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN);
        String selfRegistrationAutoLoginAliasProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME);
        String enableDynamicRegistrationPortalProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL);

        if (StringUtils.isNotEmpty(selfSignUpProperty)) {
            enableSelfSignUp = selfSignUpProperty;
        }
        if (StringUtils.isNotEmpty(accountLockProperty)) {
            enableAccountLockOnCreation = accountLockProperty;
        }
        if (StringUtils.isNotEmpty(sendOTPInEmailProperty)) {
            enableSendOTPInEmail = sendOTPInEmailProperty;
        }
        if (StringUtils.isNotEmpty(selfRegistrationEmailOTPProperty)) {
            enableSelfRegistrationEmailOTP = selfRegistrationEmailOTPProperty;
        }
        if (StringUtils.isNotEmpty(useUppercaseCharactersInOTPProperty)) {
            useUppercaseCharactersInOTP = useUppercaseCharactersInOTPProperty;
        }
        if (StringUtils.isNotEmpty(useLowercaseCharactersInOTPProperty)) {
            useLowercaseCharactersInOTP = useLowercaseCharactersInOTPProperty;
        }
        if (StringUtils.isNotEmpty(useNumbersInOTPProperty)) {
            useNumbersInOTP = useNumbersInOTPProperty;
        }
        if (StringUtils.isNotEmpty(otpLengthProperty)) {
            otpLength = otpLengthProperty;
        }
        if (StringUtils.isNotEmpty(sendNotificationOnCreationProperty)) {
            enableSendNotificationOnCreation = sendNotificationOnCreationProperty;
        }
        if (StringUtils.isNotEmpty(showUsernameUnavailabilityProperty)) {
            showUsernameUnavailability = showUsernameUnavailabilityProperty;
        }
        if (StringUtils.isNotEmpty(notificationInternallyMangedProperty)) {
            enableNotificationInternallyManage = notificationInternallyMangedProperty;
        }
        if (StringUtils.isNotEmpty(reCaptchaProperty)) {
            enableSelfRegistrationReCaptcha = reCaptchaProperty;
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
            selfRegistrationCallbackRegex = selfRegistrationCallbackRegexProperty;
        }
        if (StringUtils.isNotEmpty(selfSignUpConfirmationNotificationProperty)) {
            enableSelfSignUpConfirmationNotification = selfSignUpConfirmationNotificationProperty;
        }
        if (StringUtils.isNotEmpty(selfRegistrationResendConfirmationCaptchaProperty)) {
            enableResendConfirmationRecaptcha = selfRegistrationResendConfirmationCaptchaProperty;
        }
        if (StringUtils.isNotEmpty(selfRegistrationAutoLogin)) {
            enableSelfRegistrationAutoLogin = selfRegistrationAutoLogin;
        }
        if (StringUtils.isNotEmpty(selfRegistrationAutoLoginAliasProperty)) {
            selfRegistrationAutoLoginAlias = selfRegistrationAutoLoginAliasProperty;
        }
        if (StringUtils.isNotEmpty(enableDynamicRegistrationPortalProperty)) {
            enableDynamicRegistrationPortal = enableDynamicRegistrationPortalProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, enableSelfSignUp);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                enableAccountLockOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL,
                enableSendOTPInEmail);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE,
                enableSelfRegistrationEmailOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP,
                useUppercaseCharactersInOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP,
                useLowercaseCharactersInOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP,
                useNumbersInOTP);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH,
                otpLength);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION,
                enableSendNotificationOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY,
                showUsernameUnavailability);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                enableSelfRegistrationReCaptcha);
        defaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                verificationCodeExpiryTime);
        defaultProperties
                .put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                        verificationSMSOTPExpiryTime);
        defaultProperties
                .put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX,
                        verificationSMSOTPRegex);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                enableSelfRegistrationAutoLogin);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                selfRegistrationAutoLoginAlias);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL,
                enableDynamicRegistrationPortal);

        try {
            defaultProperties.put(LIST_PURPOSE_PROPERTY_KEY, consentListURL + "&callback=" + URLEncoder.encode
                    (CALLBACK_URL, StandardCharsets.UTF_8.name()));
        } catch (UnsupportedEncodingException e) {
            throw new IdentityGovernanceException("Error while encoding callback url: " + CALLBACK_URL, e);
        }
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                selfRegistrationCallbackRegex);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                enableSelfSignUpConfirmationNotification);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                enableResendConfirmationRecaptcha);

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
        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_USE_NUMBERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_OTP_LENGTH,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SEND_CONFIRMATION_NOTIFICATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SHOW_USERNAME_UNAVAILABILITY,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMS_OTP_REGEX,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(LIST_PURPOSE_PROPERTY_KEY, getPropertyObject(IdentityMgtConstants.DataTypes.URI.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_DYNAMIC_REGISTRATION_PORTAL,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        return meta;
    }
}
