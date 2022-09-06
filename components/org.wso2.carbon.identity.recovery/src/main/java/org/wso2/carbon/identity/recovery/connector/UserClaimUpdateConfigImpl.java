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

import org.apache.axiom.om.OMElement;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.util.IdentityConfigParser;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.namespace.QName;

import static org.wso2.carbon.identity.governance.IdentityGovernanceUtil.getPropertyObject;

/**
 * Class which contains user claim update configs.
 */
public class UserClaimUpdateConfigImpl implements IdentityConnectorConfig {

    private static final String CONNECTOR_NAME = "user-claim-update";
    private static final String CATEGORY = "Other Settings";
    private static final String FRIENDLY_NAME = "User Claim Update";
    private static final String SUB_CATEGORY = "DEFAULT";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_CODE_EXPIRY_TIME = "1440";
    private static final String DEFAULT_ENABLE_VALUE_FOR_EMAIL_VERIFICATION_ON_UPDATE = "false";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL = "false";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP = "true";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP = "true";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP = "true";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH = "6";
    private static final String DEFAULT_ENABLE_VALUE_FOR_EMAIL_NOTIFICATION_ON_UPDATE = "false";
    private static final String DEFAULT_MOBILE_NUM_VERIFICATION_ON_UPDATE_SMS_OTP_EXPIRY_TIME = "5";
    private static final String DEFAULT_ENABLE_VALUE_FOR_MOBILE_NUMBER_VERIFICATION_ON_UPDATE = "false";
    private static final String DEFAULT_MOBILE_NUM_VERIFICATION_BY_PRIVILEGED_USERS = "false";
    private static final String USER_CLAIM_UPDATE_ELEMENT = "UserClaimUpdate";
    private static final String ENABLE_ELEMENT = "Enable";
    private static final String SEND_OTP_IN_EMAIL_ELEMENT = "SendOTPInEmail";
    private static final String USE_UPPERCASE_ELEMENT = "UseUppercaseInOtp";
    private static final String USE_LOWERCASE_ELEMENT = "UseLowercaseInOtp";
    private static final String USE_NUMERIC_ELEMENT = "UseNumericInOtp";
    private static final String OTP_LENGTH_ELEMENT = "OTPLength";
    private static final String CLAIM_ELEMENT = "Claim";
    private static final String OTP_ELEMENT = "OTP";
    private static final String CLAIM_URI = "uri";
    private static final String VERIFICATION_CODE_ELEMENT = "VerificationCode";
    private static final String EXPIRY_TIME_ELEMENT = "ExpiryTime";
    private static final String VERIFICATION_ON_UPDATE_ELEMENT = "VerificationOnUpdate";
    private static final String NOTIFICATION_ON_UPDATE_ELEMENT = "NotificationOnUpdate";
    private static final String ENABLE_MOBILE_VERIFICATION_PRIVILEGED_USER = "EnableVerificationByPrivilegedUser";
    private static String enableEmailVerificationOnUpdateProperty = null;
    private static String enableSendOTPInEmailProperty = null;
    private static String useUppercaseCharactersInOTPProperty = null;
    private static String useLowercaseCharactersInOTPProperty = null;
    private static String useNumbersInOTPProperty = null;
    private static String otpLengthProperty = null;
    private static String emailVerificationOnUpdateCodeExpiryProperty = null;
    private static String enableEmailNotificationOnUpdateProperty = null;
    private static String enableMobileNumVerificationOnUpdateProperty = null;
    private static String mobileNumVerificationOnUpdateCodeExpiryProperty = null;
    private static String mobileNumVerificationByPrivilegedUsersProperty = null;

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
                "Enable user email verification on update");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL,
                "Send OTP in e-mail");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP,
                "Include uppercase characters in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP,
                "Include lowercase characters in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP,
                "Include numbers in OTP");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH,
                "OTP length");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Email verification on update link expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                "Enable user email notification on update");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Enable user mobile number verification on update");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                "Enable mobile number verification by privileged users");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Mobile number verification on update SMS OTP expiry time");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Trigger a verification notification when user's email address is updated.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL,
                "Enable to send OTP in verification e-mail instead of confirmation code.");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP,
                "Enable to include uppercase characters in SMS and e-mail OTPs");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP,
                "Enable to include lowercase characters in SMS and e-mail OTPs.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP,
                "Enable to include numbers in SMS and e-mail OTPs.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH,
                "Length of the OTP for SMS and e-mail verifications. OTP length must be 4-10.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Validity time of the email confirmation link in minutes.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                "Trigger a notification to the existing email address when the user attempts to update the existing " +
                        "email address.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Trigger a verification SMS OTP when user's mobile number is updated.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Validity time of the mobile number confirmation OTP in minutes.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                "Allow privileged users to initiate mobile number verification on update.");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        String enableEmailVerificationOnUpdate = DEFAULT_ENABLE_VALUE_FOR_EMAIL_VERIFICATION_ON_UPDATE;
        String enableSendOTPInEmail = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL;
        String useUppercaseCharactersInOTP = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP;
        String useLowercaseCharactersInOTP = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP;
        String useNumbersInOTP = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP;
        String otpLength = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH;
        String emailVerificationOnUpdateCodeExpiry = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_CODE_EXPIRY_TIME;
        String enableEmailNotificationOnUpdate = DEFAULT_ENABLE_VALUE_FOR_EMAIL_NOTIFICATION_ON_UPDATE;
        String enableMobileNumVerificationOnUpdate = DEFAULT_ENABLE_VALUE_FOR_MOBILE_NUMBER_VERIFICATION_ON_UPDATE;
        String mobileNumVerificationOnUpdateCodeExpiry = DEFAULT_MOBILE_NUM_VERIFICATION_ON_UPDATE_SMS_OTP_EXPIRY_TIME;
        String mobileNumVerificationByPrivilegedUsers = DEFAULT_MOBILE_NUM_VERIFICATION_BY_PRIVILEGED_USERS;

        loadConfigurations();

        if (StringUtils.isNotBlank(enableEmailVerificationOnUpdateProperty)) {
            enableEmailVerificationOnUpdate = enableEmailVerificationOnUpdateProperty;
        }
        if (StringUtils.isNotEmpty(enableSendOTPInEmailProperty)) {
            enableSendOTPInEmail = enableSendOTPInEmailProperty;
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
        if (StringUtils.isNotBlank(emailVerificationOnUpdateCodeExpiryProperty)) {
            emailVerificationOnUpdateCodeExpiry = emailVerificationOnUpdateCodeExpiryProperty;
        }
        if (StringUtils.isNotBlank(enableEmailNotificationOnUpdateProperty)) {
            enableEmailNotificationOnUpdate = enableEmailNotificationOnUpdateProperty;
        }
        if (StringUtils.isNotBlank(enableMobileNumVerificationOnUpdateProperty)) {
            enableMobileNumVerificationOnUpdate = enableMobileNumVerificationOnUpdateProperty;
        }
        if (StringUtils.isNotBlank(mobileNumVerificationOnUpdateCodeExpiryProperty)) {
            mobileNumVerificationOnUpdateCodeExpiry = mobileNumVerificationOnUpdateCodeExpiryProperty;
        }
        if (StringUtils.isNotBlank(mobileNumVerificationByPrivilegedUsersProperty)) {
            mobileNumVerificationByPrivilegedUsers = mobileNumVerificationByPrivilegedUsersProperty;
        }

        Properties properties = new Properties();
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                enableEmailVerificationOnUpdate);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL,
                enableSendOTPInEmail);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP,
                useUppercaseCharactersInOTP);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP,
                useLowercaseCharactersInOTP);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP,
                useNumbersInOTP);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH,
                otpLength);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                emailVerificationOnUpdateCodeExpiry);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                enableEmailNotificationOnUpdate);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                enableMobileNumVerificationOnUpdate);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                mobileNumVerificationOnUpdateCodeExpiry);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                mobileNumVerificationByPrivilegedUsers);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        Properties properties = getDefaultPropertyValues(tenantDomain);
        Map<String, String> filteredDefaultProperties = new HashMap<>();
        if (ArrayUtils.isNotEmpty(propertyNames)) {
            for (String propertyName : propertyNames) {
                if (properties.containsKey(propertyName)) {
                    filteredDefaultProperties.put(propertyName, properties.getProperty(propertyName));
                }
            }
        }
        return filteredDefaultProperties;
    }

    /**
     * Read configuration values defined in identity.xml.
     */
    private void loadConfigurations() {

        // Read configuration values defined in identity.xml.
        OMElement userClaimUpdate = IdentityConfigParser.getInstance().getConfigElement(USER_CLAIM_UPDATE_ELEMENT);
        Iterator claims = null;
        OMElement otpConfigs = null;
        if (userClaimUpdate != null) {
            claims = userClaimUpdate.getChildrenWithName(new QName(IdentityCoreConstants
                    .IDENTITY_DEFAULT_NAMESPACE, CLAIM_ELEMENT));
            otpConfigs = userClaimUpdate.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, OTP_ELEMENT));
        }

        if (claims != null) {
            while (claims.hasNext()) {
                OMElement claim = (OMElement) claims.next();
                String claimURI = claim.getAttributeValue(new QName(CLAIM_URI));
                /* Currently claim verification on update support is there only for http://wso2.org/claims/emailaddress
                and http://wso2.org/claims/mobile. */
                if (IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM.equals(claimURI)) {
                    OMElement verificationOnUpdate = claim.getFirstChildWithName(new QName(IdentityCoreConstants
                            .IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_ON_UPDATE_ELEMENT));
                    if (verificationOnUpdate != null) {
                        enableEmailVerificationOnUpdateProperty = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, ENABLE_ELEMENT)).getText();
                        OMElement verificationCode = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_CODE_ELEMENT));
                        if (verificationCode != null) {
                            emailVerificationOnUpdateCodeExpiryProperty = verificationCode.getFirstChildWithName(new
                                    QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, EXPIRY_TIME_ELEMENT))
                                    .getText();
                        }
                    }
                    OMElement notificationOnUpdate = claim.getFirstChildWithName(new QName(IdentityCoreConstants
                            .IDENTITY_DEFAULT_NAMESPACE, NOTIFICATION_ON_UPDATE_ELEMENT));
                    if (notificationOnUpdate != null) {
                        enableEmailNotificationOnUpdateProperty = notificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, ENABLE_ELEMENT)).getText();
                    }
                } else if (IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM.equals(claimURI)) {
                    OMElement verificationOnUpdate = claim.getFirstChildWithName(new QName(IdentityCoreConstants
                            .IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_ON_UPDATE_ELEMENT));
                    if (verificationOnUpdate != null) {
                        enableMobileNumVerificationOnUpdateProperty = verificationOnUpdate.getFirstChildWithName(new
                                QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, ENABLE_ELEMENT)).getText();
                        OMElement privilegeUserMobileVerification = verificationOnUpdate.getFirstChildWithName(
                                new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE,
                                        ENABLE_MOBILE_VERIFICATION_PRIVILEGED_USER));
                        if (privilegeUserMobileVerification != null) {
                            mobileNumVerificationByPrivilegedUsersProperty = privilegeUserMobileVerification.getText();
                        }
                        OMElement verificationCode = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_CODE_ELEMENT));
                        if (verificationCode != null) {
                            mobileNumVerificationOnUpdateCodeExpiryProperty = verificationCode.getFirstChildWithName(
                                    new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, EXPIRY_TIME_ELEMENT))
                                    .getText();
                        }
                    }
                }
            }
        }
        if (otpConfigs != null) {
            enableSendOTPInEmailProperty = otpConfigs.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, SEND_OTP_IN_EMAIL_ELEMENT)).getText();
            useUppercaseCharactersInOTPProperty = otpConfigs.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, USE_UPPERCASE_ELEMENT)).getText();
            useLowercaseCharactersInOTPProperty = otpConfigs.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, USE_LOWERCASE_ELEMENT)).getText();
            useNumbersInOTPProperty = otpConfigs.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, USE_NUMERIC_ELEMENT)).getText();
            otpLengthProperty = otpConfigs.getFirstChildWithName(new QName
                    (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, OTP_LENGTH_ELEMENT)).getText();
        }
    }

    @Override
    public Map<String, Property> getMetaData() {

        Map<String, Property> meta = new HashMap<>();

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH,
                getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        meta.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                getPropertyObject(IdentityMgtConstants.DataTypes.INTEGER.getValue()));

        return meta;
    }

}
