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

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class SelfRegistrationConfigImpl implements IdentityConnectorConfig {

    private static String connectorName = "self-sign-up";
    private static final String CATEGORY = "Account Management Policies";
    private static final String FRIENDLY_NAME = "User Self Registration";
    private static final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurposeSelfSignUp";
    private static final String SYSTEM_PURPOSE_GROUP = "SELF-SIGNUP";
    private static final String SIGNUP_PURPOSE_GROUP_TYPE = "SYSTEM";
    private static final String CALLBACK_URL = "/carbon/idpmgt/idp-mgt-edit-local.jsp?category=" + CATEGORY +
            "&subCategory=" + FRIENDLY_NAME;
    private static String consentListURL = "/carbon/consent/list-purposes.jsp?purposeGroup=" + SYSTEM_PURPOSE_GROUP +
            "&purposeGroupType=" + SIGNUP_PURPOSE_GROUP_TYPE;

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
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable Self User Registration");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Internal Notification Management");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration code expiry time");
        nameMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Manage Self-Sign-Up purposes");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable self user registration");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user registration");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable captcha verification during self registration");
        descriptionMapping.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Set the number of minutes the user self registration verification mail would be valid.(Negative " +
                        "value for infinite validity)");
        descriptionMapping.put(LIST_PURPOSE_PROPERTY_KEY, "Click here to manage Self-Sign-Up purposes");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        properties.add(LIST_PURPOSE_PROPERTY_KEY);
        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableSelfSignUp = "false";
        String enableAccountLockOnCreation = "true";
        String enableNotificationInternallyManage = "true";
        String enableSelfRegistrationReCaptcha = "true";
        String verificationCodeExpiryTime = "1440";

        String selfSignUpProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        String accountLockProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        String notificationInternallyMangedProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        String reCaptchaProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        String verificationCodeExpiryTimeProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);

        if (StringUtils.isNotEmpty(selfSignUpProperty)) {
            enableSelfSignUp = selfSignUpProperty;
        }
        if (StringUtils.isNotEmpty(accountLockProperty)) {
            enableAccountLockOnCreation = accountLockProperty;
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

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, enableSelfSignUp);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                enableAccountLockOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                enableSelfRegistrationReCaptcha);
        defaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                verificationCodeExpiryTime);
        try {
            defaultProperties.put(LIST_PURPOSE_PROPERTY_KEY, consentListURL + "&callback=" + URLEncoder.encode
                    (CALLBACK_URL, StandardCharsets.UTF_8.name()));
        } catch (UnsupportedEncodingException e) {
            throw new IdentityGovernanceException("Error while encoding callback url: " + CALLBACK_URL, e);
        }
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        return null;
    }

}
