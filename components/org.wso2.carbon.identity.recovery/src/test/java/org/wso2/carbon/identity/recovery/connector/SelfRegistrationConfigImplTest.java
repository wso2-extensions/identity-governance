/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.recovery.connector;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * This class does unit test coverage for SelfRegistrationConfigImpl class.
 */
public class SelfRegistrationConfigImplTest {

    private SelfRegistrationConfigImpl selfRegistrationConfigImpl;
    private static final String CONNECTOR_NAME = "self-sign-up";
    private static final String CATEGORY = "User Onboarding";
    private static final String FRIENDLY_NAME = "Self Registration";
    private static final String LIST_PURPOSE_PROPERTY_KEY = "_url_listPurposeSelfSignUp";
    private static final String SYSTEM_PURPOSE_GROUP = "SELF-SIGNUP";
    private static final String SIGNUP_PURPOSE_GROUP_TYPE = "SYSTEM";
    private static final String CALLBACK_URL = "/carbon/idpmgt/idp-mgt-edit-local.jsp?category=" + CATEGORY +
            "&subCategory=" + FRIENDLY_NAME;
    private static final String CONSENT_LIST_URL = "/carbon/consent/list-purposes.jsp?purposeGroup=" +
            SYSTEM_PURPOSE_GROUP + "&purposeGroupType=" + SIGNUP_PURPOSE_GROUP_TYPE;

    @BeforeTest
    public void Init() {

        selfRegistrationConfigImpl = new SelfRegistrationConfigImpl();
    }

    @Test
    public void testGetName() {

        assertEquals(selfRegistrationConfigImpl.getName(), CONNECTOR_NAME);
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(selfRegistrationConfigImpl.getFriendlyName(), FRIENDLY_NAME);
    }

    @Test
    public void testGetCategory() {

        assertEquals(selfRegistrationConfigImpl.getCategory(), CATEGORY);
    }

    @Test
    public void testGetSubCategory() {

        assertEquals(selfRegistrationConfigImpl.getSubCategory(), "DEFAULT");
    }

    @Test
    public void testGetOrder() {

        assertEquals(selfRegistrationConfigImpl.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> nameMappingExpected = new HashMap<String, String>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, "User self registration");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account on creation");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Manage notifications sending internally");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA, "Prompt reCaptcha");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration verification link expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                "User self registration SMS OTP expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                "User self registration callback URL regex");
        nameMappingExpected.put(LIST_PURPOSE_PROPERTY_KEY, "Manage Self-Sign-Up purposes");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                "Send sign up confirmation email");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                "Prompt reCaptcha on re-send confirmation");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                "Enable Auto Login After Account Confirmation");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                "Alias of the key used to sign to cookie");
        Map<String, String> nameMapping = selfRegistrationConfigImpl.getPropertyNameMapping();

        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> descriptionMappingExpected = new HashMap<>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Allow user's to self register to the system.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock self registered user account until e-mail verification.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Disable if the client application handles notification sending");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha verification during self registration.");
        descriptionMappingExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the verification link.");
        descriptionMappingExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                "Specify the expiry time in minutes for the SMS OTP.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                "This prefix will be used to validate the callback URL.");
        descriptionMappingExpected.put(LIST_PURPOSE_PROPERTY_KEY, "Click here to manage Self-Sign-Up purposes");
        descriptionMappingExpected
                .put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                        "Enable sending notification for self sign up confirmation.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                "Prompt reCaptcha verification for resend confirmation");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                "User will be logged in automatically after completing the Account Confirmation.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                "Alias of the key used to sign to cookie. The public key has to be imported to the keystore.");
        Map<String, String> descriptionMapping = selfRegistrationConfigImpl.getPropertyDescriptionMapping();

        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyNames() {

        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);
        propertiesExpected
                .add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME);
        propertiesExpected
                .add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX);
        propertiesExpected.add(LIST_PURPOSE_PROPERTY_KEY);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME);
        String[] propertiesArrayExpected = propertiesExpected.toArray(new String[0]);

        String[] properties = selfRegistrationConfigImpl.getPropertyNames();

        for (int i = 0; i < propertiesArrayExpected.length; i++) {
            assertEquals(properties[i], propertiesArrayExpected[i]);
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {

        String testEnableSelfSignUp = "false";
        String testEnableAccountLockOnCreation = "true";
        String testEnableNotificationInternallyManage = "true";
        String testEnableSelfRegistrationReCaptcha = "true";
        String testVerificationCodeExpiryTime = "1440";
        String testVerificationSMSOTPExpiryTime = "1";
        String selfRegistrationCallbackRegex = IdentityRecoveryConstants.DEFAULT_CALLBACK_REGEX;
        String enableSelfSignUpConfirmationNotification = "false";
        String enableResendConfirmationRecaptcha = "false";
        String enableSelfRegistrationAutoLogin = "false";
        String enableSelfRegistrationAutoLoginAlias = "wso2carbon";

        Map<String, String> propertiesExpected = new HashMap<>();
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, testEnableSelfSignUp);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                testEnableAccountLockOnCreation);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                testEnableSelfRegistrationReCaptcha);
        propertiesExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                testVerificationCodeExpiryTime);
        propertiesExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                testVerificationSMSOTPExpiryTime);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX,
                selfRegistrationCallbackRegex);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN,
                enableSelfRegistrationAutoLogin);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME,
                enableSelfRegistrationAutoLoginAlias);
        try {
            propertiesExpected.put(LIST_PURPOSE_PROPERTY_KEY, CONSENT_LIST_URL + "&callback=" + (URLEncoder.encode
                    (CALLBACK_URL, StandardCharsets.UTF_8.name())));
        } catch (UnsupportedEncodingException e) {
            throw new IdentityGovernanceException("Error while encoding callback url: " + CALLBACK_URL, e);
        }
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                enableSelfSignUpConfirmationNotification);
        propertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.RESEND_CONFIRMATION_RECAPTCHA_ENABLE,
                enableResendConfirmationRecaptcha);
        String tenantDomain = "admin";
        // Here tenantDomain parameter is not used by method itself
        Properties properties = selfRegistrationConfigImpl.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultProperties = new HashMap<String, String>((Map) properties);

        assertEquals(defaultProperties, propertiesExpected, "Maps are not equal");
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {

        String tenantDomain = "admin";
        String[] propertyNames = new String[]{"property1", "property2", "property3"};

        // Here tenantDomain and propertyNames parameters are not used by method itself
        Map<String, String> defaultPropertyValues = selfRegistrationConfigImpl.getDefaultPropertyValues(propertyNames,
                tenantDomain);
        assertNull(defaultPropertyValues);
    }
}
