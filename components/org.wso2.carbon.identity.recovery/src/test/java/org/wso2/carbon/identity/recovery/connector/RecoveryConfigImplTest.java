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

import org.apache.commons.lang.StringUtils;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;

/**
 * This class does unit test coverage for RecoveryConfigImpl class.
 */
public class RecoveryConfigImplTest {

    private RecoveryConfigImpl recoveryConfigImpl;

    @BeforeTest
    public void Init() {

        recoveryConfigImpl = new RecoveryConfigImpl();
    }

    @Test
    public void testGetName() {

        assertEquals(recoveryConfigImpl.getName(), "account-recovery");
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(recoveryConfigImpl.getFriendlyName(), "Account Recovery");
    }

    @Test
    public void testGetCategory() {

        assertEquals(recoveryConfigImpl.getCategory(), "Account Management");
    }

    @Test
    public void testGetSubCategory() {

        assertEquals(recoveryConfigImpl.getSubCategory(), "DEFAULT");
    }

    @Test
    public void testGetOrder() {

        assertEquals(recoveryConfigImpl.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> nameMappingExpected = new HashMap<>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                "Notification based password recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                "Manage notifications sending internally");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_RECAPTCHA_ENABLE,
                "Enable reCaptcha for password recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                "Security question based password recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.CHALLENGE_QUESTION_ANSWER_REGEX,
                "Security question answer regex");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS,
                "Enforce security question answer uniqueness");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER, "Number of " +
                "questions required for password recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE, "Username recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_RECAPTCHA_ENABLE,
                "Enable reCaptcha for username recovery");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME,
                "Recovery link expiry time in minutes");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_EXPIRY_TIME,
                "SMS OTP expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_REGEX,
                "SMS OTP regex");

        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                "Notify when recovery success");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                "Notify when security questions based recovery starts");

        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Enable reCaptcha for security questions based password recovery");
        nameMappingExpected.put(
                IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS,
                "Max failed attempts for reCaptcha");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_ADD_PW_RECOVERY_QUESTION, "Enable forced " +
                "security questions");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_MIN_NO_QUESTION_ANSWERED,
                "Minimum number of forced security questions to be answered");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX,
                "Recovery callback URL regex");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_AUTO_LGOIN_AFTER_PASSWORD_RESET,
                "Enable Auto Login After Password Reset");

        Map<String, String> nameMapping = recoveryConfigImpl.getPropertyNameMapping();

        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> descriptionMappingExpected = new HashMap<String, String>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                "Disable if the client application handles notification sending");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.CHALLENGE_QUESTION_ANSWER_REGEX,
                "Security question answer regex");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS,
                "Enforce security question answer uniqueness");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                "Prompt reCaptcha for security question based password recovery");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_ADD_PW_RECOVERY_QUESTION,
                "Force users to provide answers to security questions during sign in");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_MIN_NO_QUESTION_ANSWERED,
                "Force users to provide answers to security questions during sign in " +
                        "if user has answered lesser than this value");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX,
                "Recovery callback URL regex");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_EXPIRY_TIME,
                "Expiration time of the SMS OTP code for password recovery");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_REGEX,
                "Regex for SMS OTP in format [allowed characters]{length}. Supported character ranges are a-z, A-Z, " +
                        "0-9. Minimum OTP length is " + IdentityMgtConstants.MINIMUM_SMS_OTP_LENGTH);
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_AUTO_LGOIN_AFTER_PASSWORD_RESET,
                "User will be logged in automatically after completing the Password Reset wizard");
        Map<String, String> descriptionMapping = recoveryConfigImpl.getPropertyDescriptionMapping();

        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are not equal");
    }

    @Test
    public void testGetPropertyNames() {

        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_RECAPTCHA_ENABLE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.CHALLENGE_QUESTION_ANSWER_REGEX);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_RECAPTCHA_ENABLE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_EXPIRY_TIME);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_REGEX);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.FORCE_ADD_PW_RECOVERY_QUESTION);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.FORCE_MIN_NO_QUESTION_ANSWERED);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_AUTO_LGOIN_AFTER_PASSWORD_RESET);

        String[] propertiesArrayExpected = propertiesExpected.toArray(new String[0]);

        String[] properties = recoveryConfigImpl.getPropertyNames();

        for (int i = 0; i < propertiesArrayExpected.length; i++) {
            assertEquals(properties[i], propertiesArrayExpected[i]);
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {

        String testEnableNotificationBasedPasswordRecovery = "false";
        String testEnableQuestionBasedPasswordRecovery = "false";
        String testMinimumAnswers = "2";
        String testEnableRecoveryQuestionPasswordReCaptcha = "true";
        String testRecoveryQuestionPasswordReCaptchaMaxFailedAttempts = "2";
        String testEnableUsernameRecovery = "false";
        String testEnableNotificationInternallyManage = "true";
        String testExpiryTime = "1440";
        String testExpiryTimeSMSOTP = "1";
        String smsOtpRegex = "[a-zA-Z0-9]{6}";
        String testNotifySuccess = "false";
        String testNotifyStart = "false";
        String testForceChallengeQuestions = "false";
        String enablePasswordRecoveryReCaptcha = "false";
        String enableUsernameRecoveryReCaptcha = "false";
        String testMinimumForcedChallengeQuestionsAnswered = "1";
        String recoveryCallbackRegex = IdentityRecoveryConstants.DEFAULT_CALLBACK_REGEX;
        String challengeQuestionAnswerRegex = IdentityRecoveryConstants.DEFAULT_REGEX;
        String enforceChallengeQuestionAnswerUniqueness = "false";
        String enableAutoLoginAfterPasswordReset = "false";

        Map<String, String> defaultPropertiesExpected = new HashMap<>();
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                testEnableNotificationBasedPasswordRecovery);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_RECAPTCHA_ENABLE,
                enablePasswordRecoveryReCaptcha);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                testEnableQuestionBasedPasswordRecovery);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER,
                testMinimumAnswers);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                testEnableRecoveryQuestionPasswordReCaptcha);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig
                        .RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS,
                testRecoveryQuestionPasswordReCaptchaMaxFailedAttempts);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE,
                testEnableUsernameRecovery);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_RECAPTCHA_ENABLE,
                enableUsernameRecoveryReCaptcha);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                testEnableNotificationInternallyManage);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, testExpiryTime);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_EXPIRY_TIME,
                testExpiryTimeSMSOTP);
        defaultPropertiesExpected
                .put(IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_REGEX, smsOtpRegex);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                testNotifySuccess);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_SECURITY_START,
                testNotifyStart);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_ADD_PW_RECOVERY_QUESTION,
                testForceChallengeQuestions);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.FORCE_MIN_NO_QUESTION_ANSWERED,
                testMinimumForcedChallengeQuestionsAnswered);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX,
                recoveryCallbackRegex);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.CHALLENGE_QUESTION_ANSWER_REGEX,
                challengeQuestionAnswerRegex);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.
                ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS, enforceChallengeQuestionAnswerUniqueness);
        defaultPropertiesExpected.put(IdentityRecoveryConstants.ConnectorConfig.
                ENABLE_AUTO_LGOIN_AFTER_PASSWORD_RESET, enableAutoLoginAfterPasswordReset);

        String tenantDomain = "admin";
        // Here tenantDomain parameter is not used by method itself
        Properties properties = recoveryConfigImpl.getDefaultPropertyValues(tenantDomain);
        Map<String, String> defaultProperties = new HashMap<String, String>((Map) properties);
        assertEquals(defaultProperties, defaultPropertiesExpected, "Maps are not equal");
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {

        String tenantDomain = "admin";
        String[] propertyNames = new String[]{"property1", "property2", "property3"};

        // Here tenantDomain and propertyNames parameters are not used by method itself
        Map<String, String> defaultPropertyValues = recoveryConfigImpl.getDefaultPropertyValues(propertyNames, tenantDomain);
        assertNull(defaultPropertyValues);
    }
}
