/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.recovery;

/**
 * Identity management related constants
 */
public class IdentityRecoveryConstants {


    public static final String IDENTITY_MANAGEMENT_PATH = "/identity";
    public static final String IDENTITY_MANAGEMENT_QUESTIONS = IDENTITY_MANAGEMENT_PATH + "/questionCollection";
    public static final String IDENTITY_MANAGEMENT_I18N_PATH = "/repository/components/identity";
    public static final String IDENTITY_I18N_QUESTIONS =
            IDENTITY_MANAGEMENT_I18N_PATH + "/questionCollection";
    public static final String LINE_SEPARATOR = "!";
    public static final String CHALLENGE_QUESTION_URI = "http://wso2.org/claims/challengeQuestionUris";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET = "passwordreset";
    public static final String NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET = "adminforcedpasswordreset";
    public static final String NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP = "adminforcedpasswordresetwithotp";
    public static final String NOTIFICATION_TYPE_ACCOUNT_CONFIRM = "accountconfirmation";
    public static final String NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM = "resendaccountconfirmation";
    public static final String NOTIFICATION_TYPE_EMAIL_CONFIRM = "emailconfirm";
    public static final String NOTIFICATION_TYPE_ASK_PASSWORD = "askPassword";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS = "passwordresetsucess";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE = "initiaterecovery";
    public static final String NOTIFICATION_ACCOUNT_ID_RECOVERY = "accountidrecovery";
    public static final String RECOVERY_STATUS_INCOMPLETE = "INCOMPLETE";
    public static final String RECOVERY_STATUS_COMPLETE = "COMPLETE";
    public static final String TEMPLATE_TYPE = "TEMPLATE_TYPE";
    public static final String CONFIRMATION_CODE = "confirmation-code";
    public static final String WSO2CARBON_CLAIM_DIALECT = "http://wso2.org/claims";
    public static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";
    public static final String ACCOUNT_UNLOCK_TIME_CLAIM = "http://wso2.org/claims/identity/unlockTime";
    public static final String ACCOUNT_DISABLED_CLAIM = "http://wso2.org/claims/identity/accountDisabled";
    public static final String VERIFY_EMAIL_CLIAM = "http://wso2.org/claims/identity/verifyEmail";
    public static final String EMAIL_VERIFIED_CLAIM = "http://wso2.org/claims/identity/emailVerified";
    public static final String ASK_PASSWORD_CLAIM = "http://wso2.org/claims/identity/askPassword";
    public static final String ADMIN_FORCED_PASSWORD_RESET_CLAIM = "http://wso2.org/claims/identity/adminForcedPasswordReset";
    public static final String OTP_PASSWORD_CLAIM = "http://wso2.org/claims/oneTimePassword";
    public static final String DEFAULT_CHALLENGE_QUESTION_SEPARATOR = "!";


    public static final String PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM = "http://wso2" +
            ".org/claims/identity/failedPasswordRecoveryAttempts";
    public static final String SIGN_UP_ROLE_SEPARATOR = ",";


    public static final String LOCALE_EN_US = "en_US";
    public static final String LOCALE_LK_LK = "lk_lk";
    public static final String SELF_SIGNUP_ROLE = "Internal/selfsignup";
    public static final String EXECUTE_ACTION = "ui.execute";


    private IdentityRecoveryConstants() {
    }

    public enum ErrorMessages {

        ERROR_CODE_INVALID_CODE("18001", "Invalid Code '%s.'"),
        ERROR_CODE_EXPIRED_CODE("18002", "Expired Code '%s.'"),
        ERROR_CODE_INVALID_USER("18003", "Invalid User '%s.'"),
        ERROR_CODE_UNEXPECTED("18013", "Unexpected error"),
        ERROR_CODE_RECOVERY_NOTIFICATION_FAILURE("18015", "Error sending recovery notification"),
        ERROR_CODE_INVALID_TENANT("18016", "Invalid tenant'%s.'"),
        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND("18017", "No challenge question found. %s"),
        ERROR_CODE_INVALID_CREDENTIALS("17002", "Invalid Credentials"),
        ERROR_CODE_LOCKED_ACCOUNT("17003", "User account is locked - '%s.'"),
        ERROR_CODE_DISABLED_ACCOUNT("17004", "user account is disabled '%s.'"),
        ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS("20001", "Registry exception while getting challenge question"),
        ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS("20002", "Registry exception while setting challenge question"),
        ERROR_CODE_GETTING_CHALLENGE_URIS("20003", "Error while getting challenge question URIs '%s.'"),
        ERROR_CODE_GETTING_CHALLENGE_QUESTIONS("20004", "Error while getting challenge questions '%s.'"),
        ERROR_CODE_GETTING_CHALLENGE_QUESTION("20005", "Error while getting challenge question '%s.'"),
        ERROR_CODE_QUESTION_OF_USER("20006", "Error setting challenge quesitons of user '%s.'"),
        ERROR_CODE_NO_HASHING_ALGO("20007", "Error while hashing the security answer"),
        ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION("20008", "Invalid answer"),
        ERROR_CODE_STORING_RECOVERY_DATA("20009", "Invalid answer for security question"),
        ERROR_CODE_NEED_TO_ANSWER_MORE_SECURITY_QUESTION("20010", "Need to answer more security questions"),
        ERROR_CODE_TRIGGER_NOTIFICATION("20011", "Error while trigger notification for user '%s.'"),
        ERROR_CODE_NEED_TO_ANSWER_TO_REQUESTED_QUESTIONS("20012", "Need to answer to all requested security questions"),
        ERROR_CODE_NO_VALID_USERNAME("20013", "No Valid username found for recovery"),
        ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY("20014", "No fileds found for username recovery"),
        ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY("20015", "No valid user found"),
        ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS("20016", "Error loading recovery configs"),
        ERROR_CODE_NOTIFICATION_BASED_PASSWORD_RECOVERY_NOT_ENABLE("20017", "Notification based password recovery is not enabled"),
        ERROR_CODE_QUESTION_BASED_RECOVERY_NOT_ENABLE("20018", "Security questions based recovery is not enabled"),
        ERROR_CODE_ADD_SELF_USER("20019", "Error while adding self signup user"),
        ERROR_CODE_LOCK_USER_USER("20020", "Error while lock user"),
        ERROR_CODE_DISABLE_SELF_SIGN_UP("20021", "Self sign up feature is disabled"),
        ERROR_CODE_LOCK_USER_ACCOUNT("20022", "Error while lock user account"),
        ERROR_CODE_UNLOCK_USER_USER("20023", "Error while unlock user"),
        ERROR_CODE_OLD_CODE_NOT_FOUND("20024", "Old confirmation code not found"),
        ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE("20025", "Failed to retrieve user realm from tenant id : %s"),
        ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER("20026", "Failed to retrieve user store manager."),
        ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS("20027", "Error occurred while retrieving user claims"),
        ERROR_CODE_FAILED_TO_LOAD_GOV_CONFIGS("20028", "Error occurred while retrieving account lock connector " +
                "configuration"),
        ERROR_CODE_HISTORY_VIOLATE("22001", "This password has been used in recent history. Please choose a different" +
                " password"),
        ERROR_CODE_MULTIPLE_QUESTION_NOT_ALLOWED("20029", "Multiple challenge question not allowed for this operation"),
        ERROR_CODE_USER_ALREADY_EXISTS("20030", "User %s already exists in the system. Please use a different username."),
        ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLE("20031", "Username recovery is not enabled"),
        ERROR_CODE_MULTIPLE_USERS_MATCHING("20032", "Multiple users found"),
        ERROR_CODE_ISSUE_IN_LOADING_SIGNUP_CONFIGS("20033", "Error loading signup configs"),
        ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS("20034", "Error occurred while updating user claims"),
        ERROR_CODE_POLICY_VIOLATION("20035", "Password Policy Violate"),
        ERROR_CODE_PROVIDED_CONFIRMATION_CODE_NOT_VALID("20036", "Provided confirmation code %s is not valid"),
        ERROR_CODE_CONFIRMATION_CODE_NOT_PROVIDED("20037", "Confirmation code is not provided for user %s."),
        ERROR_CODE_RECOVERY_SCENARIO_NOT_PROVIDED("20038", "Recovery Scenario is not provided for user %s."),
        ERROR_CODE_RECOVERY_STEP_NOT_PROVIDED("20039", "Recovery Step is not provided for user %s."),
        ERROR_CODE_NOTIFICATION_TYPE_NOT_PROVIDED("20040", "Notification Type is not provided for user %s."),
        ERROR_CODE_FAILED_TO_CHECK_ACCOUNT_LOCK_STATUS("20041", "Error while validating account lock status of user: " +
                "%s."),
        ERROR_CODE_ADD_USER_CONSENT("20042", "Error while adding consent for user %s."),;


        private final String code;
        private final String message;

        ErrorMessages(String code, String message) {
            this.code = code;
            this.message = message;
        }

        public String getCode() {
            return code;
        }

        public String getMessage() {
            return message;
        }

        @Override
        public String toString() {
            return code + " - " + message;
        }

    }

    public static class ConnectorConfig {
        public static final String NOTIFICATION_INTERNALLY_MANAGE = "Recovery.Notification.InternallyManage";
        public static final String NOTIFY_USER_EXISTENCE = "Recovery.NotifyUserExistence";
        public static final String NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS = "Recovery.NotifySuccess";
        public static final String NOTIFICATION_SEND_RECOVERY_SECURITY_START = "Recovery.Question.Password.NotifyStart";
        public static final String NOTIFICATION_BASED_PW_RECOVERY = "Recovery.Notification.Password.Enable";
        public static final String QUESTION_BASED_PW_RECOVERY = "Recovery.Question.Password.Enable";
        public static final String FORCE_ADD_PW_RECOVERY_QUESTION = "Recovery.Question.Password.Forced.Enable";
        public static final String USERNAME_RECOVERY_ENABLE = "Recovery.Notification.Username.Enable";
        public static final String QUESTION_CHALLENGE_SEPARATOR = "Recovery.Question.Password.Separator";
        public static final String QUESTION_MIN_NO_ANSWER = "Recovery.Question.Password.MinAnswers";
        public static final String EXPIRY_TIME = "Recovery.ExpiryTime";
        public static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE = "Recovery.Question.Password" +
                ".ReCaptcha.Enable";
        public static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS = "Recovery.Question" +
                ".Password.ReCaptcha.MaxFailedAttempts";
        public static final String ENABLE_SELF_SIGNUP = "SelfRegistration.Enable";
        public static final String ACCOUNT_LOCK_ON_CREATION = "SelfRegistration.LockOnCreation";
        public static final String SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE = "SelfRegistration.Notification" +
                ".InternallyManage";
        public static final String SELF_REGISTRATION_RE_CAPTCHA = "SelfRegistration.ReCaptcha";
        public static final String SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME = "SelfRegistration" +
                ".VerificationCode.ExpiryTime";

        public static final String ENABLE_EMIL_VERIFICATION = "EmailVerification.Enable";
        public static final String EMAIL_VERIFICATION_EXPIRY_TIME = "EmailVerification.ExpiryTime";
        public static final String ASK_PASSWORD_EXPIRY_TIME = "EmailVerification.AskPassword.ExpiryTime";
        public static final String ASK_PASSWORD_TEMP_PASSWORD_GENERATOR = "EmailVerification.AskPassword.PasswordGenerator";
        public static final String EMAIL_ACCOUNT_LOCK_ON_CREATION = "EmailVerification.LockOnCreation";
        public static final String EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE = "EmailVerification.Notification.InternallyManage";

        public static final String ENABLE_ADMIN_PASSWORD_RESET_OFFLINE = "Recovery.AdminPasswordReset.Offline";
        public static final String ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP = "Recovery.AdminPasswordReset.OTP";
        public static final String ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK = "Recovery.AdminPasswordReset.RecoveryLink";

        public static final String PASSWORD_RECOVERY_RECAPTCHA_ENABLE = "Recovery.ReCaptcha.Password.Enable";
        public static final String USERNAME_RECOVERY_RECAPTCHA_ENABLE = "Recovery.ReCaptcha.Username.Enable";

    }

    public static class SQLQueries {

        public static final String STORE_RECOVERY_DATA = "INSERT INTO IDN_RECOVERY_DATA "
                + "(USER_NAME, USER_DOMAIN, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED, REMAINING_SETS)"
                + "VALUES (?,?,?,?,?,?,?,?)";
        public static final String LOAD_RECOVERY_DATA = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ? AND CODE = ? AND " +
                "SCENARIO = ? AND STEP = ?";

        public static final String LOAD_RECOVERY_DATA_CASE_INSENSITIVE = "SELECT * FROM IDN_RECOVERY_DATA WHERE" +
                " LOWER(USER_NAME)=LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? AND CODE= ? AND SCENARIO = ? AND " +
                "STEP = ?";

        public static final String LOAD_RECOVERY_DATA_FROM_CODE = "SELECT * FROM IDN_RECOVERY_DATA WHERE CODE = ?";


        public static final String INVALIDATE_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_USER_CODES = "DELETE FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID =?";

        public static final String INVALIDATE_USER_CODES_CASE_INSENSITIVE = "DELETE FROM IDN_RECOVERY_DATA WHERE " +
                "LOWER(USER_NAME)=LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID =?";

        public static final String LOAD_RECOVERY_DATA_OF_USER = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ?";

        public static final String LOAD_RECOVERY_DATA_OF_USER_CASE_INSENSITIVE = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE LOWER(USER_NAME)=LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ?";

    }

    public static class Questions {

        public static final String LOCALE_CLAIM = "http://wso2.org/claims/locality";
        public static final String BLACKLIST_REGEX = ".*[/\\\\].*";

        public static final String CHALLENGE_QUESTION_SET_ID = "questionSetId";
        public static final String CHALLENGE_QUESTION_ID = "questionId";
        public static final String CHALLENGE_QUESTION_LOCALE = "locale";

        // TODO remove this
        public static final String[] SECRET_QUESTIONS_SET01 = new String[]{"City where you were born ?",
                "Father's middle name ?", "Favorite food ?", "Favorite vacation location ?"};

        // TODO remove this
        public static final String[] SECRET_QUESTIONS_SET02 = new String[]{"Model of your first car ?",
                "Name of the hospital where you were born ?", "Name of your first pet ?", "Favorite sport ?"};


    }

    public static class Consent {

        public static final String COLLECTION_METHOD_SELF_REGISTRATION = "Web Form - Self Registration";
        public static final String DEFAULT_JURISDICTION = "Global";
        public static final String LANGUAGE_ENGLISH = "en";
        public static final String CONSENT = "consent";
        public static final String SERVICES = "services";
        public static final String PURPOSES = "purposes";
        public static final String PII_CATEGORY = "piiCategory";
        public static final String PII_CATEGORY_ID = "piiCategoryId";
        public static final String EXPLICIT_CONSENT_TYPE = "EXPLICIT";
        public static final String PURPOSE_ID = "purposeId";
        public static final String INFINITE_TERMINATION = "DATE_UNTIL:INDEFINITE";
        public static final String RESIDENT_IDP = "Resident IDP";
    }
}
