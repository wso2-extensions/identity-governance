/*
 * Copyright (c) 2016-2024, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery;

import org.wso2.carbon.identity.core.util.IdentityUtil;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Identity management related constants.
 */
public class IdentityRecoveryConstants {

    public static final String IDENTITY_MANAGEMENT_PATH = "/identity";
    public static final String IDENTITY_MANAGEMENT_QUESTIONS = IDENTITY_MANAGEMENT_PATH + "/questionCollection";
    public static final String IDENTITY_MANAGEMENT_I18N_PATH = "/repository/components/identity";
    public static final String IDENTITY_I18N_QUESTIONS =
            IDENTITY_MANAGEMENT_I18N_PATH + "/questionCollection";
    public static final String LINE_SEPARATOR = "!";
    public static final String DEFAULT_REGEX = ".*";
    public static final String CHALLENGE_QUESTION_URI = "http://wso2.org/claims/challengeQuestionUris";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET = "passwordreset";
    public static final String NOTIFICATION_TYPE_RESEND_PASSWORD_RESET = "resendPasswordReset";
    public static final String NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET = "adminforcedpasswordreset";
    public static final String NOTIFICATION_TYPE_RESEND_ADMIN_FORCED_PASSWORD_RESET = "resendAdminForcedPasswordReset";
    public static final String NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP = "adminforcedpasswordresetwithotp";
    public static final String NOTIFICATION_TYPE_RESEND_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP =
            "resendAdminForcedPasswordResetWithOTP";
    public static final String NOTIFICATION_TYPE_ACCOUNT_CONFIRM = "accountconfirmation";
    public static final String NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM = "resendaccountconfirmation";
    public static final String NOTIFICATION_TYPE_EMAIL_CONFIRM = "emailconfirm";
    public static final String NOTIFICATION_TYPE_LITE_USER_EMAIL_CONFIRM = "liteUserEmailConfirmation";
    public static final String NOTIFICATION_TYPE_RESEND_LITE_USER_EMAIL_CONFIRM = "resendLiteUserEmailConfirmation";
    public static final String NOTIFICATION_TYPE_TENANT_REGISTRATION_CONFIRMATION = "tenantRegistrationConfirmation";
    public static final String NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE = "verifyEmailOnUpdate";
    // This type of notification is used to send a notification when the user updates the email with verification.
    public static final String NOTIFICATION_TYPE_NOTIFY_EMAIL_ON_UPDATE = "notifyOnExistingEmailUpdate";
    // This type of notification is used to send a notification when the user updates the email without verification.
    public static final String NOTIFICATION_TYPE_NOTIFY_EMAIL_UPDATE_WITHOUT_VERIFICATION =
            "notifyOnExistingEmailUpdateWithoutVerification";
    public static final String NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE = "verifyMobileOnUpdate";
    public static final String NOTIFICATION_TYPE_RESEND_VERIFY_EMAIL_ON_UPDATE = "resendVerifyEmailOnUpdate";
    public static final String NOTIFICATION_TYPE_ASK_PASSWORD = "askPassword";
    public static final String NOTIFICATION_TYPE_RESEND_ASK_PASSWORD = "resendAskPassword";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS = "passwordresetsucess";
    public static final String ACCOUNT_ACTIVATION_SUCCESS = "accountActivationSuccess";
    public static final String NOTIFICATION_TYPE_PASSWORD_RESET_INITIATE = "initiaterecovery";
    public static final String NOTIFICATION_ACCOUNT_ID_RECOVERY = "accountidrecovery";
    public static final String NOTIFICATION_TYPE_SELF_SIGNUP_SUCCESS = "selfSignUpSuccess";
    public static final String NOTIFICATION_TYPE_SELF_SIGNUP_NOTIFY = "selfSignUpNotify";
    public static final String RECOVERY_STATUS_INCOMPLETE = "INCOMPLETE";
    public static final String RECOVERY_STATUS_COMPLETE = "COMPLETE";
    public static final String TEMPLATE_TYPE = "TEMPLATE_TYPE";
    public static final String EMAIL_TEMPLATE_NAME = "templateName";
    public static final String RESEND_EMAIL_TEMPLATE_NAME = "resendTemplateName";
    public static final String INITIATED_PLATFORM = "initiated-platform";
    public static final String CAMPAIGN = "campaign";
    public static final String CONFIRMATION_CODE = "confirmation-code";
    public static final String OTP_TOKEN = "otpToken";
    public static final String OTP_TOKEN_STRING = "otpTokenString";
    public static final String VERIFICATION_PENDING_EMAIL = "verification-pending-email";
    public static final String NEW_EMAIL_ADDRESS = "new-email-address";
    public static final String NOTIFY = "notify";
    public static final String WSO2CARBON_CLAIM_DIALECT = "http://wso2.org/claims";
    public static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";
    public static final String ACCOUNT_LOCKED_REASON_CLAIM = "http://wso2.org/claims/identity/lockedReason";
    public static final String ACCOUNT_UNLOCK_TIME_CLAIM = "http://wso2.org/claims/identity/unlockTime";
    public static final String ACCOUNT_DISABLED_CLAIM = "http://wso2.org/claims/identity/accountDisabled";
    public static final String USER_SOURCE_ID_CLAIM_URI = "http://wso2.org/claims/identity/userSourceId";
    public static final String LOCAL_CREDENTIAL_EXISTS_CLAIM_URI = "http://wso2.org/claims/identity/localCredentialExists";
    public static final String LITE_USER_CLAIM = "http://wso2.org/claims/identity/isLiteUser";
    public static final String FAILED_LOGIN_LOCKOUT_COUNT_CLAIM =
            "http://wso2.org/claims/identity/failedLoginLockoutCount";
    public static final String ACCOUNT_CONFIRMED_TIME_CLAIM = "http://wso2.org/claims/identity/accountConfirmedTime";
    public static final String FUNCTION_LOCKOUT_COUNT_PROPERTY = "LockoutCount";
    public static final String FUNCTION_FAILED_ATTEMPTS_PROPERTY = "FailedAttempts";
    public static final String FUNCTION_MAX_ATTEMPTS_PROPERTY = "MaxAttempts";
    public static final String FUNCTION_LOCKOUT_TIME_PROPERTY = "LockoutTime";
    public static final String FUNCTION_LOGIN_FAIL_TIMEOUT_RATIO_PROPERTY = "TimeoutRatio";

    public static final String MAX_ATTEMPTS_DEFAULT = "5";
    public static final String LOCKOUT_TIME_DEFAULT = "5";
    public static final String LOGIN_FAIL_TIMEOUT_RATIO_DEFAULT = "2";

    public static final String USER_NEW_CHALLENGE_ANSWERS = "userNewChallengeAnswers";
    public static final String USER_OLD_CHALLENGE_ANSWERS = "userOldChallengeAnswers";

    public static final String FUNCTIONALITY_LOCK_RESOURCE_TYPE = "functionalityLock";

    // Notification channel claims.
    public static final String VERIFY_EMAIL_CLIAM = "http://wso2.org/claims/identity/verifyEmail";
    public static final String EMAIL_VERIFIED_CLAIM = "http://wso2.org/claims/identity/emailVerified";
    public static final String VERIFY_MOBILE_CLAIM = "http://wso2.org/claims/identity/verifyMobile";
    public static final String EMAIL_ADDRESS_PENDING_VALUE_CLAIM =
            "http://wso2.org/claims/identity/emailaddress.pendingValue";
    public static final String MOBILE_NUMBER_PENDING_VALUE_CLAIM =
            "http://wso2.org/claims/identity/mobileNumber.pendingValue";
    public static final String PREFERRED_CHANNEL_CLAIM = "http://wso2.org/claims/identity/preferredChannel";

    public static final String ASK_PASSWORD_CLAIM = "http://wso2.org/claims/identity/askPassword";
    public static final String ADMIN_FORCED_PASSWORD_RESET_CLAIM = "http://wso2.org/claims/identity/adminForcedPasswordReset";
    public static final String TENANT_ADMIN_ASK_PASSWORD_CLAIM =
            "http://wso2.org/claims/identity/tenantAdminAskPassword";
    public static final String OTP_PASSWORD_CLAIM = "http://wso2.org/claims/oneTimePassword";
    public static final String USER_ROLES_CLAIM = "http://wso2.org/claims/roles";
    public static final String EMAIL_ADDRESS_CLAIM = "http://wso2.org/claims/emailaddress";
    public static final String MOBILE_NUMBER_CLAIM = "http://wso2.org/claims/mobile";
    public static final String MOBILE_NUMBERS_CLAIM = "http://wso2.org/claims/mobileNumbers";
    public static final String VERIFIED_MOBILE_NUMBERS_CLAIM = "http://wso2.org/claims/verifiedMobileNumbers";
    public static final String EMAIL_ADDRESSES_CLAIM = "http://wso2.org/claims/emailAddresses";
    public static final String VERIFIED_EMAIL_ADDRESSES_CLAIM = "http://wso2.org/claims/verifiedEmailAddresses";
    public static final String DEFAULT_CHALLENGE_QUESTION_SEPARATOR = "!";
    public static final String ACCOUNT_STATE_CLAIM_URI = "http://wso2.org/claims/identity/accountState";
    public static final String PENDING_SELF_REGISTRATION = "PENDING_SR";
    public static final String PENDING_LITE_REGISTRATION = "PENDING_LR";
    public static final String PENDING_ASK_PASSWORD = "PENDING_AP";
    public static final String PENDING_EMAIL_VERIFICATION = "PENDING_EV";
    public static final String ACCOUNT_STATE_UNLOCKED = "UNLOCKED";

    public static final String PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM = "http://wso2" +
            ".org/claims/identity/failedPasswordRecoveryAttempts";
    public static final String SIGN_UP_ROLE_SEPARATOR = ",";

    public static final String NOTIFICATION_EVENTNAME_PREFIX = "TRIGGER_";
    public static final String NOTIFICATION_EVENTNAME_SUFFIX = "_NOTIFICATION";
    public static final String NOTIFICATION_EVENTNAME_SUFFIX_LOCAL = "_LOCAL";
    public static final String SEND_TO = "send-to";
    public static final String LOCALE_EN_US = "en_US";
    public static final String LOCALE_LK_LK = "lk_lk";
    public static final String SELF_SIGNUP_ROLE = "Internal/selfsignup";
    public static final String EXECUTE_ACTION = "ui.execute";
    public static final String UTF_8 = "UTF-8";
    public static final String CALLBACK = "callback";
    public static final String IS_ACCESS_URL_AVAILABLE = "isAccessUrlAvailable";
    public static final String IS_LITE_SIGN_UP = "isLiteSignUp";
    public static final String DEFAULT_CALLBACK_REGEX = ".*";
    public static final String IS_USER_PORTAL_URL = "isUserPortalURL";
    public static final String NOTIFY_CHANNEL_LIST_SEPARATOR = ",";
    public static final String CHANNEL_ATTRIBUTE_SEPARATOR = ":";
    public static final String SMS_OTP_GENERATE_CHAR_SET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    public static final String SMS_OTP_GENERATE_ALPHABET_CHAR_SET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    public static final String SMS_OTP_GENERATE_NUMERIC_CHAR_SET = "0123456789";
    public static final String VALID_SMS_OTP_REGEX_PATTERN = "^\\[((a-z)|(A-Z)|(0-9)){1,3}\\]\\{[0-9]+\\}$";
    public static final String EXCEPTION_SCENARIO_SEPARATOR = "-";

    public static final String USE_LEGACY_API_PROPERTY_KEY = "useLegacyAPI";
    public static final String NOTIFICATION_CHANNEL_PROPERTY_KEY = "notificationChannel";
    public static final String VERIFIED_USER_PROPERTY_KEY = "verifiedUser";
    public static final String MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY = "manageNotificationsInternally";
    public static final String CONFIRMATION_CODE_SEPARATOR = ".";

    // Recovery Scenarios.
    public static final String USER_NAME_RECOVERY = "UNR";
    public static final String USER_SELF_REGISTRATION = "USR";
    public static final String PASSWORD_RECOVERY_SCENARIO = "PWR";
    public static final String USER_ACCOUNT_RECOVERY = "UAR";

    public static final int SMS_OTP_CODE_LENGTH = 6;
    public static final int OTP_CODE_DEFAULT_LENGTH = 6;
    public static final String ENABLE_DETAILED_ERROR_RESPONSE = "Recovery.ErrorMessage.EnableDetailedErrorMessages";
    public static final int RECOVERY_FLOW_ID_DEFAULT_EXPIRY_TIME = 15;
    // Recovery code given at the username and password recovery initiation.
    public static final int RECOVERY_CODE_DEFAULT_EXPIRY_TIME = 1;
    public static final int RESEND_CODE_DEFAULT_EXPIRY_TIME = 1;

    public static final String RECOVERY_QUESTION_PASSWORD_SKIP_ON_INSUFFICIENT_ANSWERS =
            "Recovery.Question.Password.SkipOnInsufficientAnswers";
    public static final String RECOVERY_CONFIRMATION_CODE_TOLERANCE_PERIOD =
            "Recovery.Notification.Password.Email.ConfirmationCodeTolerancePeriod";
    public static final String ASK_PASSWORD_CONFIRMATION_CODE_TOLERANCE_PERIOD =
            "EmailVerification.AskPassword.Notification.ConfirmationCodeTolerancePeriod";
    public static final String SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD =
            "SelfRegistration.Notification.Email.ConfirmationCodeTolerancePeriod";
    public static final String SELF_SIGN_UP_SMS_CONFIRMATION_CODE_TOLERANCE_PERIOD =
            "SelfRegistration.Notification.SMS.ConfirmationCodeTolerancePeriod";
    public static final int RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE = 0;
    public static final int ASK_PASSWORD_CODE_DEFAULT_TOLERANCE = 0;
    public static final int SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE = 0;
    public static final String EMAIL_TEMPLATE_PATH = "/identity/email";

    // Workflow constants.
    public static final String ENTITY_TYPE_USER = "USER";
    public static final String ADD_USER_EVENT = "ADD_USER";

    public static final String CORRELATION_ID_MDC = "Correlation-ID";
    // Ask Password thread local property name.
    public static final String AP_CONFIRMATION_CODE_THREAD_LOCAL_PROPERTY = "apConfirmationCodeThreadLocalProperty";
    // Ask Password thread local initial value.
    public static final String AP_CONFIRMATION_CODE_THREAD_LOCAL_INITIAL_VALUE =
            "apConfirmationCodeThreadLocalInitialValue";

    // Self sign up properties.
    public static final String SIGNUP_PROPERTY_REGISTRATION_OPTION = "registrationOption";

    // Properties related to password recovery failure due to account status.
    public static final String ERROR_KEY = "error-key";
    public static final String NOTIFICATION_TYPE_ACCOUNT_STATUS_NOTIFY = "passwordRecoveryFailureNotify";
    public static final String ACCOUNT_STATUS_LOCKED = "password.recovery.failed.account.locked";
    public static final String ACCOUNT_STATUS_DISABLED = "password.recovery.failed.account.disabled";
    public static final String IGNORE_IF_TEMPLATE_NOT_FOUND = "ignoreIfTemplateNotFound";

    public static final String TRUE = "true";
    public static final String FALSE = "false";

    private IdentityRecoveryConstants() {

    }

    /**
     * Enum which contains the error codes and corresponding error messages.
     */
    public enum ErrorMessages {

        ERROR_CODE_INVALID_CODE("18001", "Invalid Code '%s'."),
        ERROR_CODE_EXPIRED_CODE("18002", "Expired Code '%s'."),
        ERROR_CODE_INVALID_USER("18003", "Invalid User '%s'."),
        ERROR_CODE_FEDERATED_USER("18006", "User %s doesn't have a password for local account."),
        ERROR_CODE_UNEXPECTED("18013", "Unexpected error"),
        ERROR_CODE_RECOVERY_NOTIFICATION_FAILURE("18015", "Error sending recovery notification"),
        ERROR_CODE_INVALID_TENANT("18016", "Invalid tenant '%s'."),
        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND("18017", "No challenge question found. %s"),
        ERROR_CODE_EMAIL_NOT_FOUND("18018", "Sending email address is not found for the user %s."),
        ERROR_CODE_INVALID_FLOW_ID("18019", "Invalid flow confirmation code '%s'."),
        ERROR_CODE_EXPIRED_FLOW_ID("18020", "Expired flow confirmation code '%s'."),
        ERROR_CODE_MOBILE_NOT_FOUND("18021", "Mobile number is not found for the user %s."),
        ERROR_CODE_INVALID_CREDENTIALS("17002", "Invalid Credentials"),
        ERROR_CODE_LOCKED_ACCOUNT("17003", "User account is locked - '%s'."),
        ERROR_CODE_DISABLED_ACCOUNT("17004", "user account is disabled '%s'."),
        ERROR_CODE_PENDING_SELF_REGISTERED_ACCOUNT("17005", "User account not yet verified - '%s.'"),
        ERROR_CODE_PENDING_PASSWORD_RESET_ACCOUNT("17006", "Password reset is not yet completed '%s.'"),
        ERROR_CODE_MOBILE_VERIFICATION_NOT_ENABLE_PRIVILEGED_USERS("17007", "Mobile number verification by privileged users is not enabled."),
        ERROR_CODE_USER_STORE_READONLY("17008", "User store is readonly - '%s'."),
        ERROR_CODE_USER_STORE_INVALID("17009", "User store is invalid - '%s'."),
      
        ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS("20001", "Registry exception while getting challenge question"),
        ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS("20002", "Registry exception while setting challenge question"),
        ERROR_CODE_GETTING_CHALLENGE_URIS("20003", "Error while getting challenge question URIs '%s'."),
        ERROR_CODE_GETTING_CHALLENGE_QUESTIONS("20004", "Error while getting challenge questions '%s'."),
        ERROR_CODE_GETTING_CHALLENGE_QUESTION("20005", "Error while getting challenge question '%s'."),
        ERROR_CODE_QUESTION_OF_USER("20006", "Error setting challenge quesitons of user '%s'."),
        ERROR_CODE_NO_HASHING_ALGO("20007", "Error while hashing the security answer"),
        ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION("20008", "Invalid answer"),
        ERROR_CODE_STORING_RECOVERY_DATA("20009", "Invalid answer for security question"),
        ERROR_CODE_NEED_TO_ANSWER_MORE_SECURITY_QUESTION("20010", "Need to answer more security questions"),
        ERROR_CODE_TRIGGER_NOTIFICATION("20011", "Error while trigger notification for user '%s'."),
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
        ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE("20025", "Failed to retrieve user realm from tenant id: '%s'"),
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
        ERROR_CODE_ADD_USER_CONSENT("20042", "Error while adding consent for user %s."),
        ERROR_CODE_PUBLISH_EVENT("20043", "Error while publishing event %s."),
        ERROR_CODE_CALLBACK_URL_NOT_VALID("20044", "Configured callback URL does not match with the provided callback"
                + " URL: %s in the request."),
        ERROR_CODE_USERNAME_POLICY_VIOLATED("20045", "Provided username %s violates the policy."),
        ERROR_CODE_PASSWORD_POLICY_VIOLATED("20046", "Provided password violates the policy."),
        ERROR_CODE_DOMAIN_VIOLATED("20047", "Invalid domain %s provided."),
        ERROR_CODE_NO_USER_OR_MORE_THAN_ONE_USER_FOUND("20048", "No or more than one valid user found."),
        ERROR_CODE_INVALID_CHALLENGE_QUESTION("20049", "Invalid Challenge Question Input."),
        ERROR_CODE_INVALID_CHALLENGE_QUESTION_VALUE("20050", "Empty Challenge question value provided."),
        ERROR_CODE_REMOVING_CHALLENGE_QUESTIONS("20051", "Error while removing challenge questions of user '%s."),
        ERROR_CODE_CHALLENG_ANSWER_MISSING("20052", "Challenge question missing in the user challenge " +
                "answer."),
        ERROR_CODE_DUPLICATE_ANSWERS("20053", "Validation Error. Cannot answer two questions from the same question " +
                "set claim uri"),
        ERROR_CODE_INVALID_LOCALE("20054", "Invalid Locale value provided: %s"),
        ERROR_CODE_INVALID_CHALLENGE("20055", "Attributes of Challenge question to be set" +
                " cannot be empty."),
        ERROR_CODE_INVALID_CHALLENGE_PATH("20056", "%s contains non alpha-numeric characters."),
        ERROR_CODE_ERROR_DELETING_CHALLENGE_SET("20057", "Error when deleting challenge question set %s."),
        ERROR_CODE_REGISTRY_EXCEPTION_DELETE_CHALLENGE_QUESTIONS("20058", "Registry exception while deleting " +
                "challenge question of locale %s in set %s"),
        ERROR_CODE_REGISTRY_EXCEPTION_DELETE_CHALLENGE_QUESTION("20059", "Registry exception while deleting challenge" +
                " question %s of the set %s"),
        ERROR_CODE_DISABLE_LITE_SIGN_UP("20060", "Lite sign up feature is disabled"),
        ERROR_CODE_ERROR_DELETING_RECOVERY_DATA("20061", "Error deleting user recovery data of the tenant: %s"),
        ERROR_CODE_ERROR_GETTING_CONNECTOR_CONFIG("20062", "Error while getting connector configurations"),
        ERROR_CODE_STORING_RECOVERY_FLOW_DATA("20063", "Error while storing recovery data."),
        ERROR_CODE_UPDATING_RECOVERY_FLOW_DATA("20064", "Error while updating recovery data."),
        ERROR_CODE_NO_HASHING_ALGO_FOR_CODE("20065", "Error while hashing the code."),
        ERROR_CODE_MULTIPLE_CLAIMS_WITH_MULTI_ATTRIBUTE_URI("20066", "Multiple claims not allowed " +
                "when user identifier claim is used."),
        ERROR_CODE_INVALID_PASSWORD("20067", "Error while validating the password. %s"),

        ERROR_CODE_ERROR_RETRIVING_CLAIM("18004", "Error when retrieving the locale claim of user '%s' of '%s' domain."),
        ERROR_CODE_RECOVERY_DATA_NOT_FOUND_FOR_USER("18005", "Recovery data not found."),
        ERROR_CODE_FAILED_TO_LOCK_FUNCTIONALITY_FOR_USER("55001", "Server error occurred while locking functionality."),
        ERROR_CODE_FAILED_TO_UNLOCK_FUNCTIONALITY_FOR_USER("55002", "Server error occurred while unlocking " +
                "functionality."),
        ERROR_CODE_FAILED_TO_GET_LOCK_STATUS_FOR_FUNCTIONALITY("55003", "Error occurred while fetching lock status."),
        ERROR_CODE_FAILED_TO_GET_PROPERTIES_FOR_FUNCTIONALITY("55004", "Error occurred while fetching functionality " +
                "lock properties."),
        ERROR_CODE_FAILED_TO_ADD_PROPERTIES_FOR_FUNCTIONALITY("55005", "Error occurred while adding functionality " +
                "lock properties."),
        ERROR_CODE_FAILED_TO_UPDATE_PROPERTIES_FOR_FUNCTIONALITY("55006", "Error occurred while updating " +
                "functionality lock property."),
        ERROR_CODE_FAILED_TO_ADD_RESOURCE_TYPE_TO_CONFIG_STORE("55007", "Error occurred while adding resource type to" +
                " config store."),
        ERROR_CODE_FAILED_TO_FETCH_RESOURCE_FROM_CONFIG_STORE("55008", "Error occurred while fetching " +
                "resource from config store."),
        // USR - User Self Registration - client exceptions.
        ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS("USR-10001",
                "User specified communication channel is not supported by the server"),
        ERROR_CODE_PREFERRED_CHANNEL_VALUE_EMPTY("USR-10002",
                "User specified communication channel does not have any value"),
        ERROR_CODE_BAD_SELF_REGISTER_REQUEST("USR-10003",
                "Bad Request"),
        ERROR_CODE_BAD_LITE_REGISTER_REQUEST("USR-10004", "Either email or mobile should be submitted."),
        ERROR_CODE_UNSUPPORTED_SELF_REGISTER_LITE_REQUEST("USR-10005",
                "Lite self registration is not supported."),
        ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION("USR-10006", "User attributes do not satisfy the " +
                "requirements of the selected registration option."),
        ERROR_CODE_INVALID_REGISTRATION_OPTION("USR-10007", "Invalid registration option."),
        ERROR_CODE_MULTIPLE_REGISTRATION_OPTIONS("USR-10008", "Multiple registration options are not supported."),
        ERROR_CODE_INVALID_DOMAIN("USR-10009", "The email domain does not match the organization's email domain."),

        // USR - User Self Registration - server exceptions.
        ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES("USR-15001", "Unexpected error while validating user " +
                "attributes."),

        // UAV - User Account Verification.
        ERROR_CODE_UNSUPPORTED_VERIFICATION_CHANNEL("UAV-10001",
                "Unsupported verification channel"),
        ERROR_CODE_GETTING_VERIFICATION_PENDING_EMAIL("UAV-10002", "Error while getting verification " +
                "pending email from the userstore."),

        // UNR - Username Recovery
        ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLED("UNR-10001", "Username recovery is not enabled"),
        ERROR_CODE_USERNAME_RECOVERY_EMPTY_TENANT_DOMAIN("UNR-10002", "Empty tenant domain in username "
                + "recovery request"),
        ERROR_CODE_USERNAME_RECOVERY_VALIDATION_FAILED("UNR-10003",
                "Username recovery validation failed for user account : '%s'"),
        ERROR_CODE_USERNAME_RECOVERY_MULTIPLE_DOMAINS("UNR-10004", "Multiple domains found in the " +
                "given claim set"),

        // UAR - User Account Recovery.
        ERROR_CODE_INVALID_RECOVERY_CODE("UAR-10001", "Invalid recoveryCode : '%s'"),
        ERROR_CODE_NO_USER_FOUND("UAR-10002", "No user found"),
        ERROR_CODE_NO_VERIFIED_CHANNELS_FOR_USER("UAR-10003", "No verified channels found for user"),
        ERROR_CODE_INVALID_CHANNEL_ID("UAR-10004", "Channel ID does not exist"),
        ERROR_CODE_USER_TENANT_DOMAIN_MISS_MATCH_WITH_CONTEXT("UAR-10005", "User's tenant domain does "
                + "not match the domain in the context"),
        ERROR_CODE_ERROR_LOADING_USER_CLAIMS("UAR-10006", "Error loading claims of the user"),
        ERROR_CODE_MULTIPLE_MATCHING_USERS("UAR-10007", "Multiple users found for given claims"),
        ERROR_CODE_NO_ACCOUNT_RECOVERY_DATA("UAR-10008", "No account recovery data found for "
                + "recovery code : '%s'"),
        ERROR_CODE_NO_NOTIFICATION_CHANNELS_FOR_USER("UAR-10009", "Notification channels not "
                + "found for user"),
        ERROR_CODE_INVALID_RECOVERY_STEP("UAR-10010", "Invalid recovery step: '%s'"),
        ERROR_CODE_INVALID_RECOVERY_SCENARIO("UAR-10011", "Invalid recovery scenario: '%s'"),
        ERROR_CODE_INVALID_RESEND_CODE("UAR-10012", "Invalid resend code: '%s'"),
        ERROR_CODE_EXPIRED_RECOVERY_CODE("UAR-10013", "Invalid recovery code: '%s'"),
        ERROR_CODE_USER_ACCOUNT_RECOVERY_VALIDATION_FAILED("UAR-10014",
                "User account recovery validation failed for user account: '%s'"),
        ERROR_CODE_INVALID_RECOVERY_FLOW_ID("UAR-10015", "Invalid confirmation code : '%s'."),
        ERROR_CODE_EXPIRED_RECOVERY_FLOW_ID("UAR-10016", "Expired confirmation code : '%s'."),
        ERROR_CODE_API_DISABLED("UAR-10017", "Recovery API is disabled."),
        ERROR_CODE_NO_RECOVERY_FLOW_DATA("UAR-10018", "No recovery flow data found for "
                + "recovery flow id : '%s'."),
        ERROR_CODE_ERROR_STORING_RECOVERY_DATA("UAR-15001", "Error storing user recovery data"),
        ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER("UAR-15002", "Error getting userstore manager"),
        ERROR_CODE_ERROR_RETRIEVING_USER_CLAIM("UAR-15003", "Error getting the claims: '%s' "
                + "from the userstore"),
        ERROR_CODE_UNEXPECTED_ERROR("UAR-15004", "Unexpected error"),
        ERROR_CODE_UNSUPPORTED_NOTIFICATION_CHANNEL("UAR-15005",
                "Unsupported notification channel: '%s'"),
        ERROR_CODE_ERROR_GENERATING_NEW_RESET_CODE("UAR-15006", "Error while generating new "
                + "password reset code"),
        ERROR_CODE_ERROR_RETRIEVING_RECOVERY_DATA("UAR-15007","Error while retrieving the user recovery data: '%s'"),
        ERROR_CODE_ERROR_UPDATING_RECOVERY_DATA("UAR-15008", "Error while updating recovery data: '%s'"),

        // PWR - Password Recovery.
        ERROR_CODE_INVALID_TENANT_DOMAIN_PASSWORD_RESET("PWR-10001", "User's tenant domain does "
                + "not match with the domain in the context"),
        ERROR_CODE_PASSWORD_RECOVERY_WITH_NOTIFICATIONS_NOT_ENABLED(
                "PWR-10002", "Password recovery with notifications is not enabled"),
        ERROR_CODE_NO_PASSWORD_IN_REQUEST("PWR-10003", "No password found"),
        ERROR_CODE_PASSWORD_RECOVERY_NOT_ENABLED("PWR-10004", "Password recovery is not enabled"),
        ERROR_CODE_PASSWORD_HISTORY_VIOLATION("PWR-10005", "This password has been used in recent "
                + "history. Please choose a different password"),
        ERROR_CODE_PASSWORD_POLICY_VIOLATION("PWR-10006", "Password policy violation"),
        ERROR_CODE_INVALID_CALLBACK_PASSWORD_RESET("PWR-10007", "Invalid callback url in password " +
                "reset request"),
        ERROR_CODE_PASSWORD_RECOVERY_EMPTY_TENANT_DOMAIN("PWR-10008", "Empty tenant domain in password "
                + "recovery request"),
        ERROR_CODE_PASSWORD_RECOVERY_VALIDATION_FAILED("PWR-10009",
                "Password recovery validation failed for user account: '%s'"),
        ERROR_CODE_ERROR_HANDLING_THE_EVENT("PWR-10010",
                "Error encountered while handling the event: '%s'"),
        ERROR_CODE_UNEXPECTED_ERROR_PASSWORD_RESET("PWR-15001", "Unexpected error during "
                + "password reset"),
        ERROR_CODE_UNSUPPORTED_SMS_OTP_REGEX("PWR-15008", "SMS OTP regex pattern is not supported."),
        ERROR_CODE_INVALID_USERNAME("PWR-10009", "Invalid username! Username should be in email format."),
        ERROR_CODE_SECURITY_QUESTION_BASED_PWR_LOCKED("PWR-15007", "Security question based password recovery" +
                " is locked."),
        // Resend Account Confirmation.
        ERROR_CODE_USER_OBJECT_NOT_FOUND("PWR-60001", "User object not found in the request"),

        /**
         * CQM - Challenge Question Manager.
         *
         * @see <href="https://github.com/wso2/identity-api-user/blob/master/components/org.wso2.carbon.identity.api.user.challenge/org.wso2.carbon.identity.api.user.challenge.common/src/main/java/org/wso2/carbon/identity/api/user/challenge/common/Constant.java">identity-api.user</>
         */
        ERROR_CODE_INVALID_ANSWER_FORMAT("10016", "Invalid answer format in the given answer " +
                "for the challenge question '%s'."),
        ERROR_CODE_NOT_UNIQUE_ANSWER("10017", "The given answer for the challenge question, " +
                "'%s' has been used more than once."),

        // UEV - User Email Verification.
        ERROR_CODE_VERIFICATION_EMAIL_NOT_FOUND("UEV-10001", "Email address not found for email verification"),
        ERROR_CODE_EMAIL_VERIFICATION_NOT_ENABLED("UEV-10002", "Email verification is not enabled"),
        ERROR_CODE_VERIFY_MULTIPLE_EMAILS("UEV-10003", "Unable to verify multiple email addresses " +
                "simultaneously"),
        ERROR_CODE_SUPPORT_MULTIPLE_EMAILS_NOT_ENABLED("UEV-10004", "Support for multiple email addresses " +
                "per user is not enabled"),
        ERROR_CODE_PRIMARY_EMAIL_SHOULD_BE_INCLUDED_IN_EMAILS_LIST("UEV-10005", "As multiple " +
                "email addresses support is enabled, primary email address should be included in the email " +
                "addresses list."),
        ERROR_CODE_PRIMARY_EMAIL_SHOULD_BE_INCLUDED_IN_VERIFIED_EMAILS_LIST("UEV-10006", "As multiple " +
                "email addresses support and email verification is enabled, primary email address should be included " +
                "in the verified email addresses list."),

        // UMV - User Mobile Verification.
        ERROR_CODE_MOBILE_VERIFICATION_NOT_ENABLED("UMV-10001", " Verified mobile numbers claim cannot be" +
                " updated as mobile number verification on update is disabled."),
        ERROR_CODE_VERIFY_MULTIPLE_MOBILE_NUMBERS("UMV-10002", "Unable to verify " +
                "multiple mobile numbers simultaneously."),
        ERROR_CODE_SUPPORT_MULTIPLE_MOBILE_NUMBERS_NOT_ENABLED("UEV-10003", "Support for multiple mobile " +
                "numbers per user is not enabled"),
        ERROR_CODE_PRIMARY_MOBILE_NUMBER_SHOULD_BE_INCLUDED_IN_MOBILE_NUMBERS_LIST("UMV-10004",
                "As multiple mobile numbers support is enabled, primary mobile number should be included in " +
                        "the mobile numbers list."),
        ERROR_CODE_PRIMARY_MOBILE_NUMBER_SHOULD_BE_INCLUDED_IN_VERIFIED_MOBILES_LIST("UMV-10005", "As " +
                "multiple mobile numbers support and mobile verification is enabled, primary mobile number should be " +
                "included in the verified mobile numbers list."),

        INVALID_PASSWORD_RECOVERY_REQUEST("APR-10000", "Invalid Password Recovery Request"),

        // Idle User Account Identification related Error messages.
        ERROR_RETRIEVING_ASSOCIATED_USER("UMM-65005",
                "Error retrieving the associated user for the user: %s in the tenant %s.");

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

    /**
     * Enum contains the status codes and status messages for successful user self registration scenarios.
     */
    public enum SuccessEvents {

        // USR - User Self Registration.
        SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION("USR-02001",
                "Successful user self registration. Pending account verification."),
        SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION("USR-02002",
                "Successful user self registration. Pending External verification."),
        SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION("USR-02003",
                "Successful user self registration. Account verification not required."),
        SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL("USR-02004",
                "Successful user self registration with verified channel. "
                        + "Account verification not required."),
        SUCCESS_STATUS_CODE_SUCCESSFUL_RESEND_ACCOUNT_CONFIRMATION("USR-20005",
                "Successfully resend confirmation. Pending account verification."),

        // UNR - Username Recovery.
        SUCCESS_STATUS_CODE_USERNAME_INTERNALLY_NOTIFIED("UNR-02001",
                "Username recovery information sent via user preferred notification channel."),
        SUCCESS_STATUS_CODE_USERNAME_EXTERNALLY_NOTIFIED("UNR-02002",
                "Username recovery information sent externally."),

        // PWR - Password Recovery.
        SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_INTERNALLY_NOTIFIED("PWR-02001",
                "Password recovery information sent via user preferred notification channel."),
        SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_EXTERNALLY_NOTIFIED("PWR-02002",
                "Password recovery information sent externally"),
        SUCCESS_STATUS_CODE_SUCCESSFUL_PASSWORD_UPDATE("PWR-02005", "Successful password reset."),

        // UAR - User Account Recovery.
        SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE("UAR-02001",
                "Confirmation code resent to the user.");

        private final String code;
        private final String message;

        SuccessEvents(String code, String message) {

            this.code = code;
            this.message = message;
        }

        /**
         * Get the code of the SuccessEvents.
         *
         * @return Code
         */
        public String getCode() {

            return code;
        }

        /**
         * Get the message of the success event.
         *
         * @return Message
         */
        public String getMessage() {

            return message;
        }

        @Override
        public String toString() {

            return code + " - " + message;
        }
    }

    public static class ConnectorConfig {

        public static final String PASSWORD_RECOVERY_SMS_OTP_EXPIRY_TIME =
                "Recovery.Notification.Password.ExpiryTime.smsOtp";
        public static final String PASSWORD_RECOVERY_SMS_OTP_REGEX = "Recovery.Notification.Password.smsOtp.Regex";
        public static final String RESEND_CODE_EXPIRY_TIME = "Recovery.Notification.ExpiryTime.ResendCode";
        public static final String RECOVERY_CODE_EXPIRY_TIME = "Recovery.Notification.ExpiryTime.RecoveryCode";
        public static final String ENABLE_ACCOUNT_LOCK_FOR_VERIFIED_PREFERRED_CHANNEL =
                "SelfRegistration.EnableAccountLockForVerifiedPreferredChannel";
        public static final String PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL = "Recovery.Notification.Password.OTP.SendOTPInEmail";
        public static final String PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE = "Recovery.Notification.Password.OTP.SendOnlyOTPAsConfirmationCode";
        public static final String PASSWORD_RECOVERY_USE_UPPERCASE_CHARACTERS_IN_OTP = "Recovery.Notification.Password." +
                "OTP.UseUppercaseCharactersInOTP";
        public static final String PASSWORD_RECOVERY_USE_LOWERCASE_CHARACTERS_IN_OTP = "Recovery.Notification.Password." +
                "OTP.UseLowercaseCharactersInOTP";
        public static final String PASSWORD_RECOVERY_USE_NUMBERS_IN_OTP = "Recovery.Notification.Password.OTP." +
                "UseNumbersInOTP";
        public static final String PASSWORD_RECOVERY_OTP_LENGTH = "Recovery.Notification.Password.OTP.OTPLength";
        public static final String NOTIFICATION_INTERNALLY_MANAGE = "Recovery.Notification.InternallyManage";
        public static final String NOTIFY_USER_EXISTENCE = "Recovery.NotifyUserExistence";
        public static final String NOTIFY_RECOVERY_EMAIL_EXISTENCE = "Recovery.NotifyRecoveryEmailExistence";
        public static final String NOTIFY_USER_ACCOUNT_STATUS = "Recovery.NotifyUserAccountStatus";
        public static final String NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS = "Recovery.NotifySuccess";
        public static final String NOTIFICATION_SEND_RECOVERY_SECURITY_START = "Recovery.Question.Password.NotifyStart";
        public static final String NOTIFICATION_BASED_PW_RECOVERY = "Recovery.Notification.Password.Enable";
        public static final String QUESTION_BASED_PW_RECOVERY = "Recovery.Question.Password.Enable";
        public static final String FORCE_ADD_PW_RECOVERY_QUESTION = "Recovery.Question.Password.Forced.Enable";
        public static final String FORCE_MIN_NO_QUESTION_ANSWERED = "Recovery.Question.MinQuestionsToAnswer";
        public static final String USERNAME_RECOVERY_ENABLE = "Recovery.Notification.Username.Enable";
        public static final String USERNAME_RECOVERY_EMAIL_ENABLE = "Recovery.Notification.Username.Email.Enable";
        public static final String USERNAME_RECOVERY_SMS_ENABLE = "Recovery.Notification.Username.SMS.Enable";
        public static final String USERNAME_RECOVERY_NON_UNIQUE_USERNAME = "Recovery.Notification.Username.NonUniqueUsername";
        public static final String QUESTION_CHALLENGE_SEPARATOR = "Recovery.Question.Password.Separator";
        public static final String QUESTION_MIN_NO_ANSWER = "Recovery.Question.Password.MinAnswers";
        public static final String EXPIRY_TIME = "Recovery.ExpiryTime";
        public static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE = "Recovery.Question.Password" +
                ".ReCaptcha.Enable";
        public static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS = "Recovery.Question" +
                ".Password.ReCaptcha.MaxFailedAttempts";
        public static final String RECOVERY_CALLBACK_REGEX = "Recovery.CallbackRegex";
        public static final String ENABLE_SELF_SIGNUP = "SelfRegistration.Enable";
        public static final String ACCOUNT_LOCK_ON_CREATION = "SelfRegistration.LockOnCreation";
        public static final String SELF_REGISTRATION_SEND_OTP_IN_EMAIL = "SelfRegistration.OTP.SendOTPInEmail";
        public static final String SELF_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP = "SelfRegistration.OTP." +
                "UseUppercaseCharactersInOTP";
        public static final String SELF_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP = "SelfRegistration.OTP." +
                "UseLowercaseCharactersInOTP";
        public static final String SELF_REGISTRATION_USE_NUMBERS_IN_OTP = "SelfRegistration.OTP." +
                "UseNumbersInOTP";
        public static final String SELF_REGISTRATION_OTP_LENGTH = "SelfRegistration.OTP.OTPLength";
        public static final String SEND_CONFIRMATION_NOTIFICATION = "SelfRegistration.SendConfirmationOnCreation";
        public static final String SHOW_USERNAME_UNAVAILABILITY = "SelfRegistration.ShowUsernameUnavailability";
        public static final String SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE = "SelfRegistration.Notification" +
                ".InternallyManage";
        public static final String SELF_REGISTRATION_RE_CAPTCHA = "SelfRegistration.ReCaptcha";
        public static final String SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME = "SelfRegistration" +
                ".VerificationCode.ExpiryTime";
        public static final String SELF_REGISTRATION_CALLBACK_REGEX = "SelfRegistration.CallbackRegex";
        public static final String SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME =
                "SelfRegistration.VerificationCode.SMSOTP.ExpiryTime";
        public static final String SELF_REGISTRATION_SMS_OTP_REGEX = "SelfRegistration.SMSOTP.Regex";
        public static final String SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION = "SelfRegistration" +
                ".NotifyAccountConfirmation";
        public static final String RESEND_CONFIRMATION_RECAPTCHA_ENABLE =
                "SelfRegistration.ResendConfirmationReCaptcha";

        public static final String ENABLE_LITE_SIGN_UP = "LiteRegistration.Enable";
        public static final String LITE_ACCOUNT_LOCK_ON_CREATION = "LiteRegistration.LockOnCreation"; //if passwordless
        public static final String LITE_REGISTRATION_SEND_OTP_IN_EMAIL = "LiteRegistration.OTP.SendOTPInEmail";
        public static final String LITE_REGISTRATION_USE_UPPERCASE_CHARACTERS_IN_OTP = "LiteRegistration.OTP." +
                "UseUppercaseCharactersInOTP";
        public static final String LITE_REGISTRATION_USE_LOWERCASE_CHARACTERS_IN_OTP = "LiteRegistration.OTP." +
                "UseLowercaseCharactersInOTP";
        public static final String LITE_REGISTRATION_USE_NUMBERS_IN_OTP = "LiteRegistration.OTP." +
                "UseNumbersInOTP";
        public static final String LITE_REGISTRATION_OTP_LENGTH = "LiteRegistration.OTP.OTPLength";
        public static final String LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE = "LiteRegistration.Notification" +
                ".InternallyManage";
        public static final String LITE_REGISTRATION_RE_CAPTCHA = "LiteRegistration.ReCaptcha";
        public static final String LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME = "LiteRegistration" +
                ".VerificationCode.ExpiryTime";
        public static final String LITE_REGISTRATION_SMS_OTP_REGEX = "LiteRegistration" +
                ".SMSOTP.Regex";
        public static final String LITE_REGISTRATION_CALLBACK_REGEX = "LiteRegistration.CallbackRegex";
        public static final String LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME =
                "LiteRegistration.VerificationCode.SMSOTP.ExpiryTime";
        public static final String LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE =
                "LiteRegistration.ResendVerificationOnUserExistence";
        public static final String ENABLE_EMAIL_VERIFICATION = "EmailVerification.Enable";
        public static final String EMAIL_VERIFICATION_SEND_OTP_IN_EMAIL = "EmailVerification.OTP.SendOTPInEmail";
        public static final String EMAIL_VERIFICATION_USE_UPPERCASE_CHARACTERS_IN_OTP = "EmailVerification.OTP." +
                "UseUppercaseCharactersInOTP";
        public static final String EMAIL_VERIFICATION_USE_LOWERCASE_CHARACTERS_IN_OTP = "EmailVerification.OTP." +
                "UseLowercaseCharactersInOTP";
        public static final String EMAIL_VERIFICATION_USE_NUMBERS_IN_OTP = "EmailVerification.OTP." +
                "UseNumbersInOTP";
        public static final String EMAIL_VERIFICATION_OTP_LENGTH = "EmailVerification.OTP.OTPLength";
        public static final String EMAIL_VERIFICATION_EXPIRY_TIME = "EmailVerification.ExpiryTime";
        public static final String ENABLE_EMAIL_VERIFICATION_ON_UPDATE = "UserClaimUpdate.Email." +
                "EnableVerification";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_SEND_OTP_IN_EMAIL = "UserClaimUpdate.OTP.SendOTPInEmail";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_USE_UPPERCASE_CHARACTERS_IN_OTP = "UserClaimUpdate." +
                "OTP.UseUppercaseCharactersInOTP";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_USE_LOWERCASE_CHARACTERS_IN_OTP = "UserClaimUpdate." +
                "OTP.UseLowercaseCharactersInOTP";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_USE_NUMBERS_IN_OTP = "UserClaimUpdate." +
                "OTP.UseNumbersInOTP";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_OTP_LENGTH = "UserClaimUpdate.OTP.OTPLength";
        public static final String EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME = "UserClaimUpdate.Email.VerificationCode" +
                ".ExpiryTime";
        public static final String ENABLE_NOTIFICATION_ON_EMAIL_UPDATE = "UserClaimUpdate.Email.EnableNotification";
        public static final String ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE = "UserClaimUpdate.MobileNumber." +
                "EnableVerification";
        public static final String MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME = "UserClaimUpdate.MobileNumber." +
                "VerificationCode.ExpiryTime";
        public static final String ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER = "UserClaimUpdate.MobileNumber." +
                "EnableVerificationByPrivilegedUser";
        public static final String USE_VERIFY_CLAIM_ON_UPDATE = "UserClaimUpdate.UseVerifyClaim";
        // This config enables the support to store multiple mobile numbers and email addresses per user.
        public static final String SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER =
                "UserClaimUpdate.EnableMultipleEmailsAndMobileNumbers";
        public static final String ASK_PASSWORD_EXPIRY_TIME = "EmailVerification.AskPassword.ExpiryTime";
        public static final String ASK_PASSWORD_TEMP_PASSWORD_GENERATOR = "EmailVerification.AskPassword.PasswordGenerator";
        public static final String ASK_PASSWORD_DISABLE_RANDOM_VALUE_FOR_CREDENTIALS = "EmailVerification.AskPassword" +
                ".DisableRandomValueForCredentials";
        public static final String EMAIL_ACCOUNT_LOCK_ON_CREATION = "EmailVerification.LockOnCreation";
        public static final String EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE = "EmailVerification.Notification.InternallyManage";
        public static final String EMAIL_VERIFICATION_NOTIFICATION_ACCOUNT_ACTIVATION = "EmailVerification.AskPassword" +
                ".AccountActivation";

        public static final String TENANT_ADMIN_ASK_PASSWORD_EXPIRY_TIME = "TenantRegistrationVerification." +
                "AskPassword.ExpiryTime";

        public static final String ENABLE_ADMIN_PASSWORD_RESET_OFFLINE = "Recovery.AdminPasswordReset.Offline";
        public static final String ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP = "Recovery.AdminPasswordReset.OTP";
        public static final String ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK = "Recovery.AdminPasswordReset.RecoveryLink";
        public static final String ADMIN_PASSWORD_RESET_EXPIRY_TIME = "Recovery.AdminPasswordReset.ExpiryTime";

        public static final String PASSWORD_RECOVERY_RECAPTCHA_ENABLE = "Recovery.ReCaptcha.Password.Enable";
        public static final String USERNAME_RECOVERY_RECAPTCHA_ENABLE = "Recovery.ReCaptcha.Username.Enable";

        public static final String CHALLENGE_QUESTION_ANSWER_REGEX = "Recovery.Question.Answer.Regex";
        public static final String ENFORCE_CHALLENGE_QUESTION_ANSWER_UNIQUENESS = "Recovery.Question.Answer.Uniqueness";
        public static final String ENABLE_AUTO_LGOIN_AFTER_PASSWORD_RESET = "Recovery.AutoLogin.Enable";
        public static final String SELF_REGISTRATION_AUTO_LOGIN = "SelfRegistration.AutoLogin.Enable";
        public static final String SELF_REGISTRATION_AUTO_LOGIN_ALIAS_NAME = "SelfRegistration.AutoLogin.AliasName";
        public static final String RECOVERY_NOTIFICATION_PASSWORD_MAX_FAILED_ATTEMPTS = "Recovery.Notification" +
                ".Password.MaxFailedAttempts";
        public static final String RECOVERY_NOTIFICATION_PASSWORD_MAX_RESEND_ATTEMPTS = "Recovery.Notification" +
                ".Password.MaxResendAttempts";
        public static final String PASSWORD_RECOVERY_EMAIL_LINK_ENABLE = "Recovery.Notification.Password.emailLink.Enable";
        public static final String PASSWORD_RECOVERY_SMS_OTP_ENABLE = "Recovery.Notification.Password.smsOtp.Enable";
    }

    public static class DBConstants {

        public static final String USER_NAME = "USER_NAME";
        public static final String TENANT_ID = "TENANT_ID";
        public static final String USER_DOMAIN = "USER_DOMAIN";
        public static final String CODE = "CODE";
        public static final String SCENARIO = "SCENARIO";
        public static final String REMAINING_SETS = "REMAINING_SETS";
        public static final String RECOVERY_FLOW_ID = "RECOVERY_FLOW_ID";
        public static final String FAILED_ATTEMPTS = "FAILED_ATTEMPTS";
        public static final String RESEND_COUNT = "RESEND_COUNT";
        public static final String TIME_CREATED = "TIME_CREATED";
    }

    public static class SQLQueries {

        public static final String STORE_RECOVERY_DATA = "INSERT INTO IDN_RECOVERY_DATA "
                + "(USER_NAME, USER_DOMAIN, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED, REMAINING_SETS)"
                + "VALUES (?,?,?,?,?,?,?,?)";

        public static final String STORE_RECOVERY_DATA_WITH_FLOW_ID = "INSERT INTO IDN_RECOVERY_DATA "
                + "(USER_NAME, USER_DOMAIN, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED, REMAINING_SETS, " +
                "RECOVERY_FLOW_ID) VALUES (?,?,?,?,?,?,?,?,?)";

        public static final String LOAD_RECOVERY_DATA = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ? AND CODE = ? AND " +
                "SCENARIO = ? AND STEP = ?";

        public static final String LOAD_RECOVERY_DATA_CASE_INSENSITIVE = "SELECT * FROM IDN_RECOVERY_DATA WHERE" +
                " LOWER(USER_NAME)=LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? AND CODE= ? AND SCENARIO = ? AND " +
                "STEP = ?";

        public static final String LOAD_RECOVERY_DATA_FROM_CODE = "SELECT * FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String LOAD_RECOVERY_DATA_FROM_RECOVERY_FLOW_ID = "SELECT * FROM IDN_RECOVERY_DATA WHERE" +
                " RECOVERY_FLOW_ID = ? AND STEP = ?";

        public static final String INVALIDATE_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_USER_CODES =
                "DELETE FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID =? " +
                "AND SCENARIO <> 'EMAIL_VERIFICATION_ON_UPDATE' AND SCENARIO <> 'MOBILE_VERIFICATION_ON_UPDATE'";

        public static final String INVALIDATE_USER_CODES_CASE_INSENSITIVE =
                "DELETE FROM IDN_RECOVERY_DATA WHERE LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID =? " +
                "AND SCENARIO <> 'EMAIL_VERIFICATION_ON_UPDATE' AND SCENARIO <> 'MOBILE_VERIFICATION_ON_UPDATE'";

        public static final String INVALIDATE_USER_CODE_BY_SCENARIO = "DELETE FROM IDN_RECOVERY_DATA WHERE " +
                "USER_NAME = ? AND SCENARIO = ? AND STEP = ? AND USER_DOMAIN = ? AND TENANT_ID =?";

        public static final String UPDATE_CODE = "UPDATE IDN_RECOVERY_DATA SET CODE = ?, STEP = ?, REMAINING_SETS = ? " +
                "WHERE CODE = ?";

        public static final String INVALIDATE_USER_CODE_BY_SCENARIO_CASE_INSENSITIVE = "DELETE FROM " +
                "IDN_RECOVERY_DATA WHERE LOWER(USER_NAME)=LOWER(?) AND SCENARIO = ? AND STEP = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID =?";

        public static final String DELETE_USER_RECOVERY_DATA_BY_TENANT_ID = "DELETE FROM IDN_RECOVERY_DATA WHERE TENANT_ID = ?";

        public static final String LOAD_RECOVERY_DATA_OF_USER =
                "SELECT * FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ? " +
                "AND SCENARIO <> 'EMAIL_VERIFICATION_ON_UPDATE' AND SCENARIO <> 'MOBILE_VERIFICATION_ON_UPDATE'";

        public static final String LOAD_RECOVERY_DATA_OF_USER_CASE_INSENSITIVE =
                "SELECT * FROM IDN_RECOVERY_DATA WHERE LOWER(USER_NAME)=LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? " +
                "AND SCENARIO <> 'EMAIL_VERIFICATION_ON_UPDATE' AND SCENARIO <> 'MOBILE_VERIFICATION_ON_UPDATE'";

        public static final String LOAD_RECOVERY_DATA_OF_USER_BY_SCENARIO = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND SCENARIO = ? AND USER_DOMAIN = ? " +
                "AND TENANT_ID = ?";

        public static final String LOAD_RECOVERY_DATA_OF_USER_BY_SCENARIO_CASE_INSENSITIVE = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE LOWER(USER_NAME)=LOWER(?) AND SCENARIO = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID = ?";

        public static final String LOAD_RECOVERY_DATA_OF_USER_BY_STEP = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND SCENARIO = ? AND USER_DOMAIN = ? " +
                "AND TENANT_ID = ? AND STEP = ?";

        public static final String LOAD_RECOVERY_DATA_OF_USER_BY_STEP_CASE_INSENSITIVE = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE LOWER(USER_NAME)=LOWER(?) AND SCENARIO = ? AND USER_DOMAIN = ? " +
                "AND TENANT_ID = ? AND STEP = ?";

        public static final String STORE_RECOVERY_FLOW_DATA = "INSERT INTO IDN_RECOVERY_FLOW_DATA "
                + "(RECOVERY_FLOW_ID, CODE, FAILED_ATTEMPTS, RESEND_COUNT, TIME_CREATED) VALUES (?,?,?,?,?)";

        public static final String UPDATE_RECOVERY_FLOW_DATA = "UPDATE IDN_RECOVERY_FLOW_DATA SET CODE = ? "
                + "WHERE RECOVERY_FLOW_ID = ?";

        public static final String UPDATE_FAILED_ATTEMPTS = "UPDATE IDN_RECOVERY_FLOW_DATA SET FAILED_ATTEMPTS = ? "
                + "WHERE RECOVERY_FLOW_ID = ?";

        public static final String UPDATE_CODE_RESEND_COUNT = "UPDATE IDN_RECOVERY_FLOW_DATA SET RESEND_COUNT = ? "
                + "WHERE RECOVERY_FLOW_ID = ?";

        public static final String LOAD_RECOVERY_FLOW_DATA_FROM_RECOVERY_FLOW_ID = "SELECT * " +
                "FROM IDN_RECOVERY_FLOW_DATA WHERE RECOVERY_FLOW_ID = ?";

        public static final String INVALIDATE_BY_RECOVERY_FLOW_ID = "DELETE FROM IDN_RECOVERY_FLOW_DATA WHERE " +
                "RECOVERY_FLOW_ID = ?";
    }

    public static class Questions {

        public static final String LOCALE_CLAIM = IdentityUtil.getClaimUriLocale();
        public static final String BLACKLIST_REGEX = ".*[/\\\\].*";

        public static final String CHALLENGE_QUESTION_SET_ID = "questionSetId";
        public static final String CHALLENGE_QUESTION_ID = "questionId";
        public static final String CHALLENGE_QUESTION_LOCALE = "locale";

        // TODO remove this
        public static final List<String> SECRET_QUESTIONS_SET01 = Collections.unmodifiableList(Arrays.asList
                ("City where you were born ?", "Father's middle name ?", "Favorite food ?",
                        "Favorite vacation location ?"));

        // TODO remove this
        public static final List<String> SECRET_QUESTIONS_SET02 = Collections.unmodifiableList(Arrays.asList
                ("Model of your first car ?", "Name of the hospital where you were born ?",
                        "Name of your first pet ?", "Favorite sport ?"));

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

        // This is used for retrieve SP-UUID from the event.
        // Todo: once the framework version bump to 7.3.14.x or above use the constant from framework instead of this.
        public static final String SERVICE_PROVIDER_UUID = "serviceProviderUUID";
    }

    /**
     * Constants used for masking the notification channels.
     */
    public static class ChannelMasking {

        public static final String MASKING_CHARACTER = "*";
        public static final String EMAIL_MASKING_REGEX =
                "(?<=.)[^@](?=[^@]*?@)|(?:(?<=@.)|(?!^)\\G(?=[^@]*$)).(?=.*[^@]\\.)";
        public static final String MOBILE_MASKING_REGEX = ".(?=.{4})";
    }

    /**
     * Enum which contains scenarios where it is not required to trigger an email verification.
     */
    public enum SkipEmailVerificationOnUpdateStates {

        // State maintained to skip re-triggering an email verification when confirming the verification code.
        SKIP_ON_CONFIRM,

        /* State maintained to skip triggering an email verification when the email address to be updated is the same
        as the existing email address. */
        SKIP_ON_EXISTING_EMAIL,

        /* State maintained to skip triggering an email verification when the update request contains other claims
        without the email address claim. */
        SKIP_ON_INAPPLICABLE_CLAIMS,

        /* State maintained to skip triggering an email verification, when the email address was updated by user during
         the Email OTP flow at the first login where the email address is not previously set. At the moment email
         address was already verified during the email OTP verification. So no need to verify it again. */
        SKIP_ON_EMAIL_OTP_FLOW,

        /* State maintained to skip triggering an email verification, when the email address to be updated is included
        in the verifiedEmailAddresses claim, which has been already verified. */
        SKIP_ON_ALREADY_VERIFIED_EMAIL_ADDRESSES
    }

    /**
     * Enum contains the codes and status messages for per-user functionality locking.
     */
    public enum RecoveryLockReasons {

        PWD_RECOVERY_MAX_ATTEMPTS_EXCEEDED("RFL_001", "Maximum attempts exceeded for password recovery.");

        private final String functionalityLockCode;
        private final String functionalityLockReason;

        /**
         * Per-user lock code constructor.
         *
         * @param functionalityLockCode   Lock reason code.
         * @param functionalityLockReason Reason for the functionality lock.
         */
        RecoveryLockReasons(String functionalityLockCode, String functionalityLockReason) {

            this.functionalityLockCode = functionalityLockCode;
            this.functionalityLockReason = functionalityLockReason;
        }

        public String getFunctionalityLockReason() {

            return functionalityLockReason;
        }

        public String getFunctionalityLockCode() {

            return functionalityLockCode;
        }
    }

    /**
     * Enum contains the Functionality and Functionality Identifier.
     */
    public enum FunctionalityTypes {

        FUNCTIONALITY_SECURITY_QUESTION_PW_RECOVERY("FUNCTIONALITY_SECURITY_QUESTION_PW_RECOVERY",
                "SecurityQuestionBasedPasswordRecovery"),
        FUNCTIONALITY_NOTIFICATION_BASED_PW_RECOVERY_SMS("FUNCTIONALITY_NOTIFICATION_BASED_PW_RECOVERY_SMS",
                "SMSBasedPasswordRecovery");

        private final String functionalityName;
        private final String functionalityIdentifier;
        private static Map<String, FunctionalityTypes> functionalityToTypeMapping;

        private FunctionalityTypes(String functionalityName, String functionalityIdentifier) {

            this.functionalityName = functionalityName;
            this.functionalityIdentifier = functionalityIdentifier;
        }

        public String getFunctionalityIdentifier() {

            return this.functionalityIdentifier;
        }

        public String getDescription() {

            return this.functionalityName;
        }

        public String toString() {

            return this.functionalityIdentifier + " - " + this.functionalityName;
        }

        public static FunctionalityTypes getFunctionality(String functionalityName) {

            if (functionalityToTypeMapping == null) {
                initMapping();
            }
            return functionalityToTypeMapping.get(functionalityName);
        }

        private static void initMapping() {

            functionalityToTypeMapping = new HashMap<>();
            for (FunctionalityTypes types : values()) {
                functionalityToTypeMapping.put(types.functionalityName, types);
            }
        }
    }

    /**
     * Enum which contains scenarios where it is not required to trigger an SMS OTP verification.
     */
    public enum SkipMobileNumberVerificationOnUpdateStates {

        // State maintained to skip re-triggering an SMS OTP verification when confirming the verification code.
        SKIP_ON_CONFIRM,

        /* State maintained to skip triggering an SMS OTP verification when the mobile number to be updated is the same
        as the existing mobile number. */
        SKIP_ON_EXISTING_MOBILE_NUM,

        /* State maintained to skip triggering an SMS OTP verification when the update request contains other claims
        without the mobile number claim. */
        SKIP_ON_INAPPLICABLE_CLAIMS,

        /* State maintained to skip triggering an SMS OTP verification, when the mobile number was updated by user
        during the SMS OTP flow at the first login where the mobile number is not previously set. At the moment mobile
        number was already verified during the SMS OTP verification. So no need to verify it again. */
        SKIP_ON_SMS_OTP_FLOW,

        /* State maintained to skip triggering an SMS OTP verification, when the mobile number to be updated is included
        in the verifiedMobileNumbers claim, which has been already verified. */
        SKIP_ON_ALREADY_VERIFIED_MOBILE_NUMBERS
    }
}
