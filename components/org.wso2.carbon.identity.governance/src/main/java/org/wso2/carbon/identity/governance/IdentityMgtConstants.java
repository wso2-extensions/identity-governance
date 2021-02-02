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

package org.wso2.carbon.identity.governance;

/**
 * Identity management related constants
 */
public class IdentityMgtConstants {

    private IdentityMgtConstants(){}

    public static final String USER_IDENTITY_CLAIMS = "UserIdentityClaims";
    public static final String ERROR_CODE_DELIMITER = "-";

    /**
     * User account states.
     */
    public class AccountStates {

        private AccountStates() {}

        public static final String PENDING_ADMIN_FORCED_USER_PASSWORD_RESET = "PENDING_FUPR";
    }

    /**
     * Class that contains the error scenarios.
     */
    public static class Error_Scenario {

        public static final String NOTIFICATION_CHANNEL_MANAGER = "NCM";
        public static final String NOTIFICATION_TEMPLATE_MANAGER = "NTM";
    }

    public static class PropertyConfig {

        private PropertyConfig(){}

        // Enable notification channel resolver.
        public static final String RESOLVE_NOTIFICATION_CHANNELS = "Notification.ResolveNotificationChannels.Enable";
        public static final String CONFIG_FILE_NAME = "identity-mgt.properties";
        public static final String ACCOUNT_LOCK_ENABLE = "Account.Lock.Enable";
        public static final String AUTH_POLICY_ENABLE = "Authentication.Policy.Enable";
        public static final String AUTH_POLICY_ACCOUNT_EXIST = "Authentication.Policy.Check.Account.Exist";
        public static final String AUTH_POLICY_ACCOUNT_LOCKING_FAIL_ATTEMPTS = "Authentication.Policy.Account.Lock.On.Failure.Max.Attempts";
        public static final String PASSWORD_POLICY_EXTENSIONS = "Password.policy.extensions";
        public static final String EXTENSION_USER_DATA_STORE = "Identity.Mgt.User.Data.Store";
        public static final String EXTENSION_USER_RECOVERY_DATA_STORE = "Identity.Mgt.User.Recovery.Data.Store";
        public static final String NOTIFICATION_LINK_EXPIRE_TIME = "Notification.Expire.Time";

    }

    public static class Event {

        private Event(){}

        public static final String PRE_AUTHENTICATION = "PRE_AUTHENTICATION";

    }


    public static class EventProperty {

        private EventProperty(){}

        public static final String MODULE = "module";
        public static final String USER_NAME = "userName";
        public static final String USER_STORE_MANAGER = "userStoreManager";
        public static final String IDENTITY_MGT_CONFIG = "identityMgtConfig";

    }

    public static class ErrorMessage {

        private ErrorMessage(){}

        public static final String FAILURE = "Failure";
        public static final String FAILED_AUTHENTICATION = "Authentication Failed.";
        public static final String FAILED_ENCRYPTION = "Encryption Failed";

    }

    public static class Claim {

        private Claim(){}

        public static final String FAIL_LOGIN_ATTEMPTS = "http://wso2.org/claims/identity/failedLoginAttempts";
        public static final String UNLOCKING_TIME = "http://wso2.org/claims/identity/unlockTime";
        public static final String ACCOUNT_LOCK = "http://wso2.org/claims/identity/accountLocked";
        public static final String PREFERED_CHANNEL_CLAIM = "http://wso2.org/claims/identity/preferredChannel";

    }

    public static class NotificationChannelConstants {

        private NotificationChannelConstants() {
        }

        public static final String DEFAULT_NOTIFICATION_CHANNEL = "Notification.DefaultNotificationChannel";
    }

    public enum LockedReason {

        PENDING_SELF_REGISTRATION,
        PENDING_ADMIN_FORCED_USER_PASSWORD_RESET,
        PENDING_EMAIL_VERIFICATION,
        PENDING_ASK_PASSWORD,
        IDLE_ACCOUNT,
        ADMIN_INITIATED,
        MAX_ATTEMPTS_EXCEEDED
    }

    public enum ErrorMessages {

        ERROR_CODE_DEFAULT_BAD_REQUEST("10000", "Bad Request"),
        ERROR_CODE_DEFAULT_SERVER_ERROR("65000", "Internal Server Error"),
        ERROR_CODE_DEFAULT_UNEXPECTED_ERROR("65000", "Unexpected Error"),

        // NCM - notification Channel Manager.
        ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNEL("NCM-10001","Channel not supported"),
        ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL("NCM-10002",
                "No claim matched for preferred channel"),
        ERROR_CODE_NO_NOTIFICATION_CHANNELS("NCM-10003","User has no notification channels"),
        ERROR_CODE_BAD_REQUEST("NCM-10004", "Bad Request"),
        ERROR_CODE_SERVER_ERROR("NCM-15001", "Server Error"),

        // NTM - Notification Template Manager.
        ERROR_CODE_NO_CONTENT_IN_TEMPLATE("NTM-10001","Unable to find any content in %s:%s "
                + "email template"),
        ERROR_CODE_INVALID_EMAIL_TEMPLATE_CONTENT("NTM-10002","Template %s:%s body is in "
                + "invalid format. Missing subject,body or footer"),
        ERROR_CODE_INVALID_SMS_TEMPLATE_CONTENT("NTM-10003","Template %s:%s body is in a "
                + "invalid format. Should have only a SMS body"),
        ERROR_CODE_NO_TEMPLATE_FOUND("NTM-10004","Cannot find '%s' template in the default '%s' "
                + "locale for '%s' tenant"),
        ERROR_CODE_INVALID_NOTIFICATION_TEMPLATE("NTM-10005","Invalid notification template"),
        ERROR_CODE_DESERIALIZING_TEMPLATE_FROM_TENANT_REGISTRY("NTM-65001","Error deserializing '%s:%s' "
                + "template from tenant registry"),
        ERROR_CODE_ERROR_RETRIEVING_TEMPLATE_FROM_REGISTRY("NTM-65002","Error when retrieving '%s:%s' "
                + "template from %s tenant registry"),
        ERROR_CODE_ERROR_RETRIEVING_TEMPLATE_OBJECT_FROM_REGISTRY("NTM-65003","Error retrieving a "
                + "template object from the registry resource");

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
}
