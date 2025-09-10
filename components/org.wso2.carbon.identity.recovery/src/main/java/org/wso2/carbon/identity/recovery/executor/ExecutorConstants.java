package org.wso2.carbon.identity.recovery.executor;

import org.wso2.carbon.identity.governance.IdentityMgtConstants;

public class ExecutorConstants {

    /**
     * Enum for error messages.
     */
    protected enum ExecutorErrorMessages {

        ERROR_CODE_USERNAME_ALREADY_EXISTS("60003",
                "Username already exists.",
                "The provided username already exists in the tenant: %s"),
        ERROR_CODE_INVALID_USERNAME("60005",
                "Invalid username.",
                "The given username: %s must be an email address."),
        ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE("60012",
                "%s",
                "%s");

        private static final String ERROR_PREFIX = "FE";
        private final String code;
        private final String message;
        private final String description;

        ExecutorErrorMessages(String code, String message, String description) {

            this.code = ERROR_PREFIX + "-" + code;
            this.message = message;
            this.description = description;
        }

        public String getCode() {

            return code;
        }

        public String getMessage() {

            return message;
        }

        public String getDescription() {

            return description;
        }

        @Override
        public String toString() {

            return code + ":" + message;
        }
    }
}
