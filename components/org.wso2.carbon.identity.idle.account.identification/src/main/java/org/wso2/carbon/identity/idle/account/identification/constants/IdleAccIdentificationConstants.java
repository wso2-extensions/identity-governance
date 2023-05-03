/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.idle.account.identification.constants;

/**
 * Constants for idle account identification.
 */
public class IdleAccIdentificationConstants {

    public static final String IDLE_ACC_IDENTIFICATION_SERVICE_ERROR_PREFIX = "IDLE_ACC-";

    public static final String LAST_LOGIN_TIME_CLAIM = "http://wso2.org/claims/identity/lastLogonTime";
    public static final String EMAIL_CLAIM = "http://wso2.org/claims/emailaddress";
    public static final String DATE_INACTIVE_AFTER = "inactiveAfter";
    public static final String DATE_EXCLUDE_BEFORE = "excludeBefore";
    public static final String DATE_FORMAT_REGEX = "^\\d{4}-\\d{2}-\\d{2}$";
    public static final String DEFAULT_USER_STORE_NAME = "DEFAULT";

    /**
     * Class containing SQL queries.
     */
    public static class SQLConstants {

        public static final String GET_INACTIVE_USERS_FROM_SPECIFIC_DATE =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ?";

        public static final String GET_LIMITED_INACTIVE_USERS_FROM_SPECIFIC_DATE =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ? AND DATA_VALUE > ?";
    }

    /**
     * Error messages.
     */
    public enum ErrorMessages {

        // client errors 600xx
        ERROR_REQUIRED_PARAMETER_MISSING("60001",
                "Required parameter is not provided.",
                "%s parameter is required and cannot be empty."),

        ERROR_DATE_REGEX_MISMATCH("60002",
                "Invalid date format provided.",
                "The value provided for %s parameter is invalid. Date format should be yyyy-mm-dd"),

        ERROR_INVALID_DATE("60003",
                "Invalid date provided.",
                "The date provided for %s parameter is invalid"),


        // server errors 650xx
        ERROR_DATE_CONVERSION_TO_EPOCH("65001",
                "Error while converting date to epoch.",
                "Error while converting date to epoch for %s parameter."),

        ERROR_RETRIEVE_INACTIVE_USERS_FROM_DB("65002",
                "Error while retrieving inactive users from database.",
                "Error while retrieving inactive users for organization: %s."),

        ERROR_RETRIEVE_USER_STORE_MANAGER("65003",
                "Error while retrieving user store manager.",
                "Error while retrieving user store manager of user in organization: %s."),


        ERROR_RETRIEVE_USER_ATTRIBUTES("65004",
                "Error while retrieving values of user attributes.",
                "Error while retrieving values of user attributes from organization: %s.");


        private final String code;
        private final String message;
        private final String description;

        ErrorMessages(String code, String message, String description) {

            this.code = code;
            this.message = message;
            this.description = description;
        }

        /**
         * Return the error code.
         *
         * @return Code.
         */
        public String getCode() {

            return IDLE_ACC_IDENTIFICATION_SERVICE_ERROR_PREFIX + code;
        }

        /**
         * Return the error message.
         *
         * @return Message.
         */
        public String getMessage() {

            return message;
        }

        /**
         * Return the error description.
         *
         * @return Description.
         */
        public String getDescription() {

            return description;
        }

        @Override
        public String toString() {

            return code + " | " + message;
        }
    }
}
