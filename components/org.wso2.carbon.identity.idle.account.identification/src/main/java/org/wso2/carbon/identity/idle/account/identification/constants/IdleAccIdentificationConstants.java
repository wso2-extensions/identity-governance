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

    /**
     * Class containing SQL queries.
     */
    public static class SQLConstants {

        public static final String FILTER_USERS_BY_DATA_KEY_LESS_THAN_DATA_VALUE =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ?";

        public static final String FILTER_USERS_BY_DATA_KEY_LESS_THAN_AND_GREATER_THAN_DATA_VALUES =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ? AND DATA_VALUE > ?";
    }

    /**
     * Error messages.
     */
    public enum ErrorMessages {

        // Server errors 650xx.
        ERROR_RETRIEVE_INACTIVE_USERS_FROM_DB("65002",
                "Error while retrieving inactive users from database.",
                "Error while retrieving inactive users for organization: %s."),

        ERROR_RETRIEVE_USER_STORE_MANAGER("65003",
                "Error while retrieving user store manager.",
                "Error while retrieving user store manager of user in organization: %s."),

        ERROR_RETRIEVE_USER_UUID("65004",
                "Error while retrieving UUID of the user.",
                "Error while retrieving UUID of the user from organization: %s.");

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
