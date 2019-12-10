/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.user.rename.core.constants;

/**
 * Constants related to Username update service are defined here.
 */
public class UsernameUpdateServiceConstants {

    public static final String ACCOUNT_DISABLE_CLAIM = "http://wso2.org/claims/identity/accountDisabled";

    private UsernameUpdateServiceConstants() {

    }

    /**
     * Response status codes and messages.
     */
    public enum Status {
        STATUS_SUCCESS(0, "Successfully updated username from %s to %s."),
        STATUS_ACCEPTED(1, "Username update from %s to %s accepted.");

        private final int code;
        private final String message;

        Status(int code, String message) {

            this.code = code;
            this.message = message;
        }

        public int getCode() {

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
     * Error codes and messages
     */
    public enum Error {

        ERROR_INVALID_USERNAME("17101", "Existing username and the expected new username cannot be empty."),
        ERROR_INVALID_NEW_USERNAME("17102", "New username should be different from the existing username"),
        ERROR_USER_NOT_FOUND("17103", "User not found in tenant: %s for username: %s."),
        ERROR_UNEXPECTED("17199", "Unexpected error.");

        private final String code;
        private final String message;

        Error(String code, String message) {

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
