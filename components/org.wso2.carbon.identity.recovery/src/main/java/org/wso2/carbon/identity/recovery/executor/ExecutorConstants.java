/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.executor;

/**
 * This class contains the constants related to the executors.
 */
public class ExecutorConstants {

    public static final String USER_ALREADY_EXISTING_USERNAME = "UserAlreadyExistingUsername";
    public static final String DUPLICATE_CLAIM_ERROR_CODE = "UCM-60001";
    public static final String DUPLICATE_CLAIMS_ERROR_CODE = "UCM-60002";
    public static final String DISPLAY_CLAIM_AVAILABILITY_CONFIG = "FlowExecution.Registration" +
            ".DisplayClaimAvailability";
    public static final String REGISTRATION_DEFAULT_USER_STORE_CONFIG = "FlowExecution.Registration.DefaultUserStore";

    /**
     * Enum for error messages.
     */
    protected enum ExecutorErrorMessages {

        ERROR_CODE_USERNAME_ALREADY_EXISTS("60001",
                "Username already exists.",
                "The provided username already exists in the tenant: %s"),
        ERROR_CODE_INVALID_USERNAME("60002",
                "Invalid username.",
                "The given username: %s must be an email address."),
        ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE("60003",
                "%s",
                "%s"),
        ERROR_CODE_USER_CLAIM_ALREADY_EXISTS("60004",
                "Duplicate claim value.",
                "%s"),
        ERROR_CODE_USER_PROVISIONING_FAILURE("60005",
                "Error while provisioning user.",
                "Error occurred while provisioning user in the request of flow id: %s"),
        ERROR_CODE_USER_ONBOARD_FAILURE("65001",
                "Error while onboarding user.",
                "Error occurred while onboarding user: %s in the request of flow id: %s"),
        ERROR_CODE_USERSTORE_MANAGER_FAILURE("65002",
                         "Error while loading the userstore manager.",
                         "Error occurred loading the userstore manager of tenant: %s while serving the" +
                         " %s request of flow id: %s."),
        ERROR_CODE_RESOLVE_NOTIFICATION_PROPERTY_FAILURE("65003",
                "Error while resolving notification properties.",
                "Error occurred while resolving notification properties for user: %s " +
                        "in the request of flow id: %s");

        private static final String ERROR_PREFIX = "FEE";
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
