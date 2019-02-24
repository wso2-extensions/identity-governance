/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.util;

/**
 * Contains constants used for claim verification.
 */
public class ClaimVerificationCoreConstants {

    private ClaimVerificationCoreConstants() {

    }

    public static final String PROP_IS_RETRY_ATTEMPT = "IS_CLAIM_VERIFICATION_RETRY_ATTEMPT";

    /**
     * Claim verification related error codes and messages.
     */
    public enum ErrorMessages {

        ERROR_MSG_STORING_DATA("CV0001", "Error when storing data."),
        ERROR_MSG_INVALIDATING_CODE("CV0002", "Error when invalidating confirmation code."),
        ERROR_MSG_RETRIEVING_DATA("CV0003", "Error when retrieving data."),
        ERROR_MSG_READING_CONFIGURATION("CV0004", "Error when reading configuration."),
        ERROR_MSG_SENDING_NOTIFICATION("CV0005", "Error when sending verification notification."),
        ERROR_MSG_RETRIEVING_USER_DATA("CV0006", "Error when retrieving user data."),
        ERROR_MSG_RETRIEVING_CLAIM_DATA("CV0007", "Error when retrieving claim data."),
        ERROR_MSG_NO_MATCHING_USER_FOUND("CV0008", "No matching user found for received user details."),
        ERROR_MSG_MULTIPLE_MATCHING_USERS_FOUND("CV3009", "Multiple users found for received user details."),
        ERROR_MSG_NO_MATCHING_CLAIM_FOUND("CV0010", "No matching claim found for received claim uri."),
        ERROR_MSG_NO_MATCHING_CLAIM_VERIFIER_FOUND("CV0011", "No matching claim verifier found."),
        ERROR_MSG_REQUIRED_PROPERTY_NOT_FOUND("CV0012", "Required property not found."),
        ERROR_MSG_UPDATING_DATA("CV0013", "Error when updating data."),
        ERROR_MSG_INVALID_CONFIRMATION_CODE("CV0014", "Invalid confirmation code."),
        ERROR_MSG_EXPIRED_CONFIRMATION_CODE("CV0015", "Expired confirmation code."),
        ERROR_MSG_CLEARING_CLAIM_DATA("CV0016", "Error when clearing claim data."),
        ERROR_MSG_ADD_CLAIM_TO_USER_STORE("CV0017", "Error when adding claim to userstore."),
        ERROR_MSG_UNEXPECTED_ERROR("CV0018", "Unexpected Error."),
        ERROR_MSG_NO_MATCHING_VALIDATION_STATUS_FOUND("CV0019", "No matching validation status found.");

        private final String code;
        private final String message;

        ErrorMessages(String code, String message) {

            this.code = code;
            this.message = message;
        }

        /**
         * Get error code for enum.
         *
         * @return Error code.
         */
        public String getCode() {

            return code;
        }

        /**
         * Get error message for enum.
         *
         * @return Error message.
         */
        public String getMessage() {

            return message;
        }

        @Override
        public String toString() {

            return code + " - " + message;
        }
    }

    /**
     * Steps for the confirmation code.
     */
    public enum Step {

        CLAIM_VALIDATION,
        CLAIM_CONFIRMATION
    }

    /**
     * Verification statuses for temporary claims.
     */
    public enum ClaimVerificationStatus {

        INITIATED,
        PENDING,
        SUCCESSFUL,
        FAILED
    }

    /**
     * SQL queries for the JDBC claim verification store.
     */
    public static class SQLQueries {

        public static final String STORE_CODE_DATA = "INSERT INTO IDN_RECOVERY_DATA " +
                "(USER_NAME, USER_DOMAIN, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED) VALUES (?,?,?,?,?,?,?)";

        public static final String STORE_CLAIM_DATA = "INSERT INTO IDN_PENDING_CLAIM_DATA " +
                "(USER_NAME, USER_DOMAIN, TENANT_ID, CLAIM_ID, CLAIM_VALUE,VERIFICATION_STATUS) VALUES (?,?,?,?,?,?)";

        public static final String UPDATE_CLAIM_VERIFICATION_STATUS = "UPDATE IDN_PENDING_CLAIM_DATA " +
                "SET VERIFICATION_STATUS= ? WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ? " +
                "AND CLAIM_ID = ?";

        public static final String UPDATE_CLAIM_VERIFICATION_STATUS_CASE_INSENSITIVE = "UPDATE IDN_PENDING_CLAIM_DATA" +
                " SET VERIFICATION_STATUS = ? WHERE LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ?" +
                " AND CLAIM_ID = ?";

        public static final String UPDATE_CLAIM_DATA = "UPDATE IDN_PENDING_CLAIM_DATA SET CLAIM_VALUE = ? , " +
                "VERIFICATION_STATUS= ? WHERE USER_NAME=? AND USER_DOMAIN = ? AND TENANT_ID = ? AND CLAIM_ID = ?";

        public static final String UPDATE_CLAIM_DATA_CASE_INSENSITIVE = "UPDATE IDN_PENDING_CLAIM_DATA " +
                "SET CLAIM_VALUE = ? , VERIFICATION_STATUS = ? WHERE LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? " +
                "AND TENANT_ID = ? AND CLAIM_ID = ?";

        public static final String LOAD_CODE_DATA_FROM_CODE = "SELECT * FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_CODE_USING_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID = ? AND SCENARIO = ? AND STEP = ?";

        public static final String INVALIDATE_CODE_CASE_INSENSITIVE = "DELETE FROM IDN_RECOVERY_DATA WHERE " +
                "LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? AND SCENARIO = ? AND STEP = ?";

        public static final String LOAD_CLAIM_DATA = "SELECT * FROM IDN_PENDING_CLAIM_DATA WHERE USER_NAME = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID = ? AND CLAIM_ID = ?";

        public static final String LOAD_CLAIM_DATA_CASE_INSENSITIVE = "SELECT * FROM IDN_PENDING_CLAIM_DATA WHERE " +
                "LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? AND CLAIM_ID = ?";

        public static final String GET_CLAIM_ID = "SELECT ID FROM IDN_CLAIM WHERE AND CLAIM_URI = ? AND TENANT_ID = ?";

        public static final String GET_CLAIM_URI = "SELECT CLAIM_URI FROM IDN_CLAIM WHERE AND ID = ? AND TENANT_ID = ?";

        public static final String DELETE_CLAIM_DATA = "DELETE FROM IDN_PENDING_CLAIM_DATA WHERE USER_NAME = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID = ? AND CLAIM_ID = ?";

        public static final String DELETE_CLAIM_DATA_CASE_INSENSITIVE = "DELETE FROM IDN_PENDING_CLAIM_DATA WHERE " +
                "LOWER(USER_NAME) = LOWER(?) AND USER_DOMAIN = ? AND TENANT_ID = ? AND CLAIM_ID = ?";
    }

    /**
     * Constants related to EmailClaimVerifier connector config
     */
    public static class ConnectorConfig {

        public static final String VALIDATION_STEP_CODE_EXPIRY_TIME =
                "EmailClaimVerification.Validation.Step.Code.ExpiryTime";
        public static final String CONFIRMATION_STEP_EXPIRY_TIME =
                "EmailClaimVerification.Confirmation.Step.Code.ExpiryTime";
    }

    /**
     * Confirmation code types.
     */
    public static class CodeType {

        public static final String VALIDATION = "CODE_TYPE_VALIDATION";
        public static final String CONFIRMATION = "CODE_TYPE_CONFIRMATION";
    }
}
