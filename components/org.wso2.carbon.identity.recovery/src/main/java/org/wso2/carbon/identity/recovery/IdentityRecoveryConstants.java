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


    public static final String IDENTITY_MANAGEMENT_PATH = "/repository/components/org.wso2.carbon.identity.mgt";
    public static final String IDENTITY_MANAGEMENT_QUESTIONS = IDENTITY_MANAGEMENT_PATH + "/questionCollection";
    public static final String LINE_SEPARATOR = "!";
    public static final String CHALLENGE_QUESTION_URI = "http://wso2.org/claims/challengeQuestionUris";

    private IdentityRecoveryConstants() {
    }

    public enum ErrorMessages {

        ERROR_CODE_INVALID_CODE("18001", "Invalid Code '%s.'"),
        ERROR_CODE_EXPIRED_CODE("18002", "Expired Code '%s.'"),
        ERROR_CODE_INVALID_USER("18003", "Invalid User '%s.'"),
        ERROR_CODE_UNEXPECTED("18013", "Expired Code '%s.'"),
        ERROR_CODE_RECOVERY_NOTIFICATION_FAILURE("18015", "Error sending recovery notification"),
        ERROR_CODE_INVALID_TENANT("18016", "Invalid tenant'%s.'"),
        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND("18017", "No challenge question found"),
        ERROR_CODE_INVALID_CREDENTIALS("17002", "Invalid Credentials"),
        ERROR_CODE_LOCKED_ACCOUNT("17003", "user account is locked '%s.'"),
        ERROR_CODE_DISABLED_ACCOUNT("17004", "user account is disabled '%s.'"),
        ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS("20001", "Registry exception while getting challenge question"),
        ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS("20002", "Registry exception while setting challenge question"),
        ERROR_CODE_GETTING_CHALLENGE_URIS("20003", "Error while getting challenge question URIs '%s.'"),
        ERROR_CODE_GETTING_CHALLENGE_QUESTIONS("20004", "Error while getting challenge questions '%s.'"),
        ERROR_CODE_GETTING_CHALLENGE_QUESTION("20005", "Error while getting challenge question '%s.'"),
        ERROR_CODE_QUESTION_OF_USER("20006", "Error setting challenge quesitons of user '%s.'"),
        ERROR_CODE_NO_HASHING_ALGO("20007", "Error while hashing the security answer"),
        ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION("20008", "Error while storing recovery data"),
        ERROR_CODE_STORING_RECOVERY_DATA("20009", "Invalid answer for security question"),
        ERROR_CODE_NEED_TO_ANSWER_MORE_SECURITY_QUESTION("20010", "Need to answer more security questions"),
        ERROR_CODE_TRIGGER_NOTIFICATION("20011", "Error while trigger notification for user '%s.'"),
        ;


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

    public static class SQLQueries {

        public static final String STORE_RECOVERY_DATA = "INSERT INTO IDN_RECOVERY_DATA "
                + "(USER_NAME, USER_DOMAIN, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED, REMAINING_SETS)"
                + "VALUES (?,?,?,?,?,?,?,?)";
        public static final String LOAD_RECOVERY_DATA = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND USER_DOMAIN = ? AND TENANT_ID = ? AND CODE = ? AND " +
                "SCENARIO = ? AND STEP = ?";

        public static final String INVALIDATE_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_USER_CODES = "DELETE FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID =?";

    }
}