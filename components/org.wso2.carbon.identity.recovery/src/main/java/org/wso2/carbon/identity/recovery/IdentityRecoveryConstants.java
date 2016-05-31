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

    private IdentityRecoveryConstants(){}

    public class ErrorCode {

        private ErrorCode(){}

        public static final String ERROR_CODE_INVALID_CODE = "18001";
        public static final String ERROR_CODE_EXPIRED_CODE = "18002";
        public static final String ERROR_CODE_INVALID_USER = "18003";
        public static final String ERROR_CODE_INVALID_CAPTCHA = "18004";
        public static final String ERROR_CODE_UNEXPECTED = "18013";
        public static final String ERROR_CODE_LOADING_DATA_FAILURE = "18014";
        public static final String ERROR_CODE_RECOVERY_NOTIFICATION_FAILURE = "18015";
        public static final String ERROR_CODE_INVALID_TENANT = "18016";
        public static final String ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND = "18016";
        public static final String ERROR_CODE_INVALID_CREDENTIALS = "17002";
        public static final String ERROR_CODE_LOCKED_ACCOUNT = "17003";
        public static final String ERROR_CODE_DISABLED_ACCOUNT = "17004";
        public static final String ERROR_CODE_REGISTRY_EXCEPTION_GET_CHALLENGE_QUESTIONS = "20001";
        public static final String ERROR_CODE_REGISTRY_EXCEPTION_SET_CHALLENGE_QUESTIONS = "20002";
        public static final String ERROR_CODE_GETTING_CHALLENGE_URIS = "20003";
        public static final String ERROR_CODE_GETTING_CHALLENGE_QUESTIONS= "20004";
        public static final String ERROR_CODE_GETTING_CHALLENGE_QUESTION= "20005";
        public static final String ERROR_CODE_QUESTION_OF_USER= "20006";
        public static final String ERROR_CODE_NO_HASHING_ALGO= "20007";
        public static final String ERROR_CODE_STORING_RECOVERY_DATA= "20008";
        public static final String ERROR_CODE_INVALID_ANSWER_FOR_SECURITY_QUESTION= "20009";
        public static final String ERROR_CODE_NEED_TO_ANSWER_MORE_SECURITY_QUESTION= "20010";


    }

    public static class SQLQueries {

        public static final String STORE_RECOVERY_DATA = "INSERT INTO IDN_RECOVERY_DATA "
                + "(USER_NAME, REALM, TENANT_ID, CODE, SCENARIO,STEP, TIME_CREATED, META_DATA)"
                + "VALUES (?,?,?,?,?,?,?,?)";

        public static final String LOAD_RECOVERY_DATA = "SELECT "
                + "* FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND REALM = ? AND TENANT_ID = ? AND CODE = ? AND " +
                "SCENARIO = ? AND STEP = ?";

        public static final String INVALIDATE_CODE = "DELETE FROM IDN_RECOVERY_DATA WHERE CODE = ?";

        public static final String INVALIDATE_USER_CODES = "DELETE FROM IDN_RECOVERY_DATA WHERE USER_NAME = ? AND " +
                "REALM = ? AND TENANT_ID =?";

    }
}