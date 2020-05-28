/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */

package org.wso2.carbon.identity.password.history.constants;

public class PasswordHistoryConstants {

    public static final String PW_HISTORY_ENABLE = "passwordHistory.enable";
    public static final String PW_HISTORY_COUNT = "passwordHistory.count";
    public static final String PW_HISTORY_HASHING_ALGORITHM = "passwordHistory.hashingAlgorithm";
    public static final String PW_HISTORY_DATA_STORE = "passwordHistory.dataStore";


    public static class SQLQueries {

        public static final String STORE_HISTORY_DATA = "INSERT INTO  IDN_PASSWORD_HISTORY_DATA "
                + "(USER_NAME, USER_DOMAIN, TENANT_ID, SALT_VALUE, HASH, TIME_CREATED)"
                + "VALUES (?,?,?,?,?,?)";
        public static final String LOAD_HISTORY_DATA = "SELECT * FROM IDN_PASSWORD_HISTORY_DATA WHERE USER_NAME= ? AND " +
                "USER_DOMAIN = ? AND TENANT_ID = ? ORDER BY TIME_CREATED DESC  ";

        public static final String DELETE_HISTORY_RECORD = "DELETE FROM IDN_PASSWORD_HISTORY_DATA WHERE ID=?";

        public static final String DELETE_USER_HISTORY = "DELETE FROM IDN_PASSWORD_HISTORY_DATA WHERE USER_NAME = ? " +
                "AND USER_DOMAIN =? AND TENANT_ID =?";

        public static final String DELETE_PASSWORD_HISTORY_DATA_BY_TENANT_ID = "DELETE FROM " +
                "IDN_PASSWORD_HISTORY_DATA WHERE TENANT_ID = ?";

    }

    public enum ErrorMessages {

        ERROR_CODE_HISTORY_VIOLATE("22001", "This password has been used in recent history. Please choose a different" +
                " password"),
        ERROR_CODE_LOADING_HISTORY_DATA_SOURCE("22002", "Error while loading history data source"),
        ERROR_CODE_VALIDATING_HISTORY("22003", "Error while validating password history"),
        ERROR_CODE_STORING_HISTORY("22004", "Error while storing password history"),
        ERROR_CODE_DELETE_HISTORY("22005", "Error while removing password history from: '%s.'"),
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
}
