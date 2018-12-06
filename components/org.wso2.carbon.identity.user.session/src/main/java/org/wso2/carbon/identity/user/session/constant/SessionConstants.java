/*
 *   Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *   WSO2 Inc. licenses this file to you under the Apache License,
 *   Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

package org.wso2.carbon.identity.user.session.constant;

/**
 * Constants related to session management.
 */
public class SessionConstants {

    public static final String APPLICATION_JSON = "application/json";
    public static final String DEFAULT_RESPONSE_CONTENT_TYPE = APPLICATION_JSON;
    public static final String HEADER_CONTENT_TYPE = "Content-Type";
    public static final String STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT = "Internal server error";
    public static final String STATUS_BAD_REQUEST_MESSAGE_DEFAULT = "Bad Request";

    public enum ErrorMessages {
        ERROR_CODE_TERMINATE_SESSION("SM_00001", "Error occurred while terminating session."),
        ERROR_CODE_GET_USER_SESSION("SM_00002", "Error occurred while retrieving user session."),
        ERROR_CODE_GET_SESSION_IDs_OF_USER_IDs("SM_00002", "Error occurred while retrieving session ids" +
                " of user ids: %s."),
        ERROR_CODE_GET_SESSION_OF_USER_ID_LIST("SM_00004", "Error occurred while retrieving sessions " +
                "of user id list: %s"),
        ERROR_CODE_GET_SESSION_ID_OF_USER_ID("SM_00005", "Error occurred while retrieving session id of " +
                "user id: %s."),
        ERROR_CODE_GET_SESSION_CONTEXT("SM_00006", "Error occurred while retrieving session context cache."),
        ERROR_CODE_GET_SESSION_INFO("SM_00007", "Error occurred while retrieving session information."),
        ERROR_CODE_GET_USER_ID("SM_00008", "Error while retrieving user Id of the user: userName: %s, Tenant Id:" +
                " %s User domain: %s"),
        ERROR_CODE_GET_USER_ID_LIST("SM_00009", "Error while retrieving user Id list in sessionId: %s"),
        ERROR_CODE_UNEXPECTED("SM_00010", "Unexpected Error"),
        ERROR_CODE_SESSION_ID_INVALID("SM_00011", "Invalid session Id: %s"),
        ERROR_CODE_SESSION_ALREADY_TERMINATED("SM_00012", "Session already terminated"),
        ERROR_CODE_USER_NOT_AUTHORIZED("SM_00013", "User: %s is not authorized to perform this operation.");

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

            return code + " : " + message;
        }
    }
}
