/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.policy.constants;

public class PasswordPolicyConstants {

    public static final String PW_POLICY_ENABLE = "passwordPolicy.enable";
    public static final String PW_POLICY_MIN_LENGTH = "passwordPolicy.min.length";
    public static final String PW_POLICY_MAX_LENGTH = "passwordPolicy.max.length";
    public static final String PW_POLICY_PATTERN = "passwordPolicy.pattern";
    public static final String PW_POLICY_ERROR_MSG = "passwordPolicy.errorMsg";
    public static final String PW_POLICY_LENGTH_CLASS = "passwordPolicy.class.PasswordLengthPolicy";
    public static final String PW_POLICY_NAME_CLASS = "passwordPolicy.class.PasswordNamePolicy";
    public static final String PW_POLICY_PATTERN_CLASS = "passwordPolicy.class.PasswordPatternPolicy";
    public static final String PW_POLICY_HANDLER_ENABLED = "PasswordPolicy.PasswordPolicyValidationHandler.Enable";
    public static final String PW_POLICY_USERNAME_CHECK_MODE = "passwordPolicy.username.check.mode";

    public enum ErrorMessages {

        ERROR_CODE_LOADING_PASSWORD_POLICY_CLASSES("40001", "Error occurred while loading Password Policies"),
        ERROR_CODE_VALIDATING_PASSWORD_POLICY("40002", "Error while validating password policy");


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
