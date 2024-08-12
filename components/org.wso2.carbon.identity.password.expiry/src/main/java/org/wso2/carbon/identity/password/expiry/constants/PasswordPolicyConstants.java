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

package org.wso2.carbon.identity.password.expiry.constants;

/**
 * Password Change authenticator's constants
 */
public class PasswordPolicyConstants {

    public static final String AUTHENTICATOR_TYPE = "LOCAL";
    public static final String LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM =
            "http://wso2.org/claims/identity/lastPasswordUpdateTime";
    public static final String GROUPS_CLAIM = "http://wso2.org/claims/groups";
    public static final String LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY =
            "http://wso2.org/claims/lastPasswordChangedTimestamp";
    public static final String PASSWORD_RESET_PAGE = "/accountrecoveryendpoint/password-recovery-confirm.jsp";
    public static final String PASSWORD_CHANGE_EVENT_HANDLER_NAME = "enforcePasswordResetEventHandler";
    public static final String ENFORCE_PASSWORD_RESET_HANDLER = "EnforcePasswordResetHandler";
    public static final String CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS = "passwordExpiry.passwordExpiryInDays";
    public static final String CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DISPLAYED_NAME = "Password Expiry In Days";
    public static final String CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DESCRIPTION =
            "Number of days after which the password will expire";
    public static final int CONNECTOR_CONFIG_PASSWORD_EXPIRY_IN_DAYS_DEFAULT_VALUE = 30;

    public static final String CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY = "passwordExpiry.enablePasswordExpiry";
    public static final String CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DISPLAYED_NAME =
            "Enable Password Expiry";
    public static final String CONNECTOR_CONFIG_ENABLE_PASSWORD_EXPIRY_DESCRIPTION =
            "Allow users to reset the password after configured number of days";
    public static final String CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES =
            "passwordExpiry.skipIfNoApplicableRules";
    public static final String CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES_DISPLAYED_NAME =
            "Skip password expiry if no applicable rules";
    public static final String CONNECTOR_CONFIG_SKIP_IF_NO_APPLICABLE_RULES_DESCRIPTION =
            "Skip password expiry if no applicable rules are found for the user";
    public static final String CONNECTOR_CONFIG_SUB_CATEGORY = "DEFAULT";
    public static final String PASSWORD_EXPIRED_ERROR_MESSAGE = "Password has expired";
    public static final String CONNECTOR_CONFIG_NAME = "passwordExpiry";
    public static final String CONNECTOR_CONFIG_FRIENDLY_NAME = "Password Expiry";
    public static final String CONNECTOR_CONFIG_CATEGORY = "Password Policies";
    public static final String PASSWORD_GRANT_POST_AUTHENTICATION_EVENT = "PASSWORD_GRANT_POST_AUTHENTICATION";
    public static final String AUTHENTICATION_STATUS = "authenticationStatus";
    public static final String BASIC_AUTHENTICATOR = "BasicAuthenticator";
    public static final String FALSE = "false";
    public static final String CONFIRMATION_QUERY_PARAM = "&confirmation=";
    public static final String PASSWORD_EXPIRED_QUERY_PARAMS = "&passwordExpired=true";
    public static final String PASSWORD_EXPIRY_RULES_PREFIX = "passwordExpiry.rule";

    public enum ErrorMessages {
        ERROR_WHILE_GETTING_USER_STORE_DOMAIN("80001",
                "Error occurred while getting the user store domain."),
        ERROR_WHILE_GETTING_USER_REALM("80002", "Error occurred while getting the user realm."),
        ERROR_WHILE_GETTING_CLAIM_MAPPINGS("80003", "Error while getting claim mappings for user, %s"),
        ERROR_WHILE_READING_SYSTEM_CONFIGURATIONS("80004", "Error while reading the configurations."),
        ERROR_WHILE_GETTING_USERID_FOR_USERNAME("80005", "Error while getting user ID for the user, %s."),
        ERROR_WHILE_BUILDING_PASSWORD_RESET_PAGE_URL("80006", "Error building password reset page URL"),
        ERROR_WHILE_REDIRECTING_TO_PASSWORD_RESET_PAGE("80007",
                "Error while redirecting to password reset page"),
        ERROR_WHILE_GENERATING_CONFIRMATION_CODE("80008", "Error while generating the confirmation code"),
        ERROR_PASSWORD_EXPIRED("80009", "Password has expired"),
        ERROR_WHILE_PASSWORD_EXPIRY_VALIDATION("80010", "Error while validating password expiry"),
        ERROR_WHILE_UPDATING_PASSWORD("80011", "Error while updating the password"),
        ERROR_RETRIEVE_PASSWORD_EXPIRED_USERS_FROM_DB("80012", "" +
                "Error while retrieving password expired users from database."),
        ERROR_RETRIEVE_USER_STORE_MANAGER("80013", "Error while retrieving user store manager."),
        ERROR_WHILE_RETRIEVING_USER_ROLES("80014", "Error while retrieving user roles."),
        ERROR_WHILE_RETRIEVING_USER_CLAIMS("80015", "Error while retrieving user claims."),
        ERROR_WHILE_RETRIEVING_USER_GROUPS("80016", "Error while retrieving user groups.");

        private final String code;
        private final String message;

        /**
         * Create an Error Message.
         *
         * @param code    Relevant error code.
         * @param message Relevant error message.
         */
        ErrorMessages(String code, String message) {

            this.code = code;
            this.message = message;
        }

        /**
         * To get the code of specific error.
         *
         * @return Error code.
         */
        public String getCode() {

            return code;
        }

        /**
         * To get the message of specific error.
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
}
