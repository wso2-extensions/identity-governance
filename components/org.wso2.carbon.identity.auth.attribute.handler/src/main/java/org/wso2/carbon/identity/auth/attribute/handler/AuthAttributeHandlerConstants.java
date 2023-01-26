/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.auth.attribute.handler;

/**
 * Class to maintain constants related to the Auth Attribute Handler component.
 */
public class AuthAttributeHandlerConstants {

    private AuthAttributeHandlerConstants(){}

    private static final String AUTH_ATTRIBUTE_HANDLER_ERROR_CODE_PREFIX = "AAH";

    /**
     * Enum containing the error codes and error messages related
     * to the Auth Attribute Handler component.
     */
    public enum ErrorMessages {

        ERROR_CODE_UNEXPECTED_ERROR("65001", "Server encountered an unexpected error."),
        ERROR_CODE_SERVICE_PROVIDER_NOT_FOUND("60001", "Service provider not found in tenant: %s for identifier: %s"),
        ERROR_CODE_ATTRIBUTE_NOT_FOUND("60002", "Required attribute not found."),
        ERROR_CODE_ATTRIBUTE_VALUE_EMPTY("60003", "Provided attribute value is empty.");

        private final String code;
        private final String message;

        ErrorMessages(String code, String message) {

            this.code = AUTH_ATTRIBUTE_HANDLER_ERROR_CODE_PREFIX + "-" + code;
            this.message = message;
        }

        public String getCode() {

            return code;
        }

        public String getMessage() {

            return message;
        }
    }
}
