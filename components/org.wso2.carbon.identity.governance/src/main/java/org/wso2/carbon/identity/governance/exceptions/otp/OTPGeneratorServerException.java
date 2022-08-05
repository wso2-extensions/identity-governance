/*
 * Copyright (c) 2022, WSO2 LLC (http://www.wso2.org).
 *
 * WSO2 LLC licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
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

package org.wso2.carbon.identity.governance.exceptions.otp;

/**
 * Exception class that represents exceptions thrown upon server errors while generating OTP for users.
 */
public class OTPGeneratorServerException extends OTPGeneratorException {

    /**
     * Constructs a new exception with the specified detail error code and message.
     *
     * @param errorCode The error code.
     * @param message   The message.
     */
    public OTPGeneratorServerException(String errorCode, String message) {

        super(errorCode, message);
    }

    /**
     * Constructs a new exception with the specified error code, detail message and cause.
     *
     * @param errorCode The error code.
     * @param message   The detail message.
     * @param cause     The cause.
     */
    public OTPGeneratorServerException(String errorCode, String message, Throwable cause) {

        super(errorCode, message, cause);
    }
}
