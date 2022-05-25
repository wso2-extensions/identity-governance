/*
 * Copyright (c) 2022, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.governance.exceptions.otp;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;

public class OTPGeneratorClientException extends OTPGeneratorException {

    /**
     * Constructs a new exception with the specified detail message.
     *
     * @param message The detail message
     */
    public OTPGeneratorClientException(String message) {
        super(message);
        this.setErrorCode(getDefaultErrorCode());
    }

    /**
     * Constructs a new exception with the specified detail error code and message.
     *
     * @param errorCode The error code
     * @param message   The message
     */
    public OTPGeneratorClientException(String errorCode, String message) {
        super(errorCode, message);
        this.setErrorCode(errorCode);
    }

    /**
     * Constructs a new exception with the specified detail message and cause.
     *
     * @param message The detail message
     * @param cause   The cause
     */
    public OTPGeneratorClientException(String message, Throwable cause) {
        super(message, cause);
        this.setErrorCode(getDefaultErrorCode());
    }

    /**
     * Constructs a new exception with the specified error code, detail message and cause.
     *
     * @param errorCode The error code
     * @param message   The detail message
     * @param cause     The cause
     */
    public OTPGeneratorClientException(String errorCode, String message, Throwable cause) {
        super(errorCode, message, cause);
        this.setErrorCode(errorCode);
    }

    /**
     * Get the default error code of the exception.
     *
     * @return Error description
     */
    private String getDefaultErrorCode() {

        String errorCode = super.getErrorCode();
        if (StringUtils.isEmpty(errorCode)) {
            errorCode = IdentityMgtConstants.Error_Scenario.OTP_GENERATOR +
                    IdentityMgtConstants.ERROR_CODE_DELIMITER
                    + IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_UNEXPECTED_ERROR.getCode();
        }
        return errorCode;
    }
}
