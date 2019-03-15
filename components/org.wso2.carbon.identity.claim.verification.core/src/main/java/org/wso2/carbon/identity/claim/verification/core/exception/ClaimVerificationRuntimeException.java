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

package org.wso2.carbon.identity.claim.verification.core.exception;

/**
 * Runtime exception for claim verification.
 */
public class ClaimVerificationRuntimeException extends RuntimeException {

    private String errorCode;

    public ClaimVerificationRuntimeException() {

        super();
    }

    /**
     * Constructs a new exception with the cause.
     *
     * @param cause The cause.
     */
    public ClaimVerificationRuntimeException(Throwable cause) {

        super(cause);
    }

    /**
     * Constructs a new exception with the specified message.
     *
     * @param message The detailed message.
     */
    public ClaimVerificationRuntimeException(String message) {

        super(message);
    }

    /**
     * Constructs a new exception with the specified message and cause.
     *
     * @param message The detailed message.
     * @param cause   The cause.
     */
    public ClaimVerificationRuntimeException(String message, Throwable cause) {

        super(message, cause);
    }

    /**
     * Constructs a new exception with the error code and the specified message.
     *
     * @param errorCode Error code.
     * @param message   The detailed message.
     */
    public ClaimVerificationRuntimeException(String errorCode, String message) {

        super(message);
        this.errorCode = errorCode;
    }

    /**
     * Constructs a new exception with the error code, the specified message, and cause.
     *
     * @param message   The detailed message.
     * @param cause     The cause.
     * @param errorCode Error code.
     */
    public ClaimVerificationRuntimeException(String errorCode, String message, Throwable cause) {

        super(message, cause);
        this.errorCode = errorCode;
    }

    /**
     * Returns the error code specified at the construct of the exception.
     *
     * @return Error code specified.
     */
    public String getErrorCode() {

        return errorCode;
    }
}
