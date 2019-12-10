/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.user.rename.core.exception;

/**
 * Exception class that represents exceptions thrown upon username update due to client request errors.
 */
public class UsernameUpdateClientException extends UsernameUpdateException {

    private ErrorType errorType;

    /**
     * Constructs a new exception with the specified message.
     *
     * @param message the detailed message
     */
    public UsernameUpdateClientException(String message) {

        super(message);
    }

    /**
     * Constructs a new exception with the specified message and the error type {@link ErrorType}.
     *
     * @param message the detailed message
     * @param errorType the error type {@link ErrorType}
     */
    public UsernameUpdateClientException(String message, ErrorType errorType) {

        super(message);
        this.errorType = errorType;
    }

    /**
     * Constructs a new exception with the specified message and the cause.
     *
     * @param message the detailed message
     * @param cause cause
     */
    public UsernameUpdateClientException(String message, Throwable cause) {

        super(message, cause);
    }

    /**
     * Constructs a new exception with the specified message, cause and the error type {@link ErrorType}.
     *
     * @param message the detailed message
     * @param cause cause
     * @param errorType the error type {@link ErrorType}
     */
    public UsernameUpdateClientException(String message, Throwable cause, ErrorType errorType) {

        super(message, cause);
        this.errorType = errorType;
    }

    /**
     * Constructs a new exception with the specified message and the error code.
     *
     * @param message the detailed message
     * @param errorCode the error code
     */
    public UsernameUpdateClientException(String message, String errorCode) {

        super(message, errorCode);
    }

    /**
     * Constructs a new exception with the specified message, error code and the error type {@link ErrorType}.
     *
     * @param message the detailed message
     * @param errorCode the error code
     * @param errorType error type {@link ErrorType}
     */
    public UsernameUpdateClientException(String message, String errorCode, ErrorType errorType) {

        super(message, errorCode);
        this.errorType = errorType;
    }

    /**
     * Constructs a new exception with the specified message, cause and the error code.
     *
     * @param message the detailed message
     * @param cause the cause
     * @param errorCode error code {@link ErrorType}
     */
    public UsernameUpdateClientException(String message, Throwable cause, String errorCode) {

        super(message, cause, errorCode);
    }

    /**
     * Constructs a new exception with the specified message, cause, error code and the error type.
     *
     * @param message the detailed message
     * @param cause the cause
     * @param errorCode error code
     * @param errorType error type
     */
    public UsernameUpdateClientException(String message, Throwable cause, String errorCode, ErrorType errorType) {

        super(message, cause, errorCode);
        this.errorType = errorType;
    }

    /**
     * Returns the specified error type
     *
     * @return error type {@link ErrorType}
     */
    public ErrorType getErrorType() {

        return errorType;
    }

    public enum ErrorType {
        BAD_REQUEST, NOT_ACCEPTABLE, NOT_FOUND, CONFLICT, UNAUTHORIZED;
    }

}
