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
 * Exception class that represents exceptions thrown upon username update.
 */
public class UsernameUpdateException extends Exception {

    private static final long serialVersionUID = -7082390511162807710L;
    private String errorCode;

    /**
     * Constructs a new exception with the specified message.
     *
     * @param message the detailed message
     */
    public UsernameUpdateException(String message) {

        super(message);
    }

    /**
     * Constructs a new exception with the specified message and cause.
     *
     * @param message the detailed message
     * @param cause the cause
     */
    public UsernameUpdateException(String message, Throwable cause) {

        super(message, cause);
    }

    /**
     * Constructs a new exception with the specified message and the error code.
     *
     * @param message the detailed message
     * @param errorCode error code
     */
    public UsernameUpdateException(String message, String errorCode) {

        super(message);
        this.errorCode = errorCode;
    }

    /**
     * Constructs a new exception with the specified message, cause and the error code.
     *
     * @param message the detailed message
     * @param cause the cause
     * @param errorCode error code
     */
    public UsernameUpdateException(String message, Throwable cause, String errorCode) {

        super(message, cause);
        this.errorCode = errorCode;
    }

    /**
     * Returns the error code specified at the construct of the exception.
     *
     * @return error code specified
     */
    public String getErrorCode() {

        return errorCode;
    }
}
