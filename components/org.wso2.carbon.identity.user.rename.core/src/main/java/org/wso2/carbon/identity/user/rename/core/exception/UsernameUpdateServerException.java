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
 * Exception class that represents exceptions thrown upon username update while executing the service.
 */
public class UsernameUpdateServerException extends UsernameUpdateException {

    /**
     * Constructs a new exception with the specified message.
     *
     * @param message the detailed message
     */
    public UsernameUpdateServerException(String message) {

        super(message);
    }

    /**
     * Constructs a new exception with the specified message and cause.
     *
     * @param message the detailed message
     * @param cause the cause
     */
    public UsernameUpdateServerException(String message, Throwable cause) {

        super(message, cause);
    }

    /**
     * Constructs a new exception with the specified message and error code.
     *
     * @param message the detailed message
     * @param errorCode the error code
     */
    public UsernameUpdateServerException(String message, String errorCode) {

        super(message, errorCode);
    }

    /**
     * Constructs a new exception with the specified message, cause and error code.
     *
     * @param message the detailed message
     * @param cause the cause
     * @param errorCode the error code
     */
    public UsernameUpdateServerException(String message, Throwable cause, String errorCode) {

        super(message, cause, errorCode);
    }
}
