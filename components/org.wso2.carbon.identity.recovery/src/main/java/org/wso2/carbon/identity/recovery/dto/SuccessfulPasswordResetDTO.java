/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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
package org.wso2.carbon.identity.recovery.dto;

/**
 * Object that encapsulates response after a successful password reset.
 */
public class SuccessfulPasswordResetDTO {

    /**
     * Successful password update status code.
     */
    private String successCode;

    /**
     * Success password update status message.
     */
    private String message;

    /**
     * Get the successful status code.
     *
     * @return Status code
     */
    public String getSuccessCode() {

        return successCode;
    }

    /**
     * Set the successful status code.
     *
     * @param successCode Successful status code
     */
    public void setSuccessCode(String successCode) {

        this.successCode = successCode;
    }

    /**
     * Get the successful status message.
     *
     * @return Status message
     */
    public String getMessage() {

        return message;
    }

    /**
     * Set the successful status message.
     *
     * @param message Status message
     */
    public void setMessage(String message) {

        this.message = message;
    }
}
