/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.governance.exceptions.notiification;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;

/**
 * Exception class for server errors while managing notification templates.
 */
public class NotificationTemplateManagerServerException extends NotificationTemplateManagerException {

    /**
     * Constructs a new exception with the specified detail message.
     *
     * @param message The detail message
     */
    public NotificationTemplateManagerServerException(String message) {

        super(message);
        this.setErrorCode(getDefaultErrorCode());
    }

    /**
     * Constructs a new exception with the specified detail message and cause.
     *
     * @param message The detail message
     * @param cause   The cause
     */
    public NotificationTemplateManagerServerException(String message, Throwable cause) {

        super(message, cause);
        this.setErrorCode(getDefaultErrorCode());
    }

    /**
     * Constructs a new exception with an error code and a detail message.
     *
     * @param errorCode The error code
     * @param message   The detail message.
     */
    public NotificationTemplateManagerServerException(String errorCode, String message) {

        super(errorCode, message);
        this.setErrorCode(errorCode);
    }

    /**
     * Constructs a new exception with an error code, detail message and throwable.
     *
     * @param errorCode The error code
     * @param message   The detail message
     * @param throwable Throwable
     */
    public NotificationTemplateManagerServerException(String errorCode, String message, Throwable throwable) {

        super(errorCode, message, throwable);
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
            errorCode = IdentityMgtConstants.Error_Scenario.NOTIFICATION_TEMPLATE_MANAGER +
                    IdentityMgtConstants.ERROR_CODE_DELIMITER
                    + IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getCode();
        }
        return errorCode;
    }
}
