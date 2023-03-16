/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.exception;

import org.wso2.carbon.identity.recovery.IdentityRecoveryException;

/**
 * This class will handle the exceptions related to the self registration.
 */
public class SelfRegistrationException extends IdentityRecoveryException {

    private static final long serialVersionUID = -6763234985286131368L;
    private String description;

    public SelfRegistrationException(String errorCode, String message, String description) {

        super(errorCode, message);
        this.description = description;
    }

    public SelfRegistrationException(String errorCode, String message, String description, Throwable cause) {

        super(errorCode, message, cause);
        this.description = description;
    }

    public void setDescription(String description) {

        this.description = description;
    }

    public String getDescription() {

        return description;
    }
}
