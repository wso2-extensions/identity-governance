/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
 * limitations und
 */

package org.wso2.carbon.identity.password.policy.exeption;

import org.wso2.carbon.identity.base.IdentityException;

/**
 * Used for creating checked exceptions that can be handled.
 */
public class IdentityPasswordPolicyException extends IdentityException {

    private static final long serialVersionUID = 1700318648018222420L;

    public IdentityPasswordPolicyException(String errorDescription) {
        super(errorDescription);
    }

    public IdentityPasswordPolicyException(String errorDescription, Throwable cause) {
        super(errorDescription, cause);
    }
}
