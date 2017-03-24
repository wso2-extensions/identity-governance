/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.password.policy.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.policy.constants.PasswordPolicyConstants;

public class Utils {

    private static final Log log = LogFactory.getLog(Utils.class);

    public static IdentityEventException handleEventException(PasswordPolicyConstants.ErrorMessages error,
                                                 String errorText, Throwable throwable) throws IdentityEventException {

        if (StringUtils.isBlank(errorText)) {
            errorText = error.getMessage();
        }

        return IdentityException.error(IdentityEventException.class, error.getCode(), errorText, throwable);
    }
}
