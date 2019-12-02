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

package org.wso2.carbon.identity.password.history.Util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.base.ServerConfiguration;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.password.history.constants.PasswordHistoryConstants;
import org.wso2.carbon.utils.CarbonUtils;

public class Utils {
    private static final Log log = LogFactory.getLog(Utils.class);

    public static IdentityEventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                                            error, String data) throws IdentityEventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityEventException.class, error.getCode(), errorDescription);
    }

    public static IdentityEventException handleEventException(PasswordHistoryConstants.ErrorMessages
                                            error, String data, Throwable throwable) throws IdentityEventException {
        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }
        return IdentityException.error(IdentityEventException.class, error.getCode(), errorDescription, throwable);
    }

    /**
     * This method is used to check whether the password trim is enabled.
     *
     * @return true if the value of <EnablePasswordTrim> is true in the carbon.xml or <EnablePasswordTrim> is not present
     * in the carbon.xml
     */
    public static boolean isPasswordTrimEnabled() {

        boolean isPasswordTrimEnabled = true;
        ServerConfiguration serverConfiguration = CarbonUtils.getServerConfiguration();
        if (serverConfiguration != null && StringUtils.isNotEmpty(serverConfiguration.getFirstProperty
                (CarbonConstants.IS_PASSWORD_TRIM_ENABLED))) {
            isPasswordTrimEnabled = Boolean.parseBoolean(serverConfiguration.getFirstProperty
                    (CarbonConstants.IS_PASSWORD_TRIM_ENABLED));
        }
        return isPasswordTrimEnabled;
    }
}
