/*
 *   Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *   WSO2 Inc. licenses this file to you under the Apache License,
 *   Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

package org.wso2.carbon.identity.user.session.util;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.user.session.constant.SessionConstants;
import org.wso2.carbon.identity.user.session.exception.SessionManagementClientException;
import org.wso2.carbon.identity.user.session.exception.SessionManagementServerException;

public class SessionMgtUtils {

    public static SessionManagementServerException handleServerException(SessionConstants.ErrorMessages error,
                                                                         String data, Throwable e) {
        String message;
        if (StringUtils.isNotBlank(data)) {
            message = String.format(error.getMessage(), data);
        } else {
            message = error.getMessage();
        }
        return new SessionManagementServerException(message, error.getCode(), e);
    }

    /**
     * This method can be used to generate a TemplateManagementClientException from TemplateMgtConstants.ErrorMessages
     * object when no exception is thrown.
     *
     * @param error templateMgtConstants.ErrorMessages.
     * @param data  data to replace if message needs to be replaced.
     * @return TemplateManagementClientException.
     */
    public static SessionManagementClientException handleClientException(SessionConstants.ErrorMessages error,
                                                                         String data) {
        String message;
        if (StringUtils.isNotBlank(data)) {
            message = String.format(error.getMessage(), data);
        } else {
            message = error.getMessage();
        }
        return new SessionManagementClientException(message, error.getCode());
    }
}
