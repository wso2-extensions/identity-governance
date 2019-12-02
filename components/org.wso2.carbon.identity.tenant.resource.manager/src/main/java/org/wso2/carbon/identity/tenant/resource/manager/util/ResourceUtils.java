/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.tenant.resource.manager.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementServerException;

/**
 * Utility methods for tenant resource management.
 */
public class ResourceUtils {

    private static final Log log = LogFactory.getLog(ResourceUtils.class);

    /**
     * This method can be used to generate a TenantResourceManagementServerException from
     * TenantResourceConstants.ErrorMessages object when no exception is thrown.
     *
     * @param error TenantResourceConstants.ErrorMessages.
     * @param data  data to replace if message needs to be replaced.
     * @return TenantResourceManagementServerException.
     */
    public static TenantResourceManagementServerException handleServerException(
            TenantResourceConstants.ErrorMessages error, String... data) {

        String message = populateMessageWithData(error, data);
        return new TenantResourceManagementServerException(message, error.getCode());
    }

    public static TenantResourceManagementServerException handleServerException(
            TenantResourceConstants.ErrorMessages error, Throwable e, String... data) {

        String message = populateMessageWithData(error, data);
        return new TenantResourceManagementServerException(message, error.getCode(), e);
    }

    public static String populateMessageWithData(TenantResourceConstants.ErrorMessages error, String... data) {

        String message;
        if (data.length != 0) {
            message = String.format(error.getMessage(), data);
        } else {
            message = error.getMessage();
        }
        return message;
    }
}
