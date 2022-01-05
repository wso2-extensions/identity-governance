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

package org.wso2.carbon.identity.tenant.resource.manager.core;

import org.apache.commons.lang.NotImplementedException;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;

/**
 * Tenant Resource manager service interface.
 */
public interface ResourceManager {

    /**
     * This API is used to add an EventPublisher for a particular tenant.
     *
     * @param resourceFile Event Publisher file.
     * @throws TenantResourceManagementException
     */
    void addEventPublisherConfiguration(ResourceFile resourceFile) throws TenantResourceManagementException;

    /**
     * This API is used to remove an EventPublisher from a particular tenant.
     *
     * @param resourceTypeName Resource type name
     * @param resourceName Resource name
     * @throws TenantResourceManagementException
     */
    default void removeEventPublisherConfiguration(String resourceTypeName, String resourceName)
            throws TenantResourceManagementException {

        throw new NotImplementedException("Method is not implemented.");
    }
}
