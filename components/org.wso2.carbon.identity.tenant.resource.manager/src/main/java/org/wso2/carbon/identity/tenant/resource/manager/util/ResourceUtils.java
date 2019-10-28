/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.tenant.resource.manager.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resources;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.core.ResourceManager;
import org.wso2.carbon.identity.tenant.resource.manager.core.ResourceManagerImpl;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementClientException;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementServerException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility methods for tenant resource management.
 */
public class ResourceUtils {

    private static final Log log = LogFactory.getLog(ResourceUtils.class);

    /**
     * This method can be used to generate a TenantResourceManagementClientException from
     * ConfigurationConstants.ErrorMessages object when no exception is thrown.
     *
     * @param error TenantResourceConstants.ErrorMessages.
     * @param data  data to replace if message needs to be replaced.
     * @return TenantResourceManagementClientException.
     */
    public static TenantResourceManagementClientException handleClientException(TenantResourceConstants.ErrorMessages error,
            String ...data) {

        String message = populateMessageWithData(error, data);
        return new TenantResourceManagementClientException(message, error.getCode());
    }

    public static TenantResourceManagementClientException handleClientException(TenantResourceConstants.ErrorMessages error,
            Throwable e, String ...data) {

        String message = populateMessageWithData(error, data);
        return new TenantResourceManagementClientException(message, error.getCode(), e);
    }

    /**
     * This method can be used to generate a TenantResourceManagementServerException from
     * TenantResourceConstants.ErrorMessages object when no exception is thrown.
     *
     * @param error TenantResourceConstants.ErrorMessages.
     * @param data  data to replace if message needs to be replaced.
     * @return TenantResourceManagementServerException.
     */
    public static TenantResourceManagementServerException handleServerException(TenantResourceConstants.ErrorMessages error,
            String ...data) {

        String message = populateMessageWithData(error, data);
        return new TenantResourceManagementServerException(message, error.getCode());
    }

    public static TenantResourceManagementServerException handleServerException(TenantResourceConstants.ErrorMessages error,
            Throwable e, String ...data) {
        String message;
            message = populateMessageWithData(error, data);

        return new TenantResourceManagementServerException(message, error.getCode(), e);
    }

    /**
     * This method can be used to get publisher configuration file from the configuration store.
     *
     * @param eventPublisherName Event Publisher Name.
     * @return event publisher file
     * @throws ConfigurationManagementException
     */
    public static ResourceFile getResourceFile(String eventPublisherName) throws ConfigurationManagementException {

        List<ResourceFile> fileList = new ArrayList<>();

        Resources resources = TenantResourceManagerDataHolder.getInstance().getConfigurationManager()
                .getResourcesByType(TenantResourceConstants.PUBLISHER);
        for (Resource resource : resources.getResources()) {
            if (eventPublisherName.equals(resource.getResourceName())) {
                fileList = TenantResourceManagerDataHolder.getInstance().getConfigurationManager()
                        .getFiles(TenantResourceConstants.PUBLISHER, eventPublisherName);
                break;
            }
        }

        if (fileList.size() > 1) {
            if (log.isDebugEnabled()) {
                log.debug("More then one file with the name: " + eventPublisherName);
            }
            return null;
        } else if(fileList.isEmpty()){
            if (log.isDebugEnabled()) {
                log.debug("No file with the name: " + eventPublisherName);
            }
            return null;
        } else {
            return fileList.get(0);
        }
    }

    private static String populateMessageWithData(TenantResourceConstants.ErrorMessages error, String[] data) {

        String message;
        if (data.length != 0) {
            message = String.format(error.getMessage(), data);
        } else {
            message = error.getMessage();
        }
        return message;
    }

    /**
     * This method can be used to get Resource Manager Impl object.
     *
     * @return ResourceManagerImpl Object.
     */
    public static ResourceManager getResourceManager(){
         return new ResourceManagerImpl();
    }

    /**
     * This method can be used to get whether configuration management rest API is enabled.
     *
     * @return boolean.
     */
    public static boolean isConfigurationManagementRestAPIEnabled() {

        return TenantResourceManagerDataHolder.getInstance().getConfigurationManager().isFeatureEnabled();
    }
}
