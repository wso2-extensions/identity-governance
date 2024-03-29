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
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfiguration;
import org.wso2.carbon.event.publisher.core.exception.EventPublisherConfigurationException;
import org.wso2.carbon.identity.configuration.mgt.core.constant.ConfigurationConstants;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementServerException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_ADDING_EVENT_PUBLISHER_CONFIGURATION;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_TENANT_SPECIFIC_PUBLISHER_FILES;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.PUBLISHER;

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

    /**
     * This method creates event publisher configurations tenant wise by using super tenant publisher configurations.
     *
     * @param activeEventPublisherConfigurations list of active super tenant publisher configurations.
     */
    public static void loadTenantPublisherConfigurationFromSuperTenantConfig(
            List<EventPublisherConfiguration> activeEventPublisherConfigurations) {

        for (EventPublisherConfiguration eventPublisherConfiguration : activeEventPublisherConfigurations) {
            try {
                if (TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                        .getActiveEventPublisherConfiguration(eventPublisherConfiguration.getEventPublisherName())
                        == null) {
                    if (log.isDebugEnabled()) {
                        log.debug("Super tenant event publisher configuration for the: " + eventPublisherConfiguration
                                .getEventPublisherName() + " will be used for the tenant domain: "
                                + PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain());
                    }
                    TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                            .addEventPublisherConfiguration(eventPublisherConfiguration);
                }
            } catch (EventPublisherConfigurationException e) {
                log.error(populateMessageWithData(
                        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG,
                        eventPublisherConfiguration.getEventPublisherName(),
                        PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
            }
        }
    }

    /**
     * This method returns super tenant event publisher configurations.
     *
     * @return list of event publisher configurations.
     */
    public static List<EventPublisherConfiguration> getSuperTenantEventPublisherConfigurations() {

        List<EventPublisherConfiguration> activeEventPublisherConfigurations = null;
        try {
            activeEventPublisherConfigurations = TenantResourceManagerDataHolder.getInstance()
                    .getCarbonEventPublisherService().getAllActiveEventPublisherConfigurations();
        } catch (EventPublisherConfigurationException e) {
            log.error(populateMessageWithData(
                    TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_PUBLISHER_CONFIGURATION,
                    PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
        }
        return activeEventPublisherConfigurations;
    }

    public static void startTenantFlow(int tenantId) {

        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(tenantId);
        PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .setTenantDomain(IdentityTenantUtil.getTenantDomain(tenantId));
    }

    public static void startSuperTenantFlow() {

        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
        carbonContext.setTenantId(MultitenantConstants.SUPER_TENANT_ID);
        carbonContext.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
    }

    /**
     * This method loads publisher configurations tenant wise by fetching them from configuration store.
     */
    public static void loadTenantPublisherConfigurationFromConfigStore() {

        try {
            List<Resource> resourcesByTypePublisher = TenantResourceManagerDataHolder.getInstance()
                    .getConfigurationManager().getResourcesByType(PUBLISHER).getResources();
            for (Resource resource : resourcesByTypePublisher) {
                if (!resource.getFiles().isEmpty()) {
                    ResourceFile tenantSpecificPublisherFile = resource.getFiles().get(0);
                    if (tenantSpecificPublisherFile != null) {
                        if (log.isDebugEnabled()) {
                            log.debug("File for publisher name: " + tenantSpecificPublisherFile.getName()
                                    + " is available in the configuration store.");
                        }
                        TenantResourceManagerDataHolder.getInstance().getResourceManager()
                                .addEventPublisherConfiguration(tenantSpecificPublisherFile);
                    }
                }
            }
        } catch (ConfigurationManagementException e) {
            if (e.getErrorCode()
                    .equals(ConfigurationConstants.ErrorMessages.ERROR_CODE_FEATURE_NOT_ENABLED.getCode())) {
                log.warn("Configuration store is disabled. Super tenant configuration will be used for the tenant "
                        + "domain: " + PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain());
            } else if (e.getErrorCode()
                    .equals(ConfigurationConstants.ErrorMessages.ERROR_CODE_RESOURCES_DOES_NOT_EXISTS.getCode())) {
                log.warn("Configuration store does not contain any resources under resource type publisher. Super "
                        + "tenant configurations will be used for the tenant domain: " + PrivilegedCarbonContext
                        .getThreadLocalCarbonContext().getTenantDomain());
            } else if (e.getErrorCode()
                    .equals(ConfigurationConstants.ErrorMessages.ERROR_CODE_RESOURCE_TYPE_DOES_NOT_EXISTS.getCode())) {
                log.warn("Configuration store does not contain  publisher resource type. Super "
                        + "tenant configurations will be used for the tenant domain: " + PrivilegedCarbonContext
                        .getThreadLocalCarbonContext().getTenantDomain());
            } else {
                log.error(populateMessageWithData(ERROR_CODE_ERROR_WHEN_FETCHING_TENANT_SPECIFIC_PUBLISHER_FILES,
                        PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
            }
        } catch (TenantResourceManagementException e) {
            log.error(populateMessageWithData(ERROR_CODE_ERROR_WHEN_ADDING_EVENT_PUBLISHER_CONFIGURATION,
                    PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
        }
    }
}
