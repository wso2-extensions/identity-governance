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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.event.publisher.core.EventPublisherService;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfiguration;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfigurationFile;
import org.wso2.carbon.event.publisher.core.exception.EventPublisherConfigurationException;
import org.wso2.carbon.event.publisher.core.exception.EventPublisherStreamValidationException;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;
import org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils;

import java.io.InputStream;
import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.PUBLISHER;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.handleServerException;

/**
 * Tenant Resource manager service implementation.
 */
public class ResourceManagerImpl implements ResourceManager {

    private static Log log = LogFactory.getLog(ResourceManagerImpl.class);

    @Override
    public void addEventPublisherConfiguration(ResourceFile resourceFile) throws TenantResourceManagementException {

        try {
            deployEventPublisherConfiguration(TenantResourceManagerDataHolder.getInstance().getConfigurationManager()
                    .getFileById(PUBLISHER, resourceFile.getName(), resourceFile.getId()));
            if (log.isDebugEnabled()) {
                log.debug("Event Publisher: " + resourceFile.getName() + " deployed from the configuration "
                        + "store for the tenant domain: " + PrivilegedCarbonContext.getThreadLocalCarbonContext()
                        .getTenantDomain());
            }
        } catch (EventPublisherConfigurationException e) {
            throw handleServerException(ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION, e,
                    resourceFile.getName());
        } catch (ConfigurationManagementException e) {
            throw handleServerException(ERROR_CODE_ERROR_WHEN_FETCHING_EVENT_PUBLISHER_FILE, e,
                    resourceFile.getName() ,PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain());
        }
    }

    @Override
    public void removeEventPublisherConfiguration(String resourceTypeName, String resourceName)
            throws TenantResourceManagementException {

        try {
            if (TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                    .getActiveEventPublisherConfiguration(resourceName) != null) {
                destroyEventPublisherConfiguration(resourceName);

                // Since the tenant event publisher was removed, we should load super tenant configs.
                loadSuperTenantEventPublisherConfigs();
            }
        } catch (EventPublisherConfigurationException e) {
            throw handleServerException(ERROR_CODE_ERROR_WHEN_DEPLOYING_EVENT_PUBLISHER_CONFIGURATION, e,
                    resourceName);
        }
    }

    private void loadSuperTenantEventPublisherConfigs() {

        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        List<EventPublisherConfiguration> activeEventPublisherConfigurations;
        try {
            ResourceUtils.startSuperTenantFlow();
            activeEventPublisherConfigurations = ResourceUtils.getSuperTenantEventPublisherConfigurations();
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
        try {
            ResourceUtils.startTenantFlow(tenantId);
            if (activeEventPublisherConfigurations != null) {
                ResourceUtils.loadTenantPublisherConfigurationFromSuperTenantConfig(activeEventPublisherConfigurations);
            }
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    /**
     * This is used to deploy an event publisher configuration using.
     *
     * @param publisherConfig Event Publisher Configuration as input stream.
     * @throws EventPublisherConfigurationException Event Publisher Configuration Exception.
     */
    private void deployEventPublisherConfiguration(InputStream publisherConfig)
            throws EventPublisherConfigurationException {

        EventPublisherService carbonEventPublisherService = TenantResourceManagerDataHolder.getInstance()
                .getCarbonEventPublisherService();
        EventPublisherConfiguration eventPublisherConfiguration;
        try {
            eventPublisherConfiguration = carbonEventPublisherService.getEventPublisherConfiguration(publisherConfig);
        } catch (EventPublisherStreamValidationException e) {
            if (log.isDebugEnabled()) {
                log.debug("The event publisher configuration not available.", e);
            }
            return;
        }

        if (TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .getActiveEventPublisherConfiguration(eventPublisherConfiguration.getEventPublisherName()) != null) {
            destroyEventPublisherConfiguration(eventPublisherConfiguration.getEventPublisherName());
        }
        carbonEventPublisherService.addEventPublisherConfiguration(eventPublisherConfiguration);
    }

    /**
     * This is used to destroy an existing EventPublisher.
     * As per the implementation in analytics-common we need to add the publisher as a file before destroying it.
     *
     * @param eventPublisherName Event Publisher Name.
     * @throws EventPublisherConfigurationException Configuration Management Exception.
     */
    private void destroyEventPublisherConfiguration(String eventPublisherName)
            throws EventPublisherConfigurationException {

        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        EventPublisherConfigurationFile eventPublisherConfigurationFile = new EventPublisherConfigurationFile();
        eventPublisherConfigurationFile.setTenantId(tenantId);
        eventPublisherConfigurationFile.setEventPublisherName(eventPublisherName);
        eventPublisherConfigurationFile.setFileName(eventPublisherName);
        eventPublisherConfigurationFile.setStatus(EventPublisherConfigurationFile.Status.DEPLOYED);

        TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .addEventPublisherConfigurationFile(eventPublisherConfigurationFile, tenantId);
        TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                .removeEventPublisherConfigurationFile(eventPublisherName, tenantId);
    }
}
