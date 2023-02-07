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

package org.wso2.carbon.identity.tenant.resource.manager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfiguration;
import org.wso2.carbon.event.stream.core.EventStreamConfiguration;
import org.wso2.carbon.event.stream.core.exception.EventStreamConfigurationException;
import org.wso2.carbon.identity.configuration.mgt.core.constant.ConfigurationConstants;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;
import org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils;
import org.wso2.carbon.utils.AbstractAxis2ConfigurationContextObserver;

import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_ADDING_EVENT_PUBLISHER_CONFIGURATION;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_TENANT_SPECIFIC_PUBLISHER_FILES;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.PUBLISHER;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.populateMessageWithData;

/**
 * Axis2Observer for generating tenant wise publisher configurations.
 */
public class TenantAwareAxis2ConfigurationContextObserver extends AbstractAxis2ConfigurationContextObserver {

    private static final Log log = LogFactory.getLog(TenantAwareAxis2ConfigurationContextObserver.class);

    /**
     * Add the tenant wise publisher and stream Configuration in tenant loading.
     *
     * @param tenantId tenant ID.
     */
    @Override
    public void creatingConfigurationContext(int tenantId) {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        log.info("Loading configuration context for tenant domain: " + tenantDomain);
        loadEventStreamAndPublisherConfigurations(tenantId);
    }

    /**
     * This method loads event stream and publisher configurations for a specific tenant.
     *
     * @param tenantId     tenant id.
     */
    private void loadEventStreamAndPublisherConfigurations(int tenantId) {

        List<EventPublisherConfiguration> activeEventPublisherConfigurations;
        List<EventStreamConfiguration> eventStreamConfigurationList;
        try {
            ResourceUtils.startSuperTenantFlow();
            activeEventPublisherConfigurations = ResourceUtils.getSuperTenantEventPublisherConfigurations();
            eventStreamConfigurationList = getSuperTenantEventStreamConfigurations();
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
        try {
            ResourceUtils.startTenantFlow(tenantId);
            loadTenantEventStreams(eventStreamConfigurationList);
            loadTenantPublisherConfigurationFromConfigStore();

            if (activeEventPublisherConfigurations != null) {
                ResourceUtils.loadTenantPublisherConfigurationFromSuperTenantConfig(activeEventPublisherConfigurations);
            }
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    /**
     * This method loads publisher configurations tenant wise by fetching them from configuration store.
     */
    private void loadTenantPublisherConfigurationFromConfigStore() {

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

    /**
     * This method returns super tenant event stream configurations.
     *
     * @return list of event stream configurations.
     */
    private List<EventStreamConfiguration> getSuperTenantEventStreamConfigurations() {

        List<EventStreamConfiguration> eventStreamConfigurationList = null;
        try {
            eventStreamConfigurationList = TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                    .getAllEventStreamConfigurations();
        } catch (EventStreamConfigurationException e) {
            log.error(populateMessageWithData(
                    TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_STREAM_CONFIGURATION,
                    PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
        }
        return eventStreamConfigurationList;
    }

    /**
     * This method loads event stream configurations tenant wise by using super tenant publisher configurations.
     *
     * @param eventStreamConfigurationList list of active super tenant stream configurations.
     */
    private void loadTenantEventStreams(List<EventStreamConfiguration> eventStreamConfigurationList) {

        if (eventStreamConfigurationList != null) {
            for (EventStreamConfiguration eventStreamConfiguration : eventStreamConfigurationList) {
                if (TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                        .getEventStreamConfiguration(eventStreamConfiguration.getStreamDefinition().getStreamId())
                        == null) {
                    try {
                        TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                                .addEventStreamConfig(eventStreamConfiguration);
                    } catch (EventStreamConfigurationException e) {
                        log.error(populateMessageWithData(
                                ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION,
                                PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain()), e);
                    }
                }
            }
        }
    }
}

