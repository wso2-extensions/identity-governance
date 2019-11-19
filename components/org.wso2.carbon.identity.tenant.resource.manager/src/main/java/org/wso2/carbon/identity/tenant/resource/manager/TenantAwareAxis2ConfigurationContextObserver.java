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
import org.wso2.carbon.event.publisher.core.exception.EventPublisherConfigurationException;
import org.wso2.carbon.event.stream.core.EventStreamConfiguration;
import org.wso2.carbon.event.stream.core.exception.EventStreamConfigurationException;
import org.wso2.carbon.identity.configuration.mgt.core.constant.ConfigurationConstants;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.ResourceFile;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.exception.TenantResourceManagementException;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;
import org.wso2.carbon.utils.AbstractAxis2ConfigurationContextObserver;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_BY_CONFIG_STORE;
import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.getResourceFile;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.getResourceManager;
import static org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils.handleServerException;

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

        log.info("creating configuration context for tenant id: " + tenantId);
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        List<EventPublisherConfiguration> activeEventPublisherConfigurations = null;
        List<EventStreamConfiguration> eventStreamConfigurationList = null;

        startSuperTenantFlow();

        activeEventPublisherConfigurations = getSuperTenantEventPublisherConfigurations(tenantId);
        eventStreamConfigurationList = getSuperTenantEventStreamConfigurations(tenantId);

        startTenantFlow(tenantId, tenantDomain);

        createTenantEventStreamConfiguration(tenantId, eventStreamConfigurationList);

        if (activeEventPublisherConfigurations != null) {
            createTenantEventPublisherConfiguration(tenantId, activeEventPublisherConfigurations);
        }
    }

    private List<EventPublisherConfiguration> getSuperTenantEventPublisherConfigurations(int tenantId) {

        List<EventPublisherConfiguration> activeEventPublisherConfigurations = null;
        try {
            activeEventPublisherConfigurations = TenantResourceManagerDataHolder.getInstance()
                    .getEventPublisherService().getAllActiveEventPublisherConfigurations();
        } catch (EventPublisherConfigurationException e) {
            log.error(handleServerException(
                    TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_PUBLISHER_CONFIGURATION,
                    e, String.valueOf(tenantId)));
        }
        return activeEventPublisherConfigurations;
    }

    private List<EventStreamConfiguration> getSuperTenantEventStreamConfigurations(int tenantId) {

        List<EventStreamConfiguration> eventStreamConfigurationList = null;
        try {
            eventStreamConfigurationList = TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                    .getAllEventStreamConfigurations();
        } catch (EventStreamConfigurationException e) {
            log.error(handleServerException(
                    TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_FETCHING_SUPER_TENANT_EVENT_STREAM_CONFIGURATION,
                    e, String.valueOf(tenantId)));
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
        return eventStreamConfigurationList;
    }

    private void startTenantFlow(int tenantId, String tenantDomain) {

        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(tenantId);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(tenantDomain);
    }

    private void startSuperTenantFlow() {

        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
        carbonContext.setTenantId(MultitenantConstants.SUPER_TENANT_ID);
        carbonContext.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
    }

    private void createTenantEventPublisherConfiguration(int tenantId,
            List<EventPublisherConfiguration> activeEventPublisherConfigurations) {

        for (EventPublisherConfiguration eventPublisherConfiguration : activeEventPublisherConfigurations) {
            try {
                ResourceFile resourceFile = getResourceFile(eventPublisherConfiguration.getEventPublisherName());
                if (resourceFile != null) {
                    if (log.isDebugEnabled()) {
                        log.debug("Configuration store is enabled and there is a file in the configuration store for "
                                + "the publisher name: " + eventPublisherConfiguration.getEventPublisherName());
                    }
                    getResourceManager().addEventPublisherConfiguration(resourceFile);
                }
            } catch (ConfigurationManagementException e) {
                if (e.getErrorCode()
                        .equals(ConfigurationConstants.ErrorMessages.ERROR_CODE_FEATURE_NOT_ENABLED.getCode())) {
                    if (log.isDebugEnabled()) {
                        log.debug("Configuration store is disabled.");
                    }
                }
            } catch (TenantResourceManagementException e) {
                log.error(handleServerException(
                        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_BY_CONFIG_STORE, e,
                        eventPublisherConfiguration.getEventPublisherName(), String.valueOf(tenantId)));
            }
            try {
                if (TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                        .getActiveEventPublisherConfiguration(eventPublisherConfiguration.getEventPublisherName())
                        == null) {
                    if (log.isDebugEnabled()) {
                        log.debug("Super tenant event publisher configuration for the :" + eventPublisherConfiguration
                                .getEventPublisherName() + " will be used for the tenant with id: " + tenantId);
                    }
                    TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                            .addEventPublisherConfiguration(eventPublisherConfiguration);
                }
            } catch (EventPublisherConfigurationException e) {
                log.error(handleServerException(
                        ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_PUBLISHER_CONFIGURATION_USING_SUPER_TENANT_CONFIG,
                        e, eventPublisherConfiguration.getEventPublisherName(), String.valueOf(tenantId)));
            }
        }
    }

    private void createTenantEventStreamConfiguration(int tenantId,
            List<EventStreamConfiguration> eventStreamConfigurationList) {

        if (eventStreamConfigurationList != null) {
            for (EventStreamConfiguration eventStreamConfiguration : eventStreamConfigurationList) {
                if (TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                        .getEventStreamConfiguration(eventStreamConfiguration.getStreamDefinition().getStreamId())
                        == null) {
                    try {
                        TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                                .addEventStreamConfig(eventStreamConfiguration);
                    } catch (EventStreamConfigurationException e) {
                        log.error(handleServerException(
                                TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION,
                                e, eventStreamConfiguration.getStreamDefinition().getName(), String.valueOf(tenantId)));
                    }
                }
            }
        }
    }
}

