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

import org.apache.axis2.context.ConfigurationContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.event.publisher.core.config.EventPublisherConfiguration;
import org.wso2.carbon.event.stream.core.EventStreamConfiguration;
import org.wso2.carbon.event.stream.core.exception.EventStreamConfigurationException;
import org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants;
import org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerDataHolder;
import org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils;
import org.wso2.carbon.utils.AbstractAxis2ConfigurationContextObserver;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.List;

import static org.wso2.carbon.identity.tenant.resource.manager.constants.TenantResourceConstants.ErrorMessages.ERROR_CODE_ERROR_WHEN_CREATING_TENANT_EVENT_STREAM_CONFIGURATION;
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
     * Remove the in-memory event publisher and stream configurations of the tenant when its
     * configuration context is unloaded (tenant idle eviction or shutdown).
     * The publisher and stream runtimes are re-created by {@link #creatingConfigurationContext(int)}
     * the next time the tenant is loaded, so removing them here keeps the in-memory publisher state
     * proportional to the set of currently loaded tenants instead of every tenant ever loaded.
     *
     * @param configContext Configuration context of the tenant being unloaded.
     */
    @Override
    public void terminatingConfigurationContext(ConfigurationContext configContext) {

        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        /*
         * Both tenant unload paths (idle eviction in TenantAxisUtils#cleanupTenants and tenant
         * deactivation in TenantMgtUtil#unloadTenantConfigurations) invoke the terminating observers
         * within the unloading tenant's carbon context flow. Guard regardless: with an unresolved
         * tenant we cannot tell whose state to remove, and the super tenant's publishers are
         * filesystem-deployed at startup and would not be re-created lazily.
         */
        if (tenantId == MultitenantConstants.INVALID_TENANT_ID
                || tenantId == MultitenantConstants.SUPER_TENANT_ID) {
            if (log.isDebugEnabled()) {
                log.debug("Skipping event publisher and stream cleanup for tenant id: " + tenantId);
            }
            return;
        }
        log.info("Unloading event publisher and stream configurations for tenant domain: " + tenantDomain);
        try {
            TenantResourceManagerDataHolder.getInstance().getCarbonEventPublisherService()
                    .removeEventPublisherConfigurations(tenantId);
            TenantResourceManagerDataHolder.getInstance().getCarbonEventStreamService()
                    .removeEventStreamConfigurations(tenantId);
        } catch (Exception e) {
            // Tenant unloading must never fail due to publisher cleanup.
            log.error("Error while removing event publisher and stream configurations for tenant domain: "
                    + tenantDomain, e);
        }
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
            ResourceUtils.loadTenantPublisherConfigurationFromConfigStore();

            if (activeEventPublisherConfigurations != null) {
                ResourceUtils.loadTenantPublisherConfigurationFromSuperTenantConfig(activeEventPublisherConfigurations);
            }
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
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

