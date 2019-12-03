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

package org.wso2.carbon.identity.tenant.resource.manager.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.event.publisher.core.EventPublisherService;
import org.wso2.carbon.event.stream.core.EventStreamService;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.tenant.resource.manager.TenantAwareAxis2ConfigurationContextObserver;
import org.wso2.carbon.identity.tenant.resource.manager.core.ResourceManager;
import org.wso2.carbon.identity.tenant.resource.manager.core.ResourceManagerImpl;
import org.wso2.carbon.utils.AbstractAxis2ConfigurationContextObserver;
import org.wso2.carbon.utils.Axis2ConfigurationContextObserver;

@Component(name = "org.wso2.carbon.identity.tenant.resource.manager.internal.TenantResourceManagerServiceDS",
           immediate = true)
public class TenantResourceManagerServiceDS extends AbstractAxis2ConfigurationContextObserver {

    private static final Log log = LogFactory.getLog(TenantResourceManagerServiceDS.class);

    /**
     * Register Tenant Aware Axis2 Configuration Context Observer as an OSGI service.
     *
     * @param context OSGI service component context.
     */
    @Activate
    protected void activate(ComponentContext context) {

        try {
            TenantAwareAxis2ConfigurationContextObserver tenantAwareAxis2ConfigurationContextObserver =
                    new TenantAwareAxis2ConfigurationContextObserver();

            context.getBundleContext().registerService(Axis2ConfigurationContextObserver.class.getName(),
                    tenantAwareAxis2ConfigurationContextObserver, null);
            ResourceManager resourceManager = new ResourceManagerImpl();
            context.getBundleContext()
                    .registerService(ResourceManager.class.getName(), resourceManager, null);
            TenantResourceManagerDataHolder.getInstance().setResourceManager(resourceManager);
            if (log.isDebugEnabled()) {
                log.debug("Successfully deployed the tenant resource manager service.");
            }
        } catch (Exception e) {
            log.error("Can not create the tenant resource manager service.", e);
        }
    }

    @Deactivate
    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Tenant resource manager bundle is de-activated");
        }
    }

    @Reference(name = "CarbonEventPublisherService",
               service = org.wso2.carbon.event.publisher.core.EventPublisherService.class,
               cardinality = ReferenceCardinality.MANDATORY,
               policy = ReferencePolicy.DYNAMIC,
               unbind = "unsetCarbonEventPublisherService")
    protected void setCarbonEventPublisherService(EventPublisherService carbonEventPublisherService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the CarbonEventPublisherService");
        }
        TenantResourceManagerDataHolder.getInstance().setCarbonEventPublisherService(carbonEventPublisherService);
    }

    protected void unsetCarbonEventPublisherService(EventPublisherService carbonEventPublisherService) {

        if (log.isDebugEnabled()) {
            log.debug("Un Setting the CarbonEventPublisherService Service");
        }
        TenantResourceManagerDataHolder.getInstance().setCarbonEventPublisherService(null);
    }

    @Reference(name = "EventStreamService",
               service = org.wso2.carbon.event.stream.core.EventStreamService.class,
               cardinality = ReferenceCardinality.MANDATORY,
               policy = ReferencePolicy.DYNAMIC,
               unbind = "unsetCarbonEventStreamService")
    protected void setCarbonEventStreamService(EventStreamService carbonEventStreamService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the EventStreamService");
        }
        TenantResourceManagerDataHolder.getInstance().setCarbonEventStreamService(carbonEventStreamService);
    }

    protected void unsetCarbonEventStreamService(EventStreamService carbonEventStreamService) {

        if (log.isDebugEnabled()) {
            log.debug("Un Setting the EventStreamService");
        }
        TenantResourceManagerDataHolder.getInstance().setCarbonEventStreamService(null);
    }

    @Reference(name = "ConfigurationManager",
               service = org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager.class,
               cardinality = ReferenceCardinality.MANDATORY,
               policy = ReferencePolicy.DYNAMIC,
               unbind = "unsetConfigurationManager")
    protected void setConfigurationManager(ConfigurationManager configurationManager) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the CarbonEventPublisherService");
        }
        TenantResourceManagerDataHolder.getInstance().setConfigurationManager(configurationManager);
    }

    protected void unsetConfigurationManager(ConfigurationManager configurationManager) {

        if (log.isDebugEnabled()) {
            log.debug("Un Setting theCarbonEventPublisherService Service");
        }
        TenantResourceManagerDataHolder.getInstance().setConfigurationManager(null);
    }
}
