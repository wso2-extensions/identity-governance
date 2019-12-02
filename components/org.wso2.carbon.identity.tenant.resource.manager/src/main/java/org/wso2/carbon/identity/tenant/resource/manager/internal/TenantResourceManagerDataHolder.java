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

import org.wso2.carbon.event.publisher.core.EventPublisherService;
import org.wso2.carbon.event.stream.core.EventStreamService;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.tenant.resource.manager.core.ResourceManager;

/**
 * DataHolder for Tenant Resource Manager.
 */
public class TenantResourceManagerDataHolder {

    private static volatile TenantResourceManagerDataHolder instance = new TenantResourceManagerDataHolder();
    private ConfigurationManager configurationManager;
    private EventPublisherService carbonEventPublisherService = null;
    private EventStreamService carbonEventStreamService = null;
    private ResourceManager resourceManager = null;

    private TenantResourceManagerDataHolder() {
    }

    public static TenantResourceManagerDataHolder getInstance() {

        return instance;
    }

    public EventPublisherService getCarbonEventPublisherService() {

        return carbonEventPublisherService;
    }

    public void setCarbonEventPublisherService(EventPublisherService carbonEventPublisherService) {

        this.carbonEventPublisherService = carbonEventPublisherService;
    }

    public void setCarbonEventStreamService(EventStreamService carbonEventStreamService) {

        this.carbonEventStreamService = carbonEventStreamService;
    }

    public EventStreamService getCarbonEventStreamService() {

        return carbonEventStreamService;
    }

    public void setConfigurationManager(ConfigurationManager configurationManager) {

        this.configurationManager = configurationManager;
    }

    public ConfigurationManager getConfigurationManager() {

        return configurationManager;
    }

    public ResourceManager getResourceManager() {

        return resourceManager;
    }

    public void setResourceManager(ResourceManager resourceManager) {

        this.resourceManager = resourceManager;
    }
}
