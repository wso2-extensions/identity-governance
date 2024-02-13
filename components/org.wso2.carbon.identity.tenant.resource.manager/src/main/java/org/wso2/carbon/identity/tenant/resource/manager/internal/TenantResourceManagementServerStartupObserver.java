/*
 * Copyright (c) 2024, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.tenant.resource.manager.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.core.ServerStartupObserver;
import org.wso2.carbon.identity.tenant.resource.manager.util.ResourceUtils;

/**
 * Server startup observer for tenant resource management component.
 */
public class TenantResourceManagementServerStartupObserver implements ServerStartupObserver {

    private static final Log LOG = LogFactory.getLog(TenantResourceManagementServerStartupObserver.class);

    @Override
    public void completingServerStartup() {

        // Do nothing.
    }

    @Override
    public void completedServerStartup() {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Attempting to load super tenant event publisher configurations from the configuration store.");
        }

        /*
            Load the super tenant publisher configuration from the config store. This is required if there are
            super tenant event publishers configured in the config store (ex: Custom SMS Provider).

            This way of loading configurations is not optimal as it is called after the server startup
            completion. These configurations should be loaded before the server startup completion event. Before we can
            add the publishers the event streams need to be loaded to the server, current event streams are file based,
            and it's loading is triggered using a server startup observer which runs on completingServerStartup
            method (org.wso2.carbon.core.internal.DeploymentServerStartupObserver). There is currently no way to make
            sure the calling order, therefore this is the only way to guarantee the publishers are loaded after the
            event streams are loaded.
        */
        ResourceUtils.loadTenantPublisherConfigurationFromConfigStore();
    }
}
