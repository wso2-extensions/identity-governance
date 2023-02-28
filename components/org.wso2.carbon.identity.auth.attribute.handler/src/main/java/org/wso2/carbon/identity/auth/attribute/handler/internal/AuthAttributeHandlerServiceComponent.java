/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
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

package org.wso2.carbon.identity.auth.attribute.handler.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandler;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerManager;

/**
 * This class contains the OSGI components of the Auth Attribute Handler.
 */
@Component(
        name = "org.wso2.carbon.identity.auth.attribute.handler.internal.AuthAttributeHandlerServiceComponent",
        immediate = true)
public class AuthAttributeHandlerServiceComponent {

    private static final Log log = LogFactory.getLog(AuthAttributeHandlerServiceComponent.class);

    @Activate
    protected void activate(ComponentContext context) {

        try {
            BundleContext bundleContext = context.getBundleContext();
            bundleContext.registerService(AuthAttributeHandlerManager.class.getName(),
                    AuthAttributeHandlerManager.getInstance(), null);
        } catch (Exception e) {
            log.error("Error while activating auth attribute handler service component.", e);
        }
    }

    @Reference(
            name = "AuthAttributeHandler",
            service = AuthAttributeHandler.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetAuthAttributeHandlers")
    protected void setAuthAttributeHandlers(AuthAttributeHandler authAttributeHandler) {

        AuthAttributeHandlerServiceDataHolder.getInstance().getAuthAttributeHandlers().add(authAttributeHandler);
    }

    protected void unsetAuthAttributeHandlers(AuthAttributeHandler authAttributeHandler) {

        AuthAttributeHandlerServiceDataHolder.getInstance().getAuthAttributeHandlers().remove(authAttributeHandler);
    }

    @Reference(
            name = "ApplicationManagementService",
            service = ApplicationManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetApplicationManagementService")
    protected void setApplicationManagementService(ApplicationManagementService applicationManagementService) {

        AuthAttributeHandlerServiceDataHolder.getInstance()
                .setApplicationManagementService(applicationManagementService);
    }

    protected void unsetApplicationManagementService(ApplicationManagementService applicationManagementService) {

        AuthAttributeHandlerServiceDataHolder.getInstance().setApplicationManagementService(null);
    }
}
