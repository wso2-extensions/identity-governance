/*
 * Copyright (c) 2023-2024, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.password.expiry.internal;

import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.password.expiry.EnforcePasswordResetAuthenticationHandler;
import org.wso2.carbon.identity.password.expiry.PasswordChangeHandler;
import org.wso2.carbon.identity.password.expiry.PasswordExpiryConfigImpl;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.PostAuthenticationHandler;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.password.expiry.listener.PasswordExpiryEventListener;
import org.wso2.carbon.identity.password.expiry.services.ExpiredPasswordIdentificationService;
import org.wso2.carbon.identity.password.expiry.services.impl.ExpiredPasswordIdentificationServiceImpl;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.identity.role.v2.mgt.core.RoleManagementService;

/**
 * OSGi declarative services component which handles registration and un-registration of password enforce reset handler
 * service.
 */
@Component(
        name = "enforce.password.reset.component",
        immediate = true
)
public class EnforcePasswordResetComponent {

    private static final Log log = LogFactory.getLog(EnforcePasswordResetComponentDataHolder.class);

    @Activate
    protected void activate(ComponentContext context) {

        try {
            // Register the listener to capture user operations.
            PasswordExpiryEventListener listener = new PasswordExpiryEventListener();
            context.getBundleContext().registerService(UserOperationEventListener.class, listener, null);

            EnforcePasswordResetAuthenticationHandler enforcePasswordResetAuthenticationHandler =
                    new EnforcePasswordResetAuthenticationHandler();
            BundleContext bundleContext = context.getBundleContext();
            PasswordChangeHandler passwordChangeHandler = new PasswordChangeHandler();

            // Register the listener to capture password change events.
            bundleContext.registerService(AbstractEventHandler.class.getName(), passwordChangeHandler, null);
            // Register the connector config to render the resident identity provider configurations
            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    new PasswordExpiryConfigImpl(), null);
            context.getBundleContext().registerService(PostAuthenticationHandler.class.getName(),
                    enforcePasswordResetAuthenticationHandler, null);
            bundleContext.registerService(ExpiredPasswordIdentificationService.class.getName(),
                    new ExpiredPasswordIdentificationServiceImpl(), null);

        } catch (Throwable e) {
            log.error("Error while activating EnforcePasswordResetAuthenticationHandler.", e);
        }
    }

    @Reference(
            name = "user.realmservice.default",
            service = RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService"
    )
    protected void setRealmService(RealmService realmService) {

        EnforcePasswordResetComponentDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        EnforcePasswordResetComponentDataHolder.getInstance().setRealmService(null);
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = org.wso2.carbon.identity.governance.IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService")
    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {

        EnforcePasswordResetComponentDataHolder.getInstance().setIdentityGovernanceService(idpManager);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {

        EnforcePasswordResetComponentDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    @Reference(
            name = "identity.governance.service",
            service = IdentityDataStoreService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityDataStoreService"
    )
    protected void setIdentityDataStoreService(IdentityDataStoreService identityDataStoreService) {

        EnforcePasswordResetComponentDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);
    }

    protected void unsetIdentityDataStoreService(IdentityDataStoreService identityDataStoreService) {

        EnforcePasswordResetComponentDataHolder.getInstance().setIdentityDataStoreService(null);
    }

    @Reference(
            name = "role.management.service",
            service = RoleManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRoleManagementService"
    )
    protected void setRoleManagementService(RoleManagementService roleManagementService) {

        EnforcePasswordResetComponentDataHolder.getInstance().setRoleManagementService(roleManagementService);
    }

    protected void unsetRoleManagementService(RoleManagementService roleManagementService) {
    
        EnforcePasswordResetComponentDataHolder.getInstance().setRoleManagementService(null);
    }
}
