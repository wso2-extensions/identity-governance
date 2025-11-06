/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
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
package org.wso2.carbon.identity.governance.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.core.ConnectorConfig;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.IdentityGovernanceServiceImpl;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.internal.service.impl.notification.DefaultNotificationChannelManager;
import org.wso2.carbon.identity.governance.internal.service.impl.otp.OTPGeneratorImpl;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreServiceImpl;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.listener.IdentityMgtEventListener;
import org.wso2.carbon.identity.governance.listener.IdentityStoreEventListener;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;
import org.wso2.carbon.user.core.service.RealmService;

@Component(
        name = "org.wso2.carbon.identity.governance.internal.IdentityMgtServiceComponent",
        immediate = true)
public class IdentityMgtServiceComponent {

    private static final Log log = LogFactory.getLog(IdentityMgtServiceComponent.class);

    @Activate
    protected void activate(ComponentContext context) {

        try {
            IdentityMgtEventListener listener = new IdentityMgtEventListener();
            context.getBundleContext().registerService(UserOperationEventListener.class, listener, null);
            // IdentityDataStoreService should be registered before the IdentityStoreEventListener.
            IdentityDataStoreService identityDataStoreService = new IdentityDataStoreServiceImpl();
            context.getBundleContext()
                    .registerService(IdentityDataStoreService.class.getName(), identityDataStoreService, null);
            IdentityMgtServiceDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);
            context.getBundleContext().registerService(UserOperationEventListener.class, new
                    IdentityStoreEventListener(), null);
            IdentityGovernanceServiceImpl identityGovernanceService = new IdentityGovernanceServiceImpl();
            context.getBundleContext().registerService(IdentityGovernanceService.class, identityGovernanceService,
                    null);
            IdentityMgtServiceDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
            DefaultNotificationChannelManager defaultNotificationChannelManager =
                    new DefaultNotificationChannelManager();
            context.getBundleContext()
                    .registerService(NotificationChannelManager.class.getName(), defaultNotificationChannelManager, null);
            OTPGeneratorImpl otpGeneratorImpl = new OTPGeneratorImpl();
            context.getBundleContext()
                    .registerService(OTPGenerator.class.getName(), otpGeneratorImpl, null);

            if (log.isDebugEnabled()) {
                log.debug("Identity Management Listener is enabled");
            }
        } catch (Exception e) {
            log.error("Error while activating identity governance component.", e);
        }
    }

    @Deactivate
    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Identity Management bundle is de-activated");
        }
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {

        IdentityMgtServiceDataHolder.getInstance().setIdentityEventService(null);
    }

    @Reference(
            name = "EventMgtService",
            service = org.wso2.carbon.identity.event.services.IdentityEventService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityEventService")
    protected void setIdentityEventService(IdentityEventService identityEventService) {

        IdentityMgtServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
    }

    @Reference(
            name = "ClaimMetadataManagementService",
            service = ClaimMetadataManagementService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimMetadataManagementService")
    protected void setClaimMetadataManagementService(ClaimMetadataManagementService claimMetadataManagementService) {

        IdentityMgtServiceDataHolder.getInstance().setClaimMetadataManagementService(claimMetadataManagementService);
    }

    protected void unsetClaimMetadataManagementService(ClaimMetadataManagementService claimMetadataManagementService) {

        IdentityMgtServiceDataHolder.getInstance().setClaimMetadataManagementService(null);
    }

    @Reference(
            name = "idp.mgt.event.listener.service",
            service = org.wso2.carbon.identity.governance.common.IdentityConnectorConfig.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceConnector")
    protected void setIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {

        IdentityMgtServiceDataHolder.getInstance().addIdentityGovernanceConnector(identityConnectorConfig);

        try {
            BundleContext bundleContext = FrameworkUtil.getBundle(IdentityMgtServiceComponent.class).getBundleContext();

            bundleContext.registerService(ConnectorConfig.class.getName(), identityConnectorConfig, null);
        } catch (Throwable e) {
            log.error(
                    "Error while re-registering the service " + identityConnectorConfig.getClass().getName() + " as " +
                            ConnectorConfig.class.getName());
        }
    }

    protected void unsetIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {

        IdentityMgtServiceDataHolder.getInstance().unsetIdentityGovernanceConnector(identityConnectorConfig);
    }

    protected void unsetIdpManager(IdpManager idpManager) {

        IdentityMgtServiceDataHolder.getInstance().setIdpManager(null);
    }

    @Reference(
            name = "IdentityProviderManager",
            service = org.wso2.carbon.idp.mgt.IdpManager.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdpManager")
    protected void setIdpManager(IdpManager idpManager) {

        IdentityMgtServiceDataHolder.getInstance().setIdpManager(idpManager);
    }

    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the Realm Service");
        }
        IdentityMgtServiceDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        log.debug("UnSetting the Realm Service");
        IdentityMgtServiceDataHolder.getInstance().setRealmService(null);
    }
}
