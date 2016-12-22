/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.IdentityGovernanceServiceImpl;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.listener.IdentityMgtEventListener;
import org.wso2.carbon.identity.governance.listener.IdentityStoreEventListener;;
import org.wso2.carbon.identity.governance.listener.TenantCreationEventListener;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

/**
 * @scr.component name="org.wso2.carbon.identity.governance.internal.IdentityMgtServiceComponent" immediate="true"
 * @scr.reference name="EventMgtService"
 * interface="org.wso2.carbon.identity.event.services.IdentityEventService" cardinality="1..1"
 * policy="dynamic" bind="setIdentityEventService" unbind="unsetIdentityEventService"
 * @scr.reference name="idp.mgt.event.listener.service"
 * interface="org.wso2.carbon.identity.governance.common.IdentityConnectorConfig"
 * cardinality="0..n" policy="dynamic"
 * bind="setIdentityGovernanceConnector"
 * unbind="unsetIdentityGovernanceConnector"
 * @scr.reference name="IdentityProviderManager"
 * interface="org.wso2.carbon.idp.mgt.IdpManager" cardinality="1..1"
 * policy="dynamic" bind="setIdpManager" unbind="unsetIdpManager"
 * @scr.reference name="RealmService"
 * interface="org.wso2.carbon.user.core.service.RealmService"
 * cardinality="1..1" policy="dynamic" bind="setRealmService"
 * unbind="unsetRealmService"
 */
public class IdentityMgtServiceComponent {

    private static Log log = LogFactory.getLog(IdentityMgtServiceComponent.class);
    private static IdentityMgtEventListener listener = null;

    protected void activate(ComponentContext context) {

        try {
            listener = new IdentityMgtEventListener();
            context.getBundleContext().registerService(UserOperationEventListener.class,
                    listener, null);
            context.getBundleContext().registerService(UserOperationEventListener.class,
                    new IdentityStoreEventListener(), null);
            context.getBundleContext().registerService(IdentityGovernanceService.class, new
                    IdentityGovernanceServiceImpl(), null);
            context.getBundleContext().registerService(TenantMgtListener.class.getName(),
                    new TenantCreationEventListener(), null);
            if (log.isDebugEnabled()) {
                log.debug("Identity Management Listener is enabled");
            }
        } catch (Exception e) {
            log.error("Error while activating identity governance component.", e);
        }
    }


    protected void deactivate(ComponentContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Identity Management bundle is de-activated");
        }
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {
        IdentityMgtServiceDataHolder.getInstance().setIdentityEventService(null);
    }

    protected void setIdentityEventService(IdentityEventService identityEventService) {
        IdentityMgtServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
    }

    protected void unsetIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {
        IdentityMgtServiceDataHolder.getInstance().unsetIdentityGovernanceConnector(identityConnectorConfig);
    }

    protected void setIdentityGovernanceConnector(IdentityConnectorConfig identityConnectorConfig) {
        IdentityMgtServiceDataHolder.getInstance().addIdentityGovernanceConnector(identityConnectorConfig);
        try {
            IdentityGovernanceUtil.saveConnectorDefaultProperties(identityConnectorConfig, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        } catch (IdentityGovernanceException e) {
            log.error("Error while saving super tenant configurations for " + identityConnectorConfig.getName() +
                    ".", e);
        }
    }

    protected void unsetIdpManager(IdpManager idpManager) {
        IdentityMgtServiceDataHolder.getInstance().setIdpManager(null);
    }

    protected void setIdpManager(IdpManager idpManager) {
        IdentityMgtServiceDataHolder.getInstance().setIdpManager(idpManager);
    }
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
