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

package org.wso2.carbon.identity.recovery.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.recovery.connector.RecoveryConnectorImpl;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.registry.core.service.RegistryService;
import org.wso2.carbon.user.core.service.RealmService;


/**
 * @scr.component name="org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent" immediate="true"
 * @scr.reference name="registry.service"
 * interface="org.wso2.carbon.registry.core.service.RegistryService" cardinality="1..1"
 * policy="dynamic" bind="setRegistryService" unbind="unsetRegistryService"
 * @scr.reference name="realm.service"
 * interface="org.wso2.carbon.user.core.service.RealmService"cardinality="1..1"
 * policy="dynamic" bind="setRealmService" unbind="unsetRealmService"
 * @scr.reference name="IdentityGovernanceService"
 * interface="org.wso2.carbon.identity.governance.IdentityGovernanceService" cardinality="1..1"
 * policy="dynamic" bind="setIdentityGovernanceService" unbind="unsetIdentityGovernanceService"
 * @scr.reference name="IdentityEventService"
 * interface="org.wso2.carbon.identity.event.services.IdentityEventService" cardinality="1..1"
 * policy="dynamic" bind="setIdentityEventService" unbind="unsetIdentityEventService"
 */
public class IdentityRecoveryServiceComponent {

    private static Log log = LogFactory.getLog(IdentityRecoveryServiceComponent.class);
    private static RealmService realmService;
    private static RegistryService registryService;


    protected void activate(ComponentContext context) {

        try {

            if (log.isDebugEnabled()) {
                log.debug("Identity Management Listener is enabled");
            }
            BundleContext bundleContext = context.getBundleContext();
            bundleContext.registerService(NotificationPasswordRecoveryManager.class.getName(),
                    new NotificationPasswordRecoveryManager(), null);
            bundleContext.registerService(SecurityQuestionPasswordRecoveryManager.class.getName(),
                    new SecurityQuestionPasswordRecoveryManager(), null);
            bundleContext.registerService(NotificationUsernameRecoveryManager.class.getName(),
                    new NotificationUsernameRecoveryManager(), null);

            context.getBundleContext().registerService(IdentityGovernanceConnector.class.getName(), new RecoveryConnectorImpl(), null);

        } catch (Exception e) {
            log.error("Error while activating identity governance component.", e);
        }
    }

    public static RealmService getRealmService() {
        return realmService;
    }

    protected void setRealmService(RealmService realmService) {
        log.debug("Setting the Realm Service");
        IdentityRecoveryServiceComponent.realmService = realmService;
    }

    public static RegistryService getRegistryService() {
        return registryService;
    }

    protected void setRegistryService(RegistryService registryService) {
        log.debug("Setting the Registry Service");
        IdentityRecoveryServiceComponent.registryService = registryService;
    }

    protected void deactivate(ComponentContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Identity Management bundle is de-activated");
        }
    }

    protected void unsetRealmService(RealmService realmService) {
        log.debug("UnSetting the Realm Service");
        IdentityRecoveryServiceComponent.realmService = null;
    }

    protected void unsetRegistryService(RegistryService registryService) {
        log.debug("UnSetting the Registry Service");
        IdentityRecoveryServiceComponent.registryService = null;
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(null);
    }

    protected void setIdentityEventService(IdentityEventService identityEventService) {
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityEventService(identityEventService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityGovernanceService(idpManager);
    }


}
