/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.idle.account.identification.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.idle.account.identification.services.IdleAccountIdentificationService;
import org.wso2.carbon.identity.idle.account.identification.services.IdleAccountIdentificationServiceImpl;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * OSGi declarative services component which handles registration and un-registration of idle account identification
 * service.
 */
@Component(
        name = "org.wso2.carbon.identity.idle.account.identification.task.internal.IdleAccountIdentificationComponent",
        immediate = true)
public class IdleAccountIdentificationComponent {

    private static final Log log = LogFactory.getLog(IdleAccountIdentificationComponent.class);

    private IdleAccountIdentificationDataHolder dataHolder = IdleAccountIdentificationDataHolder.getInstance();

    /**
     * Register IdleAccountIdentificationService as an OSGI service.
     *
     * @param componentContext OSGI service component context.
     */
    @Activate
    protected void activate(ComponentContext componentContext) {

        BundleContext bundleContext = componentContext.getBundleContext();
        bundleContext.registerService(IdleAccountIdentificationService.class.getName(),
                new IdleAccountIdentificationServiceImpl(), null);
    }

    @Deactivate
    protected void deactivate(ComponentContext componentContext) {

        log.debug("Idle Account Identification service is deactivated");
    }

    @Reference(
            name = "realm.service",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        if (log.isDebugEnabled()) {
            log.debug("Setting the Realm Service");
        }
        dataHolder.setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        log.debug("UnSetting the Realm Service");
        dataHolder.setRealmService(null);
    }
}
