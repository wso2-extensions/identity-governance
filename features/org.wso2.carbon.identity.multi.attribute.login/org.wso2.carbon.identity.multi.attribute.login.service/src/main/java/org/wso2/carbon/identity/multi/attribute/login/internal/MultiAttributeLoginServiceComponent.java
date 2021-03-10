/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.multi.attribute.login.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.multi.attribute.login.handler.MultiAttributeLoginHandler;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginService;
import org.wso2.carbon.identity.multi.attribute.login.service.MultiAttributeLoginServiceServiceImpl;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * This class is used to activate MultiAttributeLoginService.
 */
@Component(
        name = "identity.core.multi.attribute.login.component",
        immediate = true
)
public class MultiAttributeLoginServiceComponent {

    private static final Log log = LogFactory.getLog(MultiAttributeLoginServiceComponent.class);

    @Activate
    protected void activate(ComponentContext context) {

        BundleContext bundleContext = context.getBundleContext();
        try {
            IdentityConnectorConfig multiAttributeLoginHandler = new MultiAttributeLoginHandler();
            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    multiAttributeLoginHandler, null);
            if (log.isDebugEnabled()) {
                log.debug("MultiAttributeLoginHandler is registered.");
            }
        } catch (Throwable e) {
            log.error("Error while activating MultiAttributeLoginHandler.", e);
        }
        try {
            MultiAttributeLoginService multiAttributeLoginService = new MultiAttributeLoginServiceServiceImpl();
            bundleContext.registerService(MultiAttributeLoginService.class.getName(),
                    multiAttributeLoginService, null);
            if (log.isDebugEnabled()) {
                log.debug("MultiAttributeLoginService is registered.");
            }
        } catch (Throwable e) {
            log.error("Error while activating multi attribute login bundle.", e);
        }
    }

    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        MultiAttributeLoginDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        MultiAttributeLoginDataHolder.getInstance().setRealmService(null);
    }

    @Reference(
            name = "MultiAttributeLoginResolver",
            service = MultiAttributeLoginResolver.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetMultiAttributeLoginResolver"
    )
    protected void setMultiAttributeLoginResolver(MultiAttributeLoginResolver multiAttributeLoginResolver) {

        MultiAttributeLoginDataHolder.getInstance().setMultiAttributeLoginResolver(multiAttributeLoginResolver);
    }

    protected void unsetMultiAttributeLoginResolver(MultiAttributeLoginResolver multiAttributeLoginResolver) {

        MultiAttributeLoginDataHolder.getInstance().setMultiAttributeLoginResolver(null);
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService"
    )
    protected void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        MultiAttributeLoginDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        MultiAttributeLoginDataHolder.getInstance().setIdentityGovernanceService(null);
    }
}
