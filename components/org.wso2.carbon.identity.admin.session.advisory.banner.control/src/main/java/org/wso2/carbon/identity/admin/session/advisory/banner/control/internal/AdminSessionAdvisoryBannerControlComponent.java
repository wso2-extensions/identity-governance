/*
 * Copyright (c) 2023, WSO2 Inc. (http://www.wso2.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.admin.session.advisory.banner.control.internal;

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
import org.wso2.carbon.identity.admin.session.advisory.banner.control.handler.AdminSessionAdvisoryBannerControlHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.user.core.service.RealmService;

@Component(
        name = "org.wso2.carbon.identity.admin.session.advisory.banner.control.internal" +
                ".AdminSessionAdvisoryBannerControlComponent",
        immediate = true)
public class AdminSessionAdvisoryBannerControlComponent {

    private static final Log log = LogFactory.getLog(AdminSessionAdvisoryBannerControlComponent.class);

    @Activate
    protected void activate(ComponentContext context) {
        BundleContext bundleContext = context.getBundleContext();
        try {
            IdentityConnectorConfig adminSessionAdvisoryBannerControlHandler
                    = new AdminSessionAdvisoryBannerControlHandler();
            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    adminSessionAdvisoryBannerControlHandler, null);
            if (log.isDebugEnabled()) {
                log.debug("AdminSessionAdvisoryBannerControlHandler is registered.");
            }
        } catch (RuntimeException e) {
            log.error("Error while activating AdminSessionAdvisoryBannerControlHandler.", e);
        }
    }

    @Deactivate
    protected void deactivate(ComponentContext context) {

        if (log.isDebugEnabled()) {
            log.debug("Captcha Component is de-activated");
        }
    }

    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        AdminSessionAdvisoryBannerControlDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        AdminSessionAdvisoryBannerControlDataHolder.getInstance().setRealmService(null);
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService"
    )
    protected void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        AdminSessionAdvisoryBannerControlDataHolder.getInstance()
                .setIdentityGovernanceService(identityGovernanceService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        AdminSessionAdvisoryBannerControlDataHolder.getInstance().setIdentityGovernanceService(null);
    }
}
