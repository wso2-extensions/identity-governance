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

package org.wso2.carbon.identity.multi.attribute.login.resolver.regex.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.resolver.regex.RegexResolver;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * This class is used to activate MultiAttributeLoginResolver.
 */
@Component(
        name = "identity.multi.attribute.login.resolver.regex.component",
        immediate = true
)
public class RegexResolverServiceComponent {

    private static final Log log = LogFactory.getLog(RegexResolverServiceComponent.class);

    @Activate
    protected void activate(ComponentContext context) {

        BundleContext bundleContext = context.getBundleContext();
        try {
            MultiAttributeLoginResolver multiAttributeLoginResolver =
                    new RegexResolver();
            bundleContext.registerService(MultiAttributeLoginResolver.class.getName(), multiAttributeLoginResolver,
                    null);
            if (log.isDebugEnabled()) {
                log.debug("MultiAttributeLoginResolver activated successfully.");
            }
        } catch (Throwable e) {
            log.error("Error while activating MultiAttributeLoginResolver.", e);
        }
    }

    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        RegexResolverServiceDataHolder.getInstance().setRealmService(realmService);
    }

    protected void unsetRealmService(RealmService realmService) {

        RegexResolverServiceDataHolder.getInstance().setRealmService(null);
    }
}
