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

package org.wso2.carbon.identity.login.resolver.regex.internal;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.identity.login.resolver.regex.RegexLoginResolver;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * This class activates/registers the RegexLoginResolver as an OSGi service.
 */
@Component(
        name = "identity.login.resolver.regex.component",
        immediate = true
)
public class RegexLoginResolverServiceComponent {

    private static final Log log = LogFactory.getLog(RegexLoginResolverServiceComponent.class);

    /**
     * Activates/Registers the RegexLoginResolver as an OSGi service.
     *
     * @param context The component context.
     */
    @Activate
    protected void activate(ComponentContext context) {

        BundleContext bundleContext = context.getBundleContext();
        try {
            LoginResolver regexLoginResolver = new RegexLoginResolver();
            bundleContext.registerService(LoginResolver.class.getName(), regexLoginResolver, null);
            if (log.isDebugEnabled()) {
                log.debug("The RegexLoginResolver component was activated successfully.");
            }
        } catch (Throwable e) {
            log.error("An error occurred while activating the RegexLoginResolver.", e);
        }
    }

    /**
     * Sets the realm service to the RegexLoginResolverServiceDataHolder.
     *
     * @param realmService The realm service to be set to the RegexLoginResolverServiceDataHolder.
     */
    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        RegexLoginResolverServiceDataHolder.getInstance().setRealmService(realmService);
    }

    /**
     * Unsets the realm service from the RegexLoginResolverServiceDataHolder.
     *
     * @param realmService The realm service which unsets the current realm service from the
     *                     RegexLoginResolverServiceDataHolder.
     */
    protected void unsetRealmService(RealmService realmService) {

        RegexLoginResolverServiceDataHolder.getInstance().setRealmService(null);
    }
}
