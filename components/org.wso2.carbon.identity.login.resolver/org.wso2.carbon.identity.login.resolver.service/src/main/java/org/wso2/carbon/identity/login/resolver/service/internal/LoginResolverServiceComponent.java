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

package org.wso2.carbon.identity.login.resolver.service.internal;

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
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolverService;
import org.wso2.carbon.identity.login.resolver.service.handler.LoginResolverServiceHandler;
import org.wso2.carbon.identity.login.resolver.service.LoginResolverServiceImpl;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * This class activates/registers the LoginResolverServiceHandler and the LoginResolverService as an OSGi service.
 */
@Component(
        name = "identity.core.login.resolver.service.component",
        immediate = true
)
public class LoginResolverServiceComponent {

    private static final Log log = LogFactory.getLog(LoginResolverServiceComponent.class);

    /**
     * Activates/Registers the LoginResolverServiceHandler and the LoginResolverService as an OSGi service.
     *
     * @param context The component context.
     */
    @Activate
    protected void activate(ComponentContext context) {

        BundleContext bundleContext = context.getBundleContext();
        try {
            IdentityConnectorConfig loginResolverServiceHandler = new LoginResolverServiceHandler();
            bundleContext.registerService(IdentityConnectorConfig.class.getName(),
                    loginResolverServiceHandler, null);
            if (log.isDebugEnabled()) {
                log.debug("The LoginResolverServiceHandler is registered successfully.");
            }
        } catch (Throwable e) {
            log.error("An occurred while activating the LoginResolverServiceHandler.", e);
        }
        try {
            LoginResolverService loginResolverService = new LoginResolverServiceImpl();
            bundleContext.registerService(LoginResolverService.class.getName(), loginResolverService, null);
            if (log.isDebugEnabled()) {
                log.debug("The LoginResolverService is registered successfully.");
            }
        } catch (Throwable e) {
            log.error("An error occurred while activating the LoginResolverService.", e);
        }
    }

    /**
     * Sets the realm service to the LoginResolverServiceDataHolder.
     *
     * @param realmService The realm service to be set to the LoginResolverServiceDataHolder.
     */
    @Reference(
            name = "RealmService",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService")
    protected void setRealmService(RealmService realmService) {

        LoginResolverServiceDataHolder.getInstance().setRealmService(realmService);
    }

    /**
     * Unsets the realm service from the LoginResolverServiceDataHolder.
     *
     * @param realmService The realm service which unsets the current realm service from the
     *                     LoginResolverServiceDataHolder.
     */
    protected void unsetRealmService(RealmService realmService) {

        LoginResolverServiceDataHolder.getInstance().setRealmService(null);
    }

    /**
     * Sets the login resolver to the LoginResolverServiceDataHolder.
     *
     * @param loginResolver The login resolver to be set to the LoginResolverServiceDataHolder.
     */
    @Reference(
            name = "LoginResolver",
            service = LoginResolver.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetLoginResolver"
    )
    protected void setLoginResolver(LoginResolver loginResolver) {

        LoginResolverServiceDataHolder.getInstance().addLoginResolver(loginResolver);
    }

    /**
     * Unsets the login resolver from the LoginResolverServiceDataHolder.
     *
     * @param loginResolver The login resolver which unsets the current login resolver from the
     *                      LoginResolverServiceDataHolder.
     */
    protected void unsetLoginResolver(LoginResolver loginResolver) {

        LoginResolverServiceDataHolder.getInstance().removeLoginResolver(loginResolver);
    }

    /**
     * Sets the governance service to the LoginResolverServiceDataHolder.
     *
     * @param identityGovernanceService The identity governance service to be set to the LoginResolverServiceDataHolder.
     */
    @Reference(
            name = "IdentityGovernanceService",
            service = IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService"
    )
    protected void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        LoginResolverServiceDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
    }

    /**
     * Unsets the identity governance service from the LoginResolverServiceDataHolder.
     *
     * @param identityGovernanceService The identity governance service which unsets the current identity governance
     *                                  service from the LoginResolverServiceDataHolder.
     */
    protected void unsetIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        LoginResolverServiceDataHolder.getInstance().setIdentityGovernanceService(null);
    }
}
