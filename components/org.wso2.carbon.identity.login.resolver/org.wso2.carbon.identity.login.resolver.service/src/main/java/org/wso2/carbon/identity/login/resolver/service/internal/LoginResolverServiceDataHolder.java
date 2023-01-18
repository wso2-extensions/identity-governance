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

import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.login.resolver.mgt.LoginResolver;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Data holder for the LoginResolverServiceComponent.
 */
public class LoginResolverServiceDataHolder {

    private static final LoginResolverServiceDataHolder instance = new LoginResolverServiceDataHolder();
    private RealmService realmService;
    private IdentityGovernanceService identityGovernanceService;
    private LoginResolver loginResolver;

    /**
     * Private constructor to make sure that only a single instance will exist.
     */
    private LoginResolverServiceDataHolder() {

    }

    /**
     * Retrieves an instance of the LoginResolverServiceDataHolder.
     *
     * @return An instance of the LoginResolverServiceDataHolder.
     */
    public static LoginResolverServiceDataHolder getInstance() {

        return instance;
    }

    /**
     * Retrieves the realm service.
     *
     * @return The realm service.
     */
    public RealmService getRealmService() {

        return realmService;
    }

    /**
     * Sets the realm service.
     *
     * @param realmService The realm service to be set.
     */
    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }

    /**
     * Retrieves the governance service.
     *
     * @return The governance service.
     */
    public IdentityGovernanceService getIdentityGovernanceService() {

        return identityGovernanceService;
    }

    /**
     * Sets the governance service.
     *
     * @param identityGovernanceService The governance service to be set.
     */
    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    /**
     * Retrieves the login resolver.
     *
     * @return The login resolver.
     */
    public LoginResolver getLoginResolver() {

        return loginResolver;
    }

    /**
     * Sets the login resolver.
     *
     * @param loginResolver The login resolver to be set.
     */
    public void setLoginResolver(LoginResolver loginResolver) {

        this.loginResolver = loginResolver;
    }
}
