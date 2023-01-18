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

import org.wso2.carbon.user.core.service.RealmService;

/**
 * Data holder for the RegexLoginResolverServiceComponent.
 */
public class RegexLoginResolverServiceDataHolder {

    private static final RegexLoginResolverServiceDataHolder instance = new RegexLoginResolverServiceDataHolder();

    private RealmService realmService;

    /**
     * Private constructor to make sure that only a single instance will exist.
     */
    private RegexLoginResolverServiceDataHolder() {

    }

    /**
     * Retrieves an instance of the RegexLoginResolverServiceDataHolder.
     *
     * @return An instance of the RegexLoginResolverServiceDataHolder.
     */
    public static RegexLoginResolverServiceDataHolder getInstance() {

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
}
