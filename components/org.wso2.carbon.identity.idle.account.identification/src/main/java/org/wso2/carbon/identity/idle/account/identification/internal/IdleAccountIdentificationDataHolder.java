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

import org.wso2.carbon.user.core.service.RealmService;

/**
 * Data holder for idle account identification service.
 */
public class IdleAccountIdentificationDataHolder {

    private static IdleAccountIdentificationDataHolder instance = new IdleAccountIdentificationDataHolder();
    private RealmService realmService;

    /**
     * Get data holder instance.
     *
     * @return IdleAccountIdentificationDataHolder.
     */
    public static IdleAccountIdentificationDataHolder getInstance() {

        return instance;
    }

    /**
     * Get realm service.
     *
     * @return RealmService.
     */
    public RealmService getRealmService() {

        return realmService;
    }

    /**
     * Set realm service.
     */
    public void setRealmService(RealmService realmService) {

        this.realmService = realmService;
    }
}
