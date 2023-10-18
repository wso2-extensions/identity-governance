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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.wso2.carbon.identity.core.handler.AbstractIdentityHandler;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

public abstract class AbstractUserInformationProvider extends AbstractIdentityHandler implements
        UserInformationProvider {


    protected static final String WSO2_CLAIM_URI = "http://wso2.org/claims/";
    protected static final String WSO2_IDENTITY_CLAIM_URI = "http://wso2.org/claims/identity/";
    protected static final String WSO2_RUN_TIME_CLAIM_URI = "http://wso2.org/claims/runtime/";

    protected UserStoreManager getUserStoreManager(int tenantId, String userStoreDomain) throws UserExportException {

        AbstractUserStoreManager userStoreManager;
        try {
            RealmService realmService = UserProfileExportDataHolder.getRealmService();
            UserRealm userRealm = realmService.getTenantUserRealm(tenantId);
            userStoreManager = (AbstractUserStoreManager) userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw new UserExportException("Error while getting userstore", e);
        }
        return userStoreManager.getSecondaryUserStoreManager(userStoreDomain);
    }

    protected String getTenantDomain(int tenantId) throws UserStoreException {

        return UserProfileExportDataHolder.getRealmService().getTenantManager().getDomain(tenantId);
    }
}
