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

package org.wso2.carbon.identity.login.resolver.regex.utils;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.login.resolver.regex.internal.RegexLoginResolverServiceDataHolder;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UniqueIDUserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Utility class that is being used throughout the regex login resolver module.
 */
public class UserResolverUtil {

    /**
     * Private constructor to restrict from creating multiple instances of the UserResolverUtil class.
     */
    private UserResolverUtil() {

    }

    /**
     * Retrieves the user realm based on the provided tenant domain.
     *
     * @param tenantDomain The tenant domain of the user.
     * @return The user realm.
     * @throws UserStoreException If there is an error while retrieving the tenant id or the tenant user realm.
     */
    public static UserRealm getUserRealm(String tenantDomain) throws UserStoreException {

        RealmService realmService = RegexLoginResolverServiceDataHolder.getInstance().getRealmService();
        UserRealm userRealm = null;
        if (realmService != null) {
            int tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        }
        return userRealm;
    }

    /**
     * Retrieves the user store manager based on the provided tenant domain.
     *
     * @param tenantDomain The tenant domain of the user.
     * @return The user store manager.
     * @throws UserStoreException If there is an error while retrieving the user realm or the user store manager.
     */
    public static UniqueIDUserStoreManager getUserStoreManager(String tenantDomain) throws UserStoreException {

        UserRealm userRealm = getUserRealm(tenantDomain);
        return (UniqueIDUserStoreManager) userRealm.getUserStoreManager();
    }

    /**
     * Checks whether the provided claim is not valid to be used.
     *
     * @param claim The claim which is to be checked for validity.
     * @return Whether the claim is valid or not.
     */
    public static boolean isClaimNotValid(Claim claim) {

        if (claim == null) {
            return true;
        } else {
            return StringUtils.isBlank(claim.getRegEx());
        }
    }
}
