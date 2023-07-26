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

package org.wso2.carbon.identity.idle.account.identification.services.impl;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.context.CarbonContext;

import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.idle.account.identification.constants.IdleAccIdentificationConstants;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccountIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccountIdentificationServerException;
import org.wso2.carbon.identity.idle.account.identification.internal.IdleAccountIdentificationDataHolder;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;
import org.wso2.carbon.identity.idle.account.identification.services.IdleAccountIdentificationService;

import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.jdbc.UniqueIDJDBCUserStoreManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of the service interface for idle account identification.
 */
public class IdleAccountIdentificationServiceImpl implements IdleAccountIdentificationService {

    @Override
    public List<InactiveUserModel> getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, String tenantDomain)
            throws IdleAccountIdentificationException {

        List<InactiveUserModel> inactiveUsers = new ArrayList<>();
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        String lastLoginTime = Long.toString(inactiveAfter.toEpochSecond(ZoneOffset.UTC));
        try {
            List<String> usernames = IdleAccountIdentificationDataHolder.getInstance().getIdentityDataStoreService()
                    .getUserNamesLessThanProvidedClaimValue(IdleAccIdentificationConstants.LAST_LOGIN_TIME_CLAIM,
                            lastLoginTime, tenantId);
            if (!usernames.isEmpty()) {
                inactiveUsers = buildInactiveUsers(usernames);
            }
        } catch (IdentityException e) {
            IdleAccIdentificationConstants.ErrorMessages errorEnum =
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_INACTIVE_USERS_FROM_DB;
            throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return inactiveUsers;
    }

    @Override
    public List<InactiveUserModel> getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter,
                       LocalDateTime excludeBefore, String tenantDomain) throws IdleAccountIdentificationException {

        List<InactiveUserModel> inactiveUsers = new ArrayList<>();
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        String inactiveDateEpoch = Long.toString(inactiveAfter.toEpochSecond(ZoneOffset.UTC));
        String excludeDateEpoch = Long.toString(excludeBefore.toEpochSecond(ZoneOffset.UTC));
        try {
            List<String> usernames = IdleAccountIdentificationDataHolder.getInstance().getIdentityDataStoreService()
                    .getUserNamesBetweenProvidedClaimValues(IdleAccIdentificationConstants.LAST_LOGIN_TIME_CLAIM,
                            excludeDateEpoch, inactiveDateEpoch, tenantId);
            if (!usernames.isEmpty()) {
                inactiveUsers = buildInactiveUsers(usernames);
            }
        } catch (IdentityException e) {
            IdleAccIdentificationConstants.ErrorMessages errorEnum =
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_INACTIVE_USERS_FROM_DB;
            throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return inactiveUsers;
    }

    /**
     * Build a list of inactive users.
     *
     * @param usernames        list of usernames.
     * @return                 list of inactive user objects.
     */
    private List<InactiveUserModel> buildInactiveUsers(List<String> usernames)
            throws IdleAccountIdentificationServerException {

        List<InactiveUserModel> inactiveUsers = new ArrayList<>();
        for (String username : usernames) {
            String userId = fetchUserId(username);
            if (StringUtils.isNotBlank(userId)) {
                InactiveUserModel inactiveUser = new InactiveUserModel();
                inactiveUser.setUsername(username);
                inactiveUser.setUserId(userId);
                inactiveUser.setUserStoreDomain(UserCoreUtil.extractDomainFromName(username));
                inactiveUsers.add(inactiveUser);
            }
        }
        return inactiveUsers;
    }

    /**
     * Fetch UUID of the user.
     *
     * @param username          username of the user.
     * @return                  UUID of the user
     */
    public String fetchUserId(String username) throws IdleAccountIdentificationServerException {

        UserStoreManager userStoreManager = getUserStoreManager(UserCoreUtil.extractDomainFromName(username));
        try {
            if (userStoreManager instanceof UniqueIDJDBCUserStoreManager) {
                return ((UniqueIDJDBCUserStoreManager) userStoreManager).getUserIDFromUserName(username);
            }
        } catch (UserStoreException e) {
            IdleAccIdentificationConstants.ErrorMessages errorEnum =
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_USER_UUID;
            throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return null;
    }

    /**
     * Get user store manager.
     *
     * @return UserStoreManager.
     * @throws IdleAccountIdentificationServerException Exception when getting user store manager.
     */
    private UserStoreManager getUserStoreManager(String userStore) throws IdleAccountIdentificationServerException {

        try {
            UserRealm realm = (UserRealm) CarbonContext.getThreadLocalCarbonContext().getUserRealm();
            if (realm == null) {
                IdleAccIdentificationConstants.ErrorMessages errorEnum =
                        IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
                throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
            }
            if (realm.getUserStoreManager() == null) {
                IdleAccIdentificationConstants.ErrorMessages errorEnum =
                        IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
                throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
            }
            if ( IdentityUtil.getPrimaryDomainName().equals(userStore)) {
                return realm.getUserStoreManager();
            }
            if (realm.getUserStoreManager().getSecondaryUserStoreManager(userStore) != null) {
                return realm.getUserStoreManager().getSecondaryUserStoreManager(userStore);
            }
            IdleAccIdentificationConstants.ErrorMessages errorEnum =
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
            throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        } catch (UserStoreException e) {
            IdleAccIdentificationConstants.ErrorMessages errorEnum =
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
            throw new IdleAccountIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
    }
}
