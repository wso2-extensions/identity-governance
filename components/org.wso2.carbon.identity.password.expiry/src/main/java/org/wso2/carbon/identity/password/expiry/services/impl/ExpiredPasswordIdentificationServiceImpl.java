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

package org.wso2.carbon.identity.password.expiry.services.impl;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.password.expiry.exceptions.ExpiredPasswordIdentificationException;
import org.wso2.carbon.identity.password.expiry.exceptions.ExpiredPasswordIdentificationServerException;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.wso2.carbon.identity.password.expiry.models.PasswordExpiredUserModel;
import org.wso2.carbon.identity.password.expiry.services.ExpiredPasswordIdentificationService;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils.getPasswordExpiryInDays;

/**
 * Implementation of the service interface for password expired user identification.
 */
public class ExpiredPasswordIdentificationServiceImpl implements ExpiredPasswordIdentificationService {

    @Override
    public List<PasswordExpiredUserModel> getPasswordExpiredUsersFromSpecificDate(
            LocalDateTime expiredAfter, String tenantDomain) throws ExpiredPasswordIdentificationException {

        List<PasswordExpiredUserModel> passwordExpiredUsers = new ArrayList<>();
        try {
            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            LocalDateTime expireDate = expiredAfter.minusDays(getPasswordExpiryInDays(tenantDomain));
            String expireDateEpoch = Long.toString(expireDate.toEpochSecond(ZoneOffset.UTC));

            List<String> usernames = EnforcePasswordResetComponentDataHolder.getInstance().getIdentityDataStoreService()
                    .getUserNamesMoreThanProvidedClaimValue(
                            PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, expireDateEpoch, tenantId);
            if (!usernames.isEmpty()) {
                passwordExpiredUsers = buildPasswordExpiredUsers(usernames, tenantDomain);
            }
        } catch (IdentityException e) {
            PasswordPolicyConstants.ErrorMessages errorEnum =
                    PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_PASSWORD_EXPIRED_USERS_FROM_DB;
            throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return passwordExpiredUsers;
    }

    @Override
    public List<PasswordExpiredUserModel> getPasswordExpiredUsersBetweenSpecificDates(LocalDateTime expiredAfter,
               LocalDateTime excludeAfter, String tenantDomain) throws ExpiredPasswordIdentificationException {

        List<PasswordExpiredUserModel> passwordExpiredUsers = new ArrayList<>();
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);

        try {
            LocalDateTime expiredDate = expiredAfter.minusDays(getPasswordExpiryInDays(tenantDomain));
            LocalDateTime excludeDate = excludeAfter.minusDays(getPasswordExpiryInDays(tenantDomain) - 1);

            String expiredDateEpoch = Long.toString(expiredDate.toEpochSecond(ZoneOffset.UTC));
            String excludeDateEpoch = Long.toString(excludeDate.toEpochSecond(ZoneOffset.UTC));

            List<String> usernames = EnforcePasswordResetComponentDataHolder.getInstance().getIdentityDataStoreService()
                    .getUserNamesBetweenProvidedClaimValues(
                            PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, expiredDateEpoch,
                            excludeDateEpoch, tenantId);
            if (!usernames.isEmpty()) {
                passwordExpiredUsers = buildPasswordExpiredUsers(usernames, tenantDomain);
            }
        } catch (IdentityException e) {
            PasswordPolicyConstants.ErrorMessages errorEnum =
                    PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_PASSWORD_EXPIRED_USERS_FROM_DB;
            throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return passwordExpiredUsers;
    }

    /**
     * Build a list of password expired users.
     *
     * @param usernames        list of usernames.
     * @return                 list of password expired user objects.
     */
    private List<PasswordExpiredUserModel> buildPasswordExpiredUsers(List<String> usernames, String tenantDomain)
            throws ExpiredPasswordIdentificationServerException {

        List<PasswordExpiredUserModel> passwordExpiredUsers = new ArrayList<>();
        for (String username : usernames) {
            String userId = fetchUserId(username, tenantDomain);
            if (StringUtils.isNotBlank(userId)) {
                PasswordExpiredUserModel passwordExpiredUser = new PasswordExpiredUserModel();
                passwordExpiredUser.setUsername(username);
                passwordExpiredUser.setUserId(userId);
                passwordExpiredUser.setUserStoreDomain(UserCoreUtil.extractDomainFromName(username));
                passwordExpiredUsers.add(passwordExpiredUser);
            }
        }
        return passwordExpiredUsers;
    }

    /**
     * Fetch UUID of the user.
     *
     * @param username          username of the user.
     * @return                  UUID of the user
     */
    private String fetchUserId(String username, String tenantDomain)
            throws ExpiredPasswordIdentificationServerException {
        
        UserStoreManager userStoreManager = getUserStoreManager(
                UserCoreUtil.extractDomainFromName(username), tenantDomain);
        try {
            if (userStoreManager instanceof AbstractUserStoreManager) {
                return ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(username);
            }
        } catch (UserStoreException e) {
            PasswordPolicyConstants.ErrorMessages errorEnum =
                    PasswordPolicyConstants.ErrorMessages.ERROR_WHILE_GETTING_USERID_FOR_USERNAME;
            throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
        return null;
    }

    /**
     * Get user store manager.
     *
     * @return UserStoreManager.
     * @throws ExpiredPasswordIdentificationServerException Exception when getting user store manager.
     */
    private UserStoreManager getUserStoreManager(String userStoreDomainName, String tenantDomain)
            throws ExpiredPasswordIdentificationServerException {

        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserRealm realm;
            realm = (UserRealm) realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(tenantDomain));

            if (realm == null) {
                PasswordPolicyConstants.ErrorMessages errorEnum =
                        PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
                throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
            }
            if (realm.getUserStoreManager() == null) {
                PasswordPolicyConstants.ErrorMessages errorEnum =
                        PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
                throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
            }
            if (IdentityUtil.getPrimaryDomainName().equals(userStoreDomainName)) {
                return realm.getUserStoreManager();
            }
            if (realm.getUserStoreManager().getSecondaryUserStoreManager(userStoreDomainName) != null) {
                return realm.getUserStoreManager().getSecondaryUserStoreManager(userStoreDomainName);
            }
            PasswordPolicyConstants.ErrorMessages errorEnum =
                    PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
            throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        } catch (UserStoreException e) {
            PasswordPolicyConstants.ErrorMessages errorEnum =
                    PasswordPolicyConstants.ErrorMessages.ERROR_RETRIEVE_USER_STORE_MANAGER;
            throw new ExpiredPasswordIdentificationServerException(errorEnum.getCode(), errorEnum.getMessage());
        }
    }
}
