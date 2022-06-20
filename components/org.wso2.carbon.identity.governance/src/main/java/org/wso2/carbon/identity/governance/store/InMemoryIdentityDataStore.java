/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.store;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCache;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCacheKey;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.Locale;
import java.util.Map;

/**
 *
 */
public class InMemoryIdentityDataStore extends UserIdentityDataStore {

    private static final Log log = LogFactory.getLog(InMemoryIdentityDataStore.class);
    private final IdentityDataStoreCache identityDataStoreCache = IdentityDataStoreCache.getInstance();

    @Override
    public void store(UserIdentityClaim userIdentityDTO, UserStoreManager userStoreManager)
            throws IdentityException {

        try {
            if (userIdentityDTO != null && userIdentityDTO.getUserName() != null) {
                String userName = UserCoreUtil.removeDomainFromName(userIdentityDTO.getUserName());
                if (userStoreManager instanceof org.wso2.carbon.user.core.UserStoreManager) {
                    if (!IdentityUtil.isUserStoreCaseSensitive((org.wso2.carbon.user.core.UserStoreManager)
                            userStoreManager)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Case insensitive user store found. Changing username from : " + userName +
                                    " to : " + userName.toLowerCase(Locale.ENGLISH));
                        }
                        userName = userName.toLowerCase(Locale.ENGLISH);
                    } else if (!IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(
                            (org.wso2.carbon.user.core.UserStoreManager) userStoreManager)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Case insensitive username for cache key is used. Changing username from : "
                                    + userName + " to : " + userName.toLowerCase(Locale.ENGLISH));
                        }
                        userName = userName.toLowerCase(Locale.ENGLISH);
                    }
                }

                if (log.isDebugEnabled()) {
                    StringBuilder data = new StringBuilder("{");
                    if (userIdentityDTO.getUserIdentityDataMap() != null) {
                        for (Map.Entry<String, String> entry : userIdentityDTO.getUserIdentityDataMap().entrySet()) {
                            data.append("[").append(entry.getKey()).append(" = ").append(entry.getValue()).append("], ");
                        }
                    }
                    if (data.indexOf(",") >= 0) {
                        data.deleteCharAt(data.lastIndexOf(","));
                    }
                    data.append("}");
                    log.debug("Storing UserIdentityClaimsDO to cache for user: " + userName + " with claims: " + data);
                }

                org.wso2.carbon.user.core.UserStoreManager store = (org.wso2.carbon.user.core.UserStoreManager) userStoreManager;
                String domainName = store.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

                IdentityDataStoreCacheKey key = new IdentityDataStoreCacheKey(domainName, userName);
                int tenantId = userStoreManager.getTenantId();
                UserIdentityClaim cachedUserIdentityDTO = identityDataStoreCache.getValueFromCache(key, tenantId);
                if (cachedUserIdentityDTO != null) {
                    cachedUserIdentityDTO.getUserIdentityDataMap().putAll(userIdentityDTO.getUserIdentityDataMap());
                    identityDataStoreCache.addToCache(key, cachedUserIdentityDTO, tenantId);
                } else {
                    identityDataStoreCache.addToCache(key, userIdentityDTO, tenantId);
                }
            }
        } catch (UserStoreException e) {
            log.error("Error while obtaining tenant ID from user store manager", e);
        }
    }

    @Override
    public UserIdentityClaim load(String userName, UserStoreManager userStoreManager) {

        try {
            if (userName != null) {
                userName = UserCoreUtil.removeDomainFromName(userName);
                if (userStoreManager instanceof org.wso2.carbon.user.core.UserStoreManager) {
                    if (!IdentityUtil.isUserStoreCaseSensitive((org.wso2.carbon.user.core.UserStoreManager)
                            userStoreManager)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Case insensitive user store found. Changing username from : " + userName +
                                    " to : " + userName.toLowerCase(Locale.ENGLISH));
                        }
                        userName = userName.toLowerCase(Locale.ENGLISH);
                    } else if (!IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(
                            (org.wso2.carbon.user.core.UserStoreManager) userStoreManager)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Case insensitive username for cache key is used. Changing username from : "
                                    + userName + " to : " + userName.toLowerCase(Locale.ENGLISH));
                        }
                        userName = userName.toLowerCase(Locale.ENGLISH);
                    }
                }

                org.wso2.carbon.user.core.UserStoreManager store = (org.wso2.carbon.user.core.UserStoreManager) userStoreManager;

                String domainName = store.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

                IdentityDataStoreCacheKey key = new IdentityDataStoreCacheKey(domainName, userName);
                int tenantId = userStoreManager.getTenantId();
                UserIdentityClaim userIdentityDTO = identityDataStoreCache.getValueFromCache(key, tenantId);

                if (userIdentityDTO != null && log.isDebugEnabled()) {
                    StringBuilder data = new StringBuilder("{");
                    if (userIdentityDTO.getUserIdentityDataMap() != null) {
                        for (Map.Entry<String, String> entry : userIdentityDTO.getUserIdentityDataMap().entrySet()) {
                            data.append("[").append(entry.getKey()).append(" = ").append(entry.getValue())
                                    .append("], ");
                        }
                    }
                    if (data.indexOf(",") >= 0) {
                        data.deleteCharAt(data.lastIndexOf(","));
                    }
                    data.append("}");
                    log.debug("Loaded UserIdentityClaimsDO from cache for user :" + userName + " with claims: " + data);

                }
                return userIdentityDTO;
            }
        } catch (UserStoreException e) {
            log.error("Error while obtaining tenant ID from user store manager");
        }
        return null;
    }

    @Override
    public void remove(String userName, UserStoreManager userStoreManager) throws IdentityException {

        try {
            if (userName == null) {
                return;
            }
            userName = UserCoreUtil.removeDomainFromName(userName);
            if (userStoreManager instanceof org.wso2.carbon.user.core.UserStoreManager) {
                if (!IdentityUtil.isUserStoreCaseSensitive((org.wso2.carbon.user.core.UserStoreManager)
                        userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive user store found. Changing username from : " + userName + " to : " +
                                userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                } else if (!IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(
                        (org.wso2.carbon.user.core.UserStoreManager) userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive username for cache key is used. Changing username from : "
                                + userName + " to : " + userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                }
            }
            org.wso2.carbon.user.core.UserStoreManager store = (org.wso2.carbon.user.core.UserStoreManager)
                    userStoreManager;
            String domainName = store.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig
                    .PROPERTY_DOMAIN_NAME);

            IdentityDataStoreCacheKey key = new IdentityDataStoreCacheKey(domainName, userName);
            identityDataStoreCache.clearCacheEntry(key, userStoreManager.getTenantId());
        } catch (UserStoreException e) {
            log.error("Error while obtaining tenant ID from user store manager");
        }
    }

}
