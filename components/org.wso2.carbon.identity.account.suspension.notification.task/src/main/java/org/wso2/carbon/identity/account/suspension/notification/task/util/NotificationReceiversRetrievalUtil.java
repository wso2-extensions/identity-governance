/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.account.suspension.notification.task.util;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrieval;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class NotificationReceiversRetrievalUtil {

    public static final String NOTIFICATION_RECEIVERS_RETRIEVAL_CLASS = "NotificationReceiversRetrievalClass";

    public static Set<String> getSuspensionNotificationEnabledUserStores(String tenantDomain)
            throws AccountSuspensionNotificationException {

        RealmConfiguration realmConfiguration;
        Set<String> userStoreSet = new HashSet<>();
        String domain;

        try {
            realmConfiguration = CarbonContext.getThreadLocalCarbonContext().getUserRealm().getRealmConfiguration();
            domain = IdentityUtil.getPrimaryDomainName();

            if (isEffectiveUserStore(realmConfiguration, true)) {
                userStoreSet.add(domain);
            }

            do {
                realmConfiguration = realmConfiguration.getSecondaryRealmConfig();
                if (realmConfiguration != null) {
                    if (isEffectiveUserStore(realmConfiguration, false)) {
                        userStoreSet.add(realmConfiguration
                                .getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));
                    }
                }
            } while (realmConfiguration != null);

        } catch (UserStoreException e) {
            throw new AccountSuspensionNotificationException("Error while getting the notification enabled user stores",
                    e);
        }

        return userStoreSet;
    }

    public static NotificationReceiversRetrieval getNotificationReceiversRetrievalForDomain(String domain,
            String tenantDomain) throws AccountSuspensionNotificationException {

        NotificationReceiversRetrieval notificationReceiversRetrieval = null;

        if (StringUtils.isEmpty(domain)) {
            domain = IdentityUtil.getPrimaryDomainName();
        }

        RealmConfiguration realmConfiguration = getUserStoreList(tenantDomain).get(domain);

        if (realmConfiguration != null) {
            String retrieverType = realmConfiguration.getUserStoreProperty(NOTIFICATION_RECEIVERS_RETRIEVAL_CLASS);

            if (StringUtils.isNotBlank(retrieverType)) {
                notificationReceiversRetrieval = NotificationTaskDataHolder.getInstance()
                        .getNotificationReceiversRetrievalFactories().get(retrieverType)
                        .buildCountRetriever(realmConfiguration);
            }
            if (notificationReceiversRetrieval == null) {
                throw new AccountSuspensionNotificationException("Could not create an instance of class: " +
                        retrieverType + " for the domain: " + domain);
            }

        }
        return notificationReceiversRetrieval;
    }

    public static Map<String, RealmConfiguration> getUserStoreList(String tenantDomain) throws
            AccountSuspensionNotificationException {
        String domain;
        RealmConfiguration realmConfiguration;
        Map<String, RealmConfiguration> userStoreList = new HashMap<>();

        try {
            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext privilegedCarbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            privilegedCarbonContext.setTenantId(IdentityTenantUtil.getTenantId(tenantDomain));
            privilegedCarbonContext.setTenantDomain(tenantDomain);
            realmConfiguration = CarbonContext.getThreadLocalCarbonContext().getUserRealm().getRealmConfiguration();
            domain = IdentityUtil.getPrimaryDomainName();
            if (isEffectiveUserStore(realmConfiguration, true)) {
                userStoreList.put(domain, realmConfiguration);
            }

            do {
                realmConfiguration = realmConfiguration.getSecondaryRealmConfig();
                if (realmConfiguration != null) {
                    if (isEffectiveUserStore(realmConfiguration, false)) {
                        domain = realmConfiguration
                                .getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
                        userStoreList.put(domain, realmConfiguration);
                    }
                }
            } while (realmConfiguration != null);

        } catch (UserStoreException e) {
            throw new AccountSuspensionNotificationException(
                    "Error while listing user stores for notification functionality", e);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }

        return userStoreList;
    }

    private static boolean isEffectiveUserStore(RealmConfiguration realmConfiguration, boolean isPrimaryUserStore) {

        if (realmConfiguration == null) {
            return false;
        }

        //Primary User store cannot be disabled

        if (!isPrimaryUserStore) {
            if (Boolean.valueOf(
                    realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.USER_STORE_DISABLED))) {
                return false;
            }
        }

        if (StringUtils.isBlank(realmConfiguration.getUserStoreProperty(NOTIFICATION_RECEIVERS_RETRIEVAL_CLASS))) {
            return false;
        }

        return true;
    }
}
