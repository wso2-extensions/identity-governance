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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrieval;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

public class NotificationReceiversRetrievalUtil {

    public static final String NOTIFICATION_RECEIVERS_RETRIEVAL_CLASS = "NotificationReceiversRetrievalClass";
    private static final Log log = LogFactory.getLog(NotificationReceiversRetrievalUtil.class);

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

    /**
     * This method returns the list of NotificationReceiver objects for the users whose accounts have been idle more
     * than the allowed number of days.
     *
     * @param lookupMin          lookup mininum timestamp
     * @param lookupMax          lookup maximum timestamp
     * @param delayForSuspension allowed account suspension delay
     * @param realmService       realm service
     * @param tenantDomain       tenant domain
     * @param userStoreDomain    userstore domain
     * @return list of NotificationReceiver objects for the users
     * @throws AccountSuspensionNotificationException if something happens when retrieving user details
     */
    public static List<NotificationReceiver> getNotificationReceiversFromIdentityClaim(long lookupMin, long
            lookupMax, long delayForSuspension, RealmService realmService, String tenantDomain, String userStoreDomain)
            throws AccountSuspensionNotificationException {

        if (log.isDebugEnabled()) {
            log.debug("Retrieve users whose accounts have been idle more than the allowed number of days by checking " +
                    "the identity database.");
        }

        List<NotificationReceiver> users = new ArrayList<>();
        String sqlStmt = NotificationConstants.GET_USERS_FILTERED_BY_LAST_LOGIN_TIME_IDENTITY_CLAIM;
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, NotificationConstants.LAST_LOGIN_TIME_IDENTITY_CLAIM);
                prepStmt.setString(2, String.valueOf(lookupMin));
                prepStmt.setString(3, String.valueOf(lookupMax));
                prepStmt.setInt(4, IdentityTenantUtil.getTenantId(tenantDomain));
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String userName = resultSet.getString(1);
                        if (StringUtils.isNotBlank(userName)) {
                            String[] claims = new String[2];
                            claims[0] = NotificationConstants.FIRST_NAME_CLAIM;
                            claims[1] = NotificationConstants.EMAIL_CLAIM;
                            UserStoreManager userStoreManager = (UserStoreManager) realmService.getTenantUserRealm
                                    (IdentityTenantUtil.getTenantId(tenantDomain)).getUserStoreManager();

                            if (userStoreDomain != null &&
                                    userStoreDomain.equalsIgnoreCase(UserCoreUtil.extractDomainFromName(userName))) {

                                Map<String, String> map = userStoreManager.getUserClaimValues(userName, claims, null);
                                NotificationReceiver receiver = new NotificationReceiver();
                                receiver.setEmail(map.get(NotificationConstants.EMAIL_CLAIM));
                                receiver.setUsername(UserCoreUtil.removeDomainFromName(userName));
                                receiver.setFirstName(map.get(NotificationConstants.FIRST_NAME_CLAIM));
                                receiver.setUserStoreDomain(userStoreDomain);

                                long lastLoginTime = Long.parseLong(resultSet.getString(2));
                                long expireDate = lastLoginTime + TimeUnit.DAYS.toMillis(delayForSuspension);
                                receiver.setExpireDate(new SimpleDateFormat("dd-MM-yyyy").format(new Date(expireDate)));
                                users.add(receiver);
                            }
                        }
                    }
                }
                IdentityDatabaseUtil.commitTransaction(connection);
                return users;
            }
        } catch (SQLException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while running the sql query: " + sqlStmt);
            }
            throw new AccountSuspensionNotificationException(e.getMessage(), e);
        } catch (UserStoreException e) {
            throw new AccountSuspensionNotificationException(e.getMessage(), e);
        }
    }
}
