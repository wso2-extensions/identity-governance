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
package org.wso2.carbon.identity.account.suspension.notification.task.jdbc;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrieval;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.DatabaseUtil;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class JDBCNotificationReceiversRetrieval implements NotificationReceiversRetrieval {

    private static final Log log = LogFactory.getLog(JDBCNotificationReceiversRetrieval.class);
    private RealmConfiguration realmConfiguration = null;

    @Override
    public void init(RealmConfiguration realmConfiguration) {
        this.realmConfiguration = realmConfiguration;
    }

    @Override
    public List<NotificationReceiver> getNotificationReceivers(long lookupMin, long lookupMax,
            long delayForSuspension, String tenantDomain) throws AccountSuspensionNotificationException {

        List<NotificationReceiver> users = new ArrayList<NotificationReceiver>();
        RealmService realmService = NotificationTaskDataHolder.getInstance().getRealmService();

        Connection dbConnection = null;
        String sqlStmt = null;
        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;

        try {
            ClaimManager claimManager = (ClaimManager) realmService.getTenantUserRealm(IdentityTenantUtil.
                    getTenantId(tenantDomain)).getClaimManager();
            String userStoreDomain = realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.
                    PROPERTY_DOMAIN_NAME);

            if (StringUtils.isBlank(userStoreDomain)) {
                userStoreDomain = IdentityUtil.getPrimaryDomainName();
            }

            String lastLoginTimeAttribute = claimManager
                    .getAttributeName(userStoreDomain, NotificationConstants.LAST_LOGIN_TIME);

            dbConnection = getDBConnection(realmConfiguration);
            sqlStmt = NotificationConstants.GET_USERS_FILTERED_BY_LAST_LOGIN_TIME;
            prepStmt = dbConnection.prepareStatement(sqlStmt);
            prepStmt.setString(1, lastLoginTimeAttribute);
            prepStmt.setString(2, String.valueOf(lookupMin));
            prepStmt.setString(3, String.valueOf(lookupMax));
            prepStmt.setString(4, String.valueOf(IdentityTenantUtil.getTenantId(tenantDomain)));
            prepStmt.setString(5, String.valueOf(IdentityTenantUtil.getTenantId(tenantDomain)));

            resultSet = prepStmt.executeQuery();

            while (resultSet.next()) {

                String userName = resultSet.getString(1);

                if (StringUtils.isNotBlank(userName)) {

                    String[] claims = new String[3];
                    claims[0] = NotificationConstants.FIRST_NAME_CLAIM;
                    claims[1] = NotificationConstants.EMAIL_CLAIM;
                    claims[2] = NotificationConstants.LAST_LOGIN_TIME;

                    UserStoreManager userStoreManager = (UserStoreManager) realmService.getTenantUserRealm(IdentityTenantUtil
                            .getTenantId(tenantDomain)).getUserStoreManager();

                    Map<String, String> map = userStoreManager.getUserClaimValues(IdentityUtil.addDomainToName
                            (userName, userStoreDomain), claims, null);

                    NotificationReceiver receiver = new NotificationReceiver();
                    receiver.setEmail(map.get(NotificationConstants.EMAIL_CLAIM));
                    receiver.setUsername(userName);
                    receiver.setFirstName(map.get(NotificationConstants.FIRST_NAME_CLAIM));
                    receiver.setUserStoreDomain(userStoreDomain);

                    long lastLoginTime = Long.parseLong(map.get(NotificationConstants.LAST_LOGIN_TIME));
                    long expireDate = lastLoginTime + TimeUnit.DAYS.toMillis(delayForSuspension);
                    receiver.setExpireDate(new SimpleDateFormat("dd-MM-yyyy").format(new Date(expireDate)));
                    users.add(receiver);
                }
            }
            dbConnection.commit();
        } catch (SQLException e) {
            DatabaseUtil.rollBack(dbConnection);
            if (log.isDebugEnabled()) {
                log.debug("Using sql : " + sqlStmt);
            }
            throw new AccountSuspensionNotificationException(e.getMessage(), e);
        } catch (Exception e) {
            throw new AccountSuspensionNotificationException(e.getMessage(), e);
        } finally {
            DatabaseUtil.closeAllConnections(dbConnection, resultSet, prepStmt);
        }
        return users;
    }

    private Connection getDBConnection(RealmConfiguration realmConfiguration) throws SQLException, UserStoreException {

        Connection dbConnection = null;
        DataSource dataSource = DatabaseUtil.createUserStoreDataSource(realmConfiguration);

        if (dataSource != null) {
            dbConnection = DatabaseUtil.getDBConnection(dataSource);
        }

        //if primary user store, DB connection can be same as realm data source.
        if (dbConnection == null && realmConfiguration.isPrimary()) {
            dbConnection = IdentityDatabaseUtil.getUserDBConnection();
        } else if (dbConnection == null) {
            throw new UserStoreException("Could not create a database connection to " + realmConfiguration
                    .getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));
        } else {
            // db connection is present
        }
        dbConnection.setAutoCommit(false);
        dbConnection.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
        return dbConnection;
    }
}
