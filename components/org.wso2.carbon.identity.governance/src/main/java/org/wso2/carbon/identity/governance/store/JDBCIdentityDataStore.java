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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 */
public class JDBCIdentityDataStore extends InMemoryIdentityDataStore {

    private static final Log log = LogFactory.getLog(JDBCIdentityDataStore.class);

    private static final String QUERY_FILTER_STRING_ANY = "*";
    private static final String SQL_FILTER_STRING_ANY = "%";

    @Override
    public void store(UserIdentityClaim userIdentityDTO, UserStoreManager userStoreManager)
            throws IdentityException {

        if (userIdentityDTO == null || userIdentityDTO.getUserIdentityDataMap().isEmpty()) {
            return;
        }

        // Putting into cache
        String userName = userIdentityDTO.getUserName();
        String domainName = ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).getRealmConfiguration().
                getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        userName = UserCoreUtil.addDomainToName(userName, domainName);
        userIdentityDTO.setUserName(userName);

        super.store(userIdentityDTO, userStoreManager);

        int tenantId = MultitenantConstants.SUPER_TENANT_ID;
        try {
            tenantId = userStoreManager.getTenantId();
        } catch (UserStoreException e) {
            log.error("Error while getting tenant Id.", e);
        }

        Map<String, String> data = userIdentityDTO.getUserIdentityDataMap();

        Connection connection = IdentityDatabaseUtil.getDBConnection();
        try {
            Map<String, String> existingDataValues = getUserDataValues(connection, userName, tenantId);
            Map<String, String> newClaims = new HashMap<>();
            Map<String, String> availableClaims = new HashMap<>();

            // Divide claim list to already available claims (need to update those) and new claims (need to add those)
            for (Map.Entry<String, String> entry : data.entrySet()) {
                String key = entry.getKey();
                String value = entry.getValue();
                if (existingDataValues.containsKey(key)) {
                    String existingValue = existingDataValues.get(key);
                    if (existingValue == null || !existingValue.equals(value)) {
                        if (log.isDebugEnabled()) {
                            log.debug("Key:" + key + ", Value:" + value + " to be updated for user:" + userName
                                    + " in JDBCIdentityDataStore");
                        }
                        availableClaims.put(key, value);
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("Key:" + key + ", Value:" + value + " to be added for user:" + userName + " in "
                                + "JDBCIdentityDataStore");
                    }
                    newClaims.put(key, value);
                }
            }

            addUserDataValues(connection, userName, tenantId, newClaims);
            updateUserDataValues(connection, userName, tenantId, availableClaims);

            IdentityDatabaseUtil.commitTransaction(connection);
        } catch (SQLException e) {
            IdentityDatabaseUtil.rollbackTransaction(connection);
            log.error("Error while persisting user identity data", e);
        } finally {
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    private Map<String, String> getUserDataValues(Connection connection, String userName, int tenantId)
            throws SQLException {

        Map<String, String> dataValues = new HashMap<>();
        PreparedStatement prepStmt = null;
        ResultSet results = null;
        try {
            boolean isUsernameCaseSensitive = IdentityUtil.isUserStoreInUsernameCaseSensitive(userName, tenantId);
            String query;
            if (isUsernameCaseSensitive) {
                query = SQLQuery.LOAD_USER_DATA;
            } else {
                query = SQLQuery.LOAD_USER_DATA_CASE_INSENSITIVE;
            }
            prepStmt = connection.prepareStatement(query);
            prepStmt.setInt(1, tenantId);
            prepStmt.setString(2, userName);
            results = prepStmt.executeQuery();
            while (results.next()) {
                dataValues.put(results.getString(1), results.getString(2));
            }
        } finally {
            IdentityDatabaseUtil.closeResultSet(results);
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
        return dataValues;
    }
    private void addUserDataValues(Connection connection, String userName, int tenantId,
                                   Map<String, String> properties) throws SQLException {

        PreparedStatement prepStmt = null;
        try {
            prepStmt = connection.prepareStatement(SQLQuery.STORE_USER_DATA);
            for (Map.Entry<String, String> entry : properties.entrySet()) {
                prepStmt.setInt(1, tenantId);
                prepStmt.setString(2, userName);
                prepStmt.setString(3, entry.getKey());
                prepStmt.setString(4, entry.getValue());
                prepStmt.addBatch();
            }
            prepStmt.executeBatch();
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
    }

    private void updateUserDataValues(Connection connection, String userName, int tenantId,
                                      Map<String, String> properties) throws SQLException {

        PreparedStatement prepStmt = null;
        boolean isUsernameCaseSensitive = IdentityUtil.isUserStoreInUsernameCaseSensitive(userName, tenantId);
        String query;
        if (isUsernameCaseSensitive) {
            query = SQLQuery.UPDATE_USER_DATA;
        } else {
            query = SQLQuery.UPDATE_USER_DATA_CASE_INSENSITIVE;
        }
        try {
            prepStmt = connection.prepareStatement(query);
            for (Map.Entry<String, String> entry : properties.entrySet()) {
                prepStmt.setString(1, entry.getValue());
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, userName);
                prepStmt.setString(4, entry.getKey());
                prepStmt.addBatch();
            }
            prepStmt.executeBatch();
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
    }

    @Override
    public UserIdentityClaim load(String userName, UserStoreManager userStoreManager) {

        String domainName = ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).getRealmConfiguration().
                getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        userName = UserCoreUtil.addDomainToName(userName, domainName);

        // Getting from cache
        UserIdentityClaim dto = super.load(userName, userStoreManager);
        if (dto != null) {
            return dto;
        }

        Connection connection = IdentityDatabaseUtil.getDBConnection();
        try {
            int tenantId = userStoreManager.getTenantId();
            Map<String, String> data = getUserDataValues(connection, userName, tenantId);
            IdentityDatabaseUtil.commitTransaction(connection);
            if (log.isDebugEnabled()) {
                log.debug("Retrieved identity data for:" + tenantId + ":" + userName);
                for (Map.Entry<String, String> dataEntry : data.entrySet()) {
                    log.debug(dataEntry.getKey() + " : " + dataEntry.getValue());
                }
            }
            dto = new UserIdentityClaim(userName, data);
            dto.setTenantId(tenantId);
            try {
                super.store(dto, userStoreManager);
            } catch (IdentityException e) {
                log.error("Error while reading user identity data", e);
            }
            return dto;
        } catch (SQLException | UserStoreException e) {
            IdentityDatabaseUtil.rollbackTransaction(connection);
            log.error("Error while reading user identity data", e);
        } finally {
            IdentityDatabaseUtil.closeConnection(connection);
        }

        return null;
    }

    @Override
    public void remove(String userName, UserStoreManager userStoreManager) throws IdentityException {

        super.remove(userName, userStoreManager);
        String domainName = ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).
                getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        userName = UserCoreUtil.addDomainToName(userName, domainName);
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        PreparedStatement prepStmt = null;
        try {
            int tenantId = userStoreManager.getTenantId();
            boolean isUsernameCaseSensitive = IdentityUtil.isUserStoreInUsernameCaseSensitive(userName, tenantId);
            String query;
            if (isUsernameCaseSensitive) {
                query = SQLQuery.DELETE_USER_DATA;
            } else {
                query = SQLQuery.DELETE_USER_DATA_CASE_INSENSITIVE;
            }
            prepStmt = connection.prepareStatement(query);
            prepStmt.setInt(1, tenantId);
            prepStmt.setString(2, userName);
            prepStmt.execute();
            IdentityDatabaseUtil.commitTransaction(connection);
        } catch (SQLException | UserStoreException e) {
            IdentityDatabaseUtil.rollbackTransaction(connection);
            throw IdentityException.error("Error while reading user identity data", e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public List<String> list(String claimUri, String claimValue,
                             org.wso2.carbon.user.core.UserStoreManager userStoreManager) throws IdentityException {

        List<String> userNames = new ArrayList<>();

        if (claimValue.contains(QUERY_FILTER_STRING_ANY)) {
            // This is to support LDAP like queries. Value having only * is restricted except one *.
            if (!claimValue.matches("(\\*)\\1+")) {
                // Convert all the * to % except \*.
                claimValue = claimValue.replaceAll("(?<!\\\\)\\*", SQL_FILTER_STRING_ANY);
            }
        }

        try (Connection connection = IdentityDatabaseUtil.getDBConnection()) {

            // We are limiting users for current tenant and user store domain.
            int tenantId = userStoreManager.getTenantId();
            String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());

            String query = SQLQuery.LIST_USERS_FROM_CLAIM;

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, claimUri);
                preparedStatement.setString(2, claimValue);
                preparedStatement.setInt(3, tenantId);

                // If the user has a domain, domain name is appended to the user name in the user name column. If
                // we need to select user names only for current domain, we have to do a SQL like.
                String userNameWithDomain;
                if (StringUtils.equalsIgnoreCase(userStoreDomain, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME)) {
                    userNameWithDomain = SQL_FILTER_STRING_ANY;
                } else {
                    userNameWithDomain = userStoreDomain + UserCoreConstants.DOMAIN_SEPARATOR + SQL_FILTER_STRING_ANY;
                }

                preparedStatement.setString(4, userNameWithDomain);

                if (log.isDebugEnabled()) {
                    log.debug("Listing users with claim URI: " + claimUri + " with value: " + claimValue +
                            " having username pattern: " + userNameWithDomain + " in tenant: " + tenantId);
                }

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next()) {
                        userNames.add(resultSet.getString("USER_NAME"));
                    }
                }
                IdentityDatabaseUtil.commitTransaction(connection);
            } catch (SQLException e) {
                IdentityDatabaseUtil.rollbackTransaction(connection);
            }
        } catch (SQLException | UserStoreException e) {
            throw new IdentityException("Error occurred while retrieving users from claim URI: " + claimUri, e);
        }

        return userNames;
    }

    /**
     * This class contains the SQL queries.
     * Schem:
     * ||TENANT_ID || USERR_NAME || DATA_KEY || DATA_VALUE ||
     * The primary key is tenantId, userName, DatKey combination
     */
    private static class SQLQuery {
        public static final String STORE_USER_DATA = "INSERT INTO IDN_IDENTITY_USER_DATA (TENANT_ID, USER_NAME, " +
                "DATA_KEY, DATA_VALUE) VALUES (?,?,?,?)";

        public static final String UPDATE_USER_DATA = "UPDATE IDN_IDENTITY_USER_DATA SET DATA_VALUE=? WHERE " +
                "TENANT_ID=? AND USER_NAME=? AND DATA_KEY=?";
        public static final String UPDATE_USER_DATA_CASE_INSENSITIVE = "UPDATE IDN_IDENTITY_USER_DATA SET " +
                "DATA_VALUE=? WHERE TENANT_ID=? AND LOWER(USER_NAME)=LOWER(?) AND DATA_KEY=?";

        public static final String LOAD_USER_DATA = "SELECT DATA_KEY, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                "TENANT_ID = ? AND USER_NAME = ?";
        public static final String LOAD_USER_DATA_CASE_INSENSITIVE = "SELECT " + "DATA_KEY, DATA_VALUE FROM " +
                "IDN_IDENTITY_USER_DATA WHERE TENANT_ID = ? AND LOWER(USER_NAME) = LOWER(?)";

        public static final String DELETE_USER_DATA = "DELETE FROM IDN_IDENTITY_USER_DATA WHERE " +
                "TENANT_ID = ? AND USER_NAME = ?";
        public static final String DELETE_USER_DATA_CASE_INSENSITIVE = "DELETE FROM IDN_IDENTITY_USER_DATA WHERE " +
                "TENANT_ID = ? AND LOWER(USER_NAME) = LOWER(?)";

        static final String LIST_USERS_FROM_CLAIM =
                "SELECT DISTINCT USER_NAME " +
                "FROM IDN_IDENTITY_USER_DATA " +
                "WHERE DATA_KEY = ? AND DATA_VALUE LIKE ? AND TENANT_ID = ? AND USER_NAME LIKE ?";

        private SQLQuery() {
        }
    }
}
