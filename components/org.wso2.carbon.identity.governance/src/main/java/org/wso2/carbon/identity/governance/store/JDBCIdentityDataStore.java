/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.model.ExpressionOperation;
import org.wso2.carbon.user.core.model.SqlBuilder;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.dbcreator.DatabaseCreator;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 *
 */
public class JDBCIdentityDataStore extends InMemoryIdentityDataStore {

    private static final Log log = LogFactory.getLog(JDBCIdentityDataStore.class);

    private static final String QUERY_FILTER_STRING_ANY = "*";
    private static final String SQL_FILTER_STRING_ANY = "%";
    private static final String QUERY_BINDING_SYMBOL = "?";
    private static final String USER_NAME = "USER_NAME";
    private static final String DB2 = "db2";
    private static final String MSSQL = "mssql";
    private static final String ORACLE = "oracle";
    private static final String POSTGRE_SQL = "postgresql";
    private static final String MYSQL = "mysql";

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

    @Override
    public List<String> listPaginatedUsersNames(List<ExpressionCondition> identityClaimFilterExpressionConditions,
                                                List<String> identityClaimFilteredUserNames, String domain,
                                                org.wso2.carbon.user.core.UserStoreManager userStoreManager,
                                                int limit, int offset) throws IdentityException {

        try {
            int tenantId = userStoreManager.getTenantId();

            /*
             * Separate claim filter conditions by operation:
             *   - NOT_EQUAL (ne) requires a special strategy to include users not present in the identity store.
             *   - Other operations (sw, ew, co, etc.) use the standard identity-store query.
             */
            List<ExpressionCondition> notEqualConditions = new ArrayList<>();
            List<ExpressionCondition> otherConditions = new ArrayList<>();
            separateConditionsByOperation(identityClaimFilterExpressionConditions, notEqualConditions, otherConditions);

            List<String> filteredUserNames;
            if (notEqualConditions.isEmpty()) {
                // Perform users filtering based on the users only in Identity store.
                filteredUserNames = getFilteredUsernamesWithoutNotEqualConditions(otherConditions, domain, tenantId,
                        limit, offset, false);
            } else {
                // Perform users filtering based on the users in Identity Store and User Store.
                filteredUserNames = getFilteredUsernamesWithNotEqualConditions(notEqualConditions, otherConditions,
                        domain, tenantId, userStoreManager, limit, offset);
            }
            identityClaimFilteredUserNames.addAll(filteredUserNames);
            return identityClaimFilteredUserNames;
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityException("Error occurred while retrieving users.", e);
        }
    }

    @Override
    public List<String> getUserNamesByClaimURINotEqualValue(Condition condition, String claimUri, String claimValue,
                                                            org.wso2.carbon.user.core.UserStoreManager userStoreManager)
            throws IdentityException {

        String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());

        // Get all users from the user store for the specified domain.
        List<String> allUsers = getAllUsernamesFromUserStore(userStoreManager, userStoreDomain);

        /*
         * Get users from the identity store whose claimUri exactly matches claimValue.
         * These are the users we want to exclude from the final result.
         */
        List<String> usersWithClaimValue = list(claimUri, claimValue, userStoreManager);

        /*
         * Exclude users with the specified claim value from the complete list.
         * This stratergy of performing NE operation filtering is required to include users who are not present
         * in the identity store.
         */
        Set<String> usersWithClaimValueSet = new HashSet<>(usersWithClaimValue);
        return allUsers.stream()
                .filter(user -> !usersWithClaimValueSet.contains(user))
                .collect(Collectors.toList());
    }

    @Override
    public List<String> getUserNamesLessThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        String sqlStmt = SQLQuery.FILTER_USERS_BY_DATA_KEY_LESS_THAN_DATA_VALUE;
        List<String> userNames = new ArrayList<>();
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, claimURI);
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, claimValue);
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String username = resultSet.getString(1);
                        userNames.add(username);
                    }
                }
                IdentityDatabaseUtil.commitTransaction(connection);
                return userNames;
            }
        } catch (SQLException e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
    }

    @Override
    public List<String> getUserNamesMoreThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        String sqlStmt = SQLQuery.FILTER_USERS_BY_DATA_KEY_MORE_THAN_DATA_VALUE;
        List<String> userNames = new ArrayList<>();
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, claimURI);
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, claimValue);
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String username = resultSet.getString(1);
                        userNames.add(username);
                    }
                }
                IdentityDatabaseUtil.commitTransaction(connection);
                return userNames;
            }
        } catch (SQLException e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
    }

    @Override
    public  List<String> getUserNamesBetweenProvidedClaimValues(String claimURI, String startValue, String endValue,
                                                                int tenantId) throws IdentityException {

        String sqlStmt = SQLQuery.FILTER_USERS_BY_DATA_KEY_LESS_THAN_AND_GREATER_THAN_DATA_VALUES;
        List<String> userNames = new ArrayList<>();
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, claimURI);
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, endValue);
                prepStmt.setString(4, startValue);
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String username = resultSet.getString(1);
                        userNames.add(username);
                    }
                }
                IdentityDatabaseUtil.commitTransaction(connection);
                return userNames;
            }
        } catch (SQLException e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
    }

    @Override
    public List<String> getUserNamesLessThanClaimWithNestedClaim(String claimURI, String claimValue,
                                                                 String nestedClaimURI,
                                                                 String nestedClaimValue, int tenantId,
                                                                 boolean isIncluded)
            throws IdentityException {

        String sqlStmt = SQLQuery.FILTER_USERS_BY_DATA_KEY_LESS_THAN_DATA_VALUE;
        String subSqlStmt = SQLQuery.LIST_USERS_FROM_CLAIM;
        if (isIncluded) {
            sqlStmt = sqlStmt + " AND USER_NAME IN (" + subSqlStmt + ")";
        } else {
            sqlStmt = sqlStmt + " AND USER_NAME NOT IN (" + subSqlStmt + ")";
        }

        List<String> userNames = new ArrayList<>();
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(false)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, claimURI);
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, claimValue);
                prepStmt.setString(4, nestedClaimURI);
                prepStmt.setString(5, nestedClaimValue);
                prepStmt.setInt(6, tenantId);
                prepStmt.setString(7, "%");
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String username = resultSet.getString(1);
                        userNames.add(username);
                    }
                }
                return userNames;
            }
        } catch (SQLException e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
    }

    @Override
    public List<String> getUserNamesBetweenGivenClaimsWithNestedClaim(String claimURI, String startValue,
                                                                    String endValue,
                                                                    String nestedClaimURI,
                                                                    String nestedClaimValue, int tenantId,
                                                                    boolean isIncluded)
            throws IdentityException {

        String sqlStmt = SQLQuery.FILTER_USERS_BY_DATA_KEY_LESS_THAN_AND_GREATER_THAN_DATA_VALUES;
        String subSqlStmt = SQLQuery.LIST_USERS_FROM_CLAIM;
        if (isIncluded) {
            sqlStmt = sqlStmt + " AND USER_NAME IN (" + subSqlStmt + ")";
        } else {
            sqlStmt = sqlStmt + " AND USER_NAME NOT IN (" + subSqlStmt + ")";
        }
        List<String> userNames = new ArrayList<>();
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(false)) {
            try (PreparedStatement prepStmt = connection.prepareStatement(sqlStmt)) {
                prepStmt.setString(1, claimURI);
                prepStmt.setInt(2, tenantId);
                prepStmt.setString(3, endValue);
                prepStmt.setString(4, startValue);
                prepStmt.setString(5, nestedClaimURI);
                prepStmt.setString(6, nestedClaimValue);
                prepStmt.setInt(7, tenantId);
                prepStmt.setString(8, "%");
                try (ResultSet resultSet = prepStmt.executeQuery()) {
                    while (resultSet.next()) {
                        String username = resultSet.getString(1);
                        userNames.add(username);
                    }
                }
                return userNames;
            }
        } catch (SQLException e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
    }

    private void populatePrepareStatement(SqlBuilder sqlBuilder, PreparedStatement prepStmt, int startIndex,
                                          int endIndex) throws SQLException {

        Map<Integer, Integer> integerParameters = sqlBuilder.getIntegerParameters();
        Map<Integer, String> stringParameters = sqlBuilder.getStringParameters();
        Map<Integer, Long> longParameters = sqlBuilder.getLongParameters();

        integerParameters.forEach((key, value) -> {
            if (key > startIndex && key <= endIndex) {
                try {
                    prepStmt.setInt(key - startIndex, value);
                } catch (SQLException e) {
                    throw new RuntimeException("Error occurred while populating parameters for a prepared " +
                            "statement.", e);
                }
            }
        });

        stringParameters.forEach((key, value) -> {
            if (key > startIndex && key <= endIndex) {
                try {
                    prepStmt.setString(key - startIndex, value);
                } catch (SQLException e) {
                    throw new RuntimeException("Error occurred while populating parameters for a prepared " +
                            "statement.", e);
                }
            }
        });

        longParameters.forEach((key, value) -> {
            if (key > startIndex && key <= endIndex) {
                try {
                    prepStmt.setLong(key - startIndex, value);
                } catch (SQLException e) {
                    throw new RuntimeException("Error occurred while populating parameters for a prepared " +
                            "statement.", e);
                }
            }
        });
    }

    private SqlBuilder getQueryString(List<ExpressionCondition> identityClaimFilterExpressionConditions,
                                      int limit, int offset, String userStoreDomain, int tenantID, String dbType) {

        boolean hitClaimFilter = false;
        String userNameWithDomain;
        StringBuilder sqlStatement = new StringBuilder("SELECT DISTINCT USER_NAME FROM IDN_IDENTITY_USER_DATA ");
        SqlBuilder sqlBuilder = new SqlBuilder(sqlStatement);
        sqlBuilder.where("TENANT_ID = ? ", tenantID);

        if (StringUtils.equalsIgnoreCase(userStoreDomain, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME)) {
            userNameWithDomain = SQL_FILTER_STRING_ANY + UserCoreConstants.DOMAIN_SEPARATOR + SQL_FILTER_STRING_ANY;
            sqlBuilder.where(" USER_NAME NOT LIKE ? ", userNameWithDomain);
        } else {
            userNameWithDomain =
                    userStoreDomain.toUpperCase() + UserCoreConstants.DOMAIN_SEPARATOR + SQL_FILTER_STRING_ANY;
            sqlBuilder.where(" USER_NAME LIKE ? ", userNameWithDomain);
        }

        SqlBuilder header = new SqlBuilder(new StringBuilder(sqlBuilder.getSql()));
        addingWheres(sqlBuilder, header);

        for (ExpressionCondition expressionCondition : identityClaimFilterExpressionConditions) {
            String claimValue = expressionCondition.getAttributeValue();
            if (claimValue.contains(QUERY_FILTER_STRING_ANY)) {
                // This is to support LDAP like queries. Value having only * is restricted except one *.
                if (!claimValue.matches("(\\*)\\1+")) {
                    // Convert all the * to % except \*.
                    claimValue = claimValue.replaceAll("(?<!\\\\)\\*", SQL_FILTER_STRING_ANY);
                }
            }
            // Adding filter claims.
            multiClaimQueryBuilder(sqlBuilder, header, hitClaimFilter, expressionCondition);
            hitClaimFilter = true;
        }

        if (DB2.equals(dbType)) {
            sqlBuilder.setTail(" ORDER BY USER_NAME LIMIT ? , ? ", limit, offset);
        } else if (MSSQL.equals(dbType)) {
            sqlBuilder.setTail(" ORDER BY USER_NAME OFFSET ? ROWS FETCH NEXT ? ROWS ONLY ", offset, limit);
        } else if (ORACLE.equals(dbType)) {
            sqlBuilder.setTail(" ORDER BY USER_NAME OFFSET ? ROWS FETCH NEXT ? ROWS ONLY ", offset, limit);
        } else if (POSTGRE_SQL.equals(dbType)) {
            sqlBuilder.setTail(" ORDER BY USER_NAME OFFSET ? ROWS FETCH NEXT ? ROWS ONLY ", offset, limit);
        } else {
            sqlBuilder.setTail(" ORDER BY USER_NAME ASC LIMIT ? OFFSET ?", limit, offset);
        }

        return sqlBuilder;
    }

    /**
     * Builds an SQL string that returns usernames when at least one of the given filter conditions is met.
     *
     * @param identityClaimFilterExpressionConditions List of conditions to apply (joined with OR).
     * @param userStoreDomain                         User store domain to include or exclude.
     * @param tenantID                                Tenant identifier to filter by.
     * @return a SqlBuilder initialized with the complete SQL string and parameters.
     */
    private SqlBuilder getQueryStringWithOROperator(List<ExpressionCondition> identityClaimFilterExpressionConditions,
                                                    String userStoreDomain, int tenantID) {

        boolean hitClaimFilter = false;
        String userNameWithDomain;
        StringBuilder sqlStatement = new StringBuilder("SELECT DISTINCT USER_NAME FROM IDN_IDENTITY_USER_DATA ");
        SqlBuilder sqlBuilder = new SqlBuilder(sqlStatement);
        sqlBuilder.where("TENANT_ID = ? ", tenantID);

        if (StringUtils.equalsIgnoreCase(userStoreDomain, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME)) {
            userNameWithDomain = SQL_FILTER_STRING_ANY + UserCoreConstants.DOMAIN_SEPARATOR + SQL_FILTER_STRING_ANY;
            sqlBuilder.where(" USER_NAME NOT LIKE ? ", userNameWithDomain);
        } else {
            userNameWithDomain =
                    userStoreDomain.toUpperCase() + UserCoreConstants.DOMAIN_SEPARATOR + SQL_FILTER_STRING_ANY;
            sqlBuilder.where(" USER_NAME LIKE ? ", userNameWithDomain);
        }

        sqlBuilder.updateSql(" AND (");
        for (ExpressionCondition expressionCondition : identityClaimFilterExpressionConditions) {
            String claimValue = expressionCondition.getAttributeValue();
            if (claimValue.contains(QUERY_FILTER_STRING_ANY)) {
                // This is to support LDAP like queries. Value having only * is restricted except one *.
                if (!claimValue.matches("(\\*)\\1+")) {
                    // Convert all the * to % except \*.
                    claimValue = claimValue.replaceAll("(?<!\\\\)\\*", SQL_FILTER_STRING_ANY);
                }
            }
            buildClaimWhereConditionsWithOROperator(sqlBuilder, expressionCondition.getAttributeName(),
                    expressionCondition.getOperation(), claimValue, hitClaimFilter);
            hitClaimFilter = true;
        }
        sqlBuilder.updateSql(")");
        sqlBuilder.updateSql(" ORDER BY USER_NAME ASC");

        return sqlBuilder;
    }

    private void buildClaimWhereConditions(SqlBuilder sqlBuilder, String attributeName, String operation,
                                           String attributeValue) {

        sqlBuilder.where("DATA_KEY = ?", attributeName);
        if (ExpressionOperation.EQ.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE = ?", attributeValue);
        } else if (ExpressionOperation.EW.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE LIKE ?", "%" + attributeValue);
        } else if (ExpressionOperation.CO.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE LIKE ?", "%" + attributeValue + "%");
        } else if (ExpressionOperation.SW.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE LIKE ?", attributeValue + "%");
        } else if (ExpressionOperation.GE.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE >= ?", attributeValue);
        } else if (ExpressionOperation.LE.toString().equals(operation)) {
            sqlBuilder.where("DATA_VALUE <= ?", attributeValue);
        }
    }

    private void buildClaimWhereConditionsWithOROperator(SqlBuilder sqlBuilder, String attributeName, String operation,
                                                         String attributeValue, boolean isFirstCondition) {

        if (isFirstCondition) {
            sqlBuilder.updateSql(" OR ");
        }
        if (ExpressionOperation.EQ.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE = ?",
                    Arrays.asList(attributeName, attributeValue));
        } else if (ExpressionOperation.EW.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE LIKE ?",
                    Arrays.asList(attributeName, "%" + attributeValue));
        } else if (ExpressionOperation.CO.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE LIKE ?",
                    Arrays.asList(attributeName, "%" + attributeValue + "%"));
        } else if (ExpressionOperation.SW.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE LIKE ?",
                    Arrays.asList(attributeName, attributeValue + "%"));
        } else if (ExpressionOperation.GE.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE >= ?",
                    Arrays.asList(attributeName, attributeValue));
        } else if (ExpressionOperation.LE.toString().equals(operation)) {
            sqlBuilder.appendParameterizedSqlFragment("DATA_KEY = ? AND DATA_VALUE <= ?",
                    Arrays.asList(attributeName, attributeValue));
        }
    }

    private void addingWheres(SqlBuilder baseSqlBuilder, SqlBuilder newSqlBuilder) {

        for (int i = 0; i < baseSqlBuilder.getWheres().size(); i++) {

            if (baseSqlBuilder.getIntegerParameters().containsKey(i + 1)) {
                newSqlBuilder
                        .where(baseSqlBuilder.getWheres().get(i), baseSqlBuilder.getIntegerParameters().get(i + 1));

            } else if (baseSqlBuilder.getStringParameters().containsKey(i + 1)) {
                newSqlBuilder.where(baseSqlBuilder.getWheres().get(i), baseSqlBuilder.getStringParameters().get(i + 1));

            } else if (baseSqlBuilder.getIntegerParameters().containsKey(i + 1)) {
                newSqlBuilder.where(baseSqlBuilder.getWheres().get(i), baseSqlBuilder.getLongParameters().get(i + 1));
            }
        }
    }

    private void multiClaimQueryBuilder(SqlBuilder sqlBuilder, SqlBuilder header, boolean hitFirstRound,
                                        ExpressionCondition expressionCondition) {

        // Taking intersection for multi-attribute filtering.
        if (hitFirstRound) {
            sqlBuilder.updateSql(" INTERSECT " + header.getSql());
            addingWheres(header, sqlBuilder);
            buildClaimWhereConditions(sqlBuilder, expressionCondition.getAttributeName(),
                    expressionCondition.getOperation(), expressionCondition.getAttributeValue());
        } else {
            buildClaimWhereConditions(sqlBuilder, expressionCondition.getAttributeName(),
                    expressionCondition.getOperation(), expressionCondition.getAttributeValue());
        }
    }

    /**
     * Separates identity claim filter conditions into two lists:
     * - notEqualConditions: Contains conditions with the NE (not equal) operation.
     * - otherConditions: Contains all other conditions.
     *
     * @param identityClaimFilterExpressionConditions List of expression conditions to separate.
     * @param notEqualConditions                      List to hold conditions with NE operation.
     * @param otherConditions                         List to hold all other conditions.
     */
    private void separateConditionsByOperation(List<ExpressionCondition> identityClaimFilterExpressionConditions,
                                               List<ExpressionCondition> notEqualConditions,
                                               List<ExpressionCondition> otherConditions) {

        for (ExpressionCondition condition : identityClaimFilterExpressionConditions) {
            if (ExpressionOperation.NE.toString().equals(condition.getOperation())) {
                notEqualConditions.add(condition);
            } else {
                otherConditions.add(condition);
            }
        }
    }

    /**
     * Retrieves all usernames from the user store, considering the specified domain.
     *
     * @param userStoreManager UserStoreManager instance to interact with the user store.
     * @param domain           The user store domain to filter users by.
     * @return List of usernames from the user store for the specified domain.
     * @throws IdentityException if an error occurs while retrieving users.
     */
    private List<String> getAllUsernamesFromUserStore(org.wso2.carbon.user.core.UserStoreManager userStoreManager,
                                                      String domain) throws IdentityException {

        try {
            // Construct domain-aware filter.
            String filter = domain + UserCoreConstants.DOMAIN_SEPARATOR + "*";

            // Get users with domain filter - already returns domain-qualified usernames.
            String[] users = userStoreManager.listUsers(filter, -1);

            return users != null ? Arrays.asList(users) : new ArrayList<>();

        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityException("Error occurred while retrieving users from User Store.", e);
        }
    }

    /**
     * Paginates the user list based on the specified limit and offset.
     *
     * @param userList List of usernames to paginate.
     * @param limit    Maximum number of results to return.
     * @param offset   Starting offset for pagination.
     * @return Paginated list of usernames.
     */
    private List<String> paginateUserList(List<String> userList, int limit, int offset) {

        if (userList == null || userList.isEmpty()) {
            return new ArrayList<>();
        }

        if (offset <= 0) {
            offset = 0;
        } else {
            offset = offset - 1;
        }

        int paginationLimit;
        if (offset <= 0) {
            paginationLimit = limit;
        } else {
            paginationLimit = offset + limit;
        }

        if (offset > userList.size()) {
            return new ArrayList<>();
        }

        if (userList.size() < paginationLimit) {
            return new ArrayList<>(userList.subList(offset, userList.size()));
        } else {
            return new ArrayList<>(userList.subList(offset, paginationLimit));
        }
    }

    /**
     * Retrieves filtered usernames based on the provided conditions without not-equal operators.
     *
     * @param conditionsWithoutNotEqualOperator List of expression conditions (non not-equal).
     * @param domain                            User store domain.
     * @param tenantId                          Tenant ID.
     * @param limit                             Maximum number of results to return.
     * @param offset                            Starting offset for pagination.
     * @param combineConditionsWithOR           Whether to combine conditions with OR logic (true) or AND logic (false).
     * @return List of user names that match the conditions.
     * @throws IdentityException if database operation fails.
     */
    private List<String> getFilteredUsernamesWithoutNotEqualConditions(
            List<ExpressionCondition> conditionsWithoutNotEqualOperator, String domain, int tenantId, int limit,
            int offset, boolean combineConditionsWithOR) throws IdentityException {

        List<String> userNames = new ArrayList<>();

        try (Connection connection = IdentityDatabaseUtil.getDBConnection()) {
            // Based on the DB Type might need to extend support.
            String dBType = DatabaseCreator.getDatabaseType(connection);

            if (offset <= 0) {
                offset = 0;
            } else {
                offset = offset - 1;
            }

            SqlBuilder sqlBuilder;
            if (combineConditionsWithOR) {
                sqlBuilder = getQueryStringWithOROperator(conditionsWithoutNotEqualOperator, domain, tenantId);
            } else {
                sqlBuilder = getQueryString(conditionsWithoutNotEqualOperator, limit, offset, domain, tenantId, dBType);
            }
            String fullQuery = sqlBuilder.getQuery();
            int occurrence = StringUtils.countMatches(fullQuery, QUERY_BINDING_SYMBOL);

            try (PreparedStatement preparedStatement = connection.prepareStatement(fullQuery)) {

                populatePrepareStatement(sqlBuilder, preparedStatement, 0, occurrence);
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next()) {
                        userNames.add(resultSet.getString(USER_NAME));
                    }
                    IdentityDatabaseUtil.commitTransaction(connection);
                } catch (SQLException e) {
                    if (log.isDebugEnabled()) {
                        log.debug("Error occurred while retrieving users from Identity Store for " + domain +
                                "with limit " + limit + "and offset " + offset, e);
                    }
                    IdentityDatabaseUtil.rollbackTransaction(connection);
                }
            } catch (SQLException e) {
                throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
            }
        } catch (Exception e) {
            throw new IdentityException("Error occurred while retrieving users from Identity Store.", e);
        }
        return userNames;
    }

    /**
     * Retrieves filtered usernames based on the provided not-equal conditions and other conditions (ew, sw, co, etc.).
     *
     * @param notEqualConditions List of expression conditions with NOT_EQUAL operation.
     * @param otherConditions    List of expression conditions without NOT_EQUAL operation.
     * @param domain             User store domain.
     * @param tenantId           Tenant ID.
     * @param userStoreManager   UserStoreManager instance to interact with the user store.
     * @param limit              Maximum number of results to return.
     * @param offset             Starting offset for pagination.
     * @return List of usernames that match the conditions.
     * @throws IdentityException if an error occurs while retrieving users.
     */
    private List<String> getFilteredUsernamesWithNotEqualConditions(List<ExpressionCondition> notEqualConditions,
                                                                    List<ExpressionCondition> otherConditions,
                                                                    String domain, int tenantId,
                                                                    org.wso2.carbon.user.core.UserStoreManager
                                                                            userStoreManager, int limit, int offset)
            throws IdentityException {

        /*
         * Initialize the base set of user names.
         * If there are any filters other than NE, fetch those users first;
         * otherwise, load all users from the user store.
         * This avoids pulling the entire user store when other filters apply.
         */
        List<String> filteredUserNames;
        if (!otherConditions.isEmpty()) {
            filteredUserNames = getFilteredUsernamesWithoutNotEqualConditions(
                    otherConditions, domain, tenantId, Integer.MAX_VALUE, 0, false);
        } else {
            filteredUserNames = getAllUsernamesFromUserStore(userStoreManager, domain);
        }

        /*
         * Handle NOT_EQUAL conditions by converting them to EQUAL for querying.
         * First find users who DO match these conditions, then exclude them.
         */
        List<ExpressionCondition> equalConditions = new ArrayList<>();
        for (ExpressionCondition neCondition : notEqualConditions) {
            ExpressionCondition eqCondition = new ExpressionCondition(
                    ExpressionOperation.EQ.toString(),
                    neCondition.getAttributeName(),
                    neCondition.getAttributeValue()
            );
            equalConditions.add(eqCondition);
        }

        /*
         * Get users who atleast satisfy one of the converted EQUAL conditions (using OR logic).
         * These are the users we need to EXCLUDE from our result set.
         */
        List<String> equalConditionsFilteredUserNames = getFilteredUsernamesWithoutNotEqualConditions(
                equalConditions, domain, tenantId, Integer.MAX_VALUE, 0, true);

        // From the initial filtered user names, exclude users who match the NOT_EQUAL conditions' values.
        Set<String> equalConditionsSet = new HashSet<>(equalConditionsFilteredUserNames);
        filteredUserNames = filteredUserNames.stream()
                .filter(user -> !equalConditionsSet.contains(user))
                .collect(Collectors.toList());

        return paginateUserList(filteredUserNames, limit, offset);
    }

    /**
     * This class contains the SQL queries.
     * Schema:
     * ||TENANT_ID || USER_NAME || DATA_KEY || DATA_VALUE ||
     * The primary key is tenantId, userName, DataKey combination
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

        public static final String FILTER_USERS_BY_DATA_KEY_LESS_THAN_DATA_VALUE =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ?";

        public static final String FILTER_USERS_BY_DATA_KEY_MORE_THAN_DATA_VALUE =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE > ?";

        public static final String FILTER_USERS_BY_DATA_KEY_LESS_THAN_AND_GREATER_THAN_DATA_VALUES =
                "SELECT USER_NAME, DATA_VALUE FROM IDN_IDENTITY_USER_DATA WHERE " +
                        "DATA_KEY = ? AND TENANT_ID = ? AND DATA_VALUE < ? AND DATA_VALUE > ?";

        private SQLQuery() {
        }
    }
}
