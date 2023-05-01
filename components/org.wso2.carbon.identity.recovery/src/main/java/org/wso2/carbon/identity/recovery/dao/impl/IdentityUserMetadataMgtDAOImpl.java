package org.wso2.carbon.identity.recovery.dao.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCache;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCacheKey;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.dao.IdentityUserMetadataMgtDAO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.DatabaseUtil;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Locale;
import java.util.Map;
import javax.sql.DataSource;

/**
 * Implementation class for IdentityUserMetadataMgtDAO.
 */
public class IdentityUserMetadataMgtDAOImpl implements IdentityUserMetadataMgtDAO {

    private static final Log log = LogFactory.getLog(IdentityUserMetadataMgtDAOImpl.class);

    private final IdentityDataStoreCache identityDataStoreCache = IdentityDataStoreCache.getInstance();

    @Override
    public void updateUserMetadata(UserStoreManager userStoreManager, String username,
                                   String claimURI, String value, String eventName) throws IdentityEventException {

        if (username.contains(IdentityRecoveryConstants.TENANT_ASSOCIATION_MANAGER)) {
            return;
        }

        String tenantId;
        try {
            tenantId = String.valueOf(userStoreManager.getTenantId());
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while retrieving the tenantId of the user related to %s event.",
                                  eventName), e);
        }

        String userStoreDomain = UserCoreUtil.extractDomainFromName(username);
        if (StringUtils.isNotBlank(userStoreDomain) && IdentityRecoveryConstants.ASGARDEO_USER_DOMAIN_NAME
                .equalsIgnoreCase(userStoreDomain) && isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            // TODO: Store the last login time data to UM_USER_ATTRIBUTE table for Asgardeo users.
            username = UserCoreUtil.removeDomainFromName(username);
            storeMetadataToUserStore(userStoreManager, username, tenantId, claimURI, value, eventName);
            return;
        }

        boolean executionResult;
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(false)) {
            try {
                if (isMetadataExists(connection, tenantId, username, claimURI)) {
                    executionResult = updateExistingMetadata(connection, value, tenantId, username, claimURI);
                } else {
                    executionResult = insertNewMetadata(connection, value, tenantId, username, claimURI);
                }
            } catch (SQLException e) {
                DatabaseUtil.rollBack(connection);
                throw new IdentityEventException(String.format("Error while updating the user metadata in identity " +
                        "store related to %s event.", eventName), e);
            }
            connection.commit();
        } catch (SQLException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while updating user metadata related to %s event.", eventName), e);
        }

        if (log.isDebugEnabled() && executionResult) {
            log.debug(String.format("Successfully updated the user metadata related to %s event.", eventName));
        }
    }

    @Override
    public UserIdentityClaim loadUserMetadataFromCache(UserStoreManager userStoreManager, String userName) {

        try {
            if (userName != null) {
                userName = UserCoreUtil.removeDomainFromName(userName);
                if (!IdentityUtil.isUserStoreCaseSensitive(userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive user store found. Changing username from : " + userName +
                                " to : " + userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                } else if (!IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive username for cache key is used. Changing username from : "
                                + userName + " to : " + userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                }

                String domainName = userStoreManager.getRealmConfiguration().
                        getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

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
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            log.error("Error while obtaining tenant ID from user store manager");
        }
        return null;
    }

    @Override
    public void storeUserMetadataToCache(UserStoreManager userStoreManager, UserIdentityClaim userIdentityDTO) {

        try {
            if (userIdentityDTO != null && userIdentityDTO.getUserName() != null) {
                String userName = UserCoreUtil.removeDomainFromName(userIdentityDTO.getUserName());
                if (!IdentityUtil.isUserStoreCaseSensitive(userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive user store found. Changing username from : " + userName +
                                " to : " + userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                } else if (!IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(userStoreManager)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Case insensitive username for cache key is used. Changing username from : "
                                + userName + " to : " + userName.toLowerCase(Locale.ENGLISH));
                    }
                    userName = userName.toLowerCase(Locale.ENGLISH);
                }

                if (log.isDebugEnabled()) {
                    StringBuilder data = new StringBuilder("{");
                    if (userIdentityDTO.getUserIdentityDataMap() != null) {
                        for (Map.Entry<String, String> entry : userIdentityDTO.getUserIdentityDataMap().entrySet()) {
                            data.append("[").append(entry.getKey()).append(" = ")
                                    .append(entry.getValue()).append("], ");
                        }
                    }
                    if (data.indexOf(",") >= 0) {
                        data.deleteCharAt(data.lastIndexOf(","));
                    }
                    data.append("}");
                    log.debug("Storing UserIdentityClaimsDO to cache for user: " + userName + " with claims: " + data);
                }

                String domainName = userStoreManager.getRealmConfiguration().
                        getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

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
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            log.error("Error while obtaining tenant ID from user store manager", e);
        }
    }

    /**
     * Store user metadata in the user store.
     *
     * @param userStoreManager          User store manager
     * @param username                  Username
     * @param tenantId                  Tenant ID
     * @param claimURI                  Claim URI
     * @param value                     Claim attribute value
     * @param eventName                 Event name
     * @throws IdentityEventException   Error while checking the existence of the metadata
     */
    private void storeMetadataToUserStore(UserStoreManager userStoreManager, String username, String tenantId,
                                          String claimURI, String value, String eventName)
                                          throws IdentityEventException {

        String claimAttributeName = retrieveAttributeName(userStoreManager, tenantId, claimURI);
        boolean executionResult;
        try (Connection connection = getDBConnection(userStoreManager.getRealmConfiguration())) {
            try {
                if (isMetadataExistsInUserStore(connection, userStoreManager, tenantId, username, claimAttributeName)) {
                    // update claim in user store
                    executionResult = updateExistingMetadataInUserStore(connection, userStoreManager, tenantId,
                            username, claimAttributeName, value);
                } else {
                    // add claim to user store
                    executionResult = insertMetadataInUserStore(connection, userStoreManager, tenantId, username,
                            claimAttributeName, value);
                }
            } catch (SQLException e) {
                DatabaseUtil.rollBack(connection);
                throw new IdentityEventException(String.format("Error while updating the user metadata in user store " +
                        "related to %s event.", eventName), e);
            }
            connection.commit();
        } catch (SQLException e) {
            throw new IdentityEventException("Error while implementing database connection with user store.");
        }

        if (log.isDebugEnabled() && executionResult) {
            log.debug(String.format("Successfully updated the user metadata related to %s event.", eventName));
        }
    }

    /**
     * Check the existence of the user metadata.
     *
     * @param tenantId      Tenant Id.
     * @param username      Username.
     * @param claimURI      Claim URI.
     * @return              boolean value of the existence.
     * @throws SQLException SQL Exception.
     */
    private boolean isMetadataExists(Connection connection, String tenantId, String username, String claimURI)
            throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.LOAD_USER_METADATA;
        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, tenantId);
        prepStmt.setString(2, username);
        prepStmt.setString(3, claimURI);
        return prepStmt.executeQuery().next();
    }

    /**
     * Update existing user metadata.
     *
     * @param value         Value of the claim.
     * @param tenantId      Tenant Id.
     * @param username      Username.
     * @param claimURI      Claim URI.
     * @return              boolean value of the execution.
     * @throws SQLException SQL Exception.
     */
    private boolean updateExistingMetadata(Connection connection, String value, String tenantId, String username,
                                           String claimURI) throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.UPDATE_USER_METADATA;
        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, value);
        prepStmt.setString(2, tenantId);
        prepStmt.setString(3, username);
        prepStmt.setString(4, claimURI);
        int result = prepStmt.executeUpdate();
        return result > 0;
    }

    /**
     * Insert new user metadata.
     *
     * @param value         Value of the claim.
     * @param tenantId      Tenant Id.
     * @param username      Username.
     * @param claimURI      Claim URI.
     * @return              boolean value of the execution.
     * @throws SQLException SQL Exception.
     */
    private boolean insertNewMetadata(Connection connection, String value, String tenantId, String username,
                                      String claimURI) throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.INSERT_USER_METADATA;
        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, tenantId);
        prepStmt.setString(2, username);
        prepStmt.setString(3, claimURI);
        prepStmt.setString(4, value);
        int result = prepStmt.executeUpdate();
        return result > 0;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager  User Store manager.
     * @return                  Weather identity claims are stored in user store or not.
     */
    private boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(IdentityRecoveryConstants.STORE_IDENTITY_CLAIMS));
    }

    /**
     * Fetch relevant SQL statement from user store configuration.
     *
     * @param userStoreManager          User Store manager.
     * @param sqlStatementKey           Property name of the SQL statement.
     * @return                          SQL statement string.
     * @throws IdentityEventException   Error while fetching the SQL statement.
     */
    private String fetchSQLStatementFromUserStoreConfig(UserStoreManager userStoreManager, String sqlStatementKey)
            throws IdentityEventException {

        String sqlStmt = userStoreManager.getRealmConfiguration().getUserStoreProperty(sqlStatementKey);
        if (StringUtils.isNotBlank(sqlStmt)) {
            return sqlStmt;
        } else {
            throw new IdentityEventException("SQL statement is not configured in the user store configuration.");
        }
    }

    /**
     * Check the existence of the user metadata in user store.
     *
     * @param userStoreManager      UserStoreManager.
     * @param tenantId              Tenant Id.
     * @param username              Username.
     * @param claimAttributeName    Claim attribute name.
     * @return                      boolean value of the existence.
     * @throws SQLException         SQL Exception.
     */
    private boolean isMetadataExistsInUserStore(Connection connection, UserStoreManager userStoreManager,
                                                String tenantId, String username, String claimAttributeName)
                                                throws IdentityEventException, SQLException {

        String sqlStmt = fetchSQLStatementFromUserStoreConfig(userStoreManager,
                IdentityRecoveryConstants.SQLQueries.LOAD_USER_METADATA_FROM_USER_STORE_SQL_KEY);

        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, username);
        prepStmt.setString(2, claimAttributeName);
        prepStmt.setString(3, UserCoreConstants.DEFAULT_PROFILE);
        prepStmt.setString(4, tenantId);
        prepStmt.setString(5, tenantId);
        return prepStmt.executeQuery().next();
    }

    /**
     * Update existing user metadata in user store.
     *
     * @param userStoreManager      UserStoreManager.
     * @param tenantId              Tenant Id.
     * @param username              Username.
     * @param claimAttributeName    Claim attribute name.
     * @param value                 Value of the claim.
     * @return                      boolean value of the execution.
     * @throws SQLException         SQL Exception.
     */
    private boolean updateExistingMetadataInUserStore(Connection connection, UserStoreManager userStoreManager,
                                                      String tenantId, String username, String claimAttributeName,
                                                      String value) throws SQLException, IdentityEventException {

        String sqlStmt = fetchSQLStatementFromUserStoreConfig(userStoreManager,
                IdentityRecoveryConstants.SQLQueries.UPDATE_USER_METADATA_TO_USER_STORE_SQL_KEY);

        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, value);
        prepStmt.setString(2, username);
        prepStmt.setString(3, tenantId);
        prepStmt.setString(4, claimAttributeName);
        prepStmt.setString(5, UserCoreConstants.DEFAULT_PROFILE);
        prepStmt.setString(6, tenantId);
        int result = prepStmt.executeUpdate();
        return result > 0;
    }

    /**
     * Insert new user metadata in user store.
     *
     * @param userStoreManager      UserStoreManager.
     * @param tenantId              Tenant Id.
     * @param username              Username.
     * @param claimAttributeName    Claim attribute name.
     * @param value                 Value of the claim.
     * @return                      boolean value of the execution.
     * @throws SQLException         SQL Exception.
     */
    private boolean insertMetadataInUserStore(Connection connection, UserStoreManager userStoreManager, String tenantId,
                                              String username, String claimAttributeName, String value)
                                              throws SQLException, IdentityEventException {

        String sqlStmt = fetchSQLStatementFromUserStoreConfig(userStoreManager,
                IdentityRecoveryConstants.SQLQueries.INSERT_USER_METADATA_TO_USER_STORE_SQL_KEY);

        PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
        prepStmt.setString(1, username);
        prepStmt.setString(2, tenantId);
        prepStmt.setString(3, claimAttributeName);
        prepStmt.setString(4, value);
        prepStmt.setString(5, UserCoreConstants.DEFAULT_PROFILE);
        prepStmt.setString(6, tenantId);
        int result = prepStmt.executeUpdate();
        return result > 0;
    }

    /**
     * Get database connection.
     *
     * @param realmConfiguration        Realm configuration.
     * @return                          Database connection.
     * @throws IdentityEventException   Identity event exception.
     */
    private Connection getDBConnection(RealmConfiguration realmConfiguration) throws IdentityEventException {

        Connection dbConnection = null;
        DataSource dataSource = DatabaseUtil.createUserStoreDataSource(realmConfiguration);

        try {
            if (dataSource != null) {
                dbConnection = DatabaseUtil.getDBConnection(dataSource);
            }

            //if primary user store, DB connection can be same as realm data source.
            if (dbConnection == null && realmConfiguration.isPrimary()) {
                dbConnection = IdentityDatabaseUtil.getUserDBConnection(false);
            } else if (dbConnection == null) {
                throw new org.wso2.carbon.user.api.UserStoreException("Could not create a database connection to " +
                        realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));
            }
            dbConnection.setAutoCommit(false);
            dbConnection.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
            return dbConnection;
        } catch (SQLException | org.wso2.carbon.user.api.UserStoreException e) {
            throw new IdentityEventException("Error occurred while getting DB connection", e);
        }
    }

    /**
     * Get the claim attribute name.
     *
     * @param userStoreManager          User store manager.
     * @param tenantId                  Tenant Id.
     * @param claimURI                  Claim URI.
     * @return                          Claim attribute name.
     * @throws IdentityEventException   Identity event exception.
     */
    private String retrieveAttributeName(UserStoreManager userStoreManager, String tenantId,
                                         String claimURI) throws IdentityEventException {

        ClaimManager claimManager;
        String claimAttributeName;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            claimManager = (ClaimManager) realmService.getTenantUserRealm(Integer.parseInt(tenantId)).getClaimManager();
            String userStoreDomain = userStoreManager.getRealmConfiguration().
                    getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
            claimAttributeName = claimManager.getAttributeName(userStoreDomain, claimURI);
            return claimAttributeName;
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw new IdentityEventException("Error while retrieving claimAttributeName using claim manager.", e);
        }
    }

}
