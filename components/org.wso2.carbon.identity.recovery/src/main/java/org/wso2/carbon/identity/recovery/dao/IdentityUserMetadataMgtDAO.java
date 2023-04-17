package org.wso2.carbon.identity.recovery.dao;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Map;

/**
 * This class is used to access the data storage to retrieve and store identity user metadata.
 */
public class IdentityUserMetadataMgtDAO {

    private static final Log log = LogFactory.getLog(IdentityUserMetadataMgtDAO.class);
    private static final String TENANT_ASSOCIATION_MANAGER = "tenant-association-manager";
    public static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";

    /**
     * Update user metadata.
     *
     * @param eventProperties Event properties.
     * @param claimURI        Claim URI.
     * @param value           Value of the claim.
     * @param eventName       Event name.
     * @throws IdentityEventException Identity Event Exception.
     */
    public void updateUserMetadata(UserStoreManager userStoreManager, Map<String, Object> eventProperties,
                                   String claimURI, String value, String eventName) throws IdentityEventException {

        String username = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        // Todo: Check why tenant manager is being considered as a user.
        if (TENANT_ASSOCIATION_MANAGER.equalsIgnoreCase(username)) {
            return;
        }
        String userStoreDomain = userStoreManager.getRealmConfiguration().
                getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        if (userStoreDomain != null && !userStoreDomain.isEmpty()) {
            username = userStoreDomain + UserCoreConstants.DOMAIN_SEPARATOR + username;
        }

        try {
            String tenantId = String.valueOf(userStoreManager.getTenantId());

            boolean executionResult;
            if (isMetadataExists(tenantId, username, claimURI)) {
                executionResult = updateExistingMetadata(value, tenantId, username, claimURI);
            } else {
                executionResult = insertNewMetadata(value, tenantId, username, claimURI);
            }

            if (log.isDebugEnabled() && executionResult) {
                log.debug(String.format("Successfully updated the user metadata related to %s event.", eventName));
            }

        } catch (SQLException e) {
            if (log.isDebugEnabled()) {
                log.debug(String.format("Error occurred while updating user metadata related to %s event.", eventName));
            }
            throw new IdentityEventException(
                    String.format("Error occurred while updating user metadata related to %s event.", eventName), e);

        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug(String.format("Error occurred while retrieving the tenantId of the user related to %s event.",
                        eventName));
            }
            throw new IdentityEventException(
                    String.format("Error occurred while retrieving the tenantId of the user related to %s event.",
                            eventName), e);
        }
    }

    /**
     * Check the existence of the user metadata.
     *
     * @param tenantId  Tenant Id.
     * @param username  Username.
     * @param claimURI  Claim URI.
     * @return          boolean value of the existence.
     * @throws SQLException SQL Exception.
     */
    private boolean isMetadataExists(String tenantId, String username, String claimURI) throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.LOAD_USER_METADATA;
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(false)) {
            PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
            prepStmt.setString(1, tenantId);
            prepStmt.setString(2, username);
            prepStmt.setString(3, claimURI);
            return prepStmt.executeQuery().next();
        }
    }

    /**
     * Update existing user metadata.
     *
     * @param value     Value of the claim.
     * @param tenantId  Tenant Id.
     * @param username  Username.
     * @param claimURI  Claim URI.
     * @return          boolean value of the execution.
     * @throws SQLException SQL Exception.
     */
    private boolean updateExistingMetadata(String value, String tenantId, String username, String claimURI)
            throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.UPDATE_USER_METADATA;
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
            prepStmt.setString(1, value);
            prepStmt.setString(2, tenantId);
            prepStmt.setString(3, username);
            prepStmt.setString(4, claimURI);
            int result = prepStmt.executeUpdate();
            if (result > 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Insert new user metadata.
     *
     * @param value     Value of the claim.
     * @param tenantId  Tenant Id.
     * @param username  Username.
     * @param claimURI  Claim URI.
     * @return          boolean value of the execution.
     * @throws SQLException SQL Exception.
     */
    private boolean insertNewMetadata(String value, String tenantId, String username, String claimURI)
            throws SQLException {

        String sqlStmt = IdentityRecoveryConstants.SQLQueries.INSERT_USER_METADATA;
        try (Connection connection = IdentityDatabaseUtil.getDBConnection(true)) {
            PreparedStatement prepStmt = connection.prepareStatement(sqlStmt);
            prepStmt.setString(1, tenantId);
            prepStmt.setString(2, username);
            prepStmt.setString(3, claimURI);
            prepStmt.setString(4, value);
            int result = prepStmt.executeUpdate();
            if (result > 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager User Store manager.
     * @return Weather identity claims are stored in user store or not.
     */
    private boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(STORE_IDENTITY_CLAIMS));
    }

}
