package org.wso2.carbon.identity.recovery.store;

import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.util.Utils;

/**
 * JDBC recovery data store.
 */
public class JDBCRecoveryDataStore implements UserRecoveryDataStore {

    private static UserRecoveryDataStore jdbcRecoveryDataStore = new JDBCRecoveryDataStore();


    private JDBCRecoveryDataStore() {

    }

    public static UserRecoveryDataStore getInstance() {
        return jdbcRecoveryDataStore;
    }


    @Override
    public void store(UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {
//        Connection connection = IdentityDatabaseUtil.getDBConnection();
//        PreparedStatement prepStmt = null;
//        try {
//            prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries.STORE_RECOVERY_DATA);
//            prepStmt.setString(1, recoveryDataDO.getUser().getUniqueUserId());
//            prepStmt.setString(2, recoveryDataDO.getUser().getDomainName().toUpperCase(Locale.ENGLISH));
//            prepStmt.setInt(3, 1);
//            prepStmt.setString(4, recoveryDataDO.getSecret());
//            prepStmt.setString(5, String.valueOf(recoveryDataDO.getRecoveryScenario()));
//            prepStmt.setString(6, String.valueOf(recoveryDataDO.getRecoveryStep()));
//            prepStmt.setTimestamp(7, new Timestamp(new Date().getTime()));
//            prepStmt.setString(8, recoveryDataDO.getRemainingSetIds());
//            prepStmt.execute();
//            connection.setAutoCommit(false);
//            connection.commit();
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.
        // ERROR_CODE_STORING_RECOVERY_DATA,
//                    null, e);
//        } finally {
//            IdentityDatabaseUtil.closeStatement(prepStmt);
//            IdentityDatabaseUtil.closeConnection(connection);
//        }
    }

    @Override
    public UserRecoveryData load(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws
            IdentityRecoveryException {
//        PreparedStatement prepStmt = null;
//        ResultSet resultSet = null;
//        Connection connection = IdentityDatabaseUtil.getDBConnection();
//
//        try {
//            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(),
//                    IdentityTenantUtil.getTenantId(user.getTenantDomain()))) {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA);
//            } else {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries
//                        .LOAD_RECOVERY_DATA_CASE_INSENSITIVE);
//            }
//
//            prepStmt.setString(1, user.getUserName());
//            prepStmt.setString(2, user.getUserStoreDomain().toUpperCase(Locale.ENGLISH));
//            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
//            prepStmt.setString(4, code);
//            prepStmt.setString(5, String.valueOf(recoveryScenario));
//            prepStmt.setString(6, String.valueOf(recoveryStep));
//
//            resultSet = prepStmt.executeQuery();
//
//            if (resultSet.next()) {
//                UserRecoveryData userRecoveryData = new UserRecoveryData(user, code, recoveryScenario, recoveryStep);
//                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
//                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
//                }
//                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
//                long createdTimeStamp = timeCreated.getTime();
//                int notificationExpiryTimeInMinutes = Integer.parseInt(
//                        Utils.getRecoveryConfigs(IdentityRecoveryConstants
//                        .ConnectorConfig.EXPIRY_TIME, user.getTenantDomain())); //Notification expiry time in minutes
//                long expiryTime = createdTimeStamp + notificationExpiryTimeInMinutes * 60L * 1000L;
//
//                if (System.currentTimeMillis() > expiryTime) {
//                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
//                            .ERROR_CODE_EXPIRED_CODE, code);
//                }
//                return userRecoveryData;
//            }
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
//        } finally {
//            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
//        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);
    }

    @Override
    public UserRecoveryData load(String code) throws IdentityRecoveryException {
//        PreparedStatement prepStmt = null;
//        ResultSet resultSet = null;
//        Connection connection = IdentityDatabaseUtil.getDBConnection();
//
//        try {
//            String sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA_FROM_CODE;
//
//            prepStmt = connection.prepareStatement(sql);
//            prepStmt.setString(1, code);
//
//            resultSet = prepStmt.executeQuery();
//
//            if (resultSet.next()) {
//                User user = new User();
//                user.setUserName(resultSet.getString("USER_NAME"));
//                user.setTenantDomain(IdentityTenantUtil.getTenantDomain(resultSet.getInt("TENANT_ID")));
//                user.setUserStoreDomain(resultSet.getString("USER_DOMAIN"));
//
//                String recoveryScenario = resultSet.getString("SCENARIO");
//                String recoveryStep = resultSet.getString("STEP");
//
//                UserRecoveryData userRecoveryData = new UserRecoveryData(user, code, RecoveryScenarios.valueOf
//                        (recoveryScenario), RecoverySteps.valueOf(recoveryStep));
//
//                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
//                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
//                }
//                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
//                long createdTimeStamp = timeCreated.getTime();
//                int notificationExpiryTimeInMinutes = Integer.parseInt(
//                        Utils.getRecoveryConfigs(IdentityRecoveryConstants
//                        .ConnectorConfig.EXPIRY_TIME, user.getTenantDomain())); //Notification expiry time in minutes
//                long expiryTime = createdTimeStamp + notificationExpiryTimeInMinutes * 60L * 1000L;
//
//                if (System.currentTimeMillis() > expiryTime) {
//                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
//                            .ERROR_CODE_EXPIRED_CODE, code);
//                }
//                return userRecoveryData;
//            }
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
//        } finally {
//            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
//        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);

    }

    @Override
    public void invalidate(String code) throws IdentityRecoveryException {
//        PreparedStatement prepStmt = null;
//        Connection connection = IdentityDatabaseUtil.getDBConnection();
//        try {
//            String sql = IdentityRecoveryConstants.SQLQueries.INVALIDATE_CODE;
//
//            prepStmt = connection.prepareStatement(sql);
//            prepStmt.setString(1, code);
//            prepStmt.execute();
//            connection.commit();
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
//        } finally {
//            IdentityDatabaseUtil.closeStatement(prepStmt);
//        }
    }


    @Override
    public UserRecoveryData load(User user) throws IdentityRecoveryException {
//        PreparedStatement prepStmt = null;
//        ResultSet resultSet = null;
//        Connection connection = IdentityDatabaseUtil.getDBConnection();

//        try {
//            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(), IdentityTenantUtil.getTenantId(
//                    user.getTenantDomain()))) {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries.
        // LOAD_RECOVERY_DATA_OF_USER);
//            } else {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries
//                        .LOAD_RECOVERY_DATA_OF_USER_CASE_INSENSITIVE);
//            }
//
//            prepStmt.setString(1, user.getUserName());
//            prepStmt.setString(2, user.getUserStoreDomain().toUpperCase(Locale.ENGLISH));
//            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
//
//            resultSet = prepStmt.executeQuery();
//
//            if (resultSet.next()) {
//                UserRecoveryData userRecoveryData = new UserRecoveryData(user, resultSet.getString("CODE"),
//                        RecoveryScenarios.valueOf(resultSet.getString("SCENARIO")), RecoverySteps.valueOf(resultSet
//                        .getString("STEP")));
//                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
//                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
//                }
//                return userRecoveryData;
//            }
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
//        } finally {
//            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
//        }
        return null;
    }

    @Override
    public void invalidate(User user) throws IdentityRecoveryException {
//        PreparedStatement prepStmt = null;
//        Connection connection = IdentityDatabaseUtil.getDBConnection();
//        try {
//            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(), IdentityTenantUtil.getTenantId(
//                    user.getTenantDomain()))) {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries.INVALIDATE_USER_CODES);
//            } else {
//                prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries
//                        .INVALIDATE_USER_CODES_CASE_INSENSITIVE);
//            }
//
//            prepStmt.setString(1, user.getUserName());
//            prepStmt.setString(2, user.getUserStoreDomain());
//            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
//            prepStmt.execute();
//            connection.commit();
//        } catch (SQLException e) {
//            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
//        } finally {
//            IdentityDatabaseUtil.closeStatement(prepStmt);
//            IdentityDatabaseUtil.closeConnection(connection);
//        }
    }
}
