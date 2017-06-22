package org.wso2.carbon.identity.recovery.store;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.sql.*;
import java.util.Date;
import java.util.concurrent.TimeUnit;

public class JDBCRecoveryDataStore implements UserRecoveryDataStore {

    private static UserRecoveryDataStore jdbcRecoveryDataStore = new JDBCRecoveryDataStore();


    private JDBCRecoveryDataStore() {

    }

    public static UserRecoveryDataStore getInstance() {
        return jdbcRecoveryDataStore;
    }


    @Override
    public void store(UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        PreparedStatement prepStmt = null;
        try {
            prepStmt = connection.prepareStatement(IdentityRecoveryConstants.SQLQueries.STORE_RECOVERY_DATA);
            prepStmt.setString(1, recoveryDataDO.getUser().getUserName());
            prepStmt.setString(2, recoveryDataDO.getUser().getUserStoreDomain().toUpperCase());
            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(recoveryDataDO.getUser().getTenantDomain()));
            prepStmt.setString(4, recoveryDataDO.getSecret());
            prepStmt.setString(5, String.valueOf(recoveryDataDO.getRecoveryScenario()));
            prepStmt.setString(6, String.valueOf(recoveryDataDO.getRecoveryStep()));
            prepStmt.setTimestamp(7, new Timestamp(new Date().getTime()));
            prepStmt.setString(8, recoveryDataDO.getRemainingSetIds());
            prepStmt.execute();
            connection.setAutoCommit(false);
            connection.commit();
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_STORING_RECOVERY_DATA, null, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public UserRecoveryData load(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        String sql;
        try {
            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(), IdentityTenantUtil.getTenantId(user.getTenantDomain()))) {
                sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA;
            } else {
                sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA_CASE_INSENSITIVE;
            }

            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, user.getUserName());
            prepStmt.setString(2, user.getUserStoreDomain().toUpperCase());
            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            prepStmt.setString(4, code);
            prepStmt.setString(5, String.valueOf(recoveryScenario));
            prepStmt.setString(6, String.valueOf(recoveryStep));

            resultSet = prepStmt.executeQuery();

            if (resultSet.next()) {
                UserRecoveryData userRecoveryData = new UserRecoveryData(user, code, recoveryScenario, recoveryStep);
                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
                }
                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
                long createdTimeStamp = timeCreated.getTime();
                if (isCodeExpired(user.getTenantDomain(), recoveryScenario, recoveryStep, createdTimeStamp)) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_EXPIRED_CODE, code);
                }
                return userRecoveryData;
            }
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);
    }

    @Override
    public UserRecoveryData load(String code) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        try {
            String sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA_FROM_CODE;

            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, code);

            resultSet = prepStmt.executeQuery();

            if (resultSet.next()) {
                User user = new User();
                user.setUserName(resultSet.getString("USER_NAME"));
                user.setTenantDomain(IdentityTenantUtil.getTenantDomain(resultSet.getInt("TENANT_ID")));
                user.setUserStoreDomain(resultSet.getString("USER_DOMAIN"));

                Enum recoveryScenario = RecoveryScenarios.valueOf(resultSet.getString("SCENARIO"));
                Enum recoveryStep = RecoverySteps.valueOf(resultSet.getString("STEP"));

                UserRecoveryData userRecoveryData = new UserRecoveryData(user, code, recoveryScenario, recoveryStep);

                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
                }
                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
                long createdTimeStamp = timeCreated.getTime();
                if (isCodeExpired(user.getTenantDomain(), userRecoveryData.getRecoveryScenario(), userRecoveryData
                        .getRecoveryStep(), createdTimeStamp)) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_EXPIRED_CODE, code);
                }
                return userRecoveryData;
            }
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);

    }

    @Override
    public void invalidate(String code) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        try {
            String sql = IdentityRecoveryConstants.SQLQueries.INVALIDATE_CODE;

            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, code);
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }


    @Override
    public UserRecoveryData load(User user) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        try {
            String sql;
            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(), IdentityTenantUtil.getTenantId(user.getTenantDomain()))) {
                sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA_OF_USER;
            } else {
                sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA_OF_USER_CASE_INSENSITIVE;
            }

            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, user.getUserName());
            prepStmt.setString(2, user.getUserStoreDomain().toUpperCase());
            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));

            resultSet = prepStmt.executeQuery();

            if (resultSet.next()) {
                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
                RecoveryScenarios scenario = RecoveryScenarios.valueOf(resultSet.getString("SCENARIO"));
                RecoverySteps step = RecoverySteps.valueOf(resultSet.getString("STEP"));
                String code = resultSet.getString("CODE");
                if (isCodeExpired(user.getTenantDomain(), scenario, step, timeCreated.getTime())) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_EXPIRED_CODE, code);
                }

                UserRecoveryData userRecoveryData =
                        new UserRecoveryData(user, code, scenario, step);
                if (StringUtils.isNotBlank(resultSet.getString("REMAINING_SETS"))) {
                    userRecoveryData.setRemainingSetIds(resultSet.getString("REMAINING_SETS"));
                }
                return userRecoveryData;
            }
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        return null;
    }

    @Override
    public void invalidate(User user) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        try {
            String sql;
            if (IdentityUtil.isUserStoreCaseSensitive(user.getUserStoreDomain(), IdentityTenantUtil.getTenantId(user.getTenantDomain()))) {
                sql = IdentityRecoveryConstants.SQLQueries.INVALIDATE_USER_CODES;
            } else {
                sql = IdentityRecoveryConstants.SQLQueries.INVALIDATE_USER_CODES_CASE_INSENSITIVE;
            }

            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, user.getUserName());
            prepStmt.setString(2, user.getUserStoreDomain());
            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    private boolean isCodeExpired(String tenantDomain, Enum recoveryScenario, Enum recoveryStep, long createdTimestamp)
            throws IdentityRecoveryServerException {

        int notificationExpiryTimeInMinutes;
        if (RecoveryScenarios.SELF_SIGN_UP.equals(recoveryScenario) &&
                RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryStep)) {
            notificationExpiryTimeInMinutes = Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                    .ConnectorConfig.EMAIL_VERIFICATION_EXPIRY_TIME, tenantDomain));
        } else if (RecoveryScenarios.ASK_PASSWORD.equals(recoveryScenario)) {
            notificationExpiryTimeInMinutes = Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                    .ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME, tenantDomain));
        } else {
            notificationExpiryTimeInMinutes = Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                    .ConnectorConfig.EXPIRY_TIME, tenantDomain));
        }
        if (notificationExpiryTimeInMinutes < 0) {
            //make the code valid infinitely in case of negative value
            notificationExpiryTimeInMinutes = Integer.MAX_VALUE;
        }
        long expiryTime = createdTimestamp + TimeUnit.MINUTES.toMillis(notificationExpiryTimeInMinutes);
        return System.currentTimeMillis() > expiryTime;
    }
}
