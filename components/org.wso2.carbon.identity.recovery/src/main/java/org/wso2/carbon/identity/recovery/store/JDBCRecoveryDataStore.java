package org.wso2.carbon.identity.recovery.store;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

import java.sql.*;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

public class JDBCRecoveryDataStore implements UserRecoveryDataStore {
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
            prepStmt.setString(8, recoveryDataDO.getMetaData());
            prepStmt.execute();
            connection.setAutoCommit(false);
            connection.commit();
        } catch (SQLException e) {
            String errorDescription = "Error while storing user recovery data";
            throw handleException(errorDescription, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_STORING_RECOVERY_DATA);
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

        try {
            //TODO should have two sqls based on caseSenstitiveUsername
            String sql = IdentityRecoveryConstants.SQLQueries.LOAD_RECOVERY_DATA;

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
                if (StringUtils.isNotBlank(resultSet.getString("META_DATA"))) {
                    userRecoveryData.setMetaData(resultSet.getString("META_DATA"));
                }
                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");
                long createdTimeStamp = timeCreated.getTime();
                //TODO need to read from config
                int notificationExpiryTimeInMinutes = 3;
                long expiryTime = createdTimeStamp + notificationExpiryTimeInMinutes * 60 * 1000L;

                if (System.currentTimeMillis() > expiryTime) {
                    throw handleException("Expired Code :" + code, IdentityRecoveryConstants.ErrorCode
                            .ERROR_CODE_EXPIRED_CODE);
                }
                return userRecoveryData;
            }
        } catch (SQLException e) {
            String errorMsg = "Error occurred while validating code for user : " + user.getUserName();
            throw handleException(errorMsg, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED, e);
        } finally {
            IdentityDatabaseUtil.closeResultSet(resultSet);
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
        throw handleException("Invalid Code : " + code, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_INVALID_CODE);
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
            String errorMsg = "Error occurred while invalidate code";
            throw handleException(errorMsg, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
    }


    @Override
    public void invalidate(User user) throws IdentityRecoveryException {
        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        try {
            String sql = IdentityRecoveryConstants.SQLQueries.INVALIDATE_USER_CODES;

            prepStmt = connection.prepareStatement(sql);
            //TODO need to do based on caseSensitiveUserName
            prepStmt.setString(1, user.getUserName());
            prepStmt.setString(2, user.getUserStoreDomain());
            prepStmt.setInt(3, IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String errorMsg = "Error occurred while invalidate code";
            throw handleException(errorMsg, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
        }
    }

    private IdentityRecoveryException handleException(String errorDescription, String errorCode) throws
            IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryException;
    }

    private IdentityRecoveryException handleException(String errorDescription, String errorCode, Throwable e) throws
            IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        return identityRecoveryException;
    }
}
