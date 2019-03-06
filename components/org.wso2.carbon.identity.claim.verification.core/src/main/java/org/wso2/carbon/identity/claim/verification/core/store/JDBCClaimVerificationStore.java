/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.store;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.ClaimData;
import org.wso2.carbon.identity.claim.verification.core.model.ConfirmationCodeData;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Date;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ClaimVerificationStatus;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ErrorMessages;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.SQLQueries;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.Step;

/**
 * JDBC based DB access class for claim verification.
 */
public class JDBCClaimVerificationStore implements ClaimVerificationStore {

    private static final Log LOG = LogFactory.getLog(JDBCClaimVerificationStore.class);

    private static ClaimVerificationStore jdbcClaimVerificationStore = new JDBCClaimVerificationStore();

    private JDBCClaimVerificationStore() {

    }

    public static ClaimVerificationStore getInstance() {

        return jdbcClaimVerificationStore;
    }

    @Override
    public void storeConfirmationCode(ConfirmationCodeData codeData) throws ClaimVerificationException {

        Connection connection = IdentityDatabaseUtil.getDBConnection();
        PreparedStatement prepStmt = null;
        try {
            connection.setAutoCommit(false);
            prepStmt = connection.prepareStatement(SQLQueries.STORE_CODE_DATA);
            prepStmt.setString(1, codeData.getUser().getUsername());
            prepStmt.setString(2, codeData.getUser().getRealm().toUpperCase());
            prepStmt.setInt(3, codeData.getUser().getTenantId());
            prepStmt.setString(4, codeData.getCode());
            prepStmt.setString(5, codeData.getScenario());
            prepStmt.setString(6, String.valueOf(codeData.getStep()));
            prepStmt.setTimestamp(7, new Timestamp(new Date().getTime()));
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while storing confirmation code data.";
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_STORING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public void storeClaimData(ClaimData claimData) throws ClaimVerificationException {

        Connection connection = IdentityDatabaseUtil.getDBConnection();
        PreparedStatement prepStmt = null;
        try {
            connection.setAutoCommit(false);
            prepStmt = connection.prepareStatement(SQLQueries.STORE_CLAIM_DATA);
            prepStmt.setString(1, claimData.getUser().getUsername());
            prepStmt.setString(2, claimData.getUser().getRealm().toUpperCase());
            prepStmt.setInt(3, claimData.getUser().getTenantId());
            prepStmt.setInt(4, claimData.getClaimId());
            prepStmt.setString(5, claimData.getClaimValue());
            prepStmt.setString(6, String.valueOf(claimData.getVerificationStatus()));
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while storing claim data.";
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_STORING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public ConfirmationCodeData loadConfirmationCodeData(String code) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        ConfirmationCodeData codeData = null;
        String sql = SQLQueries.LOAD_CODE_DATA_FROM_CODE;

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, code);

            resultSet = prepStmt.executeQuery();

            if (resultSet.next()) {
                User user = new User();
                user.setUsername(resultSet.getString("USER_NAME"));
                user.setTenantId(resultSet.getInt("TENANT_ID"));
                user.setRealm(resultSet.getString("USER_DOMAIN"));
                String scenario = resultSet.getString("SCENARIO");
                Enum step = Step.valueOf(resultSet.getString("STEP"));
                Timestamp timeCreated = resultSet.getTimestamp("TIME_CREATED");

                codeData = new ConfirmationCodeData(user, code, scenario, step, timeCreated);
            }
        } catch (SQLException e) {
            String msg = "Error while loading confirmation code data for code:" + code;
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        return codeData;
    }

    @Override
    public int loadClaimId(String claimUri, int tenantId) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        int claimId = -1;
        String sql = SQLQueries.GET_CLAIM_ID;

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, claimUri);
            prepStmt.setInt(2, tenantId);

            resultSet = prepStmt.executeQuery();
            if (resultSet.next()) {
                claimId = resultSet.getInt("ID");
            }
        } catch (SQLException e) {
            String msg = "Error while loading claimId for claimUri:" + claimUri + " and tenantId:" + tenantId;
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_RETRIEVING_CLAIM_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        return claimId;
    }

    @Override
    public String loadClaimUri(int claimId, int tenantId) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        String claimUri = null;
        String sql = SQLQueries.GET_CLAIM_URI;

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setInt(1, claimId);
            prepStmt.setInt(2, tenantId);

            resultSet = prepStmt.executeQuery();
            if (resultSet.next()) {
                claimUri = resultSet.getString("CLAIM_URI");
            }
        } catch (SQLException e) {
            String msg = "Error while loading claimUri for claimId:" + claimId + " and tenantId:" + tenantId;
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_RETRIEVING_CLAIM_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        return claimUri;
    }

    @Override
    public void invalidateConfirmationCode(ConfirmationCodeData codeData) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        String sql;
        if (IdentityUtil.isUserStoreCaseSensitive(codeData.getUser().getRealm(),
                codeData.getUser().getTenantId())) {
            sql = SQLQueries.INVALIDATE_CODE;
        } else {
            sql = SQLQueries.INVALIDATE_CODE_CASE_INSENSITIVE;
        }

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, codeData.getUser().getUsername());
            prepStmt.setString(2, codeData.getUser().getRealm().toUpperCase());
            prepStmt.setInt(3, codeData.getUser().getTenantId());
            prepStmt.setString(4, codeData.getScenario());
            prepStmt.setString(5, String.valueOf(codeData.getStep()));
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while invalidating confirmation code for user:" + codeData.getUser().getUsername() +
                    " in userStore:" + codeData.getUser().getRealm() + "in tenantId:" + codeData.getUser().getTenantId();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_INVALIDATING_CODE,
                    e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public void invalidateConfirmationCode(String code) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        String sql = SQLQueries.INVALIDATE_CODE_USING_CODE;

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, code);
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while invalidating confirmation code:" + code;
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_INVALIDATING_CODE,
                    e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public void updateClaimVerificationStatus(ClaimData claimData) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        String sql;
        if (IdentityUtil.isUserStoreCaseSensitive(claimData.getUser().getRealm(),
                claimData.getUser().getTenantId())) {
            sql = SQLQueries.UPDATE_CLAIM_VERIFICATION_STATUS;
        } else {
            sql = SQLQueries.UPDATE_CLAIM_VERIFICATION_STATUS_CASE_INSENSITIVE;
        }

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, String.valueOf(claimData.getVerificationStatus()));
            prepStmt.setString(2, claimData.getUser().getUsername());
            prepStmt.setString(3, claimData.getUser().getRealm().toUpperCase());
            prepStmt.setInt(4, claimData.getUser().getTenantId());
            prepStmt.setInt(5, claimData.getClaimId());
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while updating claim verification status for claimId:" + claimData.getClaimId() +
                    "and user:" + claimData.getUser().getUsername() + " in userStore:" + claimData.getUser().getRealm()
                    + "in tenantId:" + claimData.getUser().getTenantId();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_UPDATING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public void updateClaimData(ClaimData claimData) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        String sql;
        if (IdentityUtil.isUserStoreCaseSensitive(claimData.getUser().getRealm(),
                claimData.getUser().getTenantId())) {
            sql = SQLQueries.UPDATE_CLAIM_DATA;
        } else {
            sql = SQLQueries.UPDATE_CLAIM_DATA_CASE_INSENSITIVE;
        }

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, claimData.getClaimValue());
            prepStmt.setString(2, String.valueOf(claimData.getVerificationStatus()));
            prepStmt.setString(3, claimData.getUser().getUsername());
            prepStmt.setString(4, claimData.getUser().getRealm().toUpperCase());
            prepStmt.setInt(5, claimData.getUser().getTenantId());
            prepStmt.setInt(6, claimData.getClaimId());
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while updating claim data for claimId:" + claimData.getClaimId() + "and user:" +
                    claimData.getUser().getUsername() + " in userStore:" + claimData.getUser().getRealm() +
                    "in tenantId:" + claimData.getUser().getTenantId();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_UPDATING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public void clearClaimData(int claimId, User user) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();

        String sql;
        if (IdentityUtil.isUserStoreCaseSensitive(user.getRealm(),
                user.getTenantId())) {
            sql = SQLQueries.DELETE_CLAIM_DATA;
        } else {
            sql = SQLQueries.DELETE_CLAIM_DATA_CASE_INSENSITIVE;
        }

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, user.getUsername());
            prepStmt.setString(2, user.getRealm().toUpperCase());
            prepStmt.setInt(3, user.getTenantId());
            prepStmt.setInt(4, claimId);
            prepStmt.execute();
            connection.commit();
        } catch (SQLException e) {
            String msg = "Error while clearing claim data for claimId:" + claimId + "user:" + user.getUsername() +
                    " in userStore:" + user.getRealm() + "in tenantId:" + user.getTenantId();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_CLEARING_CLAIM_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeStatement(prepStmt);
            IdentityDatabaseUtil.closeConnection(connection);
        }
    }

    @Override
    public ClaimData loadClaimData(int claimId, User user) throws ClaimVerificationException {

        PreparedStatement prepStmt = null;
        ResultSet resultSet = null;
        Connection connection = IdentityDatabaseUtil.getDBConnection();
        ClaimData loadedClaimData = null;

        String sql;
        if (IdentityUtil.isUserStoreCaseSensitive(user.getRealm(),
                user.getTenantId())) {
            sql = SQLQueries.LOAD_CLAIM_DATA;
        } else {
            sql = SQLQueries.LOAD_CLAIM_DATA_CASE_INSENSITIVE;
        }

        try {
            prepStmt = connection.prepareStatement(sql);
            prepStmt.setString(1, user.getUsername());
            prepStmt.setString(2, user.getRealm().toUpperCase());
            prepStmt.setInt(3, user.getTenantId());
            prepStmt.setInt(4, claimId);
            resultSet = prepStmt.executeQuery();

            if (resultSet.next()) {
                User loadedUser = new User();
                loadedUser.setUsername(resultSet.getString("USER_NAME"));
                loadedUser.setTenantId(resultSet.getInt("TENANT_ID"));
                loadedUser.setRealm(resultSet.getString("USER_DOMAIN"));
                int loadedClaimId = resultSet.getInt("CLAIM_ID");
                String loadedClaimValue = resultSet.getString("CLAIM_VALUE");
                Enum loadedVerificationStatus = ClaimVerificationStatus.valueOf(
                        resultSet.getString("VERIFICATION_STATUS"));

                loadedClaimData = new ClaimData(loadedUser, loadedClaimId, loadedClaimValue, loadedVerificationStatus);
            }
        } catch (SQLException e) {
            String msg = "Error while loading claim data for claimId:" + claimId + " and user:" + user.getUsername() +
                    " in userStore:" + user.getRealm() + "in tenantId:" + user.getTenantId();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_DATA, e);
        } finally {
            IdentityDatabaseUtil.closeAllConnections(connection, resultSet, prepStmt);
        }
        return loadedClaimData;
    }
}
