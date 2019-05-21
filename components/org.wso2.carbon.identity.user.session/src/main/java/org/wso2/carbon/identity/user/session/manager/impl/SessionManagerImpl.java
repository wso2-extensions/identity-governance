/*
 *   Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *   WSO2 Inc. licenses this file to you under the Apache License,
 *   Version 2.0 (the "License"); you may not use this file except
 *   in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

package org.wso2.carbon.identity.user.session.manager.impl;

import org.apache.commons.codec.digest.DigestUtils;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.database.utils.jdbc.JdbcTemplate;
import org.wso2.carbon.database.utils.jdbc.exceptions.DataAccessException;
import org.wso2.carbon.identity.application.authentication.framework.UserSessionManagementService;
import org.wso2.carbon.identity.application.authentication.framework.context.SessionContext;
import org.wso2.carbon.identity.application.authentication.framework.exception.UserSessionException;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.user.session.constant.SQLConstants;
import org.wso2.carbon.identity.user.session.constant.SessionConstants;
import org.wso2.carbon.identity.user.session.dao.UserSessionDAO;
import org.wso2.carbon.identity.user.session.dao.impl.UserSessionDAOImpl;
import org.wso2.carbon.identity.user.session.exception.SessionManagementException;
import org.wso2.carbon.identity.user.session.manager.SessionManager;
import org.wso2.carbon.identity.user.session.model.UserSession;
import org.wso2.carbon.identity.user.session.util.JdbcUtils;
import org.wso2.carbon.identity.user.session.util.SessionMgtUtils;

import java.util.ArrayList;
import java.util.List;

public class SessionManagerImpl implements SessionManager {

    private static UserSessionManagementService getUserSessionManagementService() {
        return (UserSessionManagementService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(UserSessionManagementService.class, null);
    }

    @Override
    public UserSession[] viewSession(String sessionId) throws
            SessionManagementException {
        List<String> userIdList;
        try {
            userIdList = getUserIDList(sessionId);
        } catch (SessionManagementException e) {
            throw SessionMgtUtils.handleClientException(SessionConstants.ErrorMessages
                    .ERROR_CODE_GET_USER_SESSION, null);
        }
        return getSessionofUserIdList(userIdList);
    }

    @Override
    public boolean terminateAllSession(String sessionId) throws SessionManagementException {
        List<String> userIdList;
        List<String> sessionList;

        userIdList = getUserIDList(sessionId);
        sessionList = getSessionID(userIdList);

        try {
            return getUserSessionManagementService().terminateSession(sessionList);
        } catch (UserSessionException e) {
            throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                    ERROR_CODE_TERMINATE_SESSION, sessionList.toString(), e);
        }
    }

    @Override
    public boolean terminateASession(String currentSessionId, String sessionId) throws SessionManagementException {
        List<String> userIdList;
        List<String> sessionList;

        userIdList = getUserIDList(currentSessionId);
        sessionList = getSessionID(userIdList);

        for (String sessionId1 : sessionList) {
            SessionContext sessionContext = FrameworkUtils.getSessionContextFromCache(sessionId1);
            if (sessionContext != null) {
                if (DigestUtils.sha256Hex(sessionId1).equals(sessionId)) {
                    try {
                        return getUserSessionManagementService().terminateASession(sessionId1);
                    } catch (UserSessionException e) {
                        throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                                ERROR_CODE_TERMINATE_SESSION, sessionId, e);
                    }
                }
            }
        }
        return true;
    }

    @Override
    public UserSession[] viewSessionUser(String userName, String tenant, String userDomain)
            throws SessionManagementException {

        UserSession[] userSessions;
        String userId = getUserId(userName, tenant, userDomain);
        userSessions = getSessionofUserId(userId);
        return userSessions;
    }

    /**
     * Method to get userId.
     *
     * @param userName   UserName
     * @param tenant     User's tenant
     * @param userDomain User's user domain
     * @return  UserId
     * @throws SessionManagementException if an error occurs when retrieving the user id list from the database.
     */
    private String getUserId(String userName, String tenant, String userDomain) throws SessionManagementException {
        String userId;
        int tenantId = IdentityTenantUtil.getTenantId(tenant);
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();
        try {
            userId = jdbcTemplate.fetchSingleRecord(SQLConstants.SQL_SELECT_USER_ID, ((resultSet, rowNumber) ->
                    resultSet.getString(1)), preparedStatement -> {
                preparedStatement.setString(1, userName);
                preparedStatement.setInt(2, tenantId);
                preparedStatement.setString(3, (userDomain == null) ? "FEDERATED" : userDomain);
            });

        } catch (DataAccessException e) {
            throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                    ERROR_CODE_GET_USER_ID, userName, e);
        }
        return userId;
    }

    /**
     * Method to get userIds in a sessionId.
     *
     * @param sessionId Id of session
     * @return UserIds in the session
     * @throws SessionManagementException if an error occurs when retrieving the user id list from the database.
     */
    private List<String> getUserIDList(String sessionId) throws SessionManagementException {
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();
        List<String> userIdList = new ArrayList();
        try {
            jdbcTemplate.executeQuery(SQLConstants.GET_USER_ID,
                    (resultSet, rowNumber) -> userIdList.add(resultSet.getString(1)), preparedStatement ->
                            preparedStatement.setString(1, sessionId));
        } catch (DataAccessException e) {
            throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                            ERROR_CODE_GET_USER_ID_LIST
                    , sessionId, null);
        }
        return userIdList;
    }

    /**
     * Method to get sessionId list of a given user Id.
     *
     * @param userIdList Id of the user
     * @return the list of session ids
     * @throws SessionManagementException if an error occurs when retrieving the session id list from the database.
     */
    private List<String> getSessionID(List<String> userIdList) throws SessionManagementException {

        List<String> sessionIdList = new ArrayList<>();
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();
        for (String userId : userIdList) {
            try {
                jdbcTemplate.executeQuery(SQLConstants.GET_SESSION_ID,
                        (resultSet, rowNumber) ->
                                sessionIdList.add(resultSet.getString(1)), preparedStatement ->
                                preparedStatement.setString(1, userId));
            } catch (DataAccessException e) {
                throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                        ERROR_CODE_GET_SESSION_IDs_OF_USER_IDs, userId, e);
            }
        }
        return sessionIdList;
    }

    /**
     * Method to get sessions of given userId list.
     *
     * @param userIdList List of user ids.
     * @return Usersessions
     * @throws SessionManagementException if an error occurs when retrieving sessionId from the database.
     */
    private UserSession[] getSessionofUserIdList(List<String> userIdList) throws SessionManagementException {

        List<String> sessionIdList = new ArrayList<>();
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();
        UserSession[] sessionList;

        for (String userId : userIdList) {
            try {
                jdbcTemplate.executeQuery(SQLConstants.GET_SESSION_ID, (resultSet, rowNumber)
                                -> sessionIdList.add(resultSet.getString(1)),
                        preparedStatement -> preparedStatement.setString(1, userId));
            } catch (DataAccessException e) {
                throw new SessionManagementException(SessionConstants.ErrorMessages.
                        ERROR_CODE_GET_SESSION_OF_USER_ID_LIST
                        + userIdList.toString(), null);
            }
        }
            sessionList = getActiveSessionList(sessionIdList);
        return sessionList;

    }

    /**
     * Method to get sessions of given userId.
     *
     * @param userId Id of user
     * @return UserSession[]
     * @throws SessionManagementException if an error occurs when retrieving the sessions from the database.
     */
    private UserSession[] getSessionofUserId(String userId) throws SessionManagementException {
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();
        List<String> sessionIDList = new ArrayList<>();
        UserSession[] sessionList;

        try {
            jdbcTemplate.executeQuery(SQLConstants.GET_SESSION_ID,
                    (resultSet, rowNumber) -> sessionIDList.add(resultSet.getString(1)),
                    preparedStatement -> preparedStatement.setString(1, userId));
        } catch (DataAccessException e) {
            throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                    ERROR_CODE_GET_SESSION_ID_OF_USER_ID, userId, e);
        }
        sessionList = getActiveSessionList(sessionIDList);
        return sessionList;

    }

    /**
     * Method to get active sessions from given sessionId list.
     *
     * @param sessionIdList List of sessionIds
     * @return UserSession[] Usersessions
     * @throws SessionManagementException if an error occurs when retrieving the UserSessions.
     */
    private UserSession[] getActiveSessionList(List<String> sessionIdList) throws SessionManagementException {

        List<UserSession> sessionsList = new ArrayList<>();
        for (String sessionId : sessionIdList) {
            if (sessionId != null) {
                try {
                    SessionContext sessionContext = FrameworkUtils.getSessionContextFromCache(sessionId);
                    if (sessionContext != null) {
                        UserSessionDAO userSessionDTO = new UserSessionDAOImpl();
                        UserSession userSession = userSessionDTO.getSession(sessionId);
                        if (userSession != null) {
                            sessionsList.add(userSession);
                        }
                    }

                } catch (SessionManagementException e) {
                    throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages
                            .ERROR_CODE_GET_SESSION_CONTEXT, sessionIdList.toString(), e);
                }
            }
        }

        if (!sessionsList.isEmpty()) {
            return sessionsList.toArray(new UserSession[sessionsList.size()]);
        }
        return new UserSession[0];
    }

}
