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

package org.wso2.carbon.identity.user.session.dao.impl;

import org.wso2.carbon.database.utils.jdbc.JdbcTemplate;
import org.wso2.carbon.database.utils.jdbc.exceptions.DataAccessException;
import org.wso2.carbon.identity.user.session.constant.SQLConstants;
import org.wso2.carbon.identity.user.session.constant.SessionConstants;
import org.wso2.carbon.identity.user.session.dao.UserSessionDAO;
import org.wso2.carbon.identity.user.session.exception.SessionManagementException;
import org.wso2.carbon.identity.user.session.model.Application;
import org.wso2.carbon.identity.user.session.model.UserSession;
import org.wso2.carbon.identity.user.session.util.JdbcUtils;
import org.wso2.carbon.identity.user.session.util.SessionMgtUtils;

import java.util.List;

/**
 * Default implementation of {@link UserSessionDAO}. This handles {@link UserSession} related DB operations.
 */
public class UserSessionDAOImpl implements UserSessionDAO {
    public UserSessionDAOImpl() {
    }

    public UserSession getSession(String sessionId) throws SessionManagementException {
        List<Application> applicationList = null;
        JdbcTemplate jdbcTemplate = JdbcUtils.getNewTemplate();

        try {
            applicationList = jdbcTemplate.executeQuery(SQLConstants.GET_APPLICATION, (resultSet, rowNumber) ->
                            new Application(resultSet.getString(1), resultSet.getString(2)),
                    preparedStatement -> preparedStatement.setString(1, sessionId));

            String userAgent = jdbcTemplate.fetchSingleRecord(SQLConstants.GET_USER_AGENT,
                    (resultSet, rowNumber) -> resultSet.getString(1), preparedStatement ->
                            preparedStatement.setString(1, sessionId));

            String ip = jdbcTemplate.fetchSingleRecord(SQLConstants.GET_IP, (resultSet, rowNumber) ->
                    resultSet.getString(1), preparedStatement -> preparedStatement.setString
                    (1, sessionId));

            String loginTime = jdbcTemplate.fetchSingleRecord(SQLConstants.GET_LOGIN_TIME,
                    (resultSet, rowNumber) -> resultSet.getString(1),
                    preparedStatement -> preparedStatement.setString(1, sessionId));

            String lastAccessTime = jdbcTemplate.fetchSingleRecord(SQLConstants.GET_LAST_ACCESS_TIME
                    , (resultSet, rowNumber) -> resultSet.getString(1),
                    preparedStatement -> preparedStatement.setString(1, sessionId));

            if (!applicationList.isEmpty()) {
                UserSession userSession = new UserSession();
                userSession.setApplications(applicationList.toArray(new Application[applicationList.size()]));
                userSession.setUserAgent(userAgent);
                userSession.setIp(ip);
                userSession.setLoginTime(loginTime);
                userSession.setLastAccessTime(lastAccessTime);
                userSession.setSessionId(sessionId);
                return userSession;
            }
        } catch (DataAccessException e) {
            throw SessionMgtUtils.handleClientException(SessionConstants.ErrorMessages
                    .ERROR_CODE_GET_SESSION_INFO, null);
        }
        return null;
    }
}
