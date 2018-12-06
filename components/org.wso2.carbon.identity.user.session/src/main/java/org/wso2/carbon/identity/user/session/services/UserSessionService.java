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

package org.wso2.carbon.identity.user.session.services;

import org.wso2.carbon.core.AbstractAdmin;
import org.wso2.carbon.identity.user.session.constant.SessionConstants;
import org.wso2.carbon.identity.user.session.exception.SessionManagementException;
import org.wso2.carbon.identity.user.session.exception.SessionManagementServerException;
import org.wso2.carbon.identity.user.session.manager.impl.SessionManagerImpl;
import org.wso2.carbon.identity.user.session.model.UserSession;
import org.wso2.carbon.identity.user.session.util.SessionMgtUtils;

public class UserSessionService extends AbstractAdmin {

    /**
     * @param userName
     * @param tenant
     * @param userStore
     * @return
     * @throws SessionManagementServerException
     */
    public UserSession[] viewUserSession(String userName, String tenant, String userStore) throws
            SessionManagementServerException {

        UserSession[] userSessions;
        SessionManagerImpl sessionManager = new SessionManagerImpl();
        try {
            userSessions = sessionManager.viewSessionUser(userName, tenant, userStore);
        } catch (SessionManagementException e) {
            throw SessionMgtUtils.handleServerException(SessionConstants.ErrorMessages.
                    ERROR_CODE_GET_USER_SESSION, null, e);
        }
        return userSessions;
    }
}
