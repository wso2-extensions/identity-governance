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

package org.wso2.carbon.identity.user.session.constant;

/**
 * Queries related to session management.
 */
public class SQLConstants {

    /**
     * Query to retrieve user ID.
     */
    public static final String SQL_SELECT_USER_ID =
            "SELECT USER_ID FROM IDN_AUTH_USER WHERE USER_NAME =? AND TENANT_ID =? AND " +
                    "DOMAIN_NAME =?";

    public static final String GET_USER_ID = "SELECT USER_ID FROM IDN_AUTH_USER_SESSION_MAPPING WHERE SESSION_ID =?";

    /**
     *  Query to retrieve session ID.
     */
    public static final String GET_SESSION_ID = "SELECT SESSION_ID FROM IDN_AUTH_USER_SESSION_MAPPING " +
            "WHERE USER_ID = ?";

    /**
     *  Query to retrieve userName and appName.
     */
    public static final String GET_APPLICATION = "SELECT SUBJECT, APP_NAME FROM IDN_" +
            "AUTH_APP_SESSION_STORE SESSION_STORE , SP_APP APP where SESSION_STORE." +
            "APP_ID = APP.ID AND SESSION_ID = ?";

    /**
     *  Query to retrieve userAgent.
     */
    public static final String GET_USER_AGENT = "SELECT VALUE  FROM IDN_AUTH_SESSION_META" +
            "_DATA WHERE PROPERTY_TYPE = 'User Agent' AND SESSION_ID = ?";

    /**
     * Query to retrieve IP.
     */
    public static final String GET_IP = "SELECT VALUE  FROM IDN_AUTH_SESSION_META_DATA" +
            " WHERE PROPERTY_TYPE ='IP' AND SESSION_ID = ?";

    /**
     * Query to retrieve loginTIme.
     */
    public static final String GET_LOGIN_TIME = "SELECT VALUE  FROM IDN_AUTH_SESSION_META_" +
            "DATA WHERE PROPERTY_TYPE = 'Login Time' AND SESSION_ID = ?";

    /**
     *  Query to retrieve lastAccessTime.
     */
    public static final String GET_LAST_ACCESS_TIME = "SELECT VALUE  FROM IDN_AUTH_" +
            "SESSION_META_DATA WHERE PROPERTY_TYPE = 'Last Access Time' AND SESSION_ID = ?";

}
