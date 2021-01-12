/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.account.suspension.notification.task.util;

/**
 * Constants for account suspension notification module.
 */
public class NotificationConstants {

    public static final String SUSPENSION_NOTIFICATION_ENABLED = "suspension.notification.enable";
    public static final String SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY = "suspension.notification.account."
            + "disable.delay";
    public static final String SUSPENSION_NOTIFICATION_TRIGGER_TIME= "suspension.notification.trigger.time";
    public static final String SUSPENSION_NOTIFICATION_DELAYS="suspension.notification.delays";
    public static final String USE_IDENTITY_CLAIM_FOR_LAST_LOGIN_TIME = "AccountSuspension.UseIdentityClaims";
    public static final String TRIGGER_TIME_FORMAT = "HH:mm:ss";
    public static final long SCHEDULER_DELAY = 24; // In hours
    public static final String SUSPENSION_NOTIFICATION_THREAD_POOL_SIZE = "suspension.notification.thread.pool.size";

    public static final String GET_USERS_FILTERED_BY_LAST_LOGIN_TIME = "SELECT UM_USER.UM_USER_NAME FROM "
    + "UM_USER, UM_USER_ATTRIBUTE WHERE UM_USER_ATTRIBUTE.UM_USER_ID = UM_USER.UM_ID AND UM_USER_ATTRIBUTE.UM_ATTR_NAME "
    + "= ? AND UM_USER_ATTRIBUTE.UM_ATTR_VALUE BETWEEN ? AND ? AND UM_USER_ATTRIBUTE.UM_TENANT_ID=? AND "
    + "UM_USER.UM_TENANT_ID=?";

    public static final String GET_USERS_FILTERED_BY_LAST_LOGIN_TIME_IDENTITY_CLAIM = "SELECT USER_NAME, DATA_VALUE " +
            "FROM IDN_IDENTITY_USER_DATA WHERE DATA_KEY = ? AND DATA_VALUE BETWEEN ? AND ? AND TENANT_ID = ?";

    public final static String USERNAME_CLAIM = "http://wso2.org/claims/username";
    public final static String FIRST_NAME_CLAIM = "http://wso2.org/claims/givenname";
    public final static String EMAIL_CLAIM = "http://wso2.org/claims/emailaddress";
    public final static String LAST_LOGIN_TIME = "http://wso2.org/claims/lastLoginTime";
    public final static String LAST_LOGIN_TIME_IDENTITY_CLAIM = "http://wso2.org/claims/identity/lastLoginTime";

    public static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";
    public static final String ACCOUNT_LOCKED_REASON_CLAIM = "http://wso2.org/claims/identity/lockedReason";
    public static final String PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM = "http://wso2" +
            ".org/claims/identity/failedPasswordRecoveryAttempts";
}