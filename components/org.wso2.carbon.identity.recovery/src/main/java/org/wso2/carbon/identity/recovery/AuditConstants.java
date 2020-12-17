/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery;

/**
 * Audit related Constants.
 */
public class AuditConstants {

    public static final String USER_AGENT_QUERY_KEY = "User-Agent";
    public static final String USER_AGENT_KEY = "User Agent";
    public static final String REMOTE_ADDRESS_QUERY_KEY = "remoteAddress";
    public static final String REMOTE_ADDRESS_KEY = "RemoteAddress";
    public static final String SERVICE_PROVIDER_KEY = "ServiceProviderName";
    public static final String SERVICE_PROVIDER_QUERY_KEY = "serviceProvider";
    public static final String USER_NOTIFIED_TYPE_KEY = "User notified type";
    public static final String ERROR_MESSAGE_KEY = "Error Message";
    public static final String EMAIL_TO_BE_CHANGED = "Email To be Changed";
    public static final String NOTIFICATION_CHANNEL = "Notification Channel";
    public static final String ACTION_PASSWORD_RECOVERY = "Password recovery";
    public static final String ACTION_USERNAME_RECOVERY = "Username recovery";
    public static final String ACTION_PASSWORD_RESET = "Password reset";
    public static final String NOTIFICATION_TEMPLATE_TYPE = "Notification template";
    public static final String USER_STORE_DOMAIN = "UserStoreDomain";
    public static final String RECOVERY_SCENARIO = "RecoveryScenario";
    public static final String RECOVERY_STEP = "RecoveryStep";
    public static final String TENANT_DOMAIN = "Tenant";
    public static final String AUDIT_MESSAGE = "Initiator : %s | Action : %s | Target : %s | Data : %s | Result : %s ";
}
