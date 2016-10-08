/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.account.suspension.notification.task.jdbc;

import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrieval;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrievalFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.user.api.RealmConfiguration;

public class JDBCNotificationReceiversRetrievalFactory implements NotificationReceiversRetrievalFactory {

    public static final String JDBC = "org.wso2.carbon.identity.account.suspension.notification.task.jdbc."
            + "JDBCNotificationReceiversRetrieval";

    @Override
    public NotificationReceiversRetrieval buildCountRetriever(RealmConfiguration realmConfiguration)
            throws AccountSuspensionNotificationException {
        JDBCNotificationReceiversRetrieval jdbcNotificationReceiversRetrieval = new JDBCNotificationReceiversRetrieval();
        jdbcNotificationReceiversRetrieval.init(realmConfiguration);
        return jdbcNotificationReceiversRetrieval;
    }

    @Override
    public String getType() {
        return JDBC;
    }
}
