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
package org.wso2.carbon.identity.account.suspension.notification.task;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.bean.AccountValidatorThreadProperties;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiversRetrievalUtil;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class NotificationReceiversRetrievalManager {

    private static final Log log = LogFactory.getLog(NotificationReceiversRetrievalManager.class);

    public static List<NotificationReceiver> getReceivers(long delay,
            AccountValidatorThreadProperties accountValidatorThreadProperties)
            throws AccountSuspensionNotificationException {

        Set<String> userStoreDomains = NotificationReceiversRetrievalUtil.getSuspensionNotificationEnabledUserStores();
        List<NotificationReceiver> receivers = new ArrayList<>();

        for (String userStoreDomain : userStoreDomains) {
            NotificationReceiversRetrieval notificationReceiversRetrieval = NotificationReceiversRetrievalUtil
                    .getNotificationReceiversRetrievalForDomain(userStoreDomain);
            if (notificationReceiversRetrieval != null) {
                long lookupMin = getCurrentExecutionTime(accountValidatorThreadProperties.getNotificationTriggerTime())
                        .getTimeInMillis() - (TimeUnit.DAYS.toMillis(delay + 1));
                long lookupMax = lookupMin + TimeUnit.DAYS.toMillis(1);
                receivers = notificationReceiversRetrieval.getNotificationReceivers(lookupMin, lookupMax,
                        accountValidatorThreadProperties.getDelayForSuspension());
            }
        }

        return receivers;
    }

    private static Calendar getCurrentExecutionTime(Date triggerTime) {

        Calendar tr = Calendar.getInstance();
        tr.setTime(triggerTime);

        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, tr.get(Calendar.HOUR_OF_DAY));
        calendar.set(Calendar.MINUTE, tr.get(Calendar.MINUTE));
        calendar.set(Calendar.SECOND, tr.get(Calendar.SECOND));
        calendar.set(Calendar.MILLISECOND, calendar.getActualMinimum(Calendar.MILLISECOND));
        return calendar;
    }

}
