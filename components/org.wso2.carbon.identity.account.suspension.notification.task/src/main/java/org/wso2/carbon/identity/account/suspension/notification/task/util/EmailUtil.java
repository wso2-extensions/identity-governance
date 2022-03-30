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
package org.wso2.carbon.identity.account.suspension.notification.task.util;

import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.mgt.NotificationSender;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

public class EmailUtil {
    private static final Log log = LogFactory.getLog(EmailUtil.class);
    private static final String DATE_FORMAT = "dd-MM-yyyy";
    private static final String REMAINING_DATES ="remaining-days";

    private NotificationSender notificationSender;

    public EmailUtil() {
        notificationSender = new NotificationSender();
    }

    /**
     * Send notification email to <code>receiver</code>
     *
     * @param receiver details of the notification receiver including the email address
     */
    public void sendEmail(NotificationReceiver receiver) {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, receiver.getUsername());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, receiver.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN,
                CarbonContext.getThreadLocalCarbonContext().getTenantDomain());
        try {
            UserStoreManager userStoreManager;
            if(IdentityUtil.getPrimaryDomainName().equals(receiver.getUserStoreDomain())) {
                userStoreManager = (UserStoreManager) CarbonContext.getThreadLocalCarbonContext().getUserRealm()
                        .getUserStoreManager();
            } else {
                userStoreManager = ((UserStoreManager) CarbonContext.getThreadLocalCarbonContext().getUserRealm()
                        .getUserStoreManager()).getSecondaryUserStoreManager(receiver.getUserStoreDomain());
            }
            properties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER,userStoreManager);
        } catch (UserStoreException e) {
            log.error("Error while getting user store manager", e);
            return;
        }

        try {
            String remainingDates = calculateRemainingDays(receiver.getExpireDate(), DATE_FORMAT);
            properties.put(REMAINING_DATES, remainingDates);
        } catch (ParseException e) {
            log.error("Error while calculating remaining days", e);
        }

        properties.put("first-name", receiver.getFirstName());
        properties.put("suspension-date", receiver.getExpireDate());
        properties.put("TEMPLATE_TYPE", "idleAccountReminder");

        Event identityMgtEvent = new Event(IdentityEventConstants.Event.TRIGGER_NOTIFICATION, properties);
        try {
            NotificationTaskDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            log.error("Error occurred while sending email to: " + receiver.getUsername(), e);
        }

    }

    /**
     * Calculates remaining days for suspension using current date and suspension date.
     * @param suspensionDate
     * @param dateFormat
     * @return remaining days as String
     * @throws ParseException
     */
    private String calculateRemainingDays(String suspensionDate, String dateFormat) throws ParseException {

        SimpleDateFormat sdf = new SimpleDateFormat(dateFormat);
        Date date = sdf.parse(suspensionDate);

        Calendar suspensionDateCalendar = Calendar.getInstance();
        suspensionDateCalendar.setTime(date);
        Calendar currentDateCalendar = Calendar.getInstance();

        long diff = suspensionDateCalendar.getTimeInMillis() - currentDateCalendar.getTimeInMillis();
        return String.valueOf(TimeUnit.DAYS.convert(diff, TimeUnit.MILLISECONDS));
    }
}
