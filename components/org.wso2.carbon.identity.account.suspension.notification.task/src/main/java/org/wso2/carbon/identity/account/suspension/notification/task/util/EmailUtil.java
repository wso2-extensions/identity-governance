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

import org.apache.log4j.Logger;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.NotificationSender;
import org.wso2.carbon.user.api.UserStoreException;

import java.util.HashMap;

/**
 * Utility class to send emails
 */
public class EmailUtil {
    private static final Logger log = Logger.getLogger(EmailUtil.class);

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
        properties.put(EventConstants.EventProperty.USER_NAME, receiver.getUsername());
        properties.put(EventConstants.EventProperty.USER_NAME, receiver.getUsername());
        properties.put(EventConstants.EventProperty.USER_STORE_DOMAIN, receiver.getUserStoreDomain());
        properties.put(EventConstants.EventProperty.TENANT_DOMAIN,
                CarbonContext.getThreadLocalCarbonContext().getTenantDomain());
        try {
            properties.put(EventConstants.EventProperty.USER_STORE_MANAGER,
                    CarbonContext.getThreadLocalCarbonContext().getUserRealm().getUserStoreManager());
        } catch (UserStoreException e) {
            log.error("Error while getting user store manager", e);
        }

        properties.put("first-name", receiver.getFirstName());
        properties.put("suspension-date", receiver.getExpireDate());
        properties.put("TEMPLATE_TYPE", "idleAccountReminder");

        Event identityMgtEvent = new Event(EventConstants.Event.TRIGGER_NOTIFICATION, properties);
        try {
            NotificationTaskDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (EventException e) {
            log.error("Error occurred while sending email to: " + receiver.getUsername(), e);
        }

    }
}
