/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.account.suspension.notification.task.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.AccountValidatorThread;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.mgt.constants.IdentityMgtConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class AccountSuspensionNotificationHandler extends AbstractEventHandler implements IdentityGovernanceConnector {

    private static final Log log = LogFactory.getLog(AccountSuspensionNotificationHandler.class);
    private static final String UPDATE_CONFIGURATION = "UPDATE_CONFIGURATION";

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        if (IdentityEventConstants.Event.POST_AUTHENTICATION.equals(event.getEventName())) {
            Map<String, Object> eventProperties = event.getEventProperties();

            String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
            String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
            UserStoreManager userStoreManager = (UserStoreManager) eventProperties
                    .get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);

            try {
                userStoreManager.setUserClaimValue(userName, IdentityMgtConstants.LAST_LOGIN_TIME,
                        Long.toString(System.currentTimeMillis()), null);
            } catch (UserStoreException e) {
                log.error("Error occurred while updating last login claim for user: ", e);
            }
        }
    }

    @Override
    public String getName() {
        return "suspension.notification";
    }

    @Override
    public String getFriendlyName() {
        return "Disable Idle Accounts";
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED, "Enable");
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY, "Lock Account After (days)");
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS, "Alert User In (days comma-separated list");
        return nameMapping;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
        NotificationTaskDataHolder.getInstance().setNotificationTriggerTime(configs.getModuleProperties().
                getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_TRIGGER_TIME));
        startScheduler();
        NotificationTaskDataHolder.getInstance().getBundleContext()
                .registerService(IdentityGovernanceConnector.class.getName(), this, null);
    }

    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS);
        return properties.toArray(new String[properties.size()]);
    }

    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();

        defaultProperties.put(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED,
                configs.getModuleProperties().getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED));

        defaultProperties.put(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY,
                configs.getModuleProperties()
                        .getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY));

        defaultProperties.put(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS,
                configs.getModuleProperties().getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS));

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {
        return null;
    }

    private void startScheduler() {

        Date notificationTriggerTime = null;
        String notificationTriggerTimeProperty = configs.getModuleProperties().getProperty(NotificationConstants.
                SUSPENSION_NOTIFICATION_TRIGGER_TIME);

        DateFormat dateFormat = new SimpleDateFormat(NotificationConstants.TRIGGER_TIME_FORMAT);

        if (notificationTriggerTimeProperty != null) {
            try {
                notificationTriggerTime = dateFormat.parse(notificationTriggerTimeProperty);
            } catch (ParseException e) {
                log.error("Invalid Date format for Notification trigger time", e);
            }
        }

        long schedulerDelayInSeconds = TimeUnit.HOURS.toSeconds(NotificationConstants.SCHEDULER_DELAY);

        Calendar currentTime = Calendar.getInstance();
        Calendar triggerTime = Calendar.getInstance();
        triggerTime.setTime(notificationTriggerTime);

        // Convert times into seconds
        long currentSecond =
                (currentTime.get(Calendar.HOUR_OF_DAY) * 3600) + currentTime.get(Calendar.MINUTE) * 60 + currentTime
                        .get(Calendar.SECOND);
        long triggerSecond =
                (triggerTime.get(Calendar.HOUR_OF_DAY) * 3600) + triggerTime.get(Calendar.MINUTE) * 60 + triggerTime
                        .get(Calendar.SECOND);
        long delay = triggerSecond - currentSecond;
        // If the notification time has passed, schedule the next day
        if (delay < 0) {
            delay += schedulerDelayInSeconds;
        }

        ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(5);
        scheduler.scheduleAtFixedRate(new AccountValidatorThread(), delay, schedulerDelayInSeconds, TimeUnit.SECONDS);
    }
}
