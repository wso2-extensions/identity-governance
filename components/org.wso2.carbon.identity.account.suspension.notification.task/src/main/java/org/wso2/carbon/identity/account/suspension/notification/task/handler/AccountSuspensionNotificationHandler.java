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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.AccountValidatorThread;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.user.core.UserStoreConfigConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.identity.application.common.model.Property;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class AccountSuspensionNotificationHandler extends AbstractEventHandler implements IdentityConnectorConfig {

    private static final Log log = LogFactory.getLog(AccountSuspensionNotificationHandler.class);
    private static final String UPDATE_CONFIGURATION = "UPDATE_CONFIGURATION";

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        boolean isEnabled = isSuspensionNotificationEnabled(event);

        if (!isEnabled) {
            return;
        }


        if (IdentityEventConstants.Event.POST_AUTHENTICATION.equals(event.getEventName())) {
            Map<String, Object> eventProperties = event.getEventProperties();

            String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
            UserStoreManager userStoreManager = (UserStoreManager) eventProperties
                    .get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
            if (!(Boolean) eventProperties.get(IdentityEventConstants.EventProperty.OPERATION_STATUS)) {
                return;
            }
            try {
                HashMap<String, String> userClaims = new HashMap<>();
                String identityClaimForLastLoginTime = IdentityUtil.
                        getProperty(NotificationConstants.USE_IDENTITY_CLAIM_FOR_LAST_LOGIN_TIME);
                boolean useIdentityClaimForLastLoginTime = StringUtils.isBlank(identityClaimForLastLoginTime) ||
                        Boolean.parseBoolean(identityClaimForLastLoginTime);
                String lastLoginClaim = NotificationConstants.LAST_LOGIN_TIME_IDENTITY_CLAIM;
                userClaims.put(lastLoginClaim, Long.toString(System.currentTimeMillis()));

                if (!useIdentityClaimForLastLoginTime) {

                    if (log.isDebugEnabled()) {
                        log.debug("Property " + NotificationConstants.USE_IDENTITY_CLAIM_FOR_LAST_LOGIN_TIME +
                                " is disabled in identity.xml file. Hence using last login time as default claim.");
                    }
                    String currentTime = getLastLoginTimeValue(userStoreManager);
                    userClaims.put(NotificationConstants.LAST_LOGIN_TIME, currentTime);
                }

                userStoreManager.setUserClaimValues(userName, userClaims, null);
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
        return "Idle Account Suspend";
    }

    @Override
    public String getCategory() {
        return "Account Management";
    }

    @Override
    public String getSubCategory() {
        return "DEFAULT";
    }

    @Override
    public int getOrder() {
        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {
        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED, "Suspend idle user accounts");
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY, "Allowed idle time span " +
                "in days");
        nameMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS, "Alert sending time periods in days");
        return nameMapping;

    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED, "Lock user account after a " +
                "given idle period.");
        descriptionMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY, "Time period in " +
                "days before locking the user account.");
        descriptionMapping.put(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS, "Send warning alerts to users " +
                "before locking the account, after each period. Comma separated multiple values accepted.");
        return descriptionMapping;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);

        if (StringUtils.isBlank(configs.getModuleProperties().
                getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_TRIGGER_TIME))) {
            NotificationTaskDataHolder.getInstance().setNotificationTriggerTime(configs.getModuleProperties().
                    getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_TRIGGER_TIME));
        }

        NotificationTaskDataHolder.getInstance().setNotificationTriggerTime(configs.getModuleProperties().
                getProperty(NotificationConstants.SUSPENSION_NOTIFICATION_TRIGGER_TIME));
        startScheduler();
        NotificationTaskDataHolder.getInstance().getBundleContext()
                .registerService(IdentityConnectorConfig.class.getName(), this, null);
    }

    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS);
        return properties.toArray(new String[0]);
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

        if(!Boolean.parseBoolean(configs.getModuleProperties().getProperty(NotificationConstants.
                SUSPENSION_NOTIFICATION_ENABLED))) {
            return;
        }

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
        // If notificationTriggerTimeProperty is not found or not in right format default to 20:00:00.
        // In Calender.HOUR_OF_DAY (i.e. in 24-hour clock) it is 20.
        if (notificationTriggerTime != null) {
            triggerTime.setTime(notificationTriggerTime);
        } else {
            triggerTime.set(Calendar.HOUR_OF_DAY, 20);
            triggerTime.set(Calendar.MINUTE, 0);
            triggerTime.set(Calendar.SECOND, 0);
        }


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

        ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(NotificationTaskDataHolder.getInstance().
                getNotificationSendingThreadPoolSize());
        scheduler.scheduleAtFixedRate(new AccountValidatorThread(), delay, schedulerDelayInSeconds, TimeUnit.SECONDS);
    }

    private boolean isSuspensionNotificationEnabled(Event event) throws IdentityEventException {

        boolean isEnabled = false;

        Map<String, Object> eventProperties = event.getEventProperties();
        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);

        Property[] identityProperties;
        try {
            identityProperties = NotificationTaskDataHolder.getInstance()
                    .getIdentityGovernanceService().getConfiguration(getPropertyNames(), tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException("Error while retrieving Account Locking Handler properties.", e);
        }

        for (Property identityProperty : identityProperties) {
            if (NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED.equals(identityProperty.getName())) {
                isEnabled = Boolean.parseBoolean(identityProperty.getValue());
                break;
            }
        }

        return isEnabled;
    }

    /**
     * If the user-store has specific timestamp format defined, calculate login time in the defined format.
     * @param userStoreManager User-store-manager instance.
     * @return Last-login-time in the defined format.
     */
    protected String getLastLoginTimeValue(UserStoreManager userStoreManager) {

        String timeStampFormat = userStoreManager.getRealmConfiguration().getUserStoreProperty
                (UserStoreConfigConstants.dateAndTimePattern);
        String currentTime = Long.toString(System.currentTimeMillis());
        if (StringUtils.isNotEmpty(timeStampFormat)) {
            currentTime = new SimpleDateFormat(timeStampFormat).format(new Date());
        }
        return currentTime;
    }
}
