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
 * limitations und
 */

package org.wso2.carbon.identity.account.suspension.notification.task.internal;

import org.osgi.framework.BundleContext;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrievalFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.service.RealmService;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class NotificationTaskDataHolder {

    private static volatile NotificationTaskDataHolder accountServiceDataHolder = new NotificationTaskDataHolder();

    private IdentityEventService identityEventService;
    private IdentityGovernanceService identityGovernanceService;
    private BundleContext bundleContext;
    private Map<String, NotificationReceiversRetrievalFactory> notificationReceiversRetrievalFactories =
            new HashMap<>();
    private RealmService realmService;
    private String notificationTriggerTime;
    private String schedulerDelay;
    private String notificationSendingThreadPoolSize = "1";

    public int getNotificationSendingThreadPoolSize() {
        return Integer.parseInt(notificationSendingThreadPoolSize);
    }

    public void setNotificationSendingThreadPoolSize(String notificationSendingThreadPoolSize) {
        this.notificationSendingThreadPoolSize = notificationSendingThreadPoolSize;
    }

    public Date getNotificationTriggerTime() throws ParseException{
        DateFormat dateFormat = new SimpleDateFormat(NotificationConstants.TRIGGER_TIME_FORMAT);
        return dateFormat.parse(notificationTriggerTime);
    }

    public void setNotificationTriggerTime(String notificationTriggerTime) {
        this.notificationTriggerTime = notificationTriggerTime;
    }

    public String getSchedulerDelay() {
        return schedulerDelay;
    }

    public void setSchedulerDelay(String schedulerDelay) {
        this.schedulerDelay = schedulerDelay;
    }

    private NotificationTaskDataHolder() {

    }

    public static NotificationTaskDataHolder getInstance() {
        return accountServiceDataHolder;
    }

    public IdentityEventService getIdentityEventService() {
        return identityEventService;
    }

    public void setIdentityEventService(IdentityEventService identityEventService) {
        this.identityEventService = identityEventService;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {
        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {
        this.identityGovernanceService = identityGovernanceService;
    }

    public BundleContext getBundleContext() {
        return bundleContext;
    }

    public void setBundleContext(BundleContext bundleContext) {
        this.bundleContext = bundleContext;
    }

    public Map<String, NotificationReceiversRetrievalFactory> getNotificationReceiversRetrievalFactories() {
        return notificationReceiversRetrievalFactories;
    }

    public void setRealmService(RealmService realmService) {
        this.realmService = realmService;
    }

    public RealmService getRealmService() {
        return realmService;
    }
}
