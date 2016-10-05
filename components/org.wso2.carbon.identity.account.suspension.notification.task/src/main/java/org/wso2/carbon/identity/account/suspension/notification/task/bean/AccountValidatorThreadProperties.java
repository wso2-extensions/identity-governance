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
package org.wso2.carbon.identity.account.suspension.notification.task.bean;

import java.util.Date;

public class AccountValidatorThreadProperties {

    private long delayForSuspension = 90;
    private long[] notificationDelays = { 30, 45, 60, 75 };
    private Date notificationTriggerTime = null;
    private String tenantDomain = "carbon.super";
    private int tenantId = -1234;

    public Date getNotificationTriggerTime() {
        return notificationTriggerTime;
    }

    public void setNotificationTriggerTime(Date notificationTriggerTime) {
        this.notificationTriggerTime = notificationTriggerTime;
    }

    public long getDelayForSuspension() {
        return delayForSuspension;
    }

    public void setDelayForSuspension(long delayForSuspension) {
        this.delayForSuspension = delayForSuspension;
    }

    public long[] getNotificationDelays() {
        return notificationDelays;
    }

    public void setNotificationDelays(long[] notificationDelays) {
        this.notificationDelays = notificationDelays;
    }

    public String getTenantDomain() {
        return tenantDomain;
    }

    public void setTenantDomain(String tenantDomain) {
        this.tenantDomain = tenantDomain;
    }

    public int getTenantId() {
        return tenantId;
    }

    public void setTenantId(int tenantId) {
        this.tenantId = tenantId;
    }

}
