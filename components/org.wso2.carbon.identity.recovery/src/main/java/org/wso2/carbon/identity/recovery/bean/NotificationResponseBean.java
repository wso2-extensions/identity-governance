/*
 *
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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
package org.wso2.carbon.identity.recovery.bean;

import org.wso2.carbon.identity.application.common.model.User;

import java.io.Serializable;

/**
 * Bean that encapsulates the verification info.
 */
public class NotificationResponseBean implements Serializable {

    private static final long serialVersionUID = -2913500114444797062L;

    /**
     * User identifier according to the user store
     */
    private User user;

    /**
     * Key that is received after successful verification
     */
    private String key;

    /**
     * Operation successful code.
     */
    private String code;

    /**
     * Operation info message.
     */
    private String message;

    /**
     * Channel which the notifications were sent to the user.
     */
    private String notificationChannel;

    /**
     * Recovery Id when notifications are externally managed.
     */
    private String recoveryId;

    /**
     * Get recovery Id when the notifications are externally managed.
     * @return Recovery Id
     */
    public String getRecoveryId() {
        return recoveryId;
    }

    /**
     * Set recovery Id when the notifications are externally managed.
     * @param recoveryId Recovery secret
     */
    public void setRecoveryId(String recoveryId) {
        this.recoveryId = recoveryId;
    }

    /**
     * Get the status code of the operation.
     *
     * @return Error code
     */
    public String getCode() {
        return code;
    }

    /**
     * Set the status code of the operation.
     *
     * @param code Error code
     */
    public void setCode(String code) {
        this.code = code;
    }

    /**
     * Get the status details massage for the operation code.
     *
     * @return  Detailed Message about the status
     */
    public String getMessage() {
        return message;
    }

    /**
     * Set the status details massage for the operation code.
     *
     * @param message Detailed Message about the status
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Get notification channel which the notifications were sent to the user.
     *
     * @return Notification Channel
     */
    public String getNotificationChannel() {
        return notificationChannel;
    }

    /**
     * Set notification channel which the notifications were sent to the user.
     *
     * @param notificationChannel Notification channel
     */
    public void setNotificationChannel(String notificationChannel) {
        this.notificationChannel = notificationChannel;
    }

    public NotificationResponseBean(User user, String key) {
        this.user = user;
        this.key = key;
    }

    public NotificationResponseBean(User user) {
        this.user = user;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }
}
