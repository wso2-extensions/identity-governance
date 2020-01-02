/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.dto;

/**
 * Object that encapsulates the account recovery channels available for the user with the recovery code.
 */
public class RecoveryChannelInfoDTO {

    /**
     * Username of the recovered user.
     */
    private String username;

    /**
     * Recovery Code given to the user.
     */
    private String recoveryCode;

    /**
     * Available communication channels for the user.
     */
    private NotificationChannelDTO[] notificationChannelDTOs;

    /**
     * Get username of the recovered user.
     *
     * @return Username
     */
    public String getUsername() {

        return username;
    }

    /**
     * Set username of the recovered user.
     *
     * @param username Username
     */
    public void setUsername(String username) {

        this.username = username;
    }

    /**
     * Get the recovery Code of the user.
     *
     * @return Recovery Code of the user
     */
    public String getRecoveryCode() {

        return recoveryCode;
    }

    /**
     * Set the recovery Code for the user.
     *
     * @param recoveryCode Recovery Code
     */
    public void setRecoveryCode(String recoveryCode) {

        this.recoveryCode = recoveryCode;
    }

    /**
     * Get the notification channels available for the user.
     *
     * @return List of NotificationChannelDTO objects
     */
    public NotificationChannelDTO[] getNotificationChannelDTOs() {

        return notificationChannelDTOs;
    }

    /**
     * Set the available notification channel list of the user.
     *
     * @param notificationChannelDTOs List of NotificationChannelDTO
     */
    public void setNotificationChannelDTOs(NotificationChannelDTO[] notificationChannelDTOs) {

        this.notificationChannelDTOs = notificationChannelDTOs;
    }
}
