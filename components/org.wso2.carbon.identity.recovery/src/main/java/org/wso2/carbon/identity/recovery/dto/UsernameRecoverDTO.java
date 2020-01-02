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
 * Object that encapsulates the username recovery notification for a successful username recovery.
 */
public class UsernameRecoverDTO {

    /**
     * Response code.
     */
    private String code;

    /**
     * Description about the response.
     */
    private String message;

    /**
     * User notified channel.
     */
    private String notificationChannel;

    /**
     * Username of the user.
     * NOTE: This will be used when the notification channel is EXTERNAL.
     */
    private String username;

    /**
     * Get the response code.
     *
     * @return Response code
     */
    public String getCode() {

        return code;
    }

    /**
     * Set the response code.
     *
     * @param code Response code
     */
    public void setCode(String code) {

        this.code = code;
    }

    /**
     * Get the response message.
     *
     * @return Response message
     */
    public String getMessage() {

        return message;
    }

    /**
     * Set the response message.
     *
     * @param message Response message
     */
    public void setMessage(String message) {

        this.message = message;
    }

    /**
     * Get the channel which the notification was sent.
     *
     * @return Notification channel
     */
    public String getNotificationChannel() {

        return notificationChannel;
    }

    /**
     * Set the channel which the notification was sent.
     *
     * @param notificationChannel Notification channel
     */
    public void setNotificationChannel(String notificationChannel) {

        this.notificationChannel = notificationChannel;
    }

    /**
     * Get the username of the user.
     * (NOTE: use when the notificationChannel is EXTERNAL)
     *
     * @return Username
     */
    public String getUsername() {

        return username;
    }

    /**
     * Set the username of the user.
     * (NOTE: use when the notificationChannel is EXTERNAL)
     *
     * @return Username
     */
    public void setUsername(String username) {

        this.username = username;
    }
}
