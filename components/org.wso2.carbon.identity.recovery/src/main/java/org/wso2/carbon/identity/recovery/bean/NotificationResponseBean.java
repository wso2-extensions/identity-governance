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

import org.wso2.carbon.identity.mgt.User;

import java.io.Serializable;

/**
 * Bean that encapsulates the verification info.
 */
public class NotificationResponseBean implements Serializable {

    private static final long serialVersionUID = -2913500114444797062L;

    /**
     * user identifier according to the user store.
     */
    private User user;

    /**
     * key that is received after successful verification.
     */
    private String key;


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
