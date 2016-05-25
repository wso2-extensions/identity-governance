/*
 * Copyright (c) 2010, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.model;


import org.wso2.carbon.identity.application.common.model.User;

/**
 * This object represents an entry of the identity metadata database.
 */
public class UserRecoveryData {
    private User user;
//    private enum  step;
//    private enum scenario;
    private String secret;
    private String expireTime;
    private String metaData;
    private int sequence;
    private boolean isValid;// Need to romove


    public UserRecoveryData() {
    }

    public UserRecoveryData(User user) {
        this.user = user;
        int expireTimeInMinutes = 10;
        //TODO read this from config
        this.expireTime = Long.toString(System.currentTimeMillis() + (expireTimeInMinutes * 60 * 1000L));
    }

    public UserRecoveryData(User user, String secret, int sequence) {
        this.user = user;
        this.sequence = sequence;
        this.secret = secret;
        //TODO read this from config
        int expireTimeInMinutes = 10;
        this.expireTime = Long.toString(System.currentTimeMillis() + (expireTimeInMinutes * 60 * 1000L));
    }

    public UserRecoveryData(User user, String secret, int sequence, String metaData) {
        this.user = user;
        this.sequence = sequence;
        this.secret = secret;
        this.metaData = metaData;
        //TODO read this from config
        int expireTimeInMinutes = 10;
        this.expireTime = Long.toString(System.currentTimeMillis() + (expireTimeInMinutes * 60 * 1000L));
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public int getSequence() {
        return sequence;
    }

    public void setSequence(int sequence) {
        this.sequence = sequence;
    }

    public String getSecret() {
        return secret;
    }

    public void setSecret(String secret) {
        this.secret = secret;
    }

    public String getExpireTime() {
        return expireTime;
    }

    public void setExpireTime(String expireTime) {
        this.expireTime = expireTime;
    }

    public boolean isValid() {
        return isValid;
    }

    public void setIsValid(boolean isValid) {
        this.isValid = isValid;
    }

    public String getMetaData() {
        return metaData;
    }

    public void setMetaData(String metaData) {
        this.metaData = metaData;
    }
}
