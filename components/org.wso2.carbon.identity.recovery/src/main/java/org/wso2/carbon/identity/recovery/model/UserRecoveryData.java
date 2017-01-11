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

import org.wso2.carbon.identity.mgt.User;

/**
 * This object represents an entry of the identity metadata database.
 */
public class UserRecoveryData {
    private User user;
    private String secret;
    private String remainingSetIds;

    private Enum recoveryScenario;
    private Enum recoveryStep;

    public UserRecoveryData(User user, String secret, Enum recoveryScenario, Enum recoveryStep) {
        this.user = user;
        this.secret = secret;
        this.recoveryScenario = recoveryScenario;
        this.recoveryStep = recoveryStep;
    }

    public UserRecoveryData(User user, String secret, Enum recoveryScenario) {
        this.user = user;
        this.secret = secret;
        this.recoveryScenario = recoveryScenario;
    }


    public String getRemainingSetIds() {
        return remainingSetIds;
    }

    public void setRemainingSetIds(String remainingSetIds) {
        this.remainingSetIds = remainingSetIds;
    }

    public String getSecret() {
        return secret;
    }

    public User getUser() {

        return user;
    }

    public Enum getRecoveryScenario() {
        return recoveryScenario;
    }

    public Enum getRecoveryStep() {
        return recoveryStep;
    }

    public void setRecoveryStep(Enum recoveryStep) {
        this.recoveryStep = recoveryStep;
    }
}
