/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.model;

import java.sql.Timestamp;

/**
 * This object represents an entry of the identity metadata database.
 */
public class UserRecoveryFlowData {
    private String recoveryId;

    private Timestamp timeCreated;

    private int attempts;

    private int resendCount;

    public UserRecoveryFlowData(String recoveryId) {

        this.recoveryId = recoveryId;
    }
    public UserRecoveryFlowData(String recoveryId, Timestamp timeCreated) {

        this.recoveryId = recoveryId;
        this.timeCreated = timeCreated;
    }

    public UserRecoveryFlowData(String recoveryId, Timestamp timeCreated, int attempts, int resendCount) {

        this.recoveryId = recoveryId;
        this.timeCreated = timeCreated;
        this.attempts = attempts;
        this.resendCount = resendCount;
    }

    public Timestamp getTimeCreated() {

        return timeCreated;
    }

    public void setTimeCreated(Timestamp timeCreated) {

        this.timeCreated = timeCreated;
    }

    public String getRecoveryId() {
        return recoveryId;
    }

    /**
     * Set the recovery id.
     *
     * @param recoveryId Recovery Id.
     */
    public void setRecoveryId(String recoveryId) {

        this.recoveryId = recoveryId;
    }

    public int getAttempts() {
        return attempts;
    }

    /**
     * Set the attempts.
     *
     * @param attempts OTP attempts.
     */
    public void setAttempts(int attempts) {

        this.attempts = attempts;
    }

    public int getResendCount() {
        return resendCount;
    }

    /**
     * Set the resendCount.
     *
     * @param resendCount resendCount.
     */
    public void setResendCount(int resendCount) {

        this.resendCount = resendCount;
    }
}
