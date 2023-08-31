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

    private String recoveryFlowId;

    private Timestamp timeCreated;

    private int attempt;

    private int resendCount;

    public UserRecoveryFlowData(String recoveryFlowId) {

        this.recoveryFlowId = recoveryFlowId;
    }
    public UserRecoveryFlowData(String recoveryFlowId, Timestamp timeCreated) {

        this.recoveryFlowId = recoveryFlowId;
        this.timeCreated = timeCreated;
    }

    public UserRecoveryFlowData(String recoveryFlowId, Timestamp timeCreated, int attempt, int resendCount) {

        this.recoveryFlowId = recoveryFlowId;
        this.timeCreated = timeCreated;
        this.attempt = attempt;
        this.resendCount = resendCount;
    }

    /**
     * Get the time created.
     *
     * @return Created time.
     */
    public Timestamp getTimeCreated() {

        return timeCreated;
    }

    /**
     * Set the time created.
     *
     * @param timeCreated Created time.
     */
    public void setTimeCreated(Timestamp timeCreated) {

        this.timeCreated = timeCreated;
    }

    /**
     * Get the recovery flow id.
     *
     * @return Recovery Flow Id.
     */
    public String getRecoveryFlowId() {

        return recoveryFlowId;
    }

    /**
     * Set the recovery flow id.
     *
     * @param recoveryFlowId Recovery Flow Id.
     */
    public void setRecoveryFlowId(String recoveryFlowId) {

        this.recoveryFlowId = recoveryFlowId;
    }

    /**
     * Get the attempt.
     *
     * @return attempt.
     */
    public int getAttempt() {

        return attempt;
    }

    /**
     * Set the attempt.
     *
     * @param attempt OTP attempt.
     */
    public void setAttempt(int attempt) {

        this.attempt = attempt;
    }

    /**
     * Get the resendCount.
     *
     * @return resendCount.
     */
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
