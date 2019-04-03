/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.model;

import java.sql.Timestamp;

/**
 * ConfirmationCodeData model.
 */
public class ConfirmationCodeData {

    private User user;
    private String code;
    private String scenario;
    private Enum step;
    private Timestamp timeCreated;

    public ConfirmationCodeData() {

    }

    public ConfirmationCodeData(User user, String scenario, Enum step) {

        this.user = user;
        this.scenario = scenario;
        this.step = step;
    }

    public ConfirmationCodeData(User user, String code, String scenario, Enum step) {

        this.user = user;
        this.code = code;
        this.scenario = scenario;
        this.step = step;
    }

    public ConfirmationCodeData(User user, String code, String scenario, Enum step, Timestamp timeCreated) {

        this.user = user;
        this.code = code;
        this.scenario = scenario;
        this.step = step;
        this.timeCreated = timeCreated;
    }

    public User getUser() {

        return user;
    }

    public void setUser(User user) {

        this.user = user;
    }

    public String getCode() {

        return code;
    }

    public void setCode(String code) {

        this.code = code;
    }

    public String getScenario() {

        return scenario;
    }

    public void setScenario(String scenario) {

        this.scenario = scenario;
    }

    public Enum getStep() {

        return step;
    }

    public void setStep(Enum step) {

        this.step = step;
    }

    public Timestamp getTimeCreated() {

        return timeCreated;
    }

    public void setTimeCreated(Timestamp timeCreated) {

        this.timeCreated = timeCreated;
    }
}
