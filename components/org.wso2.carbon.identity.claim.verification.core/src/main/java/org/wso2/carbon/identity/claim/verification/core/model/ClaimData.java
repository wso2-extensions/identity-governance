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

/**
 * ClaimData model.
 */
public class ClaimData {

    private User user;
    private int claimId;
    private String claimValue;
    private Enum verificationStatus;

    public ClaimData() {

    }

    public ClaimData(User user, int claimId, Enum verificationStatus) {

        this.user = user;
        this.claimId = claimId;
        this.verificationStatus = verificationStatus;
    }

    public ClaimData(User user, int claimId, String claimValue, Enum verificationStatus) {

        this.user = user;
        this.claimId = claimId;
        this.claimValue = claimValue;
        this.verificationStatus = verificationStatus;
    }

    public User getUser() {

        return user;
    }

    public void setUser(User user) {

        this.user = user;
    }

    public int getClaimId() {

        return claimId;
    }

    public void setClaimId(int claimId) {

        this.claimId = claimId;
    }

    public String getClaimValue() {

        return claimValue;
    }

    public void setClaimValue(String claimValue) {

        this.claimValue = claimValue;
    }

    public Enum getVerificationStatus() {

        return verificationStatus;
    }

    public void setVerificationStatus(Enum verificationStatus) {

        this.verificationStatus = verificationStatus;
    }
}
