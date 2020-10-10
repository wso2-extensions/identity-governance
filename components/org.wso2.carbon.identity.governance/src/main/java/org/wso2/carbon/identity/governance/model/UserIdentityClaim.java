/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * This encapsulates the user's data that is related user's login information
 */
public class UserIdentityClaim implements Serializable {

    private static final long serialVersionUID = 6722253136997067191L;
    private String userName;
    private Map<String, String> userIdentityDataMap = new HashMap<>();
    private int tenantId;

    public UserIdentityClaim (String userName) {
        this.userName = userName;
    }

    public UserIdentityClaim(String userName, Map<String, String> userDataMap) {

        this.userName = userName;
        this.userIdentityDataMap = userDataMap;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public Map<String, String> getUserIdentityDataMap() {
        if(userIdentityDataMap == null){
            return Collections.emptyMap();
        }
        return userIdentityDataMap;
    }

    public void setUserIdentityDataMap(Map<String, String> userIdentityDataMap) {
        this.userIdentityDataMap = userIdentityDataMap;
    }

    public int getTenantId() {
        return tenantId;
    }

    public void setTenantId(int tenantId) {
        this.tenantId = tenantId;
    }

    /**
     * Sets user identity data claim
     *
     * @param claim
     * @param value
     */
    public void setUserIdentityDataClaim(String claim, String value) {
        userIdentityDataMap.put(claim, value);
    }
}
