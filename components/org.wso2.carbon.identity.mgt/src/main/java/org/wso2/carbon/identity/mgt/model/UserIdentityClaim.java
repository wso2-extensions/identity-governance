/*
 * Copyright (c) 2015, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.mgt.model;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.mgt.IdentityMgtConstants;
import org.wso2.carbon.identity.mgt.store.UserIdentityDataStore;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * This encapsulates the user's data that is related user's login information
 */
public class UserIdentityClaim implements Serializable {

    private String userName;private long unlockTime;
    private long lastLogonTime;
    private long lastFailAttemptTime;
    private int failedAttempts;
    private boolean accountLock;
    private boolean passwordChangeRequired;
    private boolean oneTimeLogin;
    private Map<String, String> userIdentityDataMap = new HashMap<>();
    private int tenantId;

    public UserIdentityClaim (String userName) {
        this.userName = userName;
    }

    public UserIdentityClaim(String userName, Map<String, String> userDataMap) {

        this.userName = userName;
        this.userIdentityDataMap = userDataMap;

        if (userDataMap.get(IdentityMgtConstants.Claim.UNLOCKING_TIME) != null) {
            String unlockTime = userDataMap.get(IdentityMgtConstants.Claim.UNLOCKING_TIME).trim();
            if (!unlockTime.isEmpty()) {
                setUnlockTime(Long.parseLong(unlockTime));
            } else {
                setUnlockTime(0);
            }
        }
        if (userDataMap.get(IdentityMgtConstants.Claim.ACCOUNT_LOCK) != null) {
            setAccountLock(Boolean.parseBoolean(userDataMap.get(IdentityMgtConstants.Claim.ACCOUNT_LOCK)));
        }

        if (userDataMap.get(IdentityMgtConstants.Claim.FAIL_LOGIN_ATTEMPTS) != null) {
            String failedAttempts = userDataMap.get(IdentityMgtConstants.Claim.FAIL_LOGIN_ATTEMPTS)
                    .trim();
            if (!failedAttempts.isEmpty()) {
                setFailAttempts(Integer.parseInt(failedAttempts));
            } else {
                setFailAttempts(0);
            }
        }
    }

    public boolean isAccountLocked() {
        if (unlockTime != 0 && unlockTime < System.currentTimeMillis()) {
            return false;
        }
        return accountLock;
    }

    public UserIdentityClaim setAccountLock(boolean accountLock) {
        this.accountLock = accountLock;
        this.userIdentityDataMap.put(IdentityMgtConstants.Claim.ACCOUNT_LOCK,
                Boolean.toString(accountLock));
        return this;
    }

    public boolean getAccountLock() {
        return accountLock;
    }

    public void setUnlockTime(long unlockTime) {
        this.unlockTime = unlockTime;
        this.userIdentityDataMap.put(IdentityMgtConstants.Claim.UNLOCKING_TIME,
                Long.toString(unlockTime));
    }

    public long getUnlockTime() {
        return unlockTime;
    }

    public int getFailAttempts() {
        return failedAttempts;
    }

    public void setFailAttempts(int failAttempts) {
        this.failedAttempts = failAttempts;
        this.userIdentityDataMap.put(IdentityMgtConstants.Claim.FAIL_LOGIN_ATTEMPTS,
                Integer.toString(failAttempts));
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
        if(StringUtils.isBlank(value)){
            return;
        } else if (UserIdentityDataStore.FAIL_LOGIN_ATTEMPTS.equalsIgnoreCase(claim)) {
            setFailAttempts(Integer.parseInt(value));
        } else if (UserIdentityDataStore.LAST_FAILED_LOGIN_ATTEMPT_TIME.equalsIgnoreCase(claim)) {
            setLastFailAttemptTime(Long.parseLong(value));
        } else if (UserIdentityDataStore.UNLOCKING_TIME.equalsIgnoreCase(claim)) {
            setUnlockTime(Long.parseLong(value));
        } else if (UserIdentityDataStore.ONE_TIME_PASSWORD.equalsIgnoreCase(claim)) {
            setOneTimeLogin(Boolean.parseBoolean(value));
        } else if (UserIdentityDataStore.PASSWORD_CHANGE_REQUIRED.equalsIgnoreCase(claim)) {
            setPasswordChangeRequired(Boolean.parseBoolean(value));
        } else if (UserIdentityDataStore.LAST_LOGON_TIME.equalsIgnoreCase(claim)) {
            setLastLogonTime(Long.parseLong(value));
        } else if (UserIdentityDataStore.ACCOUNT_LOCK.equalsIgnoreCase(claim)) {
            setAccountLock(Boolean.parseBoolean(value));
        }
    }

    public long getLastLogonTime() {
        return lastLogonTime;
    }

    public void setLastLogonTime(long lastLogonTime) {
        this.lastLogonTime = lastLogonTime;
        this.userIdentityDataMap.put(UserIdentityDataStore.LAST_LOGON_TIME,
                Long.toString(lastLogonTime));
    }

    public long getLastFailAttemptTime() {
        return lastFailAttemptTime;
    }

    public void setLastFailAttemptTime(long lastFailAttemptTime) {
        this.lastFailAttemptTime = lastFailAttemptTime;
        this.userIdentityDataMap.put(UserIdentityDataStore.LAST_FAILED_LOGIN_ATTEMPT_TIME,
                Long.toString(lastFailAttemptTime));
    }

    public void setFailAttempts() {
        this.failedAttempts++;
        this.userIdentityDataMap.put(UserIdentityDataStore.FAIL_LOGIN_ATTEMPTS,
                Integer.toString(failedAttempts));
    }

    public boolean getOneTimeLogin() {
        return oneTimeLogin;
    }

    public void setOneTimeLogin(boolean oneTimeLogin) {
        this.oneTimeLogin = oneTimeLogin;
        this.userIdentityDataMap.put(UserIdentityDataStore.ONE_TIME_PASSWORD,
                Boolean.toString(oneTimeLogin));
    }

    public boolean getPasswordChangeRequired() {
        return passwordChangeRequired;
    }

    public void setPasswordChangeRequired(boolean passwordChangeRequired) {
        this.passwordChangeRequired = passwordChangeRequired;
        this.userIdentityDataMap.put(UserIdentityDataStore.PASSWORD_CHANGE_REQUIRED,
                Boolean.toString(passwordChangeRequired));
    }
}
