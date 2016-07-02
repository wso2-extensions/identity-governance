/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.internal;

import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Captcha Data Holder.
 */
public class CaptchaDataHolder {

    private static CaptchaDataHolder instance = new CaptchaDataHolder();

    private boolean reCaptchaEnabled;

    private String reCaptchaAPIUrl;

    private String reCaptchaVerifyUrl;

    private String reCaptchaSiteKey;

    private String reCaptchaSecretKey;

    private IdentityGovernanceService identityGovernanceService;

    private RealmService realmService;

    private List<CaptchaConnector> captchaConnectors = new ArrayList<>();

    private Map<String, String> ssoLoginReCaptchaConnectorPropertyMap = new HashMap<>();

    private Map<String, String> pathBasedReCaptchaConnectorPropertyMap = new HashMap<>();

    private Map<String, String> passwordRecoveryReCaptchaConnectorPropertyMap = new HashMap<>();

    private CaptchaDataHolder() {

    }

    public static CaptchaDataHolder getInstance() {
        return instance;
    }

    public boolean isReCaptchaEnabled() {
        return reCaptchaEnabled;
    }

    public void setReCaptchaEnabled(boolean reCaptchaEnabled) {
        this.reCaptchaEnabled = reCaptchaEnabled;
    }

    public String getReCaptchaAPIUrl() {
        return reCaptchaAPIUrl;
    }

    public void setReCaptchaAPIUrl(String reCaptchaAPIUrl) {
        this.reCaptchaAPIUrl = reCaptchaAPIUrl;
    }

    public String getReCaptchaVerifyUrl() {
        return reCaptchaVerifyUrl;
    }

    public void setReCaptchaVerifyUrl(String reCaptchaVerifyUrl) {
        this.reCaptchaVerifyUrl = reCaptchaVerifyUrl;
    }

    public String getReCaptchaSiteKey() {
        return reCaptchaSiteKey;
    }

    public void setReCaptchaSiteKey(String reCaptchaSiteKey) {
        this.reCaptchaSiteKey = reCaptchaSiteKey;
    }

    public String getReCaptchaSecretKey() {
        return reCaptchaSecretKey;
    }

    public void setReCaptchaSecretKey(String reCaptchaSecretKey) {
        this.reCaptchaSecretKey = reCaptchaSecretKey;
    }

    public IdentityGovernanceService getIdentityGovernanceService() {
        return identityGovernanceService;
    }

    public void setIdentityGovernanceService(IdentityGovernanceService identityGovernanceService) {
        this.identityGovernanceService = identityGovernanceService;
    }

    public Map<String, String> getSSOLoginReCaptchaConnectorPropertyMap() {
        return ssoLoginReCaptchaConnectorPropertyMap;
    }

    public void setSSOLoginReCaptchaConnectorPropertyMap(Map<String, String> ssoLoginReCaptchaConnectorPropertyMap) {
        this.ssoLoginReCaptchaConnectorPropertyMap = ssoLoginReCaptchaConnectorPropertyMap;
    }

    public Map<String, String> getPathBasedReCaptchaConnectorPropertyMap() {
        return pathBasedReCaptchaConnectorPropertyMap;
    }

    public void setPathBasedReCaptchaConnectorPropertyMap(Map<String, String> pathBasedReCaptchaConnectorPropertyMap) {
        this.pathBasedReCaptchaConnectorPropertyMap = pathBasedReCaptchaConnectorPropertyMap;
    }

    public Map<String, String> getPasswordRecoveryReCaptchaConnectorPropertyMap() {
        return passwordRecoveryReCaptchaConnectorPropertyMap;
    }

    public void setPasswordRecoveryReCaptchaConnectorPropertyMap(
            Map<String, String> passwordRecoveryReCaptchaConnectorPropertyMap) {
        this.passwordRecoveryReCaptchaConnectorPropertyMap = passwordRecoveryReCaptchaConnectorPropertyMap;
    }

    public List<CaptchaConnector> getCaptchaConnectors() {
        return captchaConnectors;
    }

    public void addCaptchaConnector(CaptchaConnector captchaConnector) {
        this.captchaConnectors.add(captchaConnector);
    }

    public void setRealmService(RealmService realmService) {
        this.realmService = realmService;
    }

    public RealmService getRealmService() {
        if(realmService == null) {
            throw new RuntimeException("Realm Service is not available. Component did not start correctly.");
        }
        return realmService;
    }
}
