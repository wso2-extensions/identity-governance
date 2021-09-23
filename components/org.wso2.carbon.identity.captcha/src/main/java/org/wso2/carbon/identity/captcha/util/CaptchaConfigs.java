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

package org.wso2.carbon.identity.captcha.util;

import java.util.Map;

/**
 * Captcha Configs class.
 */
public class CaptchaConfigs {

    private String captchaProviderName;

    private Map<String, Object> captchaProperties;

    public CaptchaConfigs(String captchaProviderName, Map<String, Object> captchaProperties) {

        this.captchaProviderName = captchaProviderName;
        this.captchaProperties = captchaProperties;
    }

    public String getCaptchaProviderName() {

        return captchaProviderName;
    }

    public void setCaptchaProviderName(String captchaProviderName) {

        this.captchaProviderName = captchaProviderName;
    }

    public Map<String, Object> getCaptchaProperties() {

        return captchaProperties;
    }

    public void setCaptchaProperties(Map<String, Object> captchaProperties) {

        this.captchaProperties = captchaProperties;
    }
}
