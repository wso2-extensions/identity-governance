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

package org.wso2.carbon.identity.captcha.connector;

import java.util.Map;

/**
 * reCaptcha pre validation response.
 */
public class CaptchaPreValidationResponse {

    private boolean captchaRequired;

    private boolean maxLimitReached;

    private boolean enableCaptchaForDestination;

    private boolean postValidationRequired;

    private Map<String, String> requestAttributes;

    private String onFailRedirectUrl;

    public boolean isCaptchaRequired() {
        return captchaRequired;
    }

    public void setCaptchaRequired(boolean captchaRequired) {
        this.captchaRequired = captchaRequired;
    }

    public boolean isMaxLimitReached() {
        return maxLimitReached;
    }

    public void setMaxLimitReached(boolean maxLimitReached) {
        this.maxLimitReached = maxLimitReached;
    }

    public boolean isEnableCaptchaForDestination() {
        return enableCaptchaForDestination;
    }

    public void setEnableCaptchaForDestination(boolean enableCaptchaForDestination) {
        this.enableCaptchaForDestination = enableCaptchaForDestination;
    }

    public boolean isPostValidationRequired() {
        return postValidationRequired;
    }

    public void setPostValidationRequired(boolean postValidationRequired) {
        this.postValidationRequired = postValidationRequired;
    }

    public Map<String, String> getRequestAttributes() {
        return requestAttributes;
    }

    public void setRequestAttributes(Map<String, String> requestAttributes) {
        this.requestAttributes = requestAttributes;
    }

    public String getOnFailRedirectUrl() {
        return onFailRedirectUrl;
    }

    public void setOnFailRedirectUrl(String onFailRedirectUrl) {
        this.onFailRedirectUrl = onFailRedirectUrl;
    }
}
