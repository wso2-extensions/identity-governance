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

import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * reCaptcha pre validation response.
 */
public class CaptchaPreValidationResponse {

    private boolean captchaValidationRequired;

    private boolean maxFailedLimitReached;

    private boolean enableCaptchaForRequestPath;

    private Map<String, String> captchaAttributes;

    private boolean postValidationRequired;

    private List<String> onCaptchaFailRedirectUrls;

    private HttpServletRequest wrappedHttpServletRequest;

    public boolean isCaptchaValidationRequired() {
        return captchaValidationRequired;
    }

    public void setCaptchaValidationRequired(boolean captchaValidationRequired) {
        this.captchaValidationRequired = captchaValidationRequired;
    }

    public boolean isMaxFailedLimitReached() {
        return maxFailedLimitReached;
    }

    public void setMaxFailedLimitReached(boolean maxFailedLimitReached) {
        this.maxFailedLimitReached = maxFailedLimitReached;
    }

    public boolean isEnableCaptchaForRequestPath() {
        return enableCaptchaForRequestPath;
    }

    public void setEnableCaptchaForRequestPath(boolean enableCaptchaForRequestPath) {
        this.enableCaptchaForRequestPath = enableCaptchaForRequestPath;
    }

    public boolean isPostValidationRequired() {
        return postValidationRequired;
    }

    public void setPostValidationRequired(boolean postValidationRequired) {
        this.postValidationRequired = postValidationRequired;
    }

    public Map<String, String> getCaptchaAttributes() {
        if (captchaAttributes == null) {
            return Collections.emptyMap();
        }
        return captchaAttributes;
    }

    public void setCaptchaAttributes(Map<String, String> captchaAttributes) {
        this.captchaAttributes = captchaAttributes;
    }

    public List<String> getOnCaptchaFailRedirectUrls() {
        if (onCaptchaFailRedirectUrls == null) {
            return Collections.emptyList();
        }
        return onCaptchaFailRedirectUrls;
    }

    public void setOnCaptchaFailRedirectUrls(List<String> onCaptchaFailRedirectUrls) {
        this.onCaptchaFailRedirectUrls = onCaptchaFailRedirectUrls;
    }

    public HttpServletRequest getWrappedHttpServletRequest() {
        return wrappedHttpServletRequest;
    }

    public void setWrappedHttpServletRequest(HttpServletRequest wrappedHttpServletRequest) {
        this.wrappedHttpServletRequest = wrappedHttpServletRequest;
    }
}
