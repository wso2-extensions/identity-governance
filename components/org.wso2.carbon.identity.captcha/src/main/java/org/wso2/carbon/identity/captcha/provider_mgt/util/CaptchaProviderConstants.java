/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.provider_mgt.util;

import org.wso2.carbon.identity.captcha.provider_mgt.provider.AbstractCaptchaProvider;
import org.wso2.carbon.identity.captcha.provider_mgt.provider.GoogleRecaptchaEnterpriseProvider;
import org.wso2.carbon.identity.captcha.provider_mgt.provider.GoogleRecaptchaProvider;

import java.util.HashMap;
import java.util.Map;

public class CaptchaProviderConstants {

    // Captcha Provider Configurations
    public static final String CAPTCHA_RESOURCE_TYPE = "CAPTCHA_CONFIGURATION";
    public static final String CAPTCHA_RESOURCE_NAME = "captcha";
    public static final String CAPTCHA_PROVIDER_CONFIG = "captchaProvider";
    public static final String CAPTCHA_SITEKEY_CONFIG = "siteKey";
    public static final String CAPTCHA_SITESECRET_CONFIG = "siteSecret";
    public static final String CAPTCHA_SCORE_THRESHOLD_CONFIG = "scoreThreshold";
    public static final String CAPTCHA_SCORE_WARN_THRESHOLD_CONFIG = "scoreWarnThreshold";
    public static final String CAPTCHA_API_KEY = "apiKey";
    public static final String CAPTCHA_PROJECT_ID = "projectId";
    public static final String CAPTCHA_REQUEST_WRAP_URLS = "requestWrapUrls";
    public static final String CAPTCHA_FAILED_REDIRECT_URLS = "failedRedirectUrls";
    public static final String CAPTCHA_BYPASSED_API_ENDPOINTS = "bypassedApiEndpoints";
    // Google ReCAPTCHA
    public static final String RECAPTCHA_TYPE = "reCaptcha";
    public static final String RECAPTCHA_IDENTIFIER = "g-recaptcha";
    public static final String RECAPTCHA_RESPONSE_IDENTIFIER = "g-recaptcha-response";
    public static final String RECAPTCHA_API_URL = "https://www.google.com/recaptcha/api.js";
    public static final String RECAPTCHA_VERIFY_URL = "https://www.google.com/recaptcha/api/siteverify";
    public static final String RECAPTCHA_RENDER_FUNCTION = "grecaptcha.render";
    public static final String RECAPTCHA_RESET_FUNCTION = "grecaptcha.reset";
    public static final String RECAPTCHA_GET_RESPONSE_FUNCTION = "grecaptcha.getResponse";
    public static final double RECAPTCHA_DEFAULT_THRESHOLD = 0.5;
    public static final double RECAPTCHA_DEFAULT_WARN_THRESHOLD = 0.7;

    // Google ReCAPTCHA Enterprise
    public static final String RECAPTCHA_ENTERPRISE_TYPE = "recaptcha-enterprise";
    public static final String RECAPTCHA_ENTERPRISE_RENDER_FUNCTION = "grecaptcha.enterprise.render";
    public static final String RECAPTCHA_ENTERPRISE_RESET_FUNCTION = "grecaptcha.enterprise.reset";
    public static final String RECAPTCHA_ENTERPRISE_GET_RESPONSE_FUNCTION = "grecaptcha.enterprise.getResponse";
    public static final String RECAPTCHA_ENTERPRISE_VERIFY_URL = "https://recaptchaenterprise.googleapis.com";

    // Common POST Parameters
    public static final String PARAM_SECRET = "secret";
    public static final String PARAM_RESPONSE = "response";
    public static final String PARAM__SCORE = "score";
    public static final String PARAM_SUCCESS = "success";

    public static class CaptchaProviderRegistry {

        private static final Map<String, Class<? extends AbstractCaptchaProvider>> PROVIDER_MAP = new HashMap<>();

        static {
            PROVIDER_MAP.put(RECAPTCHA_TYPE, GoogleRecaptchaProvider.class);
            PROVIDER_MAP.put(RECAPTCHA_ENTERPRISE_TYPE, GoogleRecaptchaEnterpriseProvider.class);
            // Add other providers here, e.g.:
            // PROVIDER_MAP.put("friendly-captcha", FriendlyCaptchaProvider.class);
        }

        public static Map<String, Class<? extends AbstractCaptchaProvider>> getProviderClasses() {

            return PROVIDER_MAP;
        }
    }
}
