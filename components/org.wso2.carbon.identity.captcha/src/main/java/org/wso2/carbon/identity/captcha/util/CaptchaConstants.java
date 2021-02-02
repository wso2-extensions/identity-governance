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

/**
 * Captcha Constants.
 */
public class CaptchaConstants {

    private static ThreadLocal enableSecurityMechanism = new ThreadLocal();

    public static final String CARBON_HOME = "carbon.home";

    public static final String ERROR_PAGE = "/authenticationendpoint/retry.do";

    public static final String CAPTCHA_CONFIG_FILE_NAME = "captcha-config.properties";

    public static final String RE_CAPTCHA_ENABLED = "recaptcha.enabled";

    public static final String RE_CAPTCHA_API_URL = "recaptcha.api.url";

    public static final String RE_CAPTCHA_VERIFY_URL = "recaptcha.verify.url";

    public static final String RE_CAPTCHA_SITE_KEY = "recaptcha.site.key";

    public static final String RE_CAPTCHA_SECRET_KEY = "recaptcha.secret.key";

    public static final String FAIL_LOGIN_ATTEMPT_VALIDATOR_ENABLED = "failLoginAttemptValidator.enable";

    public static final String RE_CAPTCHA_FAILED_REDIRECT_URLS = "recaptcha.failed.redirect.urls";

    public static final String BASIC_AUTHENTICATOR = "BasicAuthenticator";

    public static final String BASIC_AUTH_MECHANISM = "basic";

    public static final class ReCaptchaConnectorPropertySuffixes {

        public static final String ENABLE = ".enable";

        public static final String ENABLE_ALWAYS = ".enable.always";

        public static final String CONNECTOR_IDENTIFIER_ATTRIBUTE = ".connector.identifier.attribute";

        public static final String MAX_ATTEMPTS = ".on.max.failed.attempts";

        public static final String USER_IDENTIFIER_ATTRIBUTE = ".user.identifier.attribute";

        public static final String RECAPTCHA_VERIFICATION_CLAIM = ".verification.claim";

        public static final String SECURED_DESTINATIONS = ".secured.destinations";

        public static final String SECURED_PAGES = ".secured.pages";
    }

    public static void setEnableSecurityMechanism(String mechanism) {

        enableSecurityMechanism.set(mechanism);
    }

    public static String getEnableSecurityMechanism() {

        return (String) enableSecurityMechanism.get();
    }

    public static void removeEnabledSecurityMechanism() {
        enableSecurityMechanism.remove();
    }
}
