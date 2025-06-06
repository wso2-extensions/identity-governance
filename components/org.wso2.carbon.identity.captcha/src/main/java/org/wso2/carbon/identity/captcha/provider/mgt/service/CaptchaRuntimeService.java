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

package org.wso2.carbon.identity.captcha.provider.mgt.service;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;

import java.util.List;
import java.util.Map;

public interface CaptchaRuntimeService {

    /**
     * Checks if CAPTCHA is enabled.
     *
     * @return {@code true} if CAPTCHA is enabled; {@code false} otherwise.
     */
    boolean isCaptchaEnabled();

    /**
     * Verifies the provided CAPTCHA response with the current CAPTCHA provider.
     *
     * @param captchaResponse The CAPTCHA response to verify.
     * @return {@code true} if the verification is successful; {@code false} otherwise.
     * @throws CaptchaException if verification fails or an error occurs.
     */
    boolean verifyCaptcha(String captchaResponse) throws CaptchaException;

    /**
     * Retrieves the CAPTCHA site key from the CAPTCHA configuration.
     *
     * @return The CAPTCHA site key.
     * @throws CaptchaServerException if an error occurs while retrieving the site key.
     */
    String getCaptchaSiteKey() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA site secret from the CAPTCHA configuration.
     *
     * @return The CAPTCHA site secret.
     * @throws CaptchaServerException if an error occurs while retrieving the site secret.
     */
    String getCaptchaSiteSecret() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA project ID from the CAPTCHA configuration.
     *
     * @return The CAPTCHA project ID.
     * @throws CaptchaServerException if an error occurs while retrieving the project ID.
     */
    String getCaptchaProjectID() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA API key from the CAPTCHA configuration.
     *
     * @return The CAPTCHA API key.
     * @throws CaptchaServerException if an error occurs while retrieving the API key.
     */
    String getCaptchaApiKey() throws CaptchaServerException;

    /**
     * Retrieves widget attributes for rendering the CAPTCHA on the frontend.
     *
     * @return A map containing widget attributes; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the widget attributes.
     */
    Map<String, String> getWidgetAttributes() throws CaptchaServerException;

    /**
     * Retrieves script attributes for loading CAPTCHA scripts on the frontend.
     *
     * @return A list of maps containing script attributes; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the script attributes.
     */
    List<Map<String, String>> getScriptAttributes() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA identifier used by the provider.
     *
     * @return The CAPTCHA identifier; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the identifier.
     */
    String getCaptchaIdentifier() throws CaptchaServerException;

    /**
     * Retrieves the identifier for the CAPTCHA response field.
     *
     * @return The CAPTCHA response identifier; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the response identifier.
     */
    String getCaptchaResponseIdentifier() throws CaptchaServerException;

    /**
     * Retrieves any additional CAPTCHA functions defined by the provider.
     *
     * @return A map containing CAPTCHA functions; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the functions.
     */
    Map<String, String> getCaptchaFunctions() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA API URL for the configured provider.
     *
     * @return The CAPTCHA API URL; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the API URL.
     */
    String getCaptchaApiUrl() throws CaptchaServerException;

    /**
     * Retrieves the URL used for CAPTCHA verification by the configured provider.
     *
     * @return The CAPTCHA verification URL; empty if not available.
     * @throws CaptchaServerException if an error occurs while retrieving the verification URL.
     */
    String getCaptchaVerifyUrl() throws CaptchaServerException;

    /**
     * Retrieves the CAPTCHA score threshold configured for the system.
     *
     * @return The CAPTCHA score threshold.
     */
    String getCaptchaScoreThreshold();

    /**
     * Retrieves the CAPTCHA warn score threshold configured for the system.
     *
     * @return The CAPTCHA warn score threshold.
     */
    String getCaptchaWarnScoreThreshold();
}
