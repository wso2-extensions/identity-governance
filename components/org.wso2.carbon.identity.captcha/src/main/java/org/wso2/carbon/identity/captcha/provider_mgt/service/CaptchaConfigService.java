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

package org.wso2.carbon.identity.captcha.provider_mgt.service;

import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.provider_mgt.provider.AbstractCaptchaProvider;

import java.util.Properties;

public interface CaptchaConfigService {

    /**
     * Load captcha provider configurations from the configuration manager for the current tenant.
     *
     * @throws CaptchaServerException If an error occurs while loading the configurations.
     */
    void loadCaptchaConfigs() throws CaptchaServerException;

    /**
     * Store captcha provider instances in the CaptchaDataHolder.
     *
     * @throws CaptchaServerException If an error occurs while initializing the captcha providers.
     */
    void storeCaptchaProviders() throws CaptchaServerException;

    /**
     * Get the active captcha provider configurations for the current tenant.
     *
     * @return Captcha provider configurations as a {@link Properties} object.
     * @throws CaptchaServerException If there is an error while retrieving the configurations.
     */
    Properties getActiveCaptchaProviderConfig() throws CaptchaServerException;

    /**
     * Check if captcha is enabled for the current tenant.
     *
     * @return {@code true} if captcha is enabled; {@code false} otherwise.
     */
    boolean isCaptchaEnabled();

    /**
     * Get the active captcha provider instance for the current tenant.
     *
     * @return The active {@link AbstractCaptchaProvider}.
     * @throws CaptchaServerException If the captcha provider is not configured.
     */
    AbstractCaptchaProvider getCaptchaProvider() throws CaptchaServerException;

    /**
     * Get the captcha site key for the current tenant.
     *
     * @return Captcha site key as a string.
     * @throws CaptchaServerException If the site key is not configured.
     */
    String getCaptchaSiteKey() throws CaptchaServerException;

    /**
     * Get the captcha site secret for the current tenant.
     *
     * @return Captcha site secret as a string.
     * @throws CaptchaServerException If the site secret is not configured.
     */
    String getCaptchaSiteSecret() throws CaptchaServerException;

    /**
     * Get the captcha project ID for the current tenant.
     *
     * @return Captcha project ID as a string.
     * @throws CaptchaServerException If the project ID is not configured.
     */
    String getCaptchaProjectID() throws CaptchaServerException;

    /**
     * Get the captcha API key for the current tenant.
     *
     * @return Captcha API key as a string.
     * @throws CaptchaServerException If the API key is not configured.
     */
    String getCaptchaApiKey() throws CaptchaServerException;

    /**
     * Get the captcha score threshold for the current tenant.
     *
     * @return Captcha score threshold as a string.
     */
    String getCaptchaScoreThreshold();

    /**
     * Get the captcha warn score threshold for the current tenant.
     *
     * @return Captcha warn score threshold as a string.
     */
    String getCaptchaWarnScoreThreshold();
}

