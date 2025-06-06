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

package org.wso2.carbon.identity.captcha.provider.mgt.provider;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;

import java.util.List;
import java.util.Map;

/**
 * This interface is used to provide the captcha provider.
 */
public interface AbstractCaptchaProvider {

    /**
     * This method is used to get the captcha type.
     *
     * @return Captcha type.
     */
    String getCaptchaType();

    /**
     * Get the unique identifier for this captcha provider.
     *
     * @return the captcha identifier.
     */
    String getCaptchaIdentifier();

    /**
     * Get the identifier for the captcha response parameter expected in the request.
     *
     * @return the captcha response identifier.
     */
    String getCaptchaResponseIdentifier();

    /**
     * Get HTML widget attributes to render the reCAPTCHA widget on the client side.
     *
     * @return map containing widget attributes.
     */
    Map<String, String> getWidgetAttributes();

    /**
     * Get JavaScript script attributes needed to load and render reCAPTCHA.
     *
     * @return list of script attribute maps.
     */
    List<Map<String, String>> getScriptAttributes();

    /**
     * Get JavaScript function names used for rendering, resetting, and retrieving the reCAPTCHA response.
     *
     * @return map of function names.
     */
    Map<String, String> getCaptchaFunctions();

    /**
     * Get the reCAPTCHA API URL to load the reCAPTCHA script on the client side.
     *
     * @return the API URL.
     */
    String getCaptchaApiUrl();

    /**
     * Get the verification URL for reCAPTCHA validation.
     *
     * @return the captcha verification URL.
     */
    String getCaptchaVerifyUrl();

    /**
     * This method is used to verify the captcha response.
     *
     * @param captchaResponse captcha response received from the provider.
     * @return true if the captcha is verified, false otherwise.
     */
    boolean verifyCaptcha(String captchaResponse) throws CaptchaException;

}
