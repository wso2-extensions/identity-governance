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

package org.wso2.carbon.identity.captcha.provider.mgt.util;

import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

public class CaptchaUIUtils {

    /**
     * Checks whether CAPTCHA is enabled.
     *
     * @return true if CAPTCHA is enabled; false otherwise.
     */
    public static boolean isCaptchaEnabled() {

        return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().isCaptchaEnabled();
    }

    /**
     * Retrieves the identifier for the CAPTCHA challenge.
     *
     * @return the CAPTCHA identifier.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA identifier.
     */
    public static String getCaptchaIdentifier() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaIdentifier();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha identifier.", e);
        }
    }

    /**
     * Retrieves the identifier for the CAPTCHA response.
     *
     * @return the CAPTCHA response identifier.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA response identifier.
     */
    public static String getCaptchaResponseIdentifier() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaResponseIdentifier();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha response identifier.", e);
        }
    }

    /**
     * Retrieves the API URL for the CAPTCHA service.
     *
     * @return the CAPTCHA API URL.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA API URL.
     */
    public static String getCaptchaApiUrl() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaApiUrl();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha API URL.", e);
        }
    }

    /**
     * Retrieves the verification URL for the CAPTCHA service.
     *
     * @return the CAPTCHA verification URL.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA verification URL.
     */
    public static String getCaptchaVerifyUrl() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaVerifyUrl();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha verify URL.", e);
        }
    }

    /**
     * Retrieves the widget attributes required for rendering the CAPTCHA in the frontend.
     *
     * @return a map containing the CAPTCHA widget attributes.
     * @throws CaptchaClientException if an error occurs while retrieving the widget attributes.
     */
    public static Map<String, String> getWidgetAttributes() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getWidgetAttributes();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha widget attributes.", e);
        }
    }

    /**
     * Retrieves the script attributes required for rendering the CAPTCHA in the frontend.
     *
     * @return a list of maps, each containing script attributes for the CAPTCHA.
     * @throws CaptchaClientException if an error occurs while retrieving the script attributes.
     */
    public static List<Map<String, String>> getScriptAttributes() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getScriptAttributes();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha script attributes.", e);
        }
    }

    /**
     * Retrieves the client-side functions for initializing or interacting with the CAPTCHA.
     *
     * @return a map containing the CAPTCHA functions.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA functions.
     */
    public static Map<String, String> getCaptchaFunctions() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaFunctions();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving captcha functions.", e);
        }
    }

    /**
     * Retrieves the site key used for the CAPTCHA service.
     *
     * @return the CAPTCHA site key.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA site key.
     */
    public static String getCaptchaSiteKey() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaSiteKey();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha site key.", e);
        }
    }

    /**
     * Retrieves the site secret used for the CAPTCHA service.
     *
     * @return the CAPTCHA site secret.
     * @throws CaptchaClientException if an error occurs while retrieving the CAPTCHA site secret.
     */
    public static String getCaptchaSiteSecret() throws CaptchaClientException {

        try {
            return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaSiteSecret();
        } catch (CaptchaServerException e) {
            throw new CaptchaClientException("Error while retrieving the captcha site secret.", e);
        }
    }

    /**
     * Adds CAPTCHA-related headers to the given HTTP servlet request.
     *
     * @param request the HTTP servlet request to which CAPTCHA headers should be added.
     * @param headers a map containing CAPTCHA headers (e.g., reCaptcha, reCaptchaAPI, reCaptchaKey).
     */
    public static void addCaptchaHeaders(HttpServletRequest request, Map<String, List<String>> headers) {

        if (headers != null && headers.get("reCaptcha") != null) {
            request.setAttribute("reCaptcha", Boolean.TRUE.toString());
            request.setAttribute("reCaptchaAPI", headers.get("reCaptchaAPI").get(0));
            request.setAttribute("reCaptchaKey", headers.get("reCaptchaKey").get(0));
        }
    }
}

