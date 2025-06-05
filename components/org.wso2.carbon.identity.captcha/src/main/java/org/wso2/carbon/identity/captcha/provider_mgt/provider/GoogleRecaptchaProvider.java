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

package org.wso2.carbon.identity.captcha.provider_mgt.provider;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider_mgt.util.CaptchaProviderConstants;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.captcha.provider_mgt.util.CaptchaProviderUtils.isValidCaptcha;

public class GoogleRecaptchaProvider implements AbstractCaptchaProvider {

    private static final Log log = LogFactory.getLog(GoogleRecaptchaProvider.class);

    @Override
    public String getCaptchaType() {

        return CaptchaProviderConstants.RECAPTCHA_TYPE;
    }

    @Override
    public String getCaptchaIdentifier() {

        return CaptchaProviderConstants.RECAPTCHA_IDENTIFIER;
    }

    @Override
    public String getCaptchaResponseIdentifier() {

        return CaptchaProviderConstants.RECAPTCHA_RESPONSE_IDENTIFIER;
    }

    @Override
    public String getCaptchaVerifyUrl() {

        return CaptchaProviderConstants.RECAPTCHA_VERIFY_URL;
    }

    @Override
    public Map<String, String> getWidgetAttributes() {

        Map<String, String> attributes = new HashMap<>();
        attributes.put("class", CaptchaProviderConstants.RECAPTCHA_IDENTIFIER);
        attributes.put("data-size", "invisible");
        attributes.put("data-callback", "onCompleted");
        return attributes;
    }

    @Override
    public List<Map<String, String>> getScriptAttributes() {

        List<Map<String, String>> attributes = new ArrayList<>();
        attributes.add(Collections.singletonMap("async", "true"));
        attributes.add(Collections.singletonMap("defer", "true"));
        attributes.add(Collections.singletonMap("src", getCaptchaApiUrl()));
        return attributes;
    }

    @Override
    public Map<String, String> getCaptchaFunctions() {

        Map<String, String> functions = new HashMap<>();
        functions.put("render", CaptchaProviderConstants.RECAPTCHA_RENDER_FUNCTION);
        functions.put("reset", CaptchaProviderConstants.RECAPTCHA_RESET_FUNCTION);
        functions.put("getResponse", CaptchaProviderConstants.RECAPTCHA_GET_RESPONSE_FUNCTION);
        return functions;
    }

    @Override
    public String getCaptchaApiUrl() {

        return CaptchaProviderConstants.RECAPTCHA_API_URL;
    }

    /**
     * Get the minimum score threshold for successful reCAPTCHA verification.
     *
     * @return the score threshold.
     */
    public double getCaptchaScoreThreshold() {

        String threshold = CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaScoreThreshold();
        if (threshold != null && !threshold.isEmpty()) {
            try {
                return Double.parseDouble(threshold);
            } catch (NumberFormatException e) {
                log.warn("Invalid score threshold value: " + threshold + ". Using default value of 0.5.", e);
            }
        }
        return CaptchaProviderConstants.RECAPTCHA_DEFAULT_THRESHOLD;
    }

    /**
     * Get the warning score threshold for reCAPTCHA verification.
     * If the score is below this value, a warning is logged.
     *
     * @return the warn score threshold.
     */
    public double getCaptchaWarnScoreThreshold() {

        String warnThreshold =
                CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaWarnScoreThreshold();
        if (warnThreshold != null && !warnThreshold.isEmpty()) {
            try {
                return Double.parseDouble(warnThreshold);
            } catch (NumberFormatException e) {
                log.warn("Invalid warn score threshold value: " + warnThreshold + ". Using default value of 0.7.", e);
            }
        }
        return CaptchaProviderConstants.RECAPTCHA_DEFAULT_WARN_THRESHOLD;
    }

    @Override
    public boolean verifyCaptcha(String captchaResponse)
            throws CaptchaException {

        List<BasicNameValuePair> params =
                Arrays.asList(new BasicNameValuePair(CaptchaProviderConstants.PARAM_SECRET,
                                CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaSiteSecret()),
                        new BasicNameValuePair(CaptchaProviderConstants.PARAM_RESPONSE, captchaResponse));

        HttpEntity entity = isValidCaptcha(getCaptchaVerifyUrl(), params);

        verifyCaptchaResponse(entity);

        return true;
    }

    private void verifyCaptchaResponse(HttpEntity entity)
            throws CaptchaServerException, CaptchaClientException {

        final double scoreThreshold = getCaptchaScoreThreshold();
        final double warnScoreThreshold = getCaptchaWarnScoreThreshold();

        try {
            try (InputStream in = entity.getContent()) {
                JsonElement jsonElement = JsonParser.parseReader(new InputStreamReader(in, StandardCharsets.UTF_8));
                JsonObject verificationResponse = jsonElement.getAsJsonObject();
                if (verificationResponse == null) {
                    throw new CaptchaClientException("Error receiving reCaptcha response from the server");
                }

                boolean success = verificationResponse.get(CaptchaProviderConstants.PARAM_SUCCESS).getAsBoolean();

                if (!success) {
                    throw new CaptchaClientException("reCaptcha token is invalid. Error:" +
                            verificationResponse.get("error-codes"));
                }

                if (verificationResponse.get(CaptchaProviderConstants.PARAM__SCORE) != null) {
                    double score = verificationResponse.get(CaptchaProviderConstants.PARAM__SCORE).getAsDouble();

                    if (log.isDebugEnabled()) {
                        log.debug("CAPTCHA response :" + verificationResponse.getAsString());
                    }
                    if (score < scoreThreshold) {
                        throw new CaptchaClientException("Captcha score is less than the threshold.");
                    } else if (score < warnScoreThreshold) {
                        log.warn("Captcha score is below warn threshold.");
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("CAPTCHA response :" + verificationResponse.getAsString());
                    }
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        } catch (ClassCastException e) {
            throw new CaptchaServerException("Unable to cast the response value.", e);
        }
    }
}
