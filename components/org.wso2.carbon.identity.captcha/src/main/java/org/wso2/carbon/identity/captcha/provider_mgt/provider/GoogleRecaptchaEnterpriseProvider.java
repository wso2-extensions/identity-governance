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
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider_mgt.service.CaptchaRuntimeService;
import org.wso2.carbon.identity.captcha.provider_mgt.util.CaptchaProviderConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class GoogleRecaptchaEnterpriseProvider extends GoogleRecaptchaProvider {

    private static final Log log = LogFactory.getLog(GoogleRecaptchaProvider.class);

    @Override
    public String getCaptchaType() {

        return CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_TYPE;
    }

    @Override
    public String getCaptchaVerifyUrl() {

        return CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_VERIFY_URL;
    }

    @Override
    public Map<String, String> getCaptchaFunctions() {

        Map<String, String> functions = new HashMap<>();
        functions.put("render", CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_RENDER_FUNCTION);
        functions.put("reset", CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_RESET_FUNCTION);
        functions.put("getResponse", CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_GET_RESPONSE_FUNCTION);
        return functions;
    }

    @Override
    public boolean verifyCaptcha(String captchaResponse)
            throws CaptchaException {

        CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();

        HttpPost httpPost = createReCaptchaEnterpriseVerificationHttpPost(captchaResponse);

        HttpResponse response;
        try {
            response = httpclient.execute(httpPost);
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to get the verification response.", e);
        }

        HttpEntity entity = response.getEntity();
        if (entity == null) {
            throw new CaptchaServerException("reCaptcha verification response is not received.");
        }

        verifyReCaptchaEnterpriseResponse(entity);

        return true;
    }

    private HttpPost createReCaptchaEnterpriseVerificationHttpPost(String captchaResponse)
            throws CaptchaServerException {

        HttpPost httpPost;
        String recaptchaUrl = getCaptchaVerifyUrl();
        String projectID = getCaptchaRuntimeService().getCaptchaProjectID();
        String siteKey = getCaptchaRuntimeService().getCaptchaSiteKey();
        String apiKey = getCaptchaRuntimeService().getCaptchaApiKey();

        String verifyUrl = recaptchaUrl + "/v1/projects/" + projectID + "/assessments?key=" + apiKey;
        httpPost = new HttpPost(verifyUrl);

        httpPost.setHeader(HttpHeaders.CONTENT_TYPE, "application/json");

        String json = String.format("{ \"event\": { \"token\": \"%s\", \"siteKey\": \"%s\" } }", captchaResponse,
                siteKey);

        StringEntity entity = new StringEntity(json, StandardCharsets.UTF_8);

        httpPost.setEntity(entity);

        return httpPost;
    }

    private void verifyReCaptchaEnterpriseResponse(HttpEntity entity)
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
                JsonObject tokenProperties = verificationResponse.get(CaptchaConstants.CAPTCHA_TOKEN_PROPERTIES).
                        getAsJsonObject();
                boolean success = tokenProperties.get(CaptchaConstants.CAPTCHA_VALID).getAsBoolean();

                JsonObject riskAnalysis = verificationResponse.get(CaptchaConstants.CAPTCHA_RISK_ANALYSIS).
                        getAsJsonObject();

                // Whether this request was a valid reCAPTCHA token.
                if (!success) {
                    throw new CaptchaClientException("reCaptcha token is invalid. Error:" +
                            verificationResponse.get("error-codes"));
                }
                if (riskAnalysis.get(CaptchaConstants.CAPTCHA_SCORE) != null) {
                    double score = riskAnalysis.get(CaptchaConstants.CAPTCHA_SCORE).getAsDouble();
                    // reCAPTCHA enterprise response contains score.
                    if (log.isDebugEnabled()) {
                        log.debug("reCAPTCHA Enterprise response { timestamp:" +
                                tokenProperties.get("createTime") + ", action: " +
                                tokenProperties.get("action") + ", score: " + score + " }");
                    }
                    if (score < scoreThreshold) {
                        throw new CaptchaClientException("reCaptcha score is less than the threshold.");
                    } else if (score < warnScoreThreshold) {
                        log.warn("User access with low reCaptcha score.");
                    }
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        } catch (ClassCastException e) {
            throw new CaptchaServerException("Unable to cast the response value.", e);
        }
    }

    private CaptchaRuntimeService getCaptchaRuntimeService() {

        return CaptchaDataHolder.getInstance().getCaptchaRuntimeService();
    }
}
