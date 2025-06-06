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

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class CaptchaProviderUtils {

    /**
     * Sends a CAPTCHA verification request to the CAPTCHA service and returns the HTTP entity containing the response.
     *
     * @param captchaVerifyUrl the URL of the CAPTCHA verification endpoint.
     * @param params           the parameters to be sent in the POST request (e.g., secret, response).
     * @return the HTTP entity containing the CAPTCHA verification response.
     * @throws CaptchaException if an error occurs while sending the request or receiving the response.
     */
    public static HttpEntity isValidCaptcha(String captchaVerifyUrl, List<BasicNameValuePair> params)
            throws CaptchaException {

        CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();

        HttpPost httpPost = createCaptchaVerificationHttpPost(captchaVerifyUrl, params);

        HttpResponse response;
        try {
            response = httpclient.execute(httpPost);
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to get the captcha verification response.", e);
        }

        HttpEntity entity = response.getEntity();
        if (entity == null) {
            throw new CaptchaServerException("Captcha verification response is not received.");
        }

        return entity;
    }

    private static HttpPost createCaptchaVerificationHttpPost(String captchaVerifyUrl,
                                                              List<BasicNameValuePair> params) {

        HttpPost httpPost;
        httpPost = new HttpPost(captchaVerifyUrl);
        httpPost.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

        return httpPost;
    }

}
