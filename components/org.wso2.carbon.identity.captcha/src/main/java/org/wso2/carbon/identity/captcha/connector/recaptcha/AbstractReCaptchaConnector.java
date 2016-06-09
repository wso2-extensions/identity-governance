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

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

/**
 * Abstract ReCaptcha Connector.
 */
public abstract class AbstractReCaptchaConnector implements CaptchaConnector {

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest) throws CaptchaException {

        if (((HttpServletRequest) servletRequest).getMethod().equalsIgnoreCase("GET")) {
            throw new CaptchaClientException("reCaptcha response must send in a POST request.");
        }

        String reCaptchaResponse = servletRequest.getParameter("g-recaptcha-response");
        if (StringUtils.isBlank(reCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }

        HttpClient httpclient = HttpClients.createDefault();
        HttpPost httppost = new HttpPost(CaptchaDataHolder.getInstance().getReCaptchaVerifyUrl());

        List<BasicNameValuePair> params = Arrays.asList(new BasicNameValuePair("secret", CaptchaDataHolder
                .getInstance().getReCaptchaSecretKey()), new BasicNameValuePair("response", reCaptchaResponse));
        httppost.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

        HttpResponse response;
        try {
            response = httpclient.execute(httppost);
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to get the verification response.", e);
        }

        HttpEntity entity = response.getEntity();
        if (entity == null) {
            throw new CaptchaServerException("reCaptcha verification response is not received.");
        }

        try {
            try (InputStream in = entity.getContent()) {
                JsonObject verificationResponse = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                if (verificationResponse == null || verificationResponse.get("success") == null ||
                        !verificationResponse.get("success").getAsBoolean()) {
                    throw new CaptchaClientException("reCaptcha verification failed. Please try again.");
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        }

        return true;
    }
}
