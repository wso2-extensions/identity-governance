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

package org.wso2.carbon.identity.captcha.connector.provider;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConfigs;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

/**
 * HCaptchaProvider
 */
public class HCaptchaProvider implements CaptchaProvider{

    @Override
    public String getName() {

        return "HCaptcha";
    }

    @Override
    public int getPriority() {

        return 0;
    }

    @Override
    public CaptchaConfigs getCaptchaProperties() {

        for (CaptchaConfigs captchaConfigs: CaptchaDataHolder.getInstance().getCaptchaConfigs()){
            if (this.getName().equals(captchaConfigs.getCaptchaProviderName())){
                return captchaConfigs;
            }
        }
        return null;
    }

    @Override
    public void setCaptchaParamsForPreValidation(ServletRequest servletRequest, ServletResponse servletResponse, CaptchaConstants.Flow flow) throws CaptchaException {

    }

    @Override
    public void setCaptchaParamsForPostValidation(ServletRequest servletRequest, ServletResponse servletResponse, CaptchaConstants.Flow flow) throws CaptchaException {

    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String HCaptchaResponse = ((HttpServletRequest) servletRequest).getHeader("h-captcha-response");
        if (StringUtils.isBlank(HCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }

        CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();
        HttpPost httppost = new HttpPost(getCaptchaProperties().getCaptchaProperties().get("VerifyUrl").toString());


        List<BasicNameValuePair> params = Arrays.asList(new BasicNameValuePair("secret", getCaptchaProperties()
                .getCaptchaProperties().get("SecretKey").toString()), new BasicNameValuePair("response", HCaptchaResponse));
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
                    throw new CaptchaClientException("Captcha verification failed. Please try again.");
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        }

        return true;
    }

    @Override
    public void addPostValidationData(ServletRequest servletRequest) {

    }
}
