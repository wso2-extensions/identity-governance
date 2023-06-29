/*
 * Copyright (c) 2023, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.captcha.util;

import com.google.gson.JsonObject;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import static org.testng.Assert.*;

/**
 * Unit tests for CaptchaUtil.java
 */
public class CaptchaUtilTest {

    private final String RECAPTCHA_API_URL = "https://www.google.com/recaptcha/api/siteverify";

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
    }

    private Method getCreateReCaptchaEnterpriseVerificationHttpPostMethod() throws NoSuchMethodException {

        Method method = CaptchaUtil.class.getDeclaredMethod("createReCaptchaEnterpriseVerificationHttpPost",
                String.class);
        method.setAccessible(true);
        return method;
    }

    private Method getCreateReCaptchaVerificationHttpPostMethod() throws NoSuchMethodException {

        Method method = CaptchaUtil.class.getDeclaredMethod("createReCaptchaVerificationHttpPost",
                String.class);
        method.setAccessible(true);
        return method;
    }

    private Method getVerifyReCaptchaEnterpriseResponseMethod() throws NoSuchMethodException {

        Method method = CaptchaUtil.class.getDeclaredMethod("verifyReCaptchaEnterpriseResponse",
                HttpEntity.class);
        method.setAccessible(true);
        return method;
    }

    private Method getVerifyReCaptchaResponseMethod() throws NoSuchMethodException {

        Method method = CaptchaUtil.class.getDeclaredMethod("verifyReCaptchaResponse",
                HttpEntity.class);
        method.setAccessible(true);
        return method;
    }

    private JsonObject getReCaptchaEnterpriseJsonObject(boolean valid, double score) {

        JsonObject verificationResponse = new JsonObject();
        JsonObject tokenProperties = new JsonObject();
        tokenProperties.addProperty(CaptchaConstants.CAPTCHA_VALID, valid);
        verificationResponse.add(CaptchaConstants.CAPTCHA_TOKEN_PROPERTIES, tokenProperties);
        JsonObject riskAnalysis = new JsonObject();
        riskAnalysis.addProperty(CaptchaConstants.CAPTCHA_SCORE, score);
        verificationResponse.add(CaptchaConstants.CAPTCHA_RISK_ANALYSIS, riskAnalysis);
        return verificationResponse;
    }

    private JsonObject getReCaptchaJsonObject(boolean valid, double score) {

        JsonObject verificationResponse = new JsonObject();
        verificationResponse.addProperty(CaptchaConstants.CAPTCHA_SUCCESS, valid);
        verificationResponse.addProperty(CaptchaConstants.CAPTCHA_SCORE, score);
        return verificationResponse;
    }

    @Test (description = "This method is used to test the createReCaptchaEnterpriseVerificationHttpPost method")
    public void testCreateReCaptchaEnterpriseVerificationHttpPost() throws NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        CaptchaDataHolder.getInstance().setReCaptchaVerifyUrl(RECAPTCHA_API_URL);
        CaptchaDataHolder.getInstance().setReCaptchaSecretKey("dummyKey");
        CaptchaDataHolder.getInstance().setReCaptchaSiteKey("dummySiteKey");
        CaptchaDataHolder.getInstance().setReCaptchaProjectID("dummyProjectId");



        Method method = getCreateReCaptchaEnterpriseVerificationHttpPostMethod();
        HttpPost httpPost = (HttpPost) method.invoke(null, "reCaptchaEnterpriseResponse");
        String expectedURI = RECAPTCHA_API_URL+ "/v1/projects/dummyProjectId/assessments?key=dummyKey";
        Assert.assertEquals(httpPost.getURI().toString(), expectedURI);

    }

    @Test (description = "This method is used to test the createReCaptchaEnterpriseVerificationHttpPost method")
    public void testCreateReCaptchaVerificationHttpPost() throws NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        CaptchaDataHolder.getInstance().setReCaptchaVerifyUrl(RECAPTCHA_API_URL);
        CaptchaDataHolder.getInstance().setReCaptchaSecretKey("dummyKey");
        CaptchaDataHolder.getInstance().setReCaptchaSiteKey("dummySiteKey");
        CaptchaDataHolder.getInstance().setReCaptchaProjectID("dummyProjectId");

        Method method = getCreateReCaptchaVerificationHttpPostMethod();
        HttpPost httpPost = (HttpPost) method.invoke(null, "reCaptchaEnterpriseResponse");
        Assert.assertEquals(httpPost.getURI().toString(), RECAPTCHA_API_URL);
    }


    @Test (description = "This method is used to test the verifyReCaptchaEnterpriseResponse method, " +
            "with high captcha score")
    public void testVerifyReCaptchaEnterpriseResponseWithHighScore() throws IOException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaEnterpriseJsonObject(true, 0.7);
        Method method = getVerifyReCaptchaEnterpriseResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(
                verificationResponse.toString().getBytes()));
        // verify no exception is thrown for high score
        method.invoke(null, httpEntity);
    }

    @Test (description = "This method is used to test the verifyReCaptchaEnterpriseResponse method, " +
            "with low captcha score")
    public void testVerifyReCaptchaEnterpriseResponseWithLowScore() throws IOException, NoSuchMethodException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaEnterpriseJsonObject(true, 0.4);
        Method method = getVerifyReCaptchaEnterpriseResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(verificationResponse.toString().
                getBytes()));
        // verify an exception is thrown for low score
        assertThrows(InvocationTargetException.class, () -> method.invoke(null, httpEntity));
    }

    @Test (description = "This method is used to test the verifyReCaptchaEnterpriseResponse method, " +
                "with invalid response")
    public void testVerifyReCaptchaEnterpriseResponseWithInvalidResponse() throws IOException, NoSuchMethodException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaEnterpriseJsonObject(false, 0.7);
        Method method = getVerifyReCaptchaEnterpriseResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(verificationResponse.
                toString().getBytes()));
        // verify an exception is thrown for invalid response
        assertThrows(InvocationTargetException.class, () -> method.invoke(null, httpEntity));
    }

    @Test (description = "This method is used to test the verifyReCaptchaResponse method, " +
            "with high captcha score")
    public void testVerifyReCaptchaResponseWithHighScore() throws IOException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaJsonObject(true, 0.7);
        Method method = getVerifyReCaptchaResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(verificationResponse.toString().
                getBytes()));
        // verify no exception is thrown for high score
        method.invoke(null, httpEntity);
    }

    @Test (description = "This method is used to test the verifyReCaptchaResponse method, " +
            "with low captcha score")
    public void testVerifyReCaptchaResponseWithLowScore() throws IOException, NoSuchMethodException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaJsonObject(true, 0.4);
        Method method = getVerifyReCaptchaResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(verificationResponse.toString().
                getBytes()));
        // verify no exception is thrown for low score
        assertThrows(InvocationTargetException.class, () -> method.invoke(null, httpEntity));
    }

    @Test (description = "This method is used to test the verifyReCaptchaResponse method, " +
            "with invalid response")
    public void testVerifyReCaptchaResponseWithInvalidResponse() throws IOException, NoSuchMethodException {

        CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);

        JsonObject verificationResponse = getReCaptchaJsonObject(false, 0.7);
        Method method = getVerifyReCaptchaResponseMethod();
        HttpEntity httpEntity = Mockito.mock(HttpEntity.class);
        Mockito.when(httpEntity.getContent()).thenReturn(new ByteArrayInputStream(verificationResponse.toString().
                getBytes()));
        // verify no exception is thrown for invalid response
        assertThrows(InvocationTargetException.class, () -> method.invoke(null, httpEntity));
    }
}
