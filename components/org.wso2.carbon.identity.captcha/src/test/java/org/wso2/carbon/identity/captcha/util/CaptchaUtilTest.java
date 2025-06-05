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

import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;

/**
 * Unit tests for CaptchaUtil.java
 */
public class CaptchaUtilTest {

    private final String RECAPTCHA_API_URL = "https://www.google.com/recaptcha/api/siteverify";
    private final String RECAPTCHA_ENTERPRISE_API_URL = "https://recaptchaenterprise.googleapis.com";

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

    @Test (description = "This method is used to test the createReCaptchaEnterpriseVerificationHttpPost method")
    public void testCreateReCaptchaEnterpriseVerificationHttpPost() throws NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, URISyntaxException {

        CaptchaDataHolder.getInstance().setReCaptchaVerifyUrl(RECAPTCHA_ENTERPRISE_API_URL);
        CaptchaDataHolder.getInstance().setReCaptchaAPIKey("dummyKey");
        CaptchaDataHolder.getInstance().setReCaptchaSiteKey("dummySiteKey");
        CaptchaDataHolder.getInstance().setReCaptchaProjectID("dummyProjectId");

        Method method = getCreateReCaptchaEnterpriseVerificationHttpPostMethod();
        HttpPost httpPost = (HttpPost) method.invoke(null, "reCaptchaEnterpriseResponse");
        String expectedURI = RECAPTCHA_ENTERPRISE_API_URL+ "/v1/projects/dummyProjectId/assessments?key=dummyKey";
        Assert.assertEquals(httpPost.getUri().toString(), expectedURI);

    }

    @Test (description = "This method is used to test the createReCaptchaEnterpriseVerificationHttpPost method")
    public void testCreateReCaptchaVerificationHttpPost() throws NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, URISyntaxException {

        CaptchaDataHolder.getInstance().setReCaptchaVerifyUrl(RECAPTCHA_API_URL);
        CaptchaDataHolder.getInstance().setReCaptchaSecretKey("dummyKey");
        CaptchaDataHolder.getInstance().setReCaptchaSiteKey("dummySiteKey");
        CaptchaDataHolder.getInstance().setReCaptchaProjectID("dummyProjectId");

        Method method = getCreateReCaptchaVerificationHttpPostMethod();
        HttpPost httpPost = (HttpPost) method.invoke(null, "reCaptchaEnterpriseResponse");
        Assert.assertEquals(httpPost.getUri().toString(), RECAPTCHA_API_URL);
    }
}
