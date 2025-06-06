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

import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.provider.mgt.util.CaptchaProviderConstants;

import java.util.Map;

public class GoogleRecaptchaEnterpriseProviderTest {

    private GoogleRecaptchaEnterpriseProvider provider;

    @BeforeMethod
    public void setUp() {

        provider = new GoogleRecaptchaEnterpriseProvider();
    }

    @Test
    public void testGetCaptchaType() {

        String captchaType = provider.getCaptchaType();
        Assert.assertEquals(captchaType, CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_TYPE);
    }

    @Test
    public void testGetCaptchaVerifyUrl() {

        String verifyUrl = provider.getCaptchaVerifyUrl();
        Assert.assertEquals(verifyUrl, CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_VERIFY_URL);
    }

    @Test
    public void testGetCaptchaFunctions() {

        Map<String, String> functions = provider.getCaptchaFunctions();
        Assert.assertEquals(functions.get("render"), CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_RENDER_FUNCTION);
        Assert.assertEquals(functions.get("reset"), CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_RESET_FUNCTION);
        Assert.assertEquals(functions.get("getResponse"),
                CaptchaProviderConstants.RECAPTCHA_ENTERPRISE_GET_RESPONSE_FUNCTION);
    }
}
