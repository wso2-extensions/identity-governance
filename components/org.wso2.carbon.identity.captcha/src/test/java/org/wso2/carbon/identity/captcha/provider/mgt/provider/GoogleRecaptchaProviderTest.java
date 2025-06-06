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

import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider.mgt.service.CaptchaRuntimeService;
import org.wso2.carbon.identity.captcha.provider.mgt.util.CaptchaProviderConstants;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

public class GoogleRecaptchaProviderTest {

    private final GoogleRecaptchaProvider googleRecaptchaProvider = new GoogleRecaptchaProvider();

    @Test
    public void testGetCaptchaType() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaType(), CaptchaProviderConstants.RECAPTCHA_TYPE);
    }

    @Test
    public void testGetCaptchaIdentifier() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaIdentifier(),
                CaptchaProviderConstants.RECAPTCHA_IDENTIFIER);
    }

    @Test
    public void testGetCaptchaResponseIdentifier() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaResponseIdentifier(),
                CaptchaProviderConstants.RECAPTCHA_RESPONSE_IDENTIFIER);
    }

    @Test
    public void testGetCaptchaVerifyUrl() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaVerifyUrl(),
                CaptchaProviderConstants.RECAPTCHA_VERIFY_URL);
    }

    @Test
    public void testGetWidgetAttributes() {

        Assert.assertEquals(googleRecaptchaProvider.getWidgetAttributes().get("class"),
                CaptchaProviderConstants.RECAPTCHA_IDENTIFIER);
        Assert.assertEquals(googleRecaptchaProvider.getWidgetAttributes().get("data-size"), "invisible");
        Assert.assertEquals(googleRecaptchaProvider.getWidgetAttributes().get("data-callback"), "onCompleted");
    }

    @Test
    public void testGetScriptAttributes() {

        Assert.assertEquals(googleRecaptchaProvider.getScriptAttributes().size(), 3);
        Assert.assertTrue(googleRecaptchaProvider.getScriptAttributes().stream()
                .anyMatch(map -> "true".equals(map.get("async"))));
    }

    @Test
    public void testGetCaptchaFunctions() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaFunctions().get("render"),
                CaptchaProviderConstants.RECAPTCHA_RENDER_FUNCTION);
    }

    @Test
    public void testGetCaptchaApiUrl() {

        Assert.assertEquals(googleRecaptchaProvider.getCaptchaApiUrl(),
                CaptchaProviderConstants.RECAPTCHA_API_URL);
    }

    @Test
    public void testGetCaptchaScoreThreshold_default() {

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
            CaptchaRuntimeService runtimeService = mock(CaptchaRuntimeService.class);

            when(dataHolder.getCaptchaRuntimeService()).thenReturn(runtimeService);
            when(runtimeService.getCaptchaScoreThreshold()).thenReturn(null);

            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(googleRecaptchaProvider.getCaptchaScoreThreshold(), 0.5);
        }
    }

    @Test
    public void testGetCaptchaScoreThreshold_customValue() {

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
            CaptchaRuntimeService runtimeService = mock(CaptchaRuntimeService.class);

            when(dataHolder.getCaptchaRuntimeService()).thenReturn(runtimeService);
            when(runtimeService.getCaptchaScoreThreshold()).thenReturn("0.8");

            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(googleRecaptchaProvider.getCaptchaScoreThreshold(), 0.8);
        }
    }

    @Test
    public void testGetCaptchaWarnScoreThreshold_default() {

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
            CaptchaRuntimeService runtimeService = mock(CaptchaRuntimeService.class);

            when(dataHolder.getCaptchaRuntimeService()).thenReturn(runtimeService);
            when(runtimeService.getCaptchaWarnScoreThreshold()).thenReturn(null);

            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(googleRecaptchaProvider.getCaptchaWarnScoreThreshold(), 0.7);
        }
    }

    @Test
    public void testGetCaptchaWarnScoreThreshold_customValue() {

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
            CaptchaRuntimeService runtimeService = mock(CaptchaRuntimeService.class);

            when(dataHolder.getCaptchaRuntimeService()).thenReturn(runtimeService);
            when(runtimeService.getCaptchaWarnScoreThreshold()).thenReturn("0.9");

            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(googleRecaptchaProvider.getCaptchaWarnScoreThreshold(), 0.9);
        }
    }
}
