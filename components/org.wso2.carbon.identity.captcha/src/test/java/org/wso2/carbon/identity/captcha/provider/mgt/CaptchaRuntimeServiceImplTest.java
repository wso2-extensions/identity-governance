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

package org.wso2.carbon.identity.captcha.provider.mgt;

import org.mockito.MockedStatic;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider.mgt.provider.AbstractCaptchaProvider;
import org.wso2.carbon.identity.captcha.provider.mgt.service.CaptchaConfigService;
import org.wso2.carbon.identity.captcha.provider.mgt.service.impl.CaptchaRuntimeServiceImpl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

public class CaptchaRuntimeServiceImplTest {

    private CaptchaRuntimeServiceImpl captchaRuntimeService;

    @BeforeMethod
    public void setUp() {

        captchaRuntimeService = new CaptchaRuntimeServiceImpl();
    }

    @Test
    public void testIsCaptchaEnabled() {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.isCaptchaEnabled()).thenReturn(true);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertTrue(captchaRuntimeService.isCaptchaEnabled());
        }
    }

    @Test
    public void testVerifyCaptchaWhenEnabledAndProviderPresent() throws CaptchaException {

        AbstractCaptchaProvider provider = mock(AbstractCaptchaProvider.class);
        when(provider.verifyCaptcha("response")).thenReturn(true);

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.isCaptchaEnabled()).thenReturn(true);
        when(configService.getCaptchaProvider()).thenReturn(provider);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertTrue(captchaRuntimeService.verifyCaptcha("response"));
        }
    }

    @Test
    public void testVerifyCaptchaWhenDisabled() throws CaptchaException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.isCaptchaEnabled()).thenReturn(false);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertFalse(captchaRuntimeService.verifyCaptcha("response"));
        } catch (CaptchaException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testGetCaptchaSiteKey() throws CaptchaServerException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaSiteKey()).thenReturn("siteKey");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaSiteKey(), "siteKey");
        }
    }

    @Test
    public void testGetCaptchaProjectID() throws CaptchaServerException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaProjectID()).thenReturn("projectId");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaProjectID(), "projectId");
        }
    }

    @Test
    public void testGetCaptchaApiKey() throws CaptchaServerException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaApiKey()).thenReturn("apiKey");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaApiKey(), "apiKey");
        }
    }

    @Test
    public void testGetCaptchaSiteSecret() throws CaptchaServerException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaSiteSecret()).thenReturn("siteSecret");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaSiteSecret(), "siteSecret");
        }
    }

    @Test
    public void testGetCaptchaProviderAttributesWhenEnabled() throws CaptchaServerException {

        AbstractCaptchaProvider provider = mock(AbstractCaptchaProvider.class);
        Map<String, String> widgetAttrs = new HashMap<>();
        widgetAttrs.put("key", "value");
        when(provider.getWidgetAttributes()).thenReturn(widgetAttrs);

        List<Map<String, String>> scriptAttrs = new ArrayList<>();
        scriptAttrs.add(Collections.singletonMap("script", "url"));
        when(provider.getScriptAttributes()).thenReturn(scriptAttrs);

        when(provider.getCaptchaIdentifier()).thenReturn("identifier");
        when(provider.getCaptchaResponseIdentifier()).thenReturn("responseId");
        when(provider.getCaptchaFunctions()).thenReturn(Collections.singletonMap("func", "val"));
        when(provider.getCaptchaApiUrl()).thenReturn("apiUrl");
        when(provider.getCaptchaVerifyUrl()).thenReturn("verifyUrl");

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.isCaptchaEnabled()).thenReturn(true);
        when(configService.getCaptchaProvider()).thenReturn(provider);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getWidgetAttributes(), widgetAttrs);
            Assert.assertEquals(captchaRuntimeService.getScriptAttributes(), scriptAttrs);
            Assert.assertEquals(captchaRuntimeService.getCaptchaIdentifier(), "identifier");
            Assert.assertEquals(captchaRuntimeService.getCaptchaResponseIdentifier(), "responseId");
            Assert.assertEquals(captchaRuntimeService.getCaptchaFunctions(), Collections.singletonMap("func", "val"));
            Assert.assertEquals(captchaRuntimeService.getCaptchaApiUrl(), "apiUrl");
            Assert.assertEquals(captchaRuntimeService.getCaptchaVerifyUrl(), "verifyUrl");
        }
    }

    @Test
    public void testGetCaptchaProviderAttributesWhenDisabled() throws CaptchaServerException {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.isCaptchaEnabled()).thenReturn(false);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertTrue(captchaRuntimeService.getWidgetAttributes().isEmpty());
            Assert.assertTrue(captchaRuntimeService.getScriptAttributes().isEmpty());
            Assert.assertEquals(captchaRuntimeService.getCaptchaIdentifier(), "");
            Assert.assertEquals(captchaRuntimeService.getCaptchaResponseIdentifier(), "");
            Assert.assertTrue(captchaRuntimeService.getCaptchaFunctions().isEmpty());
            Assert.assertEquals(captchaRuntimeService.getCaptchaApiUrl(), "");
            Assert.assertEquals(captchaRuntimeService.getCaptchaVerifyUrl(), "");
        }
    }

    @Test
    public void testGetCaptchaScoreThreshold() {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaScoreThreshold()).thenReturn("0.5");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaScoreThreshold(), "0.5");
        }
    }

    @Test
    public void testGetCaptchaWarnScoreThreshold() {

        CaptchaConfigService configService = mock(CaptchaConfigService.class);
        when(configService.getCaptchaWarnScoreThreshold()).thenReturn("0.3");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigService()).thenReturn(configService);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.assertEquals(captchaRuntimeService.getCaptchaWarnScoreThreshold(), "0.3");
        }
    }

}
