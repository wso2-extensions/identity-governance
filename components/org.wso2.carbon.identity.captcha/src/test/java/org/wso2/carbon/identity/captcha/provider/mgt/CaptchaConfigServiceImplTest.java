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
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider.mgt.service.impl.CaptchaConfigServiceImpl;
import org.wso2.carbon.identity.captcha.provider.mgt.util.CaptchaProviderConstants;
import org.wso2.carbon.identity.captcha.provider.mgt.provider.AbstractCaptchaProvider;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.configuration.mgt.core.exception.ConfigurationManagementException;
import org.wso2.carbon.identity.configuration.mgt.core.model.Attribute;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@WithCarbonHome
public class CaptchaConfigServiceImplTest {

    private CaptchaConfigServiceImpl captchaConfigService;
    private Properties captchaProperties;

    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;

    @BeforeMethod
    public void setUp() {

        captchaConfigService = new CaptchaConfigServiceImpl();
        captchaProperties = new Properties();

        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED, "true");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_TYPE, "reCaptcha");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY, "siteKey");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY, "siteSecret");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD, "0.5");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_WARN_SCORE_THRESHOLD, "0.3");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_API_KEY, "apiKey");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_PROJECT_ID, "projectId");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_REQUEST_WRAP_URLS, "urls");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_FAILED_REDIRECT_URLS, "errors");
        captchaProperties.setProperty(CaptchaConstants.RE_CAPTCHA_BYPASSED_API_ENDPOINTS, "endpoint1,endpoint2");

        mockedPrivilegedCarbonContext = Mockito.mockStatic(PrivilegedCarbonContext.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedPrivilegedCarbonContext.close();
    }

    private void mockCarbonContext() {

        PrivilegedCarbonContext privilegedCarbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);
        mockedPrivilegedCarbonContext.when(privilegedCarbonContext::getTenantDomain).thenReturn("TENANT_DOMAIN");
        mockedPrivilegedCarbonContext.when(privilegedCarbonContext::getTenantId).thenReturn(-1234);
    }

    @Test
    public void testIsCaptchaEnabledFalse() {

        mockCarbonContext();
        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(null);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(false);
        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            boolean result = captchaConfigService.isCaptchaEnabled();
            Assert.assertFalse(result);
        }
    }

    @Test
    public void testIsCaptchaEnabledTrue() {

        mockCarbonContext();
        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(false);
        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);

        try (MockedStatic<CaptchaDataHolder> mockedStatic = mockStatic(CaptchaDataHolder.class)) {
            mockedStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            boolean result = captchaConfigService.isCaptchaEnabled();
            Assert.assertTrue(result);
        }
    }

    @Test
    public void testGetCaptchaProviderConfig() throws CaptchaServerException {

        AbstractCaptchaProvider provider = mock(AbstractCaptchaProvider.class);
        when(provider.getCaptchaApiUrl()).thenReturn("apiUrl");
        when(provider.getCaptchaVerifyUrl()).thenReturn("verifyUrl");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.getCaptchaProviderForCaptchaType("reCaptcha")).thenReturn(provider);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Properties result = captchaConfigService.getActiveCaptchaProviderConfig();
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL), "apiUrl");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL), "verifyUrl");
        }
    }

    @Test
    public void testGetCaptchaProviderConfigWhenTenantConfigMissing() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(null);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(true);
        when(dataHolder.getReCaptchaType()).thenReturn("defaultType");
        when(dataHolder.getReCaptchaSiteKey()).thenReturn("defaultSiteKey");
        when(dataHolder.getReCaptchaSecretKey()).thenReturn("defaultSecretKey");
        when(dataHolder.getReCaptchaAPIUrl()).thenReturn("defaultApiUrl");
        when(dataHolder.getReCaptchaVerifyUrl()).thenReturn("defaultVerifyUrl");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Properties result = captchaConfigService.getActiveCaptchaProviderConfig();

            // Validate the fallback/default properties
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_ENABLED), "true");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_TYPE), "defaultType");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY), "defaultSiteKey");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY), "defaultSecretKey");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL), "defaultApiUrl");
            Assert.assertEquals(result.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL), "defaultVerifyUrl");
        }
    }

    @Test
    public void testGetCaptchaProviderWhenNoProvider() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaType()).thenReturn("");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {

            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.getCaptchaProvider());
        }
    }

    @Test
    public void testGetCaptchaProviderWhenTenantConfigMissingButDefaultTypeAvailable() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(null);
        when(dataHolder.getReCaptchaType()).thenReturn("defaultType");

        AbstractCaptchaProvider defaultProvider = mock(AbstractCaptchaProvider.class);
        when(dataHolder.getCaptchaProviderForCaptchaType("defaultType")).thenReturn(defaultProvider);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            AbstractCaptchaProvider result = captchaConfigService.getCaptchaProvider();

            Assert.assertNotNull(result);
            Assert.assertEquals(result, defaultProvider);
        }
    }

    @Test
    public void testGetCaptchaSiteKey() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.getReCaptchaSiteKey()).thenReturn("defaultSiteKey");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {

            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaSiteKey(), "siteKey");
        }
    }

    @Test
    public void testGetCaptchaSiteKeyWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaSiteKey()).thenReturn("");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {

            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.getCaptchaSiteKey());
        }
    }

    @Test
    public void testGetCaptchaSiteSecret() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.getReCaptchaSecretKey()).thenReturn("defaultSecret");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaSiteSecret(), "siteSecret");
        }
    }

    @Test
    public void testGetCaptchaSiteSecretWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaSecretKey()).thenReturn("");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.getCaptchaSiteSecret());
        }
    }

    @Test
    public void testGetCaptchaProjectID() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.getReCaptchaProjectID()).thenReturn("defaultProject");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaProjectID(), "projectId");
        }
    }

    @Test
    public void testGetCaptchaProjectIDWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaProjectID()).thenReturn("");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.getCaptchaProjectID());
        }
    }

    @Test
    public void testGetCaptchaApiKey() throws CaptchaServerException {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(captchaProperties);
        when(dataHolder.getReCaptchaAPIKey()).thenReturn("defaultApiKey");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaApiKey(), "apiKey");
        }
    }

    @Test
    public void testGetCaptchaApiKeyWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaAPIKey()).thenReturn("");

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.getCaptchaApiKey());
        }
    }

    @Test
    public void testGetCaptchaScoreThreshold() {

        Properties props = new Properties();
        props.setProperty(CaptchaConstants.RE_CAPTCHA_WARN_SCORE_THRESHOLD, "0.4");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(props);
        when(dataHolder.getReCaptchaScoreThreshold()).thenReturn(0.5);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaScoreThreshold(), "0.4");
        }
    }

    @Test
    public void testGetCaptchaScoreThresholdWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(new Properties());
        when(dataHolder.getReCaptchaScoreThreshold()).thenReturn(0.5);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaScoreThreshold(), "0.5");
        }
    }

    @Test
    public void testGetCaptchaWarnScoreThreshold() {

        Properties props = new Properties();
        props.setProperty(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD, "0.6");

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(-1234)).thenReturn(props);
        when(dataHolder.getReCaptchaWarnScoreThreshold()).thenReturn(0.4);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaWarnScoreThreshold(), "0.6");
        }
    }

    @Test
    public void testGetCaptchaWarnScoreThresholdWhenMissing() {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getCaptchaConfigsForTenant(anyInt())).thenReturn(new Properties());
        when(dataHolder.getReCaptchaWarnScoreThreshold()).thenReturn(0.4);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            mockCarbonContext();

            Assert.assertEquals(captchaConfigService.getCaptchaWarnScoreThreshold(), "0.4");
        }
    }

    @Test
    public void testLoadCaptchaConfigs() throws Exception {

        Resource resource = mock(Resource.class);
        List<Attribute> attributes =
                Arrays.asList(new Attribute(CaptchaProviderConstants.CAPTCHA_PROVIDER_CONFIG, "type"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_SITEKEY_CONFIG, "siteKey"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_SITESECRET_CONFIG, "siteSecret"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_SCORE_THRESHOLD_CONFIG, "0.5"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_SCORE_WARN_THRESHOLD_CONFIG, "0.3"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_API_KEY, "apiKey"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_PROJECT_ID, "projectId"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_REQUEST_WRAP_URLS, "urls"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_FAILED_REDIRECT_URLS, "errors"),
                        new Attribute(CaptchaProviderConstants.CAPTCHA_BYPASSED_API_ENDPOINTS, "endpoint1,endpoint2"));
        when(resource.getAttributes()).thenReturn(attributes);

        ConfigurationManager configManager = mock(ConfigurationManager.class);
        when(configManager.getResource(anyString(), anyString())).thenReturn(resource);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getConfigurationManager()).thenReturn(configManager);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            captchaConfigService.loadCaptchaConfigs();

            verify(dataHolder, atLeastOnce()).addCaptchaConfigsForTenant(-123, any());
        }
    }

    @Test
    public void testLoadCaptchaConfigsException() throws ConfigurationManagementException {

        ConfigurationManager configManager = mock(ConfigurationManager.class);
        when(configManager.getResource(anyString(), anyString())).thenThrow(new RuntimeException("error"));

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        when(dataHolder.getConfigurationManager()).thenReturn(configManager);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            mockCarbonContext();

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.loadCaptchaConfigs());
        }
    }

    @Test
    public void testStoreCaptchaProviders() throws Exception {

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            captchaConfigService.storeCaptchaProviders();

            verify(dataHolder, atLeastOnce()).setCaptchaProviderForCaptchaType(anyString(), any());
        }
    }

    @Test
    public void testStoreCaptchaProvidersException() {

        Map<String, Class<? extends AbstractCaptchaProvider>> registry =
                new HashMap<String, Class<? extends AbstractCaptchaProvider>>() {{
                    put("type", AbstractCaptchaProvider.class);
                }};

        CaptchaProviderConstants.CaptchaProviderRegistry.getProviderClasses().clear();
        CaptchaProviderConstants.CaptchaProviderRegistry.getProviderClasses().putAll(registry);

        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);

        try (MockedStatic<CaptchaDataHolder> mocked = mockStatic(CaptchaDataHolder.class)) {
            mocked.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);

            Assert.expectThrows(CaptchaServerException.class, () -> captchaConfigService.storeCaptchaProviders());
        }
    }

}
