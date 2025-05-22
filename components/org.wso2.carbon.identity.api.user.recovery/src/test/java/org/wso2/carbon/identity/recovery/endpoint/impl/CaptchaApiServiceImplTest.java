/*
 *
 *  Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaPropertiesDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ReCaptchaResponseTokenDTO;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import javax.ws.rs.core.Response;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;

/**
 * Unit tests for CaptchaApiServiceImpl.java
 */
public class CaptchaApiServiceImplTest{

    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;

    @InjectMocks
    CaptchaApiServiceImpl captchaApiService;
    @Mock
    HttpEntity httpEntity;
    @Mock
    ClassicHttpResponse classicHttpResponse;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryUtil.close();
    }

    @DataProvider(name = "captchaTestDataProvider")
    public static Object[][] getCaptchaTestDataProvider() {

        String reCaptchaEnterprise = CaptchaConstants.RE_CAPTCHA_TYPE_ENTERPRISE;

        return new Object[][]{
                {false, "", ""},
                {true, "", "https://www.google.com/recaptcha/api.js"},
                {true, reCaptchaEnterprise, "https://www.google.com/recaptcha/enterprise.js"}
        };
    }

    @Test(description = "This method test, getReCaptcha method for username recovery",
            dataProvider = "captchaTestDataProvider")
    public void testGetCaptcha(boolean reCaptchaEnabled, String reCaptchaType,
                               String reCaptchaAPI) throws IOException {

        Properties sampleProperties = getSampleConfigFile();

        sampleProperties.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED, String.valueOf(reCaptchaEnabled));
        sampleProperties.setProperty(CaptchaConstants.RE_CAPTCHA_TYPE, reCaptchaType);
        sampleProperties.setProperty(CaptchaConstants.RE_CAPTCHA_API_URL, reCaptchaAPI);

        mockedRecoveryUtil.when(RecoveryUtil::getValidatedCaptchaConfigs).thenReturn(sampleProperties);
        mockedRecoveryUtil.when(() -> RecoveryUtil.checkCaptchaEnabledResidentIdpConfiguration(Mockito.anyString(),
                Mockito.anyString())).thenReturn(true);
        Response response = captchaApiService.getCaptcha("ReCaptcha", "username-recovery",
                "test.org");

        ReCaptchaPropertiesDTO reCaptchaPropertiesDTO = response.readEntity(ReCaptchaPropertiesDTO.class);

        assertEquals(reCaptchaPropertiesDTO.getReCaptchaEnabled().booleanValue(), reCaptchaEnabled);
        if (reCaptchaPropertiesDTO.getReCaptchaType() == null) {
            assertNull(reCaptchaPropertiesDTO.getReCaptchaType());
        } else {
            assertEquals(reCaptchaPropertiesDTO.getReCaptchaType(), reCaptchaType);
        }
        if (StringUtils.isBlank(reCaptchaAPI)) {
            assertNull(reCaptchaPropertiesDTO.getReCaptchaAPI());
        } else {
            assertEquals(reCaptchaPropertiesDTO.getReCaptchaAPI(), reCaptchaAPI);
        }
    }

    @DataProvider(name = "captchaResponseDataProvider")
    public static Object[][] captchaResponseDataProvider() {

        String reCaptchaEnterprise = CaptchaConstants.RE_CAPTCHA_TYPE_ENTERPRISE;

        return new Object[][]{
                {"", "{\"success\":\"true\"}"},
                {reCaptchaEnterprise, "{ \"tokenProperties\": { \"valid\": \"true\" } }"}
        };
    }

    @Test(description = "Test verify successful captcha response", dataProvider = "captchaResponseDataProvider")
    void testVerifyCaptchaSuccessful(String reCaptchaType, String responseString) throws Exception {

        ReCaptchaResponseTokenDTO reCaptchaResponse = new ReCaptchaResponseTokenDTO();
        String captchaType = "ReCaptcha";
        InputStream jsonStream = new ByteArrayInputStream(responseString.getBytes());


        Properties sampleProperties = getSampleConfigFile();
        sampleProperties.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED, "true");
        sampleProperties.setProperty(CaptchaConstants.RE_CAPTCHA_TYPE, reCaptchaType);

        mockedRecoveryUtil.when(RecoveryUtil::getValidatedCaptchaConfigs).thenReturn(sampleProperties);
        mockedRecoveryUtil.when(() -> RecoveryUtil.makeCaptchaVerificationHttpRequestUsingHttpClient5(
                reCaptchaResponse, sampleProperties)).thenReturn(classicHttpResponse);

        when(classicHttpResponse.getEntity()).thenReturn(httpEntity);
        when(httpEntity.getContent()).thenReturn(jsonStream);
        when(classicHttpResponse.getCode()).thenReturn(200);

        Response response = captchaApiService.verifyCaptcha(reCaptchaResponse, captchaType, "carbon.super");

        assertNotNull(response);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
    }

    public Properties getSampleConfigFile() throws IOException {

        Path path = Paths.get("src/test/resources", "repository", "conf", "identity",
                CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);
        Properties sampleProperties = new Properties();
        if (Files.exists(path)) {
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                sampleProperties.load(in);
            } catch (IOException e) {
                throw new IOException("Unable to read the captcha configuration file.", e);
            }
        }
        return sampleProperties;
    }
}
