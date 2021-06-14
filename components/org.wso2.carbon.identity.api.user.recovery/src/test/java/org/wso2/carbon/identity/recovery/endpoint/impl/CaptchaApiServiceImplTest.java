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

import org.mockito.InjectMocks;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import static org.testng.Assert.assertEquals;

/**
 * Unit tests for CaptchaApiServiceImpl.java
 */
public class CaptchaApiServiceImplTest{

    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;

    @InjectMocks
    CaptchaApiServiceImpl captchaApiService;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryUtil.close();
    }

    @Test(description = "This method test, getReCaptcha method for username recovery")
    public void testGetCaptcha() throws IOException {

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

        mockedRecoveryUtil.when(RecoveryUtil::getValidatedCaptchaConfigs).thenReturn(sampleProperties);
        mockedRecoveryUtil.when(() -> RecoveryUtil.checkCaptchaEnabledResidentIdpConfiguration(Mockito.anyString(),
                Mockito.anyString())).thenReturn(true);
        assertEquals(captchaApiService.getCaptcha("ReCaptcha", "username-recovery",
                null).getStatus(), 200);
    }
}
