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

package org.wso2.carbon.identity.captcha.listener;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.user.registration.engine.exception.RegistrationEngineClientException;
import org.wso2.carbon.identity.user.registration.engine.exception.RegistrationEngineException;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationContext;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationStep;
import org.wso2.carbon.identity.user.registration.mgt.Constants;
import org.wso2.carbon.identity.user.registration.mgt.model.ComponentDTO;
import org.wso2.carbon.identity.user.registration.mgt.model.DataDTO;

import java.util.Collections;
import java.util.HashSet;

import static org.mockito.ArgumentMatchers.anyString;
import static org.wso2.carbon.identity.captcha.listener.CaptchaFlowExecutionListener.CAPTCHA_ENABLED;
import static org.wso2.carbon.identity.captcha.listener.CaptchaFlowExecutionListener.CAPTCHA_KEY;
import static org.wso2.carbon.identity.captcha.listener.CaptchaFlowExecutionListener.CAPTCHA_RESPONSE;

/**
 * Unit test for {@link CaptchaFlowExecutionListener}.
 */
public class CaptchaFlowExecutionListenerTest {

    private static CaptchaFlowExecutionListener captchaFlowExecutionListener;

    @BeforeMethod
    public void setUp() {

        captchaFlowExecutionListener = new CaptchaFlowExecutionListener();
    }

    @AfterMethod
    public void tearDown() {

    }

    @Test
    public void testDoPostInitiate() throws RegistrationEngineException {

        RegistrationStep registrationStep = getRegistrationStep();
        RegistrationContext registrationContext = getRegistrationContext();
        try (MockedStatic<CaptchaUtil> captchaUtilMockedStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            captchaUtilMockedStatic.when(CaptchaUtil::reCaptchaSiteKey).thenReturn("captchaKeyValue");
            captchaUtilMockedStatic.when(CaptchaUtil::isReCaptchaEnabled).thenReturn(true);
            captchaFlowExecutionListener.doPostInitiate(registrationStep, registrationContext);
            Assert.assertTrue(registrationStep.getData().getComponents().get(0).getConfigs().containsKey(CAPTCHA_KEY));
            Assert.assertTrue(registrationContext.getCurrentStepInputs().get("action1").contains(CAPTCHA_RESPONSE));
            Assert.assertTrue(registrationContext.getCurrentRequiredInputs().get("action1").contains(CAPTCHA_RESPONSE));
            Assert.assertTrue((boolean) registrationContext.getProperty(CAPTCHA_ENABLED));
        }
    }

    @Test(expectedExceptions = RegistrationEngineClientException.class)
    public void testDoPreContinueMissingCaptchaResponse() throws RegistrationEngineException {

        RegistrationContext registrationContext = getRegistrationContext();
        registrationContext.setProperty(CAPTCHA_ENABLED, true);
        registrationContext.getUserInputData().put(CAPTCHA_RESPONSE, "");
        try (MockedStatic<CaptchaUtil> captchaUtilMockedStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            captchaUtilMockedStatic.when(CaptchaUtil::isReCaptchaEnabled).thenReturn(true);
            captchaFlowExecutionListener.doPreContinue(registrationContext);
        }
    }

    @Test(expectedExceptions = RegistrationEngineClientException.class)
    public void testDoPreContinueInvalidCaptcha() throws RegistrationEngineException {

        RegistrationContext registrationContext = getRegistrationContext();
        registrationContext.setProperty(CAPTCHA_ENABLED, true);
        registrationContext.getUserInputData().put(CAPTCHA_RESPONSE, "");
        try (MockedStatic<CaptchaUtil> captchaUtilMockedStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            captchaUtilMockedStatic.when(() -> CaptchaUtil.isValidCaptcha(anyString())).thenReturn(false);
            captchaUtilMockedStatic.when(CaptchaUtil::isReCaptchaEnabled).thenReturn(true);
            captchaFlowExecutionListener.doPreContinue(registrationContext);
        }
    }

    @Test
    public void testDoPostContinue() throws RegistrationEngineException {

        RegistrationStep registrationStep = getRegistrationStep();
        RegistrationContext registrationContext = getRegistrationContext();
        try (MockedStatic<CaptchaUtil> captchaUtilMockedStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            captchaUtilMockedStatic.when(CaptchaUtil::reCaptchaSiteKey).thenReturn("captchaKeyValue");
            captchaUtilMockedStatic.when(CaptchaUtil::isReCaptchaEnabled).thenReturn(true);
            captchaFlowExecutionListener.doPostContinue(registrationStep, registrationContext);
            Assert.assertTrue(registrationStep.getData().getComponents().get(0).getConfigs().containsKey(CAPTCHA_KEY));
            Assert.assertTrue(registrationContext.getCurrentStepInputs().get("action1").contains(CAPTCHA_RESPONSE));
            Assert.assertTrue(registrationContext.getCurrentRequiredInputs().get("action1").contains(CAPTCHA_RESPONSE));
            Assert.assertTrue((boolean) registrationContext.getProperty(CAPTCHA_ENABLED));
        }
    }

    private RegistrationContext getRegistrationContext() {

        RegistrationContext registrationContext = new RegistrationContext();
        registrationContext.setTenantDomain("test.com");
        registrationContext.setContextIdentifier("contextId");
        registrationContext.getCurrentRequiredInputs().put("action1", new HashSet<>());
        registrationContext.getCurrentStepInputs().put("action1", new HashSet<>());
        return registrationContext;
    }

    private RegistrationStep getRegistrationStep() {

        ComponentDTO componentDTO = new ComponentDTO.Builder()
                .id("captcha_f12v")
                .type("CAPTCHA")
                .variant("RECAPTCHA_V2")
                .build();
        return new RegistrationStep.Builder()
                .stepType(Constants.StepTypes.VIEW)
                .flowId("flowId")
                .data(new DataDTO.Builder()
                        .components(Collections.singletonList(componentDTO)).build()).build();
    }
}
