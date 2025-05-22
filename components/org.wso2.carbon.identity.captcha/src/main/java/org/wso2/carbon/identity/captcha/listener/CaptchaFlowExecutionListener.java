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

import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.user.registration.engine.Constants.ErrorMessages;
import org.wso2.carbon.identity.user.registration.engine.exception.RegistrationEngineClientException;
import org.wso2.carbon.identity.user.registration.engine.exception.RegistrationEngineException;
import org.wso2.carbon.identity.user.registration.engine.exception.RegistrationEngineServerException;
import org.wso2.carbon.identity.user.registration.engine.listener.AbstractFlowExecutionListener;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationContext;
import org.wso2.carbon.identity.user.registration.engine.model.RegistrationStep;
import org.wso2.carbon.identity.user.registration.mgt.Constants;
import org.wso2.carbon.identity.user.registration.mgt.model.ComponentDTO;

import java.util.Map;

/**
 * Listener to handle captcha validation.
 */
public class CaptchaFlowExecutionListener extends AbstractFlowExecutionListener {

    public static final String CAPTCHA_ENABLED = "captchaEnabled";
    public static final String CAPTCHA_RESPONSE = "captchaResponse";
    public static final String CAPTCHA_KEY = "captchaKey";
    public static final String CAPTCHA_URL = "captchaURL";
    private static final String CAPTCHA_GOVERNANCE_CONFIG_KEY = "sso.login.recaptcha.enable";

    @Override
    public int getDefaultOrderId() {

        return 3;
    }

    @Override
    public int getExecutionOrderId() {

        return 3;
    }

    @Override
    public boolean isEnabled() {

        return true;
    }

    @Override
    public boolean doPreExecute(RegistrationContext registrationContext) throws RegistrationEngineException {

        if (isReCaptchaDisabled(registrationContext.getTenantDomain())) {
            return true;
        }

        validateCaptcha(registrationContext);
        return true;
    }

    @Override
    public boolean doPostExecute(RegistrationStep step, RegistrationContext registrationContext)
            throws RegistrationEngineException {

        if (isReCaptchaDisabled(registrationContext.getTenantDomain())) {
            return true;
        }
        addCaptchaKeys(step, registrationContext);
        return true;
    }

    private boolean isReCaptchaDisabled(String tenantDomain) {

        return !CaptchaUtil.isReCaptchaEnabledForFlow(CAPTCHA_GOVERNANCE_CONFIG_KEY, tenantDomain);
    }

    private void addCaptchaKeys(RegistrationStep step, RegistrationContext context) {

        if (step.getData().getComponents() == null) {
            return;
        }

        for (ComponentDTO componentDTO : step.getData().getComponents()) {
            if (componentDTO.getType().equals(Constants.ComponentTypes.CAPTCHA)) {
                /*  Currently, we are only supporting reCaptcha. When generic captcha support is added,
                this needs to be updated. */
                componentDTO.addConfig(CAPTCHA_KEY, CaptchaUtil.reCaptchaSiteKey());
                componentDTO.addConfig(CAPTCHA_URL, CaptchaUtil.reCaptchaAPIURL());
                context.setProperty(CAPTCHA_ENABLED, true);
                step.getData().addAdditionalData(CAPTCHA_ENABLED, String.valueOf(true));
                context.getCurrentStepInputs().forEach(
                        (key, value) -> value.add(CAPTCHA_RESPONSE)
                );
                context.getCurrentRequiredInputs().forEach(
                        (key, value) -> value.add(CAPTCHA_RESPONSE)
                );
                return;
            }
        }
    }

    private void validateCaptcha(RegistrationContext registrationContext) throws RegistrationEngineException {

        Map<String, Object> properties = registrationContext.getProperties();
        Map<String, String> userInputData = registrationContext.getUserInputData();

        // Check if captcha is enabled for the current step.
        if (!Boolean.TRUE.equals(properties.get(CAPTCHA_ENABLED))) {
            return;
        }

        registrationContext.setProperty(CAPTCHA_ENABLED, false);
        String captchaResponse = userInputData.get(CAPTCHA_RESPONSE);
        String flowId = registrationContext.getContextIdentifier();
        if (captchaResponse == null || captchaResponse.isEmpty()) {
            throw getRegistrationEngineClientException(flowId);
        }

        try {
            if (!CaptchaUtil.isValidCaptcha(captchaResponse)) {
                throw getRegistrationEngineClientException(flowId);
            }
        } catch (CaptchaClientException e) {
            throw getRegistrationEngineClientException(flowId);
        } catch (CaptchaException e) {
            throw getRegistrationEngineServerException(flowId);
        }
    }

    private static RegistrationEngineServerException getRegistrationEngineServerException(String flowId) {

        return new RegistrationEngineServerException(
                ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getCode(),
                ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getMessage(),
                String.format(ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getDescription(), flowId)
        );
    }

    private static RegistrationEngineClientException getRegistrationEngineClientException(String flowId) {

        return new RegistrationEngineClientException(ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getCode(),
                ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getMessage(),
                String.format(ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getDescription(), flowId));
    }
}
