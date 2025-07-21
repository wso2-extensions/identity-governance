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
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineClientException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineServerException;
import org.wso2.carbon.identity.flow.execution.engine.listener.AbstractFlowExecutionListener;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.flow.mgt.model.ComponentDTO;

import java.util.Map;

/**
 * Listener to handle captcha validation.
 */
public class CaptchaFlowExecutionListener extends AbstractFlowExecutionListener {

    public static final String CAPTCHA_ENABLED = "captchaEnabled";
    public static final String CAPTCHA_RESPONSE = "captchaResponse";
    public static final String CAPTCHA_KEY = "captchaKey";
    public static final String CAPTCHA_URL = "captchaURL";
    public static final String RECAPTCHA_TYPE = "recaptchaType";
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
    public boolean doPreExecute(FlowExecutionContext flowExecutionContext) throws FlowEngineException {

        if (isReCaptchaDisabled(flowExecutionContext.getTenantDomain())) {
            return true;
        }

        validateCaptcha(flowExecutionContext);
        return true;
    }

    @Override
    public boolean doPostExecute(FlowExecutionStep step, FlowExecutionContext flowExecutionContext)
            throws FlowEngineException {

        if (isReCaptchaDisabled(flowExecutionContext.getTenantDomain())) {
            return true;
        }
        addCaptchaKeys(step, flowExecutionContext);
        return true;
    }

    private boolean isReCaptchaDisabled(String tenantDomain) {

        return !CaptchaUtil.isReCaptchaEnabledForFlow(CAPTCHA_GOVERNANCE_CONFIG_KEY, tenantDomain);
    }

    private void addCaptchaKeys(FlowExecutionStep step, FlowExecutionContext context) {

        if (step.getData().getComponents() == null) {
            return;
        }

        for (ComponentDTO componentDTO : step.getData().getComponents()) {
            if (componentDTO.getType()
                    .equals(org.wso2.carbon.identity.flow.mgt.Constants.ComponentTypes.CAPTCHA)) {
                /*  Currently, we are only supporting reCaptcha. When generic captcha support is added,
                this needs to be updated. */
                componentDTO.addConfig(CAPTCHA_KEY, CaptchaUtil.reCaptchaSiteKey());
                componentDTO.addConfig(CAPTCHA_URL, CaptchaUtil.reCaptchaAPIURL());
                componentDTO.addConfig(RECAPTCHA_TYPE, CaptchaUtil.getReCaptchaType());
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

    private void validateCaptcha(FlowExecutionContext flowExecutionContext) throws FlowEngineException {

        Map<String, Object> properties = flowExecutionContext.getProperties();
        Map<String, String> userInputData = flowExecutionContext.getUserInputData();

        // Check if captcha is enabled for the current step.
        if (!Boolean.TRUE.equals(properties.get(CAPTCHA_ENABLED))) {
            return;
        }

        flowExecutionContext.setProperty(CAPTCHA_ENABLED, false);
        String captchaResponse = userInputData.get(CAPTCHA_RESPONSE);
        String flowId = flowExecutionContext.getContextIdentifier();
        if (captchaResponse == null || captchaResponse.isEmpty()) {
            throw getFlowEngineClientException(flowId, flowExecutionContext.getFlowType());
        }

        try {
            if (!CaptchaUtil.isValidCaptcha(captchaResponse)) {
                throw getFlowEngineClientException(flowId, flowExecutionContext.getFlowType());
            }
        } catch (CaptchaClientException e) {
            throw getFlowEngineClientException(flowId, flowExecutionContext.getFlowType());
        } catch (CaptchaException e) {
            throw getFlowEngineServerException(flowId, flowExecutionContext.getFlowType());
        }
    }

    private static FlowEngineServerException getFlowEngineServerException(String flowId, String flowType) {

        return new FlowEngineServerException(
                Constants.ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getCode(),
                Constants.ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getMessage(),
                String.format(Constants.ErrorMessages.ERROR_CODE_CAPTCHA_VERIFICATION_FAILURE.getDescription(),
                        flowType, flowId)
        );
    }

    private static FlowEngineClientException getFlowEngineClientException(String flowId, String flowType) {

        return new FlowEngineClientException(Constants.ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getCode(),
                Constants.ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getMessage(),
                String.format(Constants.ErrorMessages.ERROR_CODE_INVALID_CAPTCHA.getDescription(), flowType, flowId));
    }
}
