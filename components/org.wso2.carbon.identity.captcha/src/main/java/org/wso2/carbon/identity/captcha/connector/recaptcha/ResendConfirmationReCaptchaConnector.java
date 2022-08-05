/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

/**
 * This class handle the resend confirmation ReCaptcha.
 */
public class ResendConfirmationReCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(ResendConfirmationReCaptchaConnector.class);
    private static final String RESEND_CONFIRMATION_URL = "/api/identity/user/v1.0/resend-code";
    private static final String RESEND_CONFIRMATION_RECAPTCHA_ENABLE = "SelfRegistration.ResendConfirmationReCaptcha";
    private IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {

        return 3;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        if (StringUtils.isBlank(path) || !(CaptchaUtil.isPathAvailable(path, RESEND_CONFIRMATION_URL))) {
            return false;
        }

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants()) {
            return true;
        }

        return CaptchaUtil.isRecaptchaEnabledForConnector(identityGovernanceService, servletRequest,
                RESEND_CONFIRMATION_RECAPTCHA_ENABLE);
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();
        String path = ((HttpServletRequest) servletRequest).getRequestURI();
        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);

        if (CaptchaUtil.isPathAvailable(path, RESEND_CONFIRMATION_URL)) {
            httpServletResponse.setHeader("reCaptchaResend", "true");
            preValidationResponse.setCaptchaValidationRequired(true);
            Map<String, String> params = new HashMap<>();
            params.put("errorMessage", "recaptcha.fail.message");
            preValidationResponse.setCaptchaAttributes(params);
            preValidationResponse.setCaptchaValidationRequired(true);
        }
        return preValidationResponse;
    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        String reCaptchaResponse = ((HttpServletRequest) servletRequest).getHeader("g-recaptcha-response");
        if (StringUtils.isBlank(reCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }
        return CaptchaUtil.isValidCaptcha(reCaptchaResponse);
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        // No need post validation.
        return null;
    }
}
