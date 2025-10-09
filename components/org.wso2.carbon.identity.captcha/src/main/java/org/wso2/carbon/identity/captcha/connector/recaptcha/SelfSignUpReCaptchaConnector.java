/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
 *
 *  WSO2 LLC. licenses this file to you under the Apache License,
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

import java.util.List;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * ReCaptcha Page Based Connector.
 */
public class SelfSignUpReCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(SelfSignUpReCaptchaConnector.class);

    private static final String SELF_REGISTRATION_INITIATE_URL = "/api/identity/recovery/v0.9/claims";

    private static final String SELF_REGISTRATION_URL = "/api/identity/user/v1.0/me";

    private static final String SELF_REGISTRATION_SIGN_UP_URL = "/accountrecoveryendpoint/signup.do";

    private static final String RECAPTCHA_RESPONSE_PARAM = "g-recaptcha-response";

    private final String PROPERTY_ENABLE_RECAPTCHA = "SelfRegistration.ReCaptcha";

    private IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {
        return 10;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        List<String> reCaptchaBypassedApiEndpoints = CaptchaDataHolder.getInstance().getReCaptchaBypassedApiEndpoints();
        if (StringUtils.isBlank(path) || reCaptchaBypassedApiEndpoints.contains(path) ||
                (!CaptchaUtil.isPathAvailable(path, SELF_REGISTRATION_INITIATE_URL) &&
                !CaptchaUtil.isPathAvailable(path, SELF_REGISTRATION_URL) &&
                !CaptchaUtil.isPathAvailable(path, SELF_REGISTRATION_SIGN_UP_URL))) {
            return false;
        }

        String isUsernameRecovery = servletRequest.getParameter("isUsernameRecovery");
        if (isUsernameRecovery != null) {
            return false;
        }
        String requestMethod = ((HttpServletRequest) servletRequest).getMethod();
        if (StringUtils.equalsIgnoreCase(path, SELF_REGISTRATION_URL) &&
                StringUtils.equalsIgnoreCase(requestMethod, "GET")) {
            return false;
        }
        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants()) {
            return true;
        }

        return CaptchaUtil.isRecaptchaEnabledForConnector(identityGovernanceService, servletRequest,
                PROPERTY_ENABLE_RECAPTCHA);
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        // reCaptcha is required for all requests.
        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);

        if (CaptchaUtil.isPathAvailable(path, SELF_REGISTRATION_INITIATE_URL)) {
            httpServletResponse.setHeader("reCaptcha", "true");
        } else {
            httpServletResponse.setHeader("reCaptcha", "conditional");
            preValidationResponse.setCaptchaValidationRequired(true);
            preValidationResponse.setMaxFailedLimitReached(true);
        }
        httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
        httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());

        return preValidationResponse;
    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        // Extract the reCaptcha response from the request parameters or headers.
        String reCaptchaResponse = servletRequest.getParameter(RECAPTCHA_RESPONSE_PARAM);
        if (StringUtils.isBlank(reCaptchaResponse)) {
            reCaptchaResponse = ((HttpServletRequest) servletRequest).getHeader(RECAPTCHA_RESPONSE_PARAM);
        }
        if (StringUtils.isBlank(reCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }

        return CaptchaUtil.isValidCaptcha(reCaptchaResponse);
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        //No validation at this stage.
        return null;
    }
}
