/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import javax.servlet.ServletRequest;

public class LocalSMSOTPCaptchaConnector extends AbstractOTPCaptchaConnector {

    private static final String AUTHENTICATOR_NAME = "sms-otp-authenticator";
    private static final String REDIRECT_CONTEXT_PROPERTY_KEY = "isRedirectToSmsOTP";
    private static final String OTP_PARAM_NAME = "OTPcode";
    private static final String RESEND_PARAM_NAME = "resendCode";
    private static final String FAILED_ATTEMPTS_CLAIM_URI = "http://wso2.org/claims/identity/failedLoginAttempts";
    private static final String ON_FAIL_REDIRECT_URL = "/authenticationendpoint/login.do";
    private static final int AUTHENTICATOR_PRIORITY = 30;

    @Override
    protected boolean isOTPParamPresent(ServletRequest servletRequest) {


        return servletRequest.getParameter(OTP_PARAM_NAME) != null;
    }

    @Override
    protected String getAuthenticatorName() {

        return AUTHENTICATOR_NAME;
    }

    @Override
    protected String getRedirectContextPropertyKey() {

        return REDIRECT_CONTEXT_PROPERTY_KEY;
    }

    @Override
    protected String getResendParamName() {

        return RESEND_PARAM_NAME;
    }

    @Override protected String getFailedAttemptsClaimUri() {

        return FAILED_ATTEMPTS_CLAIM_URI;
    }

    @Override
    protected String getOnFailRedirectUrl() {

        return ON_FAIL_REDIRECT_URL;
    }

    @Override
    protected int getAuthenticatorPriority() {

        return AUTHENTICATOR_PRIORITY;
    }
}

