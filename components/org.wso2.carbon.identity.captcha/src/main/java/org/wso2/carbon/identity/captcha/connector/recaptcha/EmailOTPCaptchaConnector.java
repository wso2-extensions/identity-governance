/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.org) All Rights Reserved.
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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.AuthenticationFlowHandler;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedIdPData;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME;

/**
 * Recaptcha Connector for Email OTP.
 */
public class EmailOTPCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(EmailOTPCaptchaConnector.class);
    private static final String SECURED_DESTINATIONS = "/commonauth";
    public static final String EMAIL_OTP_AUTHENTICATOR_NAME = "EmailOTP";
    public static final String IS_REDIRECT_TO_EMAIL_OTP = "isRedirectToEmailOTP";
    public static final String RESEND_CODE = "resendCode";
    private static final String ON_FAIL_REDIRECT_URL = "/authenticationendpoint/email_otp.do";
    private static final String EMAIL_OTP_LOGIN_ATTEMPT_FAIL_CLAIM = "http://wso2.org/claims/identity/failedEmailOtpAttempts";
    private static final String RECAPTCHA_PARAM = "reCaptcha";
    private static final String AUTH_FAILURE = "authFailure";
    private static final String AUTH_FAILURE_MSG = "authFailureMsg";
    private static final String RECAPTCHA_FAIL_MSG = "recaptcha.fail.message";
    private static final String OTP_CODE_PARAM = "OTPCode";
    private IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {
        return 30;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        if (StringUtils.isBlank(path) || !(CaptchaUtil.isPathAvailable(path, SECURED_DESTINATIONS))) {
            return false;
        }

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (sessionDataKey == null) {
            return false;
        }

        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null
                || !EMAIL_OTP_AUTHENTICATOR_NAME.equals(context.getCurrentAuthenticator())
                || (EMAIL_OTP_AUTHENTICATOR_NAME.equals(context.getCurrentAuthenticator())
                && !Boolean.parseBoolean((String) context.getProperty(IS_REDIRECT_TO_EMAIL_OTP)))) {
            return false;
        }

        if (servletRequest.getParameter(OTP_CODE_PARAM) == null) {
            return false;
        }

        if (context.getCurrentStep() != 1 || isPreviousIdPAuthenticationFlowHandler(context)) {
            return false;
        }

        if (Boolean.parseBoolean(servletRequest.getParameter(RESEND_CODE))) {
            return false;
        }

        return isEmailRecaptchaEnabled(servletRequest);
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        String username = context.getLastAuthenticatedUser().getUserName();
        String tenantDomain = context.getLastAuthenticatedUser().getTenantDomain();

        Property[] connectorConfigs = null;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE_ALWAYS},
                tenantDomain);
        } catch (IdentityGovernanceException e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration.
            log.error("Unable to load connector configuration.", e);
        }

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants() || (connectorConfigs != null &&
                connectorConfigs.length != 0 && (Boolean.parseBoolean(connectorConfigs[0].getValue())))) {

            Map<String, String> params = new HashMap<>();
            params.put(AUTH_FAILURE, Boolean.TRUE.toString());
            params.put(AUTH_FAILURE_MSG, RECAPTCHA_FAIL_MSG);
            preValidationResponse.setCaptchaAttributes(params);
            preValidationResponse.setOnCaptchaFailRedirectUrls(getFailedUrlList());
            preValidationResponse.setCaptchaValidationRequired(true);

        } else if (CaptchaUtil.isMaximumFailedLoginAttemptsReached(MultitenantUtils.getTenantAwareUsername(username),
                tenantDomain, EMAIL_OTP_LOGIN_ATTEMPT_FAIL_CLAIM)) {
            preValidationResponse.setCaptchaValidationRequired(true);
            preValidationResponse.setMaxFailedLimitReached(true);

            preValidationResponse.setOnCaptchaFailRedirectUrls(getFailedUrlList());
            Map<String, String> params = new HashMap<>();
            params.put(RECAPTCHA_PARAM, Boolean.TRUE.toString());
            params.put(AUTH_FAILURE, Boolean.TRUE.toString());
            params.put(AUTH_FAILURE_MSG, RECAPTCHA_FAIL_MSG);
            preValidationResponse.setCaptchaAttributes(params);
        }
        // Post validate all requests
        preValidationResponse.setMaxFailedLimitReached(true);
        preValidationResponse.setPostValidationRequired(true);

        return preValidationResponse;
    }

    /**
     * Get the URLs  which need to send back in case of failure.
     *
     * @return list of failed urls
     */
    private List<String> getFailedUrlList() {

        List<String> failedRedirectUrls = new ArrayList<>();

        String failedRedirectUrlStr = CaptchaDataHolder.getInstance().getReCaptchaErrorRedirectUrls();

        if (StringUtils.isNotBlank(failedRedirectUrlStr)) {
            failedRedirectUrls = new ArrayList<>(Arrays.asList(failedRedirectUrlStr.split(",")));
        }

        failedRedirectUrls.add(ON_FAIL_REDIRECT_URL);
        return failedRedirectUrls;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        if (!StringUtils.isBlank(CaptchaConstants.getEnableSecurityMechanism())) {
            CaptchaConstants.removeEnabledSecurityMechanism();
            CaptchaPostValidationResponse validationResponse = new CaptchaPostValidationResponse();
            validationResponse.setSuccessfulAttempt(false);
            validationResponse.setEnableCaptchaResponsePath(true);
            Map<String, String> params = new HashMap<>();
            params.put(RECAPTCHA_PARAM, Boolean.TRUE.toString());
            validationResponse.setCaptchaAttributes(params);
            return validationResponse;
        }
        return null;
    }

    public boolean isEmailRecaptchaEnabled(ServletRequest servletRequest) throws CaptchaException {

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants()) {
            return true;
        }

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (sessionDataKey == null) {
            return false;
        }

        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null
                || context.getLastAuthenticatedUser() == null
                || StringUtils.isBlank(context.getLastAuthenticatedUser().getUserName())) {
            return false;
        }

        String username = context.getLastAuthenticatedUser().getUserName();
        String tenantDomain = context.getLastAuthenticatedUser().getTenantDomain();

        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE_ALWAYS,
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS},
                tenantDomain);
        } catch (IdentityGovernanceException e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration.
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        if (ArrayUtils.isEmpty(connectorConfigs) || connectorConfigs.length != 3 ||
                !(Boolean.parseBoolean(connectorConfigs[0].getValue()) ||
                        Boolean.parseBoolean(connectorConfigs[1].getValue()))) {
            return false;
        }

        if (!Boolean.parseBoolean(connectorConfigs[0].getValue())
                && Boolean.parseBoolean(connectorConfigs[1].getValue())
                && !CaptchaUtil.isMaximumFailedLoginAttemptsReached(username, tenantDomain,
                EMAIL_OTP_LOGIN_ATTEMPT_FAIL_CLAIM)) {
            return false;
        }

        return CaptchaDataHolder.getInstance().isReCaptchaEnabled();
    }

    /**
     * This method checks if all the authentication steps up to now have been performed by authenticators that
     * implements AuthenticationFlowHandler interface. If so, it returns true.
     * AuthenticationFlowHandlers may not perform actual authentication though the authenticated user is set in the
     * context. Hence, this method can be used to determine if the user has been authenticated by a previous step.
     *
     * @param context   AuthenticationContext
     * @return true if all the authentication steps up to now have been performed by AuthenticationFlowHandlers.
     */
    private boolean isPreviousIdPAuthenticationFlowHandler(AuthenticationContext context) {

        boolean hasPreviousAuthenticators = false;
        Map<String, AuthenticatedIdPData> currentAuthenticatedIdPs = context.getCurrentAuthenticatedIdPs();
        if (currentAuthenticatedIdPs != null && !currentAuthenticatedIdPs.isEmpty()) {
            for (Map.Entry<String, AuthenticatedIdPData> entry : currentAuthenticatedIdPs.entrySet()) {
                AuthenticatedIdPData authenticatedIdPData = entry.getValue();
                if (authenticatedIdPData != null) {
                    List<AuthenticatorConfig> authenticators = authenticatedIdPData.getAuthenticators();
                    if (authenticators != null) {
                        for (AuthenticatorConfig authenticator : authenticators) {
                            hasPreviousAuthenticators = true;
                            if (!(authenticator.getApplicationAuthenticator() instanceof AuthenticationFlowHandler)) {
                                return false;
                            }
                        }
                    }
                }
            }
        }

        if (hasPreviousAuthenticators) {
            return true;
        }

        return false;
    }
}
