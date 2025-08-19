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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.AuthenticationFlowHandler;
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
import java.util.Objects;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME;

/**
 * Abstract class for OTP based captcha connectors.
 * This class provides the common functionality for handling OTP based captcha flows.
 */
public abstract class AbstractOTPCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(AbstractOTPCaptchaConnector.class);
    private static final String SECURED_DESTINATIONS = "/commonauth";
    private static final String RECAPTCHA_PARAM = "reCaptcha";
    private static final String AUTH_FAILURE = "authFailure";
    private static final String AUTH_FAILURE_MSG = "authFailureMsg";
    private static final String RECAPTCHA_FAIL_MSG = "recaptcha.fail.message";
    private static final String USER_NAME = "username";

    protected IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {
        return 30;
    }

    /**
     * NOTE: This connector handles ONLY the local authenticator name for the channel.
     * Legacy names are intentionally ignored so the old connectors can keep handling them.
     */
    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String path = ((HttpServletRequest) servletRequest).getRequestURI();
        if (StringUtils.isBlank(path) || !CaptchaUtil.isPathAvailable(path, SECURED_DESTINATIONS)) {
            return false;
        }

        if (!isCaptchaValidationEnabledForLocalOTPAuthenticators()) {
            return false;
        }

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (sessionDataKey == null) {
            return false;
        }

        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null) {
            return false;
        }

        if (!getAuthenticatorName().equals(context.getCurrentAuthenticator())) {
            return false;
        }

        if (!Boolean.parseBoolean((String) context.getProperty(getRedirectContextPropertyKey()))) {
            return false;
        }

        if (!isOTPParamPresent(servletRequest)) {
            return false;
        }

        if (context.getCurrentStep() != 1 || isPreviousIdPAuthenticationFlowHandler(context)) {
            return false;
        }

        if (Boolean.parseBoolean(servletRequest.getParameter(getResendParamName()))) {
            return false;
        }

        return isRecaptchaEnabled(servletRequest);
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null) {
            throw new CaptchaException("Authentication context is not available for the session data key: "
                    + sessionDataKey);
        }

        String username = resolveUserName(context, servletRequest);
        String tenantDomain = resolveTenantDomain(context);
        if (StringUtils.isBlank(username) || StringUtils.isBlank(tenantDomain)) {
            throw new CaptchaException("Username or tenant domain is not available in the authentication context.");
        }

        Property[] connectorConfigs = null;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(
                    new String[]{SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME +
                            CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE_ALWAYS},
                    tenantDomain);
        } catch (IdentityGovernanceException e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration.
            log.error("Unable to load connector configuration.", e);
        }

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants() || (connectorConfigs != null &&
                connectorConfigs.length != 0 && Boolean.parseBoolean(connectorConfigs[0].getValue()))) {

            Map<String, String> params = new HashMap<>();
            params.put(AUTH_FAILURE, Boolean.TRUE.toString());
            params.put(AUTH_FAILURE_MSG, RECAPTCHA_FAIL_MSG);
            preValidationResponse.setCaptchaAttributes(params);
            preValidationResponse.setOnCaptchaFailRedirectUrls(getFailedUrlList());
            preValidationResponse.setCaptchaValidationRequired(true);

        } else if (CaptchaUtil.isMaximumFailedLoginAttemptsReached(
                MultitenantUtils.getTenantAwareUsername(username), tenantDomain, getFailedAttemptsClaimUri())) {

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

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        if (!StringUtils.isBlank(CaptchaConstants.getEnableSecurityMechanism())) {
            CaptchaConstants.removeEnabledSecurityMechanism();
            CaptchaPostValidationResponse validateResponse = new CaptchaPostValidationResponse();
            validateResponse.setSuccessfulAttempt(false);
            validateResponse.setEnableCaptchaResponsePath(true);
            Map<String, String> params = new HashMap<>();
            params.put(RECAPTCHA_PARAM, Boolean.TRUE.toString());
            validateResponse.setCaptchaAttributes(params);
            return validateResponse;
        }
        return null;
    }

    /**
     * Get the list of URLs to send back when the reCaptcha validation fails.
     *
     * @return List of failed urls.
     */
    private List<String> getFailedUrlList() {

        List<String> failedRedirectUrls = new ArrayList<>();
        String failedRedirectUrlStr = CaptchaDataHolder.getInstance().getReCaptchaErrorRedirectUrls();
        if (StringUtils.isNotBlank(failedRedirectUrlStr)) {
            failedRedirectUrls.addAll(Arrays.asList(failedRedirectUrlStr.split(",")));
        }
        failedRedirectUrls.add(getOnFailRedirectUrl());
        return failedRedirectUrls;
    }

    /**
     * Check whether reCaptcha is enabled for the current OTP based authenticator.
     *
     * @param servletRequest Servlet request.
     * @return true if reCaptcha is enabled, false otherwise.
     * @throws CaptchaException If an error occurs while checking reCaptcha status.
     */
    public boolean isRecaptchaEnabled(ServletRequest servletRequest) throws CaptchaException {

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants()) {
            return true;
        }

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (sessionDataKey == null) {
            return false;
        }

        AuthenticationContext ctx = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (ctx == null || ctx.getLastAuthenticatedUser() == null ||
                StringUtils.isBlank(ctx.getLastAuthenticatedUser().getUserName())) {
            return false;
        }

        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null) {
            return false;
        }

        String username = resolveUserName(context, servletRequest);
        String tenant = resolveTenantDomain(context);

        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE_ALWAYS,
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS},
                    tenant);
        } catch (IdentityGovernanceException e) {
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
                && !CaptchaUtil.isMaximumFailedLoginAttemptsReached(username, tenant, getFailedAttemptsClaimUri())) {
            return false;
        }

        return CaptchaDataHolder.getInstance().isReCaptchaEnabled()
                && isCaptchaValidationEnabledForLocalOTPAuthenticators();
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

        Map<String, AuthenticatedIdPData> currentAuthenticatedIdPs = context.getCurrentAuthenticatedIdPs();
        return currentAuthenticatedIdPs != null && !currentAuthenticatedIdPs.isEmpty() &&
                currentAuthenticatedIdPs.values().stream().filter(Objects::nonNull)
                        .map(AuthenticatedIdPData::getAuthenticators).filter(Objects::nonNull)
                        .flatMap(List::stream)
                        .allMatch(authenticator ->
                                authenticator.getApplicationAuthenticator() instanceof AuthenticationFlowHandler);
    }

    /**
     * Resolve the username from the authentication context or servlet request.
     *
     * @param context         AuthenticationContext
     * @param servletRequest  ServletRequest
     * @return Username if available, null otherwise.
     */
    private String resolveUserName(AuthenticationContext context, ServletRequest servletRequest) {

        if (context != null && context.getLastAuthenticatedUser() != null) {
            return context.getLastAuthenticatedUser().getUserName();
        }

        if (servletRequest != null && servletRequest.getParameter(USER_NAME) != null) {
            return servletRequest.getParameter(USER_NAME);
        }
        return null;
    }

    /**
     * Resolve the tenant domain from the authentication context.
     *
     * @param context AuthenticationContext
     * @return Tenant domain if available, null otherwise.
     */
    private String resolveTenantDomain(AuthenticationContext context) {

        if (context != null && context.getLastAuthenticatedUser() != null) {
            return context.getLastAuthenticatedUser().getTenantDomain();
        }

        if (context != null && context.getUserTenantDomain() != null) {
            return context.getUserTenantDomain();
        }
        return null;
    }

    /**
     * Check if the captcha validation is enabled for local OTP authenticators.
     *
     * @return true if captcha validation is enabled for local OTP authenticators, false otherwise.
     */
    protected boolean isCaptchaValidationEnabledForLocalOTPAuthenticators() {

        return CaptchaUtil.isCaptchaValidationEnabledForLocalOTPAuthenticators();
    }

    /**
     * Check if the OTP parameter is present in the servlet request.
     *
     * @param servletRequest Servlet request.
     * @return true if OTP parameter is present, false otherwise.
     */
    protected abstract boolean isOTPParamPresent(ServletRequest servletRequest);

    /**
     * Get the name of the authenticator that this connector handles.
     *
     * @return Authenticator name.
     */
    protected abstract String getAuthenticatorName();

    /**
     * Get the key for the redirect context property.
     *
     * @return Redirect context property key.
     */
    protected abstract String getRedirectContextPropertyKey();

    /**
     * Get the name of the parameter used for resending the OTP.
     *
     * @return Resend parameter name.
     */
    protected abstract String getResendParamName();

    /**
     * Get the URI of the claim that holds the number of failed attempts.
     *
     * @return Failed attempts claim URI.
     */
    protected abstract String getFailedAttemptsClaimUri();

    /**
     * Get the URL to redirect to when the captcha validation fails.
     *
     * @return Redirect URL on failure.
     */
    protected abstract String getOnFailRedirectUrl();
}
