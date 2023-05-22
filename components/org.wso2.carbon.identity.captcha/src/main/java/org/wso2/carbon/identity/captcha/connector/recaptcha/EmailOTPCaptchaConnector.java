package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
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

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME;

/**
 * Recaptcha Connector for Email OTP.
 */
public class EmailOTPCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(EmailOTPCaptchaConnector.class);
    private static final String SECURED_DESTINATIONS = "/commonauth";
    public static final String EMAIL_OTP_AUTHENTICATOR_NAME = "email-otp-authenticator";
    public static final String IS_REDIRECT_TO_EMAIL_OTP = "isRedirectToEmailOTP";
    public static final String RESEND_CODE = "resendCode";

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
        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);

        if (context == null
                || !EMAIL_OTP_AUTHENTICATOR_NAME.equals(context.getCurrentAuthenticator())
                || (EMAIL_OTP_AUTHENTICATOR_NAME.equals(context.getCurrentAuthenticator())
                && !Boolean.parseBoolean((String) context.getProperty(IS_REDIRECT_TO_EMAIL_OTP)))) {
            return false;
        }

        if (Boolean.parseBoolean(servletRequest.getParameter(RESEND_CODE))) {
            return false;
        }

        return isEmailRecaptchaEnabled(servletRequest);
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse) {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();
        preValidationResponse.setCaptchaValidationRequired(true);
        return preValidationResponse;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse) {

        return null;
    }

    public boolean isEmailRecaptchaEnabled(ServletRequest request) {

        if (CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants()) {
            return true;
        }

        String sessionDataKey = request.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (sessionDataKey == null) {
            return false;
        }
        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        if (context == null) {
            return false;
        }

        String tenantDomain = context.getTenantDomain();
        if (StringUtils.isBlank(tenantDomain)) {
            return false;
        }

        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE_ALWAYS,
                SSO_LOGIN_RECAPTCHA_CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE},
                tenantDomain);
        } catch (IdentityGovernanceException e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration.
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        if (ArrayUtils.isEmpty(connectorConfigs) || connectorConfigs.length != 2 ||
                !(Boolean.parseBoolean(connectorConfigs[0].getValue()) ||
                        Boolean.parseBoolean(connectorConfigs[1].getValue()))) {
            return false;
        }

        if (!CaptchaDataHolder.getInstance().isReCaptchaEnabled()) {
            return false;
        }

        return true;
    }
}
