package org.wso2.carbon.identity.captcha.connector.kaptcha;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.captcha.connector.CaptchaProvider;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public class KaptchaConnector implements CaptchaProvider {


    @Override
    public void preValidateForSelfSignUp(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }

    @Override
    public void preValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }

    @Override
    public void preValidateForUsernameRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }

    @Override
    public void preValidateForPasswordRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {
        AuthenticationContext context = getAuthenticationContext(servletRequest);
        String generatedCaptchaText = (String) context.getProperty("CAPTCHA_TEXT");
        String userCaptcha = servletRequest.getParameter("CAPTCHA_TEXT");
        return StringUtils.equalsIgnoreCase(generatedCaptchaText, userCaptcha);
    }

    protected AuthenticationContext getAuthenticationContext(ServletRequest request) {

        String sessionDataKey = request.getParameter(FrameworkConstants.SESSION_DATA_KEY);
        if (StringUtils.isNotEmpty(sessionDataKey)) {
            return FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
        }
        return null;
    }



    @Override
    public void postValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }


    @Override
    public void addPostValidationData(ServletRequest servletRequest) {

    }


}
