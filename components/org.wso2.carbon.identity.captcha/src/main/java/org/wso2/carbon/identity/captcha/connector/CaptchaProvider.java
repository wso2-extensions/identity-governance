package org.wso2.carbon.identity.captcha.connector;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public interface CaptchaProvider {

    void preValidateForSelfSignUp(ServletRequest servletRequest, ServletResponse servletResponse) throws
            CaptchaException;

    void preValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws
            CaptchaException;

    void preValidateForUsernameRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws
            CaptchaException;

    void preValidateForPasswordRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws
            CaptchaException;

    boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException;

    void postValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws
            CaptchaException;

    void addPostValidationData(ServletRequest servletRequest);
}
