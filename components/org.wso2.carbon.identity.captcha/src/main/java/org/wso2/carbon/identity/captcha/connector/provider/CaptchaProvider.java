package org.wso2.carbon.identity.captcha.connector.provider;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.util.CaptchaConfigs;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

/**
 * Captcha Provider interface.
 */
public interface CaptchaProvider {

    String getName();

    int getPriority();

    void handleCaptchaProperties(CaptchaConfigs captchaConfigs);

    void init();

    void setCaptchaParamsForPreValidation(ServletRequest servletRequest, ServletResponse servletResponse,
                                          CaptchaConstants.Flow flow) throws CaptchaException;

    boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException;

    void addPostValidationData(ServletRequest servletRequest);
}
