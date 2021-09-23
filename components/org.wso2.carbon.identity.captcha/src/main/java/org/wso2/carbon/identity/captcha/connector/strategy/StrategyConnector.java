package org.wso2.carbon.identity.captcha.connector.strategy;

import org.wso2.carbon.identity.captcha.connector.provider.CaptchaProvider;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

/**
 * Strategy connector interface.
 */
public interface StrategyConnector {

    boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse);

    int getPriority();

    CaptchaProvider getCaptchaProvider(ServletRequest servletRequest, ServletResponse servletResponse);

}
