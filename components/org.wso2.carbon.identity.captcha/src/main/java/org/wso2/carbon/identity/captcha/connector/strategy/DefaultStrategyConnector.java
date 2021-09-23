package org.wso2.carbon.identity.captcha.connector.strategy;

import org.wso2.carbon.identity.captcha.connector.provider.CaptchaProvider;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import java.util.List;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public class DefaultStrategyConnector implements StrategyConnector{

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) {

        return true;
    }

    @Override
    public int getPriority() {

        return 0;
    }

    @Override
    public CaptchaProvider getCaptchaProvider(ServletRequest servletRequest, ServletResponse servletResponse) {

        //loop all the registered providers and return the highest priority

        List<CaptchaProvider> captchaProviders = CaptchaDataHolder.getInstance().getCaptchaProviders();
        CaptchaProvider selectedCaptchaProvider = null;

        for(CaptchaProvider captchaProvider: captchaProviders){
            if (selectedCaptchaProvider == null ||
                    captchaProvider.getPriority() > selectedCaptchaProvider.getPriority()) {
                selectedCaptchaProvider = captchaProvider;
            }
        }
        return selectedCaptchaProvider;
    }
}
