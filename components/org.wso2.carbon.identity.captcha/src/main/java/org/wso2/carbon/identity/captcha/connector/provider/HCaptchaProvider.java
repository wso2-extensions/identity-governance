package org.wso2.carbon.identity.captcha.connector.provider;

import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConfigs;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

public class HCaptchaProvider implements CaptchaProvider{

    @Override
    public String getName() {

        return "HCaptcha";
    }

    @Override
    public int getPriority() {

        return 0;
    }

    @Override
    public void handleCaptchaProperties(CaptchaConfigs captchaConfigs) {

        for (CaptchaConfigs captchaConfigs1: CaptchaDataHolder.getInstance().getCaptchaConfigs()){
            if (this.getName().equals(captchaConfigs.getCaptchaProviderName())){
                return captchaConfigs;
            }
        }
        return null;
    }

    @Override
    public void init() {

    }

    @Override
    public void setCaptchaParamsForPreValidation(ServletRequest servletRequest, ServletResponse servletResponse, CaptchaConstants.Flow flow) throws CaptchaException {

    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        return false;
    }

    @Override
    public void addPostValidationData(ServletRequest servletRequest) {

    }
}
