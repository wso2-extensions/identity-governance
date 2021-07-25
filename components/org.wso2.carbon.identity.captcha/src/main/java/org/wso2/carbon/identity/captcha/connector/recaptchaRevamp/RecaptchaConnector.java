package org.wso2.carbon.identity.captcha.connector.recaptchaRevamp;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.captcha.connector.CaptchaProvider;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.captcha.util.EnabledSecurityMechanism;

import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class RecaptchaConnector implements CaptchaProvider {

    private static final String SELF_REGISTRATION_INITIATE_URL = "/api/identity/recovery/v0.9/claims";


    @Override
    public void preValidateForSelfSignUp(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
        httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());

    }

    @Override
    public void preValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }

    @Override
    public void preValidateForUsernameRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {
        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        httpServletResponse.setHeader("reCaptcha", "conditional");
    }

    @Override
    public void preValidateForPasswordRecovery(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {
        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
        httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
    }

    public void addPostValidationData(ServletRequest servletRequest){
        EnabledSecurityMechanism enabledSecurityMechanism = new EnabledSecurityMechanism();
        enabledSecurityMechanism.setMechanism("reCaptcha");
        Map<String, String> properties = new HashMap<>();
        properties.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
        properties.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
        enabledSecurityMechanism.setProperties(properties);
        ((HttpServletRequest) servletRequest).getSession().setAttribute("enabled-security-mechanism",
                enabledSecurityMechanism);
    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        String reCaptchaResponse = ((HttpServletRequest) servletRequest).getHeader("g-recaptcha-response");
        if (StringUtils.isBlank(reCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }

        return CaptchaUtil.isValidCaptcha(reCaptchaResponse);
    }


    @Override
    public void postValidateForSSOLogin(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

    }



}
