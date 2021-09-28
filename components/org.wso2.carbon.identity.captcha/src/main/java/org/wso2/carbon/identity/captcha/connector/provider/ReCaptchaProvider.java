/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.captcha.connector.provider;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConfigs;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * RecaptchaProvider
 */
public class ReCaptchaProvider implements CaptchaProvider{

    @Override
    public String getName() {

        return "Recaptcha";
    }

    @Override
    public int getPriority() {

        return 0;
    }

    @Override
    public CaptchaConfigs getCaptchaProperties() {

        for (CaptchaConfigs captchaConfigs: CaptchaDataHolder.getInstance().getCaptchaConfigs()){
            if (this.getName().equals(captchaConfigs.getCaptchaProviderName())){
                return captchaConfigs;
            }
        }
        return null;
    }

    @Override
    public void setCaptchaParamsForPreValidation(ServletRequest servletRequest, ServletResponse servletResponse, CaptchaConstants.Flow flow) throws CaptchaException {
        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        switch (flow){
            case SELF_SIGNUP_FLOW:
            case PASSWORD_RECOVERY_FLOW:
                httpServletResponse.setHeader("reCaptchaKey", (String) getCaptchaProperties().getCaptchaProperties().get("SiteKey"));
                httpServletResponse.setHeader("reCaptchaAPI", (String) getCaptchaProperties().getCaptchaProperties().get("API"));
                break;
        }

    }

    @Override
    public void setCaptchaParamsForPostValidation(ServletRequest servletRequest, ServletResponse servletResponse, CaptchaConstants.Flow flow) throws CaptchaException {

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
    public void addPostValidationData(ServletRequest servletRequest) {

    }
}
