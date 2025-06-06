/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

/**
 * Abstract ReCaptcha Connector.
 */
public abstract class AbstractReCaptchaConnector implements CaptchaConnector {

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        if (((HttpServletRequest) servletRequest).getMethod().equalsIgnoreCase("GET")) {
            throw new CaptchaClientException("Captcha response must send in a POST request.");
        }

        String captchaResponse = servletRequest.getParameter(
                CaptchaDataHolder.getInstance().getCaptchaRuntimeService().getCaptchaResponseIdentifier());
        if (StringUtils.isBlank(captchaResponse)) {
            throw new CaptchaClientException("Captcha response is not available in the request.");
        }

        return CaptchaDataHolder.getInstance().getCaptchaRuntimeService().verifyCaptcha(captchaResponse);
    }
}
