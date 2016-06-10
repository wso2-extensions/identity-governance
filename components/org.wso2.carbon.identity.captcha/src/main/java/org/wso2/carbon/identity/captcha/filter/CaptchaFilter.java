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

package org.wso2.carbon.identity.captcha.filter;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.client.utils.URIBuilder;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaResponseWrapper;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

/**
 * Captcha filter.
 */
public class CaptchaFilter implements Filter {

    private static final Log log = LogFactory.getLog(CaptchaFilter.class);

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {

        if (log.isDebugEnabled()) {
            log.debug("Captcha filter activated.");
        }
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {

        try {

            List<CaptchaConnector> captchaConnectors = CaptchaDataHolder.getInstance().getCaptchaConnectors();

            CaptchaConnector selectedCaptchaConnector = null;
            for (CaptchaConnector captchaConnector : captchaConnectors) {
                if (captchaConnector.canHandle(servletRequest, servletResponse) && (selectedCaptchaConnector == null ||
                        captchaConnector.getPriority() > selectedCaptchaConnector.getPriority())) {
                    selectedCaptchaConnector = captchaConnector;
                }
            }

            if (selectedCaptchaConnector == null) {
                filterChain.doFilter(servletRequest, servletResponse);
                return;
            }

            // Check whether captcha is required or will reach to the max failed attempts with the current attempt.
            CaptchaPreValidationResponse captchaPreValidationResponse = selectedCaptchaConnector
                    .preValidate(servletRequest, servletResponse);

            if (captchaPreValidationResponse == null) {
                // Captcha connector failed response. Default is success.
                filterChain.doFilter(servletRequest, servletResponse);
                return;
            }

            if (captchaPreValidationResponse.isCaptchaRequired()) {
                try {
                    boolean validCaptcha = selectedCaptchaConnector.verifyCaptcha(servletRequest, servletResponse);
                    if (!validCaptcha) {
                        log.warn("Captcha validation failed for the user.");
                        redirectToErrorPage(servletResponse, captchaPreValidationResponse.getOnFailRedirectUrl(),
                                "Human User Verification Failed.", "Captcha validation failed for the user.");
                        return;
                    }
                } catch (CaptchaClientException e) {
                    log.warn("Captcha validation failed for the user. Cause : " + e.getMessage());
                    redirectToErrorPage(servletResponse, captchaPreValidationResponse.getOnFailRedirectUrl(),
                            "Human User Verification Failed.", e.getMessage());
                    return;
                }
            }

            // Enable reCaptcha for the destination.
            if (captchaPreValidationResponse.isEnableCaptchaForDestination()) {
                if (captchaPreValidationResponse.getRequestAttributes() != null) {
                    for (Map.Entry<String, String> parameter : captchaPreValidationResponse.getRequestAttributes()
                            .entrySet()) {
                        servletRequest.setAttribute(parameter.getKey(), parameter.getValue());
                    }
                }
                filterChain.doFilter(servletRequest, servletResponse);
                return;
            }

            // Below the no. of max failed attempts, including the current attempt
            if (!captchaPreValidationResponse.isPostValidationRequired() || (!captchaPreValidationResponse
                    .isCaptchaRequired() && !captchaPreValidationResponse.isMaxLimitReached())) {
                filterChain.doFilter(servletRequest, servletResponse);
                return;
            }

            HttpServletResponse httpResponse = (HttpServletResponse) servletResponse;
            CaptchaResponseWrapper responseWrapper = new CaptchaResponseWrapper(httpResponse);
            filterChain.doFilter(servletRequest, responseWrapper);

            CaptchaPostValidationResponse postValidationResponse = selectedCaptchaConnector
                    .postValidate(servletRequest, responseWrapper);

            // Check whether this attempt is failed
            if (postValidationResponse == null || postValidationResponse.isSuccessfulAttempt()) {
                if (responseWrapper.isRedirect()) {
                    httpResponse.sendRedirect(responseWrapper.getRedirectURL());
                }
                return;
            }

            if (postValidationResponse.isEnableCaptcha() && responseWrapper.isRedirect()) {
                if (postValidationResponse.getResponseParameters() != null) {
                    URIBuilder uriBuilder;
                    try {
                        uriBuilder = new URIBuilder(responseWrapper.getRedirectURL());
                        for (Map.Entry<String, String> entry : postValidationResponse.getResponseParameters()
                                .entrySet()) {
                            uriBuilder.addParameter(entry.getKey(), entry.getValue());
                        }
                        httpResponse.sendRedirect(uriBuilder.build().toString());
                    } catch (URISyntaxException e) {
                        httpResponse.sendRedirect(responseWrapper.getRedirectURL());
                    }
                }
            }
        } catch (CaptchaException e) {
            log.error("Error occurred in processing captcha.", e);
            redirectToErrorPage(servletResponse, null, "Server Error.", "Something went wrong. Please try again");
        }
    }

    @Override
    public void destroy() {

    }

    private void redirectToErrorPage(ServletResponse servletResponse, String url, String status, String statusMsg)
            throws IOException {

        if (StringUtils.isBlank(url)) {
            url = "/authenticationendpoint/retry.do";
        }

        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder(url);
            uriBuilder.addParameter("status", status);
            uriBuilder.addParameter("statusMsg", statusMsg);
            ((HttpServletResponse) servletResponse).sendRedirect(uriBuilder.build().toString());
        } catch (URISyntaxException e) {
            ((HttpServletResponse) servletResponse).sendRedirect(url);
        }
    }
}
