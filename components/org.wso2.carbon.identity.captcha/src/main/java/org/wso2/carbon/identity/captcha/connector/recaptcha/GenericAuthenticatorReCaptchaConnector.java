/*
 * Copyright (c) 2023, WSO2 LLC (http://www.wso2.org).
 *
 * WSO2 LLC licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

/**
 * This class handles the generic ReCaptcha validation for local authenticators.
 */
public class GenericAuthenticatorReCaptchaConnector extends AbstractReCaptchaConnector {

    private static final String IDF = "IdentifierExecutor";

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

    }

    @Override
    public int getPriority() {

        // Giving the lowest priority for the generic captcha connector, as any custom captcha connector
        // should have a higher priority than this.
        return 2;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (StringUtils.isBlank(sessionDataKey)) {
            return false;
        }
        AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);

        return context != null && context.getCurrentAuthenticator() != null &&
                CaptchaUtil.isGenericRecaptchaEnabledAuthenticator(context.getCurrentAuthenticator());
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse) {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        Map<String, String> params = new HashMap<>();
        params.put(CaptchaConstants.RE_CAPTCHA, CaptchaConstants.TRUE);
        params.put(CaptchaConstants.AUTH_FAILURE, CaptchaConstants.TRUE);
        params.put(CaptchaConstants.AUTH_FAILURE_MSG, CaptchaConstants.RECAPTCHA_FAIL_MSG_KEY);
        preValidationResponse.setCaptchaAttributes(params);
        preValidationResponse.setCaptchaValidationRequired(true);

        String sessionDataKey = servletRequest.getParameter(FrameworkUtils.SESSION_DATA_KEY);
        if (StringUtils.isNotBlank(sessionDataKey)) {
            AuthenticationContext context = FrameworkUtils.getAuthenticationContextFromCache(sessionDataKey);
            if (context != null) {
                String authenticator = context.getCurrentAuthenticator();
                // Currently, we are only adding the failed login URLs for the identifier first authenticator.
                // This could be done for any other authenticators as well, when extending the connector's support
                // for other authenticators.
                if (StringUtils.isNotBlank(authenticator) && authenticator.equals(IDF)) {
                    preValidationResponse.setOnCaptchaFailRedirectUrls(CaptchaUtil.getOnFailedLoginUrls());
                }
            }
        }

        return preValidationResponse;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        // No post validation required.
        return null;
    }
}
