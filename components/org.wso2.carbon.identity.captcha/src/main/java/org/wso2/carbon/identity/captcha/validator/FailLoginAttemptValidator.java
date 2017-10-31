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

package org.wso2.carbon.identity.captcha.validator;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.AuthenticationDataPublisher;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.context.SessionContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * FailLoginAttemptValidator.
 */
public class FailLoginAttemptValidator extends AbstractEventHandler implements AuthenticationDataPublisher {

    private static Log log = LogFactory.getLog(FailLoginAttemptValidator.class);

    @Override
    public String getName() {
        return "FailLoginAttemptValidator";
    }

    @Override
    public void publishAuthenticationStepSuccess(HttpServletRequest httpServletRequest,
                                                 AuthenticationContext authenticationContext, Map<String, Object> map) {

    }

    @Override
    public void publishAuthenticationStepFailure(HttpServletRequest httpServletRequest,
                                                 AuthenticationContext authenticationContext, Map<String, Object> map) {

        if ("BasicAuthenticator".equals(authenticationContext.getCurrentAuthenticator()) && map != null && map.get
                (FrameworkConstants.AnalyticsAttributes.USER) != null) {

            if (map.get(FrameworkConstants.AnalyticsAttributes.USER) instanceof User) {
                User failedUser = (User) map.get(FrameworkConstants.AnalyticsAttributes.USER);
                String username = failedUser.getUserName();
                if (!StringUtils.isBlank(failedUser.getUserStoreDomain()) &&
                        !IdentityUtil.getPrimaryDomainName().equals(failedUser.getUserStoreDomain())) {
                    username = UserCoreUtil.addDomainToName(username, failedUser.getUserStoreDomain());
                }
                try {
                    if (CaptchaUtil.isMaximumFailedLoginAttemptsReached(username, failedUser.getTenantDomain())) {
                        CaptchaConstants.setEnableSecurityMechanism("enable");
                    }
                } catch (CaptchaException e) {
                    log.error("Failed to evaluate max failed attempts of the user.", e);
                }
            }
        }
    }

    @Override
    public void publishAuthenticationSuccess(HttpServletRequest httpServletRequest,
                                             AuthenticationContext authenticationContext, Map<String, Object> map) {

    }

    @Override
    public void publishAuthenticationFailure(HttpServletRequest httpServletRequest,
                                             AuthenticationContext authenticationContext, Map<String, Object> map) {

    }

    @Override
    public void publishSessionCreation(HttpServletRequest httpServletRequest,
                                       AuthenticationContext authenticationContext, SessionContext sessionContext,
                                       Map<String, Object> map) {

    }

    @Override
    public void publishSessionUpdate(HttpServletRequest httpServletRequest, AuthenticationContext authenticationContext,
                                     SessionContext sessionContext, Map<String, Object> map) {

    }

    @Override
    public void publishSessionTermination(HttpServletRequest httpServletRequest,
                                          AuthenticationContext authenticationContext, SessionContext sessionContext,
                                          Map<String, Object> map) {

    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {
        HttpServletRequest request = (HttpServletRequest) event.getEventProperties().get("request");
        AuthenticationContext context = (AuthenticationContext) event.getEventProperties().get("context");
        Map<String, Object> unmodifiableParamMap = (Map<String, Object>) event.getEventProperties().get("params");
        String eventName = event.getEventName();

        if ("AUTHENTICATION_STEP_FAILURE".equalsIgnoreCase(eventName)) {
            publishAuthenticationStepFailure(request, context, unmodifiableParamMap);
        }
    }

}
