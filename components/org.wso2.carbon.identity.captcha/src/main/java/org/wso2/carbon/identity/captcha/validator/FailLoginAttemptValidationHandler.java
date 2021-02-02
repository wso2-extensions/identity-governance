/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.captcha.validator;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.Map;

import static org.wso2.carbon.identity.captcha.util.CaptchaUtil.isValidAuthenticator;

/**
 * Validate no of failed login attempts against the no configured and engage captcha
 */
public class FailLoginAttemptValidationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(FailLoginAttemptValidationHandler.class);

    @Override
    public String getName() {

        return "failLoginAttemptValidator";
    }

    @Override
    public boolean canHandle(MessageContext messageContext) throws IdentityRuntimeException {

        return super.canHandle(messageContext) && isFailLoginAttemptValidatorEnabled();
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        if (canHandleEvent(event)) {

            AuthenticationContext context = (AuthenticationContext) event.getEventProperties().get
                    (IdentityEventConstants.EventProperty.CONTEXT);
            Map<String, Object> unmodifiableParamMap = (Map<String, Object>) event.getEventProperties()
                    .get(IdentityEventConstants.EventProperty.PARAMS);
            String eventName = event.getEventName();

            if (log.isDebugEnabled()) {
                log.debug(this.getName() + " received event : " + eventName);
            }

            if (IdentityEventConstants.EventName.AUTHENTICATION_STEP_FAILURE.name().equals(eventName)) {

                handleAuthenticationStepFailure(context, unmodifiableParamMap);
            }
        }
    }

    /**
     * Handles 'AUTHENTICATION_STEP_FAILURE' event and validates no of failed login attempts of the user and
     * enables captcha.
     *
     * @param authenticationContext authentication context {@link AuthenticationContext}
     * @param map                   event property parameter map
     */
    protected void handleAuthenticationStepFailure(AuthenticationContext authenticationContext, Map<String, Object>
            map) {

        String currentAuthenticator = authenticationContext.getCurrentAuthenticator();
        if (StringUtils.isBlank(currentAuthenticator) && MapUtils.isNotEmpty(map)) {
            currentAuthenticator = (String) map.get(FrameworkConstants.AUTHENTICATOR);
        }
        if (("BasicAuthenticator".equals(currentAuthenticator) ||
                isValidAuthenticator(authenticationContext, currentAuthenticator)) &&
                MapUtils.isNotEmpty(map) && map.get(FrameworkConstants.AnalyticsAttributes.USER) != null) {

            if (map.get(FrameworkConstants.AnalyticsAttributes.USER) instanceof User) {
                User failedUser = (User) map.get(FrameworkConstants.AnalyticsAttributes.USER);
                String username = getDomainQualifiedUsername(failedUser);

                if (log.isDebugEnabled()) {
                    log.debug("Evaluating failed login attempts for user: " + username + " authenticated from: " +
                            currentAuthenticator);
                }

                try {
                    if (CaptchaUtil.isMaximumFailedLoginAttemptsReached(username, failedUser.getTenantDomain())) {
                        CaptchaConstants.setEnableSecurityMechanism("enable");
                        if (log.isDebugEnabled()) {
                            log.debug("User: " + username + " reached maximum no of failed login attempts. Enabling " +
                                    "captcha for user.");
                        }
                    }
                } catch (CaptchaException e) {
                    log.error("Failed to evaluate max failed login attempts of the user: " + username, e);
                }
            }
        }
    }

    private boolean isFailLoginAttemptValidatorEnabled() throws IdentityRuntimeException {

        if (this.configs.getModuleProperties() != null) {
            String handlerEnabled = this.configs.getModuleProperties().getProperty(CaptchaConstants
                    .FAIL_LOGIN_ATTEMPT_VALIDATOR_ENABLED);
            return Boolean.parseBoolean(handlerEnabled);
        }

        return false;
    }

    private boolean canHandleEvent(Event event) {

        return event != null && event.getEventProperties().get(IdentityEventConstants.EventProperty.CONTEXT) instanceof
                AuthenticationContext && event.getEventProperties().get(IdentityEventConstants.EventProperty.PARAMS)
                instanceof Map;
    }

    private String getDomainQualifiedUsername(User user) {

        String username = user.getUserName();
        if (!StringUtils.isBlank(user.getUserStoreDomain()) &&
                !IdentityUtil.getPrimaryDomainName().equals(user.getUserStoreDomain())) {
            username = UserCoreUtil.addDomainToName(username, user.getUserStoreDomain());
        }

        return username;
    }
}
