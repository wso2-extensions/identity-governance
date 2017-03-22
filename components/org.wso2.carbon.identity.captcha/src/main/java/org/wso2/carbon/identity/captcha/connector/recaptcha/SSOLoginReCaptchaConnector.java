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

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaHttpServletResponseWrapper;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.ReCaptchaConnectorPropertySuffixes;

/**
 * reCaptcha login identity governance connector.
 */
public class SSOLoginReCaptchaConnector extends AbstractReCaptchaConnector implements IdentityGovernanceConnector {

    private static final Log log = LogFactory.getLog(SSOLoginReCaptchaConnector.class);

    private static final String CONNECTOR_NAME = "sso.login.recaptcha";

    private static final String CONNECTOR_IDENTIFIER_ATTRIBUTE = "username,password";

    private static final String RECAPTCHA_VERIFICATION_CLAIM = "http://wso2.org/claims/identity/failedLoginAttempts";

    private static final String SECURED_DESTINATIONS = "/commonauth,/samlsso,/oauth2";

    private static final String ON_FAIL_REDIRECT_URL = "/authenticationendpoint/login.do";

    private IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {
        return 20;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        if (!CaptchaDataHolder.getInstance().isReCaptchaEnabled()) {
            return false;
        }

        String userName = servletRequest.getParameter("username");
        if (StringUtils.isBlank(userName)) {
            return false;
        }

        String tenantDomain = MultitenantUtils.getTenantDomain(userName);
        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE},
                    tenantDomain);
        } catch (Exception e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        if (connectorConfigs == null || connectorConfigs.length == 0
                || !Boolean.valueOf(connectorConfigs[0].getValue())) {
            return false;
        }

        String currentPath = ((HttpServletRequest) servletRequest).getRequestURI();
        if (StringUtils.isBlank(currentPath) || !CaptchaUtil.isPathAvailable(currentPath, SECURED_DESTINATIONS)) {
            return false;
        }

        String[] connectorIdentifierAttributes = CONNECTOR_IDENTIFIER_ATTRIBUTE.split(",");
        for (String attribute : connectorIdentifierAttributes) {
            if (servletRequest.getParameter(attribute) == null) {
                return false;
            }
        }

        return true;
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        String userName = servletRequest.getParameter("username");
        String tenantDomain = MultitenantUtils.getTenantDomain(userName);

        if (CaptchaUtil.isMaximumFailedLoginAttemptsReached(MultitenantUtils.getTenantAwareUsername(userName),
                tenantDomain)) {
            preValidationResponse.setCaptchaValidationRequired(true);
            preValidationResponse.setMaxFailedLimitReached(true);
            // Add parameters which need to send back in case of failure.
            preValidationResponse.setOnCaptchaFailRedirectUrls(Collections.singletonList(ON_FAIL_REDIRECT_URL));
            Map<String, String> params = new HashMap<>();
            params.put("reCaptcha", "true");
            params.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            params.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
            params.put("authFailure", "true");
            params.put("authFailureMsg", "recaptcha.fail.message");
            preValidationResponse.setCaptchaAttributes(params);
        }
        // Post validate all requests
        preValidationResponse.setMaxFailedLimitReached(true);
        preValidationResponse.setPostValidationRequired(true);

        return preValidationResponse;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        if (!StringUtils.isBlank(CaptchaConstants.getEnableSecurityMechanism())) {
            CaptchaConstants.removeEnabledSecurityMechanism();
            CaptchaPostValidationResponse validationResponse = new CaptchaPostValidationResponse();
            validationResponse.setSuccessfulAttempt(false);
            validationResponse.setEnableCaptchaResponsePath(true);
            Map<String, String> params = new HashMap<>();
            params.put("reCaptcha", "true");
            params.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            params.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
            validationResponse.setCaptchaAttributes(params);
            return validationResponse;
        }
        return null;
    }

    @Override
    public String getName() {
        return CONNECTOR_NAME;
    }

    @Override
    public String getFriendlyName() {

        return "reCaptcha for SSO Login";
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE, "Enable");
        nameMapping.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                "Max failed attempts");
        return nameMapping;
    }

    @Override
    public String[] getPropertyNames() {

        return new String[]{
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS
        };
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = CaptchaDataHolder.getInstance()
                .getSSOLoginReCaptchaConnectorPropertyMap();
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.ENABLE))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE, "false");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS, "3");
        }

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        return null;
    }
}
