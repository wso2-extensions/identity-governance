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
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.util.CaptchaResponseWrapper;
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
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.ReCaptchaConnectorPropertySuffixes;

/**
 * reCaptcha login identity governance connector.
 */
public class SSOLoginReCaptchaConnector extends AbstractReCaptchaConnector implements IdentityGovernanceConnector {

    private static final Log log = LogFactory.getLog(SSOLoginReCaptchaConnector.class);

    private final String CONNECTOR_NAME = "sso.login";

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
        Map<String, String> connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE,
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE,
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS},
                    tenantDomain);
        } catch (Exception e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        String securedPaths = connectorConfigs.get(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                .SECURED_DESTINATIONS);
        if (!StringUtils.isBlank(securedPaths)) {
            String currentPath = ((HttpServletRequest) servletRequest).getRequestURI();
            if (StringUtils.isBlank(currentPath)) {
                return false;
            }
            if (!CaptchaUtil.isPathAvailable(currentPath, securedPaths)) {
                return false;
            }
        }

        if (!"TRUE".equalsIgnoreCase(connectorConfigs.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.ENABLE))) {
            return false;
        }

        String connectorIdentifierAttribute = connectorConfigs.get(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                .CONNECTOR_IDENTIFIER_ATTRIBUTE);
        if (StringUtils.isBlank(connectorIdentifierAttribute)) {
            return false;
        }

        String[] connectorIdentifierAttributes = connectorIdentifierAttribute.split(",");
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

        Map<String, String> connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM,
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL},
                    tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new CaptchaServerException("Unable to retrieve connector configs.", e);
        }

        String verificationClaim = connectorConfigs.get(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                .RECAPTCHA_VERIFICATION_CLAIM);
        String maxAttemptsStr = connectorConfigs.get(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS);

        if (StringUtils.isBlank(verificationClaim) || StringUtils.isBlank(maxAttemptsStr) ||
                !NumberUtils.isNumber(maxAttemptsStr)) {
            throw new CaptchaServerException("Invalid reCaptcha configuration.");
        }

        int maxAttempts = Integer.parseInt(maxAttemptsStr);

        RealmService realmService = CaptchaDataHolder.getInstance().getRealmService();
        int tenantId;
        try {
            tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
        } catch (UserStoreException e) {
            //Tenant is already validated in the canHandle section.
            throw new CaptchaServerException("Failed to retrieve tenant id from tenant domain : " + tenantDomain, e);
        }

        if (MultitenantConstants.INVALID_TENANT_ID == tenantId) {
            throw new CaptchaServerException("Invalid tenant domain : " + tenantDomain);
        }

        UserRealm userRealm;
        try {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        } catch (UserStoreException e) {
            throw new CaptchaServerException("Failed to retrieve user realm from tenant id : " + tenantId, e);
        }

        UserStoreManager userStoreManager;
        try {
            userStoreManager = userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw new CaptchaServerException("Failed to retrieve user store manager.", e);
        }

        Map<String, String> claimValues;
        try {
            claimValues = userStoreManager.getUserClaimValues(MultitenantUtils.getTenantAwareUsername(userName),
                    new String[]{verificationClaim}, UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while retrieving user claims.", e);
            }
            // Invalid user
            return preValidationResponse;
        }

        int currentAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(verificationClaim))) {
            currentAttempts = Integer.parseInt(claimValues.get(verificationClaim));
        }

        if (currentAttempts > maxAttempts) {
            preValidationResponse.setCaptchaValidationRequired(true);
            preValidationResponse.setMaxFailedLimitReached(true);
            preValidationResponse.setPostValidationRequired(true);
            if (!StringUtils.isBlank(connectorConfigs.get(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                    .ON_FAIL_REDIRECT_URL))) {
                preValidationResponse.setOnCaptchaFailRedirectUrls(Arrays.asList(connectorConfigs.get(CONNECTOR_NAME +
                        ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL).split(",")));
                // Add parameters which need to send back in case of failure.
                Map<String, String> params = new HashMap<>();
                params.put("reCaptcha", "true");
                params.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
                params.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
                params.put("authFailure", "true");
                params.put("authFailureMsg", "login.fail.message");
                preValidationResponse.setCaptchaAttributes(params);
            }
        } else if ((currentAttempts + 1) == maxAttempts) {
            preValidationResponse.setMaxFailedLimitReached(true);
            preValidationResponse.setPostValidationRequired(true);
        }

        return preValidationResponse;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        CaptchaPostValidationResponse validationResponse = new CaptchaPostValidationResponse();
        String redirectURL = ((CaptchaResponseWrapper) servletResponse).getRedirectURL();
        if (redirectURL != null && redirectURL.contains("authFailure=true")) {
            validationResponse.setSuccessfulAttempt(false);
            validationResponse.setEnableCaptchaResponsePath(true);
            Map<String, String> params = new HashMap<>();
            params.put("reCaptcha", "true");
            params.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            params.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
            validationResponse.setCaptchaAttributes(params);
            return validationResponse;
        }

        //Account lock situation also considered as successful, it will redirect to a error page
        validationResponse.setSuccessfulAttempt(true);
        validationResponse.setEnableCaptchaResponsePath(false);
        return validationResponse;
    }

    @Override
    public String getName() {
        return CONNECTOR_NAME;
    }

    @Override
    public String getFriendlyName() {

        String friendlyName = "reCaptcha for SSO Login";
        if (!CaptchaDataHolder.getInstance().isReCaptchaEnabled()) {
            friendlyName += " (reCaptcha is not enabled)";
        }
        return friendlyName;
    }

    @Override
    public String[] getPropertyNames() {

        return new String[]{
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.USER_IDENTIFIER_ATTRIBUTE,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL
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
                ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE,
                    "username,password");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                    "/samlsso,/oauth2,/commonauth");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.USER_IDENTIFIER_ATTRIBUTE))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.USER_IDENTIFIER_ATTRIBUTE,
                    "username");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM,
                    "http://wso2.org/claims/identity/failedLoginAttempts");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS, "3");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL))) {
            defaultProperties.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL,
                    "/authenticationendpoint/retry.do");
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
