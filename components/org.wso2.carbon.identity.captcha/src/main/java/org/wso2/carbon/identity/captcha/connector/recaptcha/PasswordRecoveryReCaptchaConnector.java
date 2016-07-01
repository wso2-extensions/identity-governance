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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaHttpServletRequestWrapper;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Password Recovery reCaptcha Connector.
 */
public class PasswordRecoveryReCaptchaConnector extends AbstractReCaptchaConnector implements IdentityGovernanceConnector {

    private static final Log log = LogFactory.getLog(PasswordRecoveryReCaptchaConnector.class);

    private static final String CONNECTOR_NAME = "password.recovery.recaptcha";

    private static final String FAIL_ATTEMPTS_CLAIM = "http://wso2.org/claims/identity/failedPasswordRecoveryAttempts";

    private static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";

    private static final String ACCOUNT_RECOVERY_INITIATE_URL = "/account-recovery/questions/initiate";

    private static final String ACCOUNT_RECOVERY_VERIFY_URL = "/account-recovery/questions/verify";

    private static final String ACCOUNT_RECOVERY_INITIATE_ALL_URL = "/account-recovery/questions/initiate-all";

    private static final String ACCOUNT_RECOVERY_VERIFY_ALL_URL = "/account-recovery/questions/verify-all";

    private IdentityGovernanceService identityGovernanceService;

    @Override
    public void init(IdentityGovernanceService identityGovernanceService) {

        this.identityGovernanceService = identityGovernanceService;
    }

    @Override
    public int getPriority() {
        return 10;
    }

    @Override
    public boolean canHandle(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        if (!CaptchaDataHolder.getInstance().isReCaptchaEnabled()) {
            return false;
        }

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        return !StringUtils.isBlank(path) &&
                (CaptchaUtil.isPathAvailable(path, ACCOUNT_RECOVERY_INITIATE_URL) ||
                        CaptchaUtil.isPathAvailable(path, ACCOUNT_RECOVERY_VERIFY_URL) ||
                        CaptchaUtil.isPathAvailable(path, ACCOUNT_RECOVERY_INITIATE_ALL_URL) ||
                        CaptchaUtil.isPathAvailable(path, ACCOUNT_RECOVERY_VERIFY_ALL_URL));
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        HttpServletRequest httpServletRequestWrapper;
        try {
            httpServletRequestWrapper = new CaptchaHttpServletRequestWrapper((HttpServletRequest) servletRequest);
            preValidationResponse.setWrappedHttpServletRequest(httpServletRequestWrapper);
        } catch (IOException e) {
            log.error("Error occurred while wrapping ServletRequest.", e);
            return preValidationResponse;
        }

        String path = httpServletRequestWrapper.getRequestURI();

        JsonObject requestObject;
        try {
            try (InputStream in = httpServletRequestWrapper.getInputStream()) {
                requestObject = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
            }
        } catch (IOException e) {
            return preValidationResponse;
        }

        String tenantDomain = null;
        String userStoreDomain = null;
        String userName = null;
        boolean initializationFlow = false;
        if (CaptchaUtil.isPathAvailable(path, ACCOUNT_RECOVERY_INITIATE_URL) || CaptchaUtil.isPathAvailable(path,
                ACCOUNT_RECOVERY_INITIATE_ALL_URL)) {
            tenantDomain = requestObject.get("tenantDomain").getAsString();
            userStoreDomain = requestObject.get("userStoreDomain").getAsString();
            userName = requestObject.get("userName").getAsString();
            initializationFlow = true;
        } else {
            JsonObject userJson = requestObject.getAsJsonObject("user");
            if (userJson != null) {
                tenantDomain = userJson.get("tenantDomain").getAsString();
                userStoreDomain = userJson.get("userStoreDomain").getAsString();
                userName = userJson.get("userName").getAsString();
            }
        }

        if (StringUtils.isBlank(userName)) {
            // Invalid Request
            return preValidationResponse;
        }

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                            CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                            CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS},
                    tenantDomain);
        } catch (IdentityGovernanceException e) {
            throw new CaptchaServerException("Unable to retrieve connector configs.", e);
        }

        String connectorEnabled = null;
        String maxAttemptsStr = null;
        for (Property connectorConfig : connectorConfigs) {
            if ((CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE)
                    .equals(connectorConfig.getName())) {
                connectorEnabled = connectorConfig.getValue();
            } else if ((CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS)
                    .equals(connectorConfig.getName())) {
                maxAttemptsStr = connectorConfig.getValue();
            }
        }

        if (!Boolean.parseBoolean(connectorEnabled)) {
            return preValidationResponse;
        }

        if (StringUtils.isBlank(maxAttemptsStr) || !NumberUtils.isNumber(maxAttemptsStr)) {
            log.warn("Invalid configuration found in the PasswordRecoveryReCaptchaConnector for the tenant - " +
                    tenantDomain);
            return preValidationResponse;
        }
        int maxFailedAttempts = Integer.parseInt(maxAttemptsStr);

        if (!StringUtils.isBlank(userStoreDomain) && !"PRIMARY".equals(userStoreDomain)) {
            userName = IdentityUtil.addDomainToName(userName, userStoreDomain);
        }

        int tenantId;
        try {
            tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        } catch (Exception e) {
            //Invalid tenant
            return preValidationResponse;
        }

        Map<String, String> claimValues = CaptchaUtil.getClaimValues(userName, tenantDomain, tenantId,
                new String[]{FAIL_ATTEMPTS_CLAIM, ACCOUNT_LOCKED_CLAIM});

        if (claimValues == null || claimValues.isEmpty()) {
            // Invalid user
            return preValidationResponse;
        }

        if (Boolean.parseBoolean(claimValues.get(ACCOUNT_LOCKED_CLAIM))) {
            return preValidationResponse;
        }

        int currentFailedAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(FAIL_ATTEMPTS_CLAIM))) {
            currentFailedAttempts = Integer.parseInt(claimValues.get(FAIL_ATTEMPTS_CLAIM));
        }

        // TODO This need to be fixed before 5.3.0 Alpha release. Since response is committed from the CXF layer we
        // can't set headers from the post validation section. Need to use javax.ws.rs.container
        // .ContainerResponseFilter to handle this properly,
        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        if (currentFailedAttempts >= maxFailedAttempts) {
            if (initializationFlow) {
                httpServletResponse.setHeader("reCaptcha", "true");
            } else {
                httpServletResponse.setHeader("reCaptcha", "conditional");
                preValidationResponse.setCaptchaValidationRequired(true);
                preValidationResponse.setMaxFailedLimitReached(true);
            }
            httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
        } else if ((currentFailedAttempts + 1) == maxFailedAttempts) {
            httpServletResponse.setHeader("reCaptcha", "conditional");
            httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
        }

        return preValidationResponse;
    }

    @Override
    public boolean verifyCaptcha(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        String reCaptchaResponse = ((HttpServletRequest) servletRequest).getHeader("g-recaptcha-response");
        if (StringUtils.isBlank(reCaptchaResponse)) {
            throw new CaptchaClientException("reCaptcha response is not available in the request.");
        }

        return CaptchaUtil.isValidCaptcha(reCaptchaResponse);
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        //TODO This will not executed since post validation will fixed in 5.3.0 Alpha release.
        return null;
    }

    @Override
    public String getName() {

        return CONNECTOR_NAME;
    }

    @Override
    public String getFriendlyName() {

        return "reCaptcha for Password Recovery";
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                "Enable");
        nameMapping.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                "Max failed attempts");
        return nameMapping;
    }

    @Override
    public String[] getPropertyNames() {

        return new String[]{
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS
        };
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = CaptchaDataHolder.getInstance()
                .getPasswordRecoveryReCaptchaConnectorPropertyMap();
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE, "false");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                    "3");
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
