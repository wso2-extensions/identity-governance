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
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaHttpServletRequestWrapper;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.captcha.util.EnabledSecurityMechanism;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.handler.event.account.lock.exception.AccountLockServiceException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Password Recovery reCaptcha Connector.
 */
public class PasswordRecoveryReCaptchaConnector extends AbstractReCaptchaConnector {

    private static final Log log = LogFactory.getLog(PasswordRecoveryReCaptchaConnector.class);

    private static final String FAIL_ATTEMPTS_CLAIM = "http://wso2.org/claims/identity/failedPasswordRecoveryAttempts";

    private static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";

    private static final String ACCOUNT_SECURITY_QUESTION_URL = "/api/identity/recovery/v0.9/security-question";

    private static final String ACCOUNT_SECURITY_QUESTIONS_URL = "/api/identity/recovery/v0.9/security-questions";

    private static final String ACCOUNT_VALIDATE_ANSWER_URL = "/api/identity/recovery/v0.9/validate-answer";

    private static final String RECOVER_PASSWORD_URL = "/api/identity/recovery/v0.9/recover-password";

    private static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE = "Recovery.Question.Password" +
            ".ReCaptcha.Enable";
    private static final String RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS = "Recovery.Question" +
            ".Password.ReCaptcha.MaxFailedAttempts";

    private final String FORGOT_PASSWORD_RECAPTCHA_ENABLE = "Recovery.ReCaptcha.Password.Enable";

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

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        return !StringUtils.isBlank(path) &&
                (CaptchaUtil.isPathAvailable(path, ACCOUNT_SECURITY_QUESTION_URL) ||
                        CaptchaUtil.isPathAvailable(path, ACCOUNT_SECURITY_QUESTIONS_URL) ||
                        CaptchaUtil.isPathAvailable(path, ACCOUNT_VALIDATE_ANSWER_URL) ||
                        CaptchaUtil.isPathAvailable(path, RECOVER_PASSWORD_URL));
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest, ServletResponse servletResponse) throws CaptchaException {

        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();
        boolean forgotPasswordRecaptchaEnabled = CaptchaUtil.isRecaptchaEnabledForConnector(identityGovernanceService,
                servletRequest, FORGOT_PASSWORD_RECAPTCHA_ENABLE);
        String pathUrl = ((HttpServletRequest) servletRequest).getRequestURI();

        if ((CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants() ||
                forgotPasswordRecaptchaEnabled) &&
                (CaptchaUtil.isPathAvailable(pathUrl, ACCOUNT_SECURITY_QUESTION_URL) ||
                CaptchaUtil.isPathAvailable(pathUrl, ACCOUNT_SECURITY_QUESTIONS_URL) ||
                CaptchaUtil.isPathAvailable(pathUrl, RECOVER_PASSWORD_URL))) {
            preValidationResponse.setCaptchaValidationRequired(true);
        }

        // Handle recover with Email option.
        if (pathUrl.equals(RECOVER_PASSWORD_URL)) {
            return preValidationResponse;
        }

        // Handle recover with security questions option.
        HttpServletRequest httpServletRequestWrapper;
        try {
            httpServletRequestWrapper = new CaptchaHttpServletRequestWrapper((HttpServletRequest) servletRequest);
            preValidationResponse.setWrappedHttpServletRequest(httpServletRequestWrapper);
        } catch (IOException e) {
            log.error("Error occurred while wrapping ServletRequest.", e);
            return preValidationResponse;
        }

        String path = httpServletRequestWrapper.getRequestURI();

        User user = new User();
        boolean initializationFlow = false;
        if (CaptchaUtil.isPathAvailable(path, ACCOUNT_SECURITY_QUESTION_URL)
                || CaptchaUtil.isPathAvailable(path, ACCOUNT_SECURITY_QUESTIONS_URL)) {
            user.setUserName(servletRequest.getParameter("username"));
            if (StringUtils.isNotBlank(servletRequest.getParameter("realm"))) {
                user.setUserStoreDomain(servletRequest.getParameter("realm"));
            } else {
                user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            }
            user.setTenantDomain(servletRequest.getParameter("tenant-domain"));
            initializationFlow = true;
        } else {
            JsonObject requestObject;
            try {
                try (InputStream in = httpServletRequestWrapper.getInputStream()) {
                    requestObject = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                }
            } catch (IOException e) {
                return preValidationResponse;
            }
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            try {
                UserRecoveryData userRecoveryData = userRecoveryDataStore.load(requestObject.get("key").getAsString());
                if(userRecoveryData != null) {
                    user = userRecoveryData.getUser();
                }
            } catch (IdentityRecoveryException e) {
                return preValidationResponse;
            }
        }

        if (StringUtils.isBlank(user.getUserName())) {
            // Invalid Request
            return preValidationResponse;
        }

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        }

        Property[] connectorConfigs;
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                            RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE,
                            RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS},
                    user.getTenantDomain());
        } catch (IdentityGovernanceException e) {
            throw new CaptchaServerException("Unable to retrieve connector configs.", e);
        }

        String connectorEnabled = null;
        String maxAttemptsStr = null;
        for (Property connectorConfig : connectorConfigs) {
            if ((RECOVERY_QUESTION_PASSWORD_RECAPTCHA_ENABLE)
                    .equals(connectorConfig.getName())) {
                connectorEnabled = connectorConfig.getValue();
            } else if ((RECOVERY_QUESTION_PASSWORD_RECAPTCHA_MAX_FAILED_ATTEMPTS)
                    .equals(connectorConfig.getName())) {
                maxAttemptsStr = connectorConfig.getValue();
            }
        }

        if (!CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants() &&
                !Boolean.parseBoolean(connectorEnabled)) {
            return preValidationResponse;
        }

        if (StringUtils.isBlank(maxAttemptsStr) || !NumberUtils.isNumber(maxAttemptsStr)) {
            log.warn("Invalid configuration found in the PasswordRecoveryReCaptchaConnector for the tenant - " +
                    user.getTenantDomain());
            return preValidationResponse;
        }
        int maxFailedAttempts = Integer.parseInt(maxAttemptsStr);

        int tenantId;
        try {
            tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        } catch (Exception e) {
            //Invalid tenant
            return preValidationResponse;
        }

        try {
            if (CaptchaDataHolder.getInstance().getAccountLockService().isAccountLocked(user.getUserName(), user
                    .getTenantDomain(), user.getUserStoreDomain())) {
                return preValidationResponse;
            }
        } catch (AccountLockServiceException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while validating if account is locked for user: " + user.getUserName() + " of user " +
                        "store domain: " + user.getUserStoreDomain() + " and tenant domain: " +
                        user.getTenantDomain());
            }
            return preValidationResponse;
        }

        Map<String, String> claimValues = CaptchaUtil.getClaimValues(user, tenantId, new String[]{FAIL_ATTEMPTS_CLAIM});

        if (claimValues == null || claimValues.isEmpty()) {
            // Invalid user
            return preValidationResponse;
        }

        int currentFailedAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(FAIL_ATTEMPTS_CLAIM))) {
            currentFailedAttempts = Integer.parseInt(claimValues.get(FAIL_ATTEMPTS_CLAIM));
        }

        HttpServletResponse httpServletResponse = ((HttpServletResponse) servletResponse);
        if (currentFailedAttempts > maxFailedAttempts) {
            if (initializationFlow) {
                httpServletResponse.setHeader("reCaptcha", "true");
                httpServletResponse.setHeader("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
                httpServletResponse.setHeader("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
            } else {
                preValidationResponse.setCaptchaValidationRequired(true);
                preValidationResponse.setMaxFailedLimitReached(true);
                addPostValidationData(servletRequest);
            }
        } else if (currentFailedAttempts == maxFailedAttempts && !initializationFlow) {
            addPostValidationData(servletRequest);
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

        //This validation will happens through a CXF Filter
        return null;
    }

    private void addPostValidationData(ServletRequest servletRequest) {
        EnabledSecurityMechanism enabledSecurityMechanism = new EnabledSecurityMechanism();
        enabledSecurityMechanism.setMechanism("reCaptcha");
        Map<String, String> properties = new HashMap<>();
        properties.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
        properties.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
        enabledSecurityMechanism.setProperties(properties);
        ((HttpServletRequest) servletRequest).getSession().setAttribute("enabled-security-mechanism",
                enabledSecurityMechanism);
    }
}
