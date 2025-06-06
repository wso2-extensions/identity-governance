/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.captcha.provider.mgt.service.impl;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.provider.mgt.provider.AbstractCaptchaProvider;
import org.wso2.carbon.identity.captcha.provider.mgt.service.CaptchaConfigService;
import org.wso2.carbon.identity.captcha.provider.mgt.util.CaptchaProviderConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.configuration.mgt.core.ConfigurationManager;
import org.wso2.carbon.identity.configuration.mgt.core.model.Attribute;
import org.wso2.carbon.identity.configuration.mgt.core.model.Resource;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class CaptchaConfigServiceImpl implements CaptchaConfigService {

    @Override
    public Properties getActiveCaptchaProviderConfig() throws CaptchaServerException {

        CaptchaDataHolder captchaDataHolder = CaptchaDataHolder.getInstance();
        Properties captchaProviderConfigs = captchaDataHolder
                .getCaptchaConfigsForTenant(getTenantId());
        if (captchaProviderConfigs != null && !captchaProviderConfigs.isEmpty()) {
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_API_URL,
                    getCaptchaProvider().getCaptchaApiUrl());
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL,
                    getCaptchaProvider().getCaptchaVerifyUrl());
        } else {
            captchaProviderConfigs = new Properties();
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED,
                    Boolean.toString(captchaDataHolder.isReCaptchaEnabled()));
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_TYPE,
                    captchaDataHolder.getReCaptchaType() != null ? captchaDataHolder.getReCaptchaType() : "");
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY,
                    captchaDataHolder.getReCaptchaSiteKey() != null ? captchaDataHolder.getReCaptchaSiteKey() : "");
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY,
                    captchaDataHolder.getReCaptchaSecretKey() != null ? captchaDataHolder.getReCaptchaSecretKey() : "");
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_API_URL,
                    captchaDataHolder.getReCaptchaAPIUrl() != null ? captchaDataHolder.getReCaptchaAPIUrl() : "");
            captchaProviderConfigs.setProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL,
                    captchaDataHolder.getReCaptchaVerifyUrl() != null ? captchaDataHolder.getReCaptchaVerifyUrl() : "");
        }
        return captchaProviderConfigs;
    }

    @Override
    public boolean isCaptchaEnabled() {

        int tenantId = getTenantId();
        boolean isCaptchaConfigured = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(tenantId) != null
                && !CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(tenantId).isEmpty();

        return CaptchaDataHolder.getInstance().isReCaptchaEnabled() || isCaptchaConfigured ||
                CaptchaDataHolder.getInstance().isForcefullyEnabledRecaptchaForAllTenants();
    }

    @Override
    public AbstractCaptchaProvider getCaptchaProvider() throws CaptchaServerException {

        Properties captchaProviderConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        AbstractCaptchaProvider captchaProvider = null;
        if (captchaProviderConfigs != null && !captchaProviderConfigs.isEmpty()) {
            captchaProvider = CaptchaDataHolder.getInstance()
                    .getCaptchaProviderForCaptchaType(
                            captchaProviderConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_TYPE));
        }

        if (captchaProvider == null && CaptchaDataHolder.getInstance().getReCaptchaType() != null &&
                !CaptchaDataHolder.getInstance().getReCaptchaType().isEmpty()) {
            captchaProvider = CaptchaDataHolder.getInstance()
                    .getCaptchaProviderForCaptchaType(CaptchaDataHolder.getInstance().getReCaptchaType());
        }

        if (captchaProvider == null) {
            throw new CaptchaServerException("Captcha site secret is not configured.");
        }

        return captchaProvider;
    }

    @Override
    public String getCaptchaSiteKey() throws CaptchaServerException {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());
        String siteKey = "";

        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            siteKey = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY);
        }

        if (siteKey.isEmpty()) {
            siteKey = CaptchaDataHolder.getInstance().getReCaptchaSiteKey();
        }

        if (siteKey.isEmpty()) {
            throw new CaptchaServerException("Captcha site key is not configured.");
        }

        return siteKey;
    }

    @Override
    public String getCaptchaSiteSecret() throws CaptchaServerException {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        String siteSecret = "";
        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            siteSecret = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY);
        }

        if (siteSecret.isEmpty()) {
            siteSecret = CaptchaDataHolder.getInstance().getReCaptchaSecretKey();
        }

        if (siteSecret.isEmpty()) {
            throw new CaptchaServerException("Captcha site secret is not configured.");
        }

        return siteSecret;
    }

    @Override
    public String getCaptchaProjectID() throws CaptchaServerException {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        String projectID = "";
        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            projectID = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_PROJECT_ID);
        }

        if (projectID.isEmpty()) {
            projectID = CaptchaDataHolder.getInstance().getReCaptchaProjectID();
        }

        if (projectID.isEmpty()) {
            throw new CaptchaServerException("Captcha project ID is not configured.");
        }

        return projectID;
    }

    @Override
    public String getCaptchaApiKey() throws CaptchaServerException {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        String apiKey = "";
        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            apiKey = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_API_KEY);
        }

        if (apiKey.isEmpty()) {
            apiKey = CaptchaDataHolder.getInstance().getReCaptchaAPIKey();
        }

        if (apiKey.isEmpty()) {
            throw new CaptchaServerException("Captcha site secret is not configured.");
        }

        return apiKey;
    }

    @Override
    public String getCaptchaScoreThreshold() {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        String warnThreshold = "";
        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            warnThreshold = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_WARN_SCORE_THRESHOLD);
        }

        if (warnThreshold.isEmpty()) {
            warnThreshold = String.valueOf(CaptchaDataHolder.getInstance().getReCaptchaScoreThreshold());
        }

        return warnThreshold;
    }

    @Override
    public String getCaptchaWarnScoreThreshold() {

        Properties tenantCaptchaConfigs = CaptchaDataHolder.getInstance().getCaptchaConfigsForTenant(getTenantId());

        String scoreThreshold = "";
        if (tenantCaptchaConfigs != null && !tenantCaptchaConfigs.isEmpty()) {
            scoreThreshold = tenantCaptchaConfigs.getProperty(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD);
        }

        if (scoreThreshold.isEmpty()) {
            scoreThreshold = String.valueOf(CaptchaDataHolder.getInstance().getReCaptchaWarnScoreThreshold());
        }

        return scoreThreshold;
    }

    @Override
    public void loadCaptchaConfigs() throws CaptchaServerException {

        try {
            int tenantId = getTenantId();
            Properties captchaProviderConfig = new Properties();

            Resource captchaResource = getConfigurationManager().getResource(
                    CaptchaProviderConstants.CAPTCHA_RESOURCE_TYPE, CaptchaProviderConstants.CAPTCHA_RESOURCE_NAME);
            List<Attribute> captchaResourceAttributes = captchaResource.getAttributes();

            if (captchaResourceAttributes != null && !captchaResourceAttributes.isEmpty()) {
                captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED, "true");

                for (Attribute attribute : captchaResourceAttributes) {
                    String key = attribute.getKey();
                    String value = attribute.getValue();

                    switch (key) {
                        case CaptchaProviderConstants.CAPTCHA_PROVIDER_CONFIG:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_TYPE, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_SITEKEY_CONFIG:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_SITESECRET_CONFIG:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_SCORE_THRESHOLD_CONFIG:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_SCORE_WARN_THRESHOLD_CONFIG:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_WARN_SCORE_THRESHOLD, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_API_KEY:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_API_KEY, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_PROJECT_ID:
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_PROJECT_ID, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_REQUEST_WRAP_URLS:
                            CaptchaDataHolder.getInstance().setReCaptchaRequestWrapUrls(value);
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_REQUEST_WRAP_URLS, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_FAILED_REDIRECT_URLS:
                            CaptchaDataHolder.getInstance().setReCaptchaErrorRedirectUrls(value);
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_FAILED_REDIRECT_URLS, value);
                            break;
                        case CaptchaProviderConstants.CAPTCHA_BYPASSED_API_ENDPOINTS:
                            CaptchaDataHolder.getInstance()
                                    .setReCaptchaBypassedApiEndpoints(Arrays.asList(value.split(",")));
                            captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_BYPASSED_API_ENDPOINTS,
                                    value);
                            break;
                        default:
                            // Handle unknown config keys if needed
                            break;
                    }
                }

                captchaProviderConfig.setProperty(CaptchaConstants.RE_CAPTCHA_ENABLED, "true");

                // Store the config in CaptchaDataHolder
                CaptchaDataHolder.getInstance().addCaptchaConfigsForTenant(tenantId, captchaProviderConfig);
            }
        } catch (Exception e) {
            throw new CaptchaServerException("Error while setting captcha provider configurations.", e);
        }
    }

    @Override
    public void storeCaptchaProviders() throws CaptchaServerException {

        CaptchaDataHolder captchaDataHolder = CaptchaDataHolder.getInstance();

        for (Map.Entry<String, Class<? extends AbstractCaptchaProvider>> entry :
                CaptchaProviderConstants.CaptchaProviderRegistry.getProviderClasses().entrySet()) {
            String captchaType = entry.getKey();
            Class<? extends AbstractCaptchaProvider> providerClass = entry.getValue();

            try {
                AbstractCaptchaProvider providerInstance = providerClass.getDeclaredConstructor().newInstance();
                captchaDataHolder.setCaptchaProviderForCaptchaType(captchaType, providerInstance);
            } catch (Exception e) {
                throw new CaptchaServerException("Failed to initialize captcha provider for type: " + captchaType, e);
            }
        }
    }

    private ConfigurationManager getConfigurationManager() {

        return CaptchaDataHolder.getInstance().getConfigurationManager();
    }

    private static int getTenantId() {

        return PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
    }
}
