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

package org.wso2.carbon.identity.captcha.util;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.ReCaptchaConnectorPropertySuffixes;

/**
 * Captcha util functions.
 */
public class CaptchaUtil {

    private static final Log log = LogFactory.getLog(CaptchaUtil.class);

    public static void buildReCaptchaFilterProperties() {

        Path path = Paths.get(getCarbonHomeDirectory().toString(), "repository",
                "conf", "identity", CaptchaConstants.CAPTCHA_CONFIG_FILE_NAME);

        if (Files.exists(path)) {
            Properties properties = new Properties();
            try (Reader in = new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)) {
                properties.load(in);
            } catch (IOException e) {
                throw new RuntimeException("Error while loading '" + CaptchaConstants
                        .CAPTCHA_CONFIG_FILE_NAME + "' configuration file", e);
            }

            boolean reCaptchaEnabled = Boolean.valueOf(properties.getProperty(CaptchaConstants
                    .RE_CAPTCHA_ENABLED));

            if (reCaptchaEnabled) {
                CaptchaDataHolder.getInstance().setReCaptchaEnabled(true);
                setReCaptchaConfigs(properties);
                //setSSOLoginConnectorConfigs(properties);
                //setPathBasedConnectorConfigs(properties);
            } else {
                CaptchaDataHolder.getInstance().setReCaptchaEnabled(false);

            }
        }

    }

    public static Path getCarbonHomeDirectory() {

        return Paths.get(System.getProperty(CaptchaConstants.CARBON_HOME));
    }

    public static boolean isPathAvailable(String currentPath, String securedPaths) {

        if (!StringUtils.isBlank(securedPaths)) {
            String[] paths = securedPaths.split(",");
            for (String path : paths) {
                if (currentPath.equals(path)) {
                    return true;
                }
            }
        }
        return false;
    }

    public static String getUpdatedUrl(String url, Map<String, String> attributes) {

        try {
            URIBuilder uriBuilder = new URIBuilder(url);
            for (Map.Entry<String, String> entry : attributes.entrySet()) {
                uriBuilder.addParameter(entry.getKey(), entry.getValue());
            }
            return uriBuilder.build().toString();
        } catch (URISyntaxException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while building URL.", e);
            }
            return url;
        }
    }

    public static String getOnFailRedirectUrl(String referrerUrl, List<String> onFailRedirectUrls,
                                              Map<String, String> attributes) {

        if (StringUtils.isBlank(referrerUrl) || onFailRedirectUrls.isEmpty()) {
            return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
        }

        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder(referrerUrl);
        } catch (URISyntaxException e) {
            return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
        }

        for (String url : onFailRedirectUrls) {
            if (!StringUtils.isBlank(url) && url.equalsIgnoreCase(uriBuilder.getPath())) {
                for (NameValuePair pair : uriBuilder.getQueryParams()) {
                    attributes.put(pair.getName(), pair.getValue());
                }
                return getUpdatedUrl(url, attributes);
            }
        }

        return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
    }

    public static String getErrorPage(String status, String statusMsg) {

        try {
            URIBuilder uriBuilder = new URIBuilder(CaptchaConstants.ERROR_PAGE);
            uriBuilder.addParameter("status", status);
            uriBuilder.addParameter("statusMsg", statusMsg);
            return uriBuilder.build().toString();
        } catch (URISyntaxException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while building URL.", e);
            }
            return CaptchaConstants.ERROR_PAGE;
        }
    }

    public static Map<String, String> getClaimValues(User user, int tenantId,
                                                     String[] claimUris) throws CaptchaServerException {

        String username = user.getUserName();
        if (!StringUtils.isBlank(user.getUserStoreDomain()) && !"PRIMARY".equals(user.getUserStoreDomain())) {
            username = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        }

        RealmService realmService = CaptchaDataHolder.getInstance().getRealmService();
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

        Map<String, String> claimValues = null;
        try {
            claimValues = userStoreManager.getUserClaimValues(username, claimUris, UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while retrieving user claims.", e);
            }
        }

        return claimValues;
    }

    public static boolean isValidCaptcha(String reCaptchaResponse) throws CaptchaException {

        HttpClient httpclient = HttpClients.createDefault();
        HttpPost httppost = new HttpPost(CaptchaDataHolder.getInstance().getReCaptchaVerifyUrl());

        List<BasicNameValuePair> params = Arrays.asList(new BasicNameValuePair("secret", CaptchaDataHolder
                .getInstance().getReCaptchaSecretKey()), new BasicNameValuePair("response", reCaptchaResponse));
        httppost.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

        HttpResponse response;
        try {
            response = httpclient.execute(httppost);
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to get the verification response.", e);
        }

        HttpEntity entity = response.getEntity();
        if (entity == null) {
            throw new CaptchaServerException("reCaptcha verification response is not received.");
        }

        try {
            try (InputStream in = entity.getContent()) {
                JsonObject verificationResponse = new JsonParser().parse(IOUtils.toString(in)).getAsJsonObject();
                if (verificationResponse == null || verificationResponse.get("success") == null ||
                        !verificationResponse.get("success").getAsBoolean()) {
                    throw new CaptchaClientException("reCaptcha verification failed. Please try again.");
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        }

        return true;
    }

    public static boolean isMaximumFailedLoginAttemptsReached(String usernameWithDomain, String tenantDomain) throws
            CaptchaException {

        String connectorName = "sso.login.recaptcha";
        String reCaptchaVerificationClaim = "http://wso2.org/claims/identity/failedLoginAttempts";
        Property[] connectorConfigs;
        try {
            connectorConfigs = CaptchaDataHolder.getInstance().getIdentityGovernanceService()
                    .getConfiguration(new String[]{connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE,
                            connectorName + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS}, tenantDomain);
        } catch (Exception e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        if (connectorConfigs == null) {
            return false;
        }

        String maxAttemptsStr = null;
        for (Property property : connectorConfigs) {
            if ((connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE).equals(property.getName())
                    && !Boolean.valueOf(property.getValue())) {
                return false;
            } else if ((connectorName + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS).equals(property.getName())) {
                maxAttemptsStr = property.getValue();
            }
        }

        if (StringUtils.isBlank(maxAttemptsStr) || !NumberUtils.isNumber(maxAttemptsStr)) {
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
            claimValues = userStoreManager.getUserClaimValues(MultitenantUtils
                    .getTenantAwareUsername(usernameWithDomain),
                    new String[]{reCaptchaVerificationClaim}, UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while retrieving user claims.", e);
            }
            // Invalid user
            return false;
        }

        int currentAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(reCaptchaVerificationClaim))) {
            currentAttempts = Integer.parseInt(claimValues.get(reCaptchaVerificationClaim));
        }

        return currentAttempts >= maxAttempts;
    }

    private static void setReCaptchaConfigs(Properties properties) {

        String reCaptchaAPIUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_API_URL);
        if (StringUtils.isBlank(reCaptchaAPIUrl)) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_API_URL));
        }
        CaptchaDataHolder.getInstance().setReCaptchaAPIUrl(reCaptchaAPIUrl);

        String reCaptchaVerifyUrl = properties.getProperty(CaptchaConstants.RE_CAPTCHA_VERIFY_URL);
        if (StringUtils.isBlank(reCaptchaVerifyUrl)) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_VERIFY_URL));
        }
        CaptchaDataHolder.getInstance().setReCaptchaVerifyUrl(reCaptchaVerifyUrl);

        String reCaptchaSiteKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SITE_KEY);
        if (StringUtils.isBlank(reCaptchaSiteKey)) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_SITE_KEY));
        }
        CaptchaDataHolder.getInstance().setReCaptchaSiteKey(reCaptchaSiteKey);

        String reCaptchaSecretKey = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SECRET_KEY);
        if (StringUtils.isBlank(reCaptchaSecretKey)) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_SECRET_KEY));
        }
        CaptchaDataHolder.getInstance().setReCaptchaSecretKey(reCaptchaSecretKey);
    }

    public static void setSSOLoginConnectorConfigs(Properties properties) {

        Map<String, String> connectorPropertyMap = new HashMap<>();

        final String connectorName = "sso.login";
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE, properties
                .getProperty(connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes
                        .CONNECTOR_IDENTIFIER_ATTRIBUTE));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.USER_IDENTIFIER_ATTRIBUTE,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes
                        .USER_IDENTIFIER_ATTRIBUTE));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes
                        .RECAPTCHA_VERIFICATION_CLAIM));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS));

        CaptchaDataHolder.getInstance().setSSOLoginReCaptchaConnectorPropertyMap(connectorPropertyMap);
    }

    public static void setPathBasedConnectorConfigs(Properties properties) {

        Map<String, String> connectorPropertyMap = new HashMap<>();

        final String connectorName = "path.based";
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE, properties
                .getProperty(connectorName + ReCaptchaConnectorPropertySuffixes.ENABLE));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.SECURED_PAGES,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes.SECURED_PAGES));
        connectorPropertyMap.put(connectorName + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                properties.getProperty(connectorName + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS));

        CaptchaDataHolder.getInstance().setPathBasedReCaptchaConnectorPropertyMap(connectorPropertyMap);
    }

    private static String getValidationErrorMessage(String property) {

        return "Invalid value for " + property + " in the " + CaptchaConstants
                .CAPTCHA_CONFIG_FILE_NAME + " file.";
    }

}
