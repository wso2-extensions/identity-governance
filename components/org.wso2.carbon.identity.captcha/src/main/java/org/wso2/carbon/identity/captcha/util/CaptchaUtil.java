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
import org.apache.commons.collections.MapUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.ApplicationAuthenticator;
import org.wso2.carbon.identity.application.authentication.framework.config.ConfigurationFacade;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.config.model.SequenceConfig;
import org.wso2.carbon.identity.application.authentication.framework.config.model.StepConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.captcha.exception.CaptchaClientException;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;
import org.wso2.securevault.SecretResolver;
import org.wso2.securevault.SecretResolverFactory;
import org.wso2.securevault.commons.MiscellaneousUtil;

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
import javax.servlet.ServletRequest;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.BASIC_AUTH_MECHANISM;
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

            boolean reCaptchaEnabled = Boolean.parseBoolean(properties.getProperty(CaptchaConstants
                    .RE_CAPTCHA_ENABLED));

            String reCaptchaFailedRedirectUrls = properties.getProperty(CaptchaConstants.
                    RE_CAPTCHA_FAILED_REDIRECT_URLS);
            if (StringUtils.isNotBlank(reCaptchaFailedRedirectUrls)) {
                CaptchaDataHolder.getInstance().setReCaptchaErrorRedirectUrls(reCaptchaFailedRedirectUrls);
            }

            if (reCaptchaEnabled) {
                CaptchaDataHolder.getInstance().setReCaptchaEnabled(true);
                resolveSecrets(properties);
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

    public static String getOnFailRedirectUrl(String redirectURL, List<String> onFailRedirectUrls,
                                              Map<String, String> attributes) {

        if (StringUtils.isBlank(redirectURL) || onFailRedirectUrls.isEmpty()) {
            return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
        }

        URIBuilder uriBuilder;
        try {
            uriBuilder = new URIBuilder(redirectURL);
        } catch (URISyntaxException e) {
            return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
        }

        for (String url : onFailRedirectUrls) {
            if (!StringUtils.isBlank(url) && url.equalsIgnoreCase(uriBuilder.getPath())) {
                return getUpdatedUrl(redirectURL, attributes);
            }
        }

        return getErrorPage("Human Verification Failed.", "Something went wrong. Please try again.");
    }

    public static String getErrorPage(String status, String statusMsg) {

        try {
            URIBuilder uriBuilder = new URIBuilder(ConfigurationFacade.getInstance().getAuthenticationEndpointRetryURL());
            uriBuilder.addParameter("status", status);
            uriBuilder.addParameter("statusMsg", statusMsg);
            return uriBuilder.build().toString();
        } catch (URISyntaxException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while building URL.", e);
            }
            return ConfigurationFacade.getInstance().getAuthenticationEndpointRetryURL();
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

        CloseableHttpClient httpclient = HttpClientBuilder.create().useSystemProperties().build();
        HttpPost httppost = new HttpPost(CaptchaDataHolder.getInstance().getReCaptchaVerifyUrl());
        final double scoreThreshold = CaptchaDataHolder.getInstance().getReCaptchaScoreThreshold();

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
                if (verificationResponse == null) {
                    throw new CaptchaClientException("Error receiving reCaptcha response from the server");
                }
                boolean success = verificationResponse.get(CaptchaConstants.CAPTCHA_SUCCESS) != null
                        && verificationResponse.get(CaptchaConstants.CAPTCHA_SUCCESS).getAsBoolean();
                // Whether this request was a valid reCAPTCHA token.
                if (!success) {
                    throw new CaptchaClientException("reCaptcha token is invalid. Error:" +
                            verificationResponse.get("error-codes"));
                }
                if (verificationResponse.get(CaptchaConstants.CAPTCHA_SCORE) != null) {
                    double score = verificationResponse.get(CaptchaConstants.CAPTCHA_SCORE).getAsDouble();
                    // reCAPTCHA v3 response contains score
                    if (log.isDebugEnabled()) {
                        log.debug("reCAPTCHA v3 response { timestamp:" +
                                verificationResponse.get("challenge_ts") + ", action: " +
                                verificationResponse.get("action") + ", score: " + score + " }");
                    }
                    if (score < scoreThreshold) {
                        throw new CaptchaClientException("reCaptcha score is less than the threshold.");
                    }
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("reCAPTCHA v2 response { timestamp:" +
                                verificationResponse.get("challenge_ts") + " }");
                    }
                }
            }
        } catch (IOException e) {
            throw new CaptchaServerException("Unable to read the verification response.", e);
        }

        return true;
    }

    public static boolean isMaximumFailedLoginAttemptsReached(String usernameWithDomain, String tenantDomain) throws
            CaptchaException {

        String CONNECTOR_NAME = "sso.login.recaptcha";
        String RECAPTCHA_VERIFICATION_CLAIM = "http://wso2.org/claims/identity/failedLoginAttempts";
        Property[] connectorConfigs;
        try {
            connectorConfigs = CaptchaDataHolder.getInstance().getIdentityGovernanceService()
                    .getConfiguration(new String[]{CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE,
                            CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS}, tenantDomain);
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
            if ((CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE).equals(property.getName())
                    && !Boolean.valueOf(property.getValue())) {
                return false;
            } else if ((CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS).equals(property.getName())) {
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

        Map<String, String> claimValues;
        try {
            userStoreManager = userRealm.getUserStoreManager();
            // Checking that domain name is already prepended to the username.
            // If so user claims can be retrieved.
            // Otherwise, user store manager should be resolved.
            if (!usernameWithDomain.contains(UserCoreConstants.DOMAIN_SEPARATOR)) {
                userStoreManager = getUserStoreManagerForUser(usernameWithDomain, userStoreManager);
            }
            if (userStoreManager == null) {
                if (log.isDebugEnabled()) {
                    log.debug("User store manager cannot be found for the user.");
                }
                // Invalid user. User cannot be found in any user store.
                return false;
            }
            claimValues = userStoreManager.getUserClaimValues(MultitenantUtils
                    .getTenantAwareUsername(usernameWithDomain),
                    new String[]{RECAPTCHA_VERIFICATION_CLAIM}, UserCoreConstants.DEFAULT_PROFILE);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while retrieving user claims.", e);
            }
            // Invalid user
            return false;
        }

        int currentAttempts = 0;
        if (NumberUtils.isNumber(claimValues.get(RECAPTCHA_VERIFICATION_CLAIM))) {
            currentAttempts = Integer.parseInt(claimValues.get(RECAPTCHA_VERIFICATION_CLAIM));
        }

        return currentAttempts >= maxAttempts;
    }

    /**
     * Resolve the user store manager for the user.
     *
     * @param userStoreManager primary user store manager of the user.
     * @param userName        Username.
     * @return Resolved user store manager of the user. Null will be returned if the user is not in any user store for the given tenant.
     * @throws org.wso2.carbon.user.core.UserStoreException Error while checking the user's existence in the given user store.
     */
    private static UserStoreManager getUserStoreManagerForUser(String userName,
           UserStoreManager userStoreManager) throws org.wso2.carbon.user.core.UserStoreException {

        UserStoreManager userStore = userStoreManager;
        while (userStore != null) {
            if (userStore.isExistingUser(userName)) {
                return userStore;
            }
            userStore = userStore.getSecondaryUserStoreManager();
        }
        return null;
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

        String reCaptchaRequestWrapUrls = properties.getProperty(CaptchaConstants.RE_CAPTCHA_REQUEST_WRAP_URLS);
        if (reCaptchaRequestWrapUrls == null) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_REQUEST_WRAP_URLS));
        }
        CaptchaDataHolder.getInstance().setReCaptchaRequestWrapUrls(reCaptchaRequestWrapUrls);

        try {
            Double reCaptchaScoreThreshold = getReCaptchaThreshold(properties);
            CaptchaDataHolder.getInstance().setReCaptchaScoreThreshold(reCaptchaScoreThreshold);
        } catch (NumberFormatException e) {
            throw new RuntimeException(getValidationErrorMessage(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD));
        }

        String forcefullyEnableRecaptchaForAllTenants =
                properties.getProperty(CaptchaConstants.FORCEFULLY_ENABLED_RECAPTCHA_FOR_ALL_TENANTS);
        CaptchaDataHolder.getInstance().setForcefullyEnabledRecaptchaForAllTenants(
                Boolean.parseBoolean(forcefullyEnableRecaptchaForAllTenants));
    }

    /**
     * Method to get the threshold value used by reCAPTCHA v3.
     *
     * @param properties Properties.
     * @return Threshold value set by the user or the default threshold.
     * @throws java.lang.NumberFormatException Error while parsing the threshold value into double.
     */
    private static double getReCaptchaThreshold(Properties properties) throws NumberFormatException {

        String threshold = properties.getProperty(CaptchaConstants.RE_CAPTCHA_SCORE_THRESHOLD);
        if (StringUtils.isBlank(threshold)) {
            if (log.isDebugEnabled()) {
                log.debug("Error parsing recaptcha.threshold from config. Hence using the default value : " +
                        CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD);
            }
           return CaptchaConstants.CAPTCHA_V3_DEFAULT_THRESHOLD;
        }
        return Double.parseDouble(threshold);
    }

    private static void setSSOLoginConnectorConfigs(Properties properties) {

        Map<String, String> connectorPropertyMap = new HashMap<>();

        final String CONNECTOR_NAME = "sso.login";
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE, properties
                .getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.CONNECTOR_IDENTIFIER_ATTRIBUTE,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                        .CONNECTOR_IDENTIFIER_ATTRIBUTE));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.USER_IDENTIFIER_ATTRIBUTE,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                        .USER_IDENTIFIER_ATTRIBUTE));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.RECAPTCHA_VERIFICATION_CLAIM,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes
                        .RECAPTCHA_VERIFICATION_CLAIM));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.MAX_ATTEMPTS));

        CaptchaDataHolder.getInstance().setSSOLoginReCaptchaConnectorPropertyMap(connectorPropertyMap);
    }

    private static void setPathBasedConnectorConfigs(Properties properties) {

        Map<String, String> connectorPropertyMap = new HashMap<>();

        final String CONNECTOR_NAME = "path.based";
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE, properties
                .getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.ENABLE));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_PAGES,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_PAGES));
        connectorPropertyMap.put(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                properties.getProperty(CONNECTOR_NAME + ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS));

        CaptchaDataHolder.getInstance().setPathBasedReCaptchaConnectorPropertyMap(connectorPropertyMap);
    }

    private static String getValidationErrorMessage(String property) {

        return "Invalid value for " + property + " in the " + CaptchaConstants
                .CAPTCHA_CONFIG_FILE_NAME + " file.";
    }

    /**
     * Retrieving resident Idp configuration property for provided property
     *
     * @param servletRequest
     * @param identityGovernanceService
     * @param PROPERTY_ENABLE_RECAPTCHA
     * @return
     * @throws Exception
     */
    public static Property[] getConnectorConfigs(ServletRequest servletRequest,
                                                 IdentityGovernanceService identityGovernanceService,
                                                 String PROPERTY_ENABLE_RECAPTCHA) throws Exception {

        String tenantDomain = servletRequest.getParameter("tenantDomain");
        // This is because from swagger def we expect tenant domain as "tenant-domain"
        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = servletRequest.getParameter("tenant-domain");
        }
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        }
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        Property[] connectorConfigs = identityGovernanceService.getConfiguration(
                new String[]{PROPERTY_ENABLE_RECAPTCHA}, tenantDomain);

        return connectorConfigs;
    }

    /**
     * Validate whether the authentication mechanism of the current authenticator is 'basic'.
     *
     * @param authenticationContext     Authentication context.
     * @param currentAuthenticatorName  Name of the current authenticator.
     * @return  True if auth mechanism is 'basic', false otherwise.
     */
    public static boolean isValidAuthenticator(AuthenticationContext authenticationContext, String currentAuthenticatorName) {

        ApplicationAuthenticator currentApplicationAuthenticator =
                getCurrentAuthenticator(authenticationContext, currentAuthenticatorName);
        if (currentApplicationAuthenticator != null) {
            return (BASIC_AUTH_MECHANISM.equals(currentApplicationAuthenticator.getAuthMechanism()));
        }
        return false;
    }

    /**
     * Get the current authenticator from the sequence configuration in the authentication context.
     *
     * @param authenticationContext     Authentication context.
     * @param currentAuthenticatorName  Name of the current authenticator.
     * @return Current authenticator object.
     */
    private static ApplicationAuthenticator getCurrentAuthenticator(AuthenticationContext authenticationContext,
                                                             String currentAuthenticatorName) {

        int currentStep = authenticationContext.getCurrentStep();
        SequenceConfig sequenceConfig = authenticationContext.getSequenceConfig();
        if (sequenceConfig != null) {
            Map<Integer, StepConfig> stepConfigMap = sequenceConfig.getStepMap();
            if (MapUtils.isNotEmpty(stepConfigMap) && stepConfigMap.containsKey(currentStep)) {
                List<AuthenticatorConfig> authenticatorList = stepConfigMap.get(currentStep).getAuthenticatorList();
                for (AuthenticatorConfig authenticatorConfig : authenticatorList) {
                    if (authenticatorConfig.getName().equals(currentAuthenticatorName)) {
                        return authenticatorConfig.getApplicationAuthenticator();
                    }
                }
            }
        }
        return null;
    }

    /**
     * Resolves site-key, secret-key and any other property if they are configured using secure vault.
     *
     * @param properties    Loaded reCaptcha properties.
     */
    private static void resolveSecrets(Properties properties) {

        SecretResolver secretResolver = SecretResolverFactory.create(properties);
        // Iterate through whole config file and find encrypted properties and resolve them
        if (secretResolver != null && secretResolver.isInitialized()) {
            for (Map.Entry<Object, Object> entry : properties.entrySet()) {
                String key = entry.getKey().toString();
                String value = entry.getValue().toString();
                if (value != null) {
                    value = MiscellaneousUtil.resolve(value, secretResolver);
                }
                properties.put(key, value);
            }
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Secret Resolver is not present. Will not resolve encryptions for captcha");
            }
        }

    }

    /**
     * Get the ReCaptcha Site Key.
     *
     * @return ReCaptcha Site Key.
     */
    public static String reCaptchaSiteKey() {

        return CaptchaDataHolder.getInstance().getReCaptchaSiteKey();
    }

    /**
     * Get the ReCaptcha API URL.
     *
     * @return ReCaptcha API URL.
     */
    public static String reCaptchaAPIURL() {

        return CaptchaDataHolder.getInstance().getReCaptchaAPIUrl();
    }

    /**
     * Check whether ReCaptcha is enabled.
     *
     * @return True if ReCaptcha is enabled.
     */
    public static Boolean isReCaptchaEnabled() {

        return CaptchaDataHolder.getInstance().isReCaptchaEnabled();
    }

    /**
     * Check whether ReCaptcha is enabled for the given flow.
     *
     * @param configName    Name of the configuration.
     * @param tenantDomain  Tenant Domain.
     * @return True if ReCaptcha is enabled for the given flow.
     */
    public static Boolean isReCaptchaEnabledForFlow(String configName, String tenantDomain) {

        Property[] connectorConfigs = null;
        String configValue = null;
        IdentityGovernanceService identityGovernanceService = CaptchaDataHolder.getInstance()
                .getIdentityGovernanceService();
        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }
        try {
            connectorConfigs = identityGovernanceService.getConfiguration(tenantDomain);
        } catch (IdentityGovernanceException e) {
            log.error("Error while retrieving resident Idp configurations for tenant: " + tenantDomain, e);
        }
        if (connectorConfigs != null && StringUtils.isNotEmpty(configName)) {
            for (Property connectorConfig : connectorConfigs) {
                if (configName.equals(connectorConfig.getName())) {
                    configValue = connectorConfig.getValue();
                }
            }
        } else {
            log.warn(String.format("Connector configurations are null. Hence return true for %s configuration.",
                    configName));
        }

        return !Boolean.FALSE.toString().equalsIgnoreCase(configValue);
    }

    /**
     * Check resident IDP reCaptcha configuration for the given property.
     *
     * @param identityGovernanceService Identity governance service.
     * @param servletRequest            Servlet request.
     * @param propertyName              Property name.
     * @return true if reCaptcha is enabled, false otherwise.
     */
    public static boolean isRecaptchaEnabledForConnector(IdentityGovernanceService identityGovernanceService,
                                                         ServletRequest servletRequest, String propertyName) {

        Property[] connectorConfigs;
        try {
            connectorConfigs = CaptchaUtil.getConnectorConfigs(servletRequest, identityGovernanceService,
                    propertyName);
        } catch (Exception e) {
            // Can happen due to invalid tenant/ invalid configuration
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        String enable = null;
        for (Property connectorConfig : connectorConfigs) {
            if ((propertyName).equals(connectorConfig.getName())) {
                enable = connectorConfig.getValue();
            }
        }
        return Boolean.parseBoolean(enable);
    }
}
