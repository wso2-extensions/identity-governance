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

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.wso2.carbon.identity.captcha.util.CaptchaConstants.ReCaptchaConnectorPropertySuffixes;

/**
 * Captcha util functions.
 */
public class CaptchaUtil {

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
                setSSOLoginConnectorConfigs(properties);
                setPathBasedConnectorConfigs(properties);
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

}
