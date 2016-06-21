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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.exception.CaptchaServerException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * ReCaptcha Page Based Connector.
 */
public class PathBasedReCaptchaConnector extends AbstractReCaptchaConnector implements IdentityGovernanceConnector {

    private static final Log log = LogFactory.getLog(PathBasedReCaptchaConnector.class);

    private final String CONNECTOR_NAME = "path.based";

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
        if (StringUtils.isBlank(path)) {
            return false;
        }

        Map<String, String> connectorConfigs;
        try {
            connectorConfigs = getConnectorConfigs(servletRequest);
        } catch (Exception e) {
            // Can happen due to invalid user/ invalid tenant/ invalid configuration
            if (log.isDebugEnabled()) {
                log.debug("Unable to load connector configuration.", e);
            }
            return false;
        }

        if (!"TRUE".equalsIgnoreCase(connectorConfigs.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE))) {
            return false;
        }

        if (CaptchaUtil.isPathAvailable(path, connectorConfigs.get(CONNECTOR_NAME + CaptchaConstants
                .ReCaptchaConnectorPropertySuffixes.SECURED_PAGES))) {
            return true;
        } else if (CaptchaUtil.isPathAvailable(path, connectorConfigs.get(CONNECTOR_NAME + CaptchaConstants
                .ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS))) {
            return true;
        }

        return false;
    }

    @Override
    public CaptchaPreValidationResponse preValidate(ServletRequest servletRequest,  ServletResponse servletResponse)
            throws CaptchaException {

        // reCaptcha is required for all requests.
        CaptchaPreValidationResponse preValidationResponse = new CaptchaPreValidationResponse();

        String path = ((HttpServletRequest) servletRequest).getRequestURI();

        Map<String, String> connectorConfigs;
        try {
            connectorConfigs = getConnectorConfigs(servletRequest);
        } catch (Exception e) {
            throw new CaptchaServerException("Error occurred while retrieving reCaptcha configuration.", e);
        }

        if (CaptchaUtil.isPathAvailable(path, connectorConfigs.get(CONNECTOR_NAME + CaptchaConstants
                .ReCaptchaConnectorPropertySuffixes.SECURED_PAGES))) {
            preValidationResponse.setEnableCaptchaForRequestPath(true);
            Map<String, String> params = new HashMap<>();
            params.put("reCaptcha", "true");
            params.put("reCaptchaKey", CaptchaDataHolder.getInstance().getReCaptchaSiteKey());
            params.put("reCaptchaAPI", CaptchaDataHolder.getInstance().getReCaptchaAPIUrl());
            preValidationResponse.setCaptchaAttributes(params);
        }

        if (CaptchaUtil.isPathAvailable(path, connectorConfigs.get(CONNECTOR_NAME + CaptchaConstants
                .ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS))) {
            preValidationResponse.setCaptchaValidationRequired(true);
            if (!StringUtils.isBlank(connectorConfigs.get(CONNECTOR_NAME + CaptchaConstants
                    .ReCaptchaConnectorPropertySuffixes.SECURED_PAGES))) {
                preValidationResponse.setOnCaptchaFailRedirectUrls(Arrays.asList(connectorConfigs.get(CONNECTOR_NAME +
                        CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_PAGES).split(",")));
                // Add parameters which need to send back in case of failure.
                Map<String, String> params = new HashMap<>();
                params.put("error", "true");
                params.put("errorMsg", "Human verification failed. Please select reCaptcha.");
                preValidationResponse.setCaptchaAttributes(params);
            }
        }
        preValidationResponse.setPostValidationRequired(false);

        return preValidationResponse;
    }

    @Override
    public CaptchaPostValidationResponse postValidate(ServletRequest servletRequest, ServletResponse servletResponse)
            throws CaptchaException {

        //No validation at this stage.
        return null;
    }

    @Override
    public String getName() {

        return CONNECTOR_NAME;
    }

    @Override
    public String getFriendlyName() {

        String friendlyName = "reCaptcha for Request Path";
        if(!CaptchaDataHolder.getInstance().isReCaptchaEnabled()) {
            friendlyName += " (reCaptcha is not enabled)";
        }
        return friendlyName;
    }

    @Override
    public String[] getPropertyNames() {

        return new String[]{
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_PAGES,
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL
        };
    }

    @Override
    public Properties getDefaultPropertyValues(String s) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = CaptchaDataHolder.getInstance().getPathBasedReCaptchaConnectorPropertyMap();
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                    "false");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_PAGES))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes
                    .SECURED_PAGES, "");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes
                    .SECURED_DESTINATIONS, "");
        }
        if (StringUtils.isBlank(defaultProperties.get(CONNECTOR_NAME +
                CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ON_FAIL_REDIRECT_URL))) {
            defaultProperties.put(CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes
                            .ON_FAIL_REDIRECT_URL,
                    "/authenticationendpoint/retry.do");
        }

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] strings, String s) throws IdentityGovernanceException {
        return null;
    }

    private Map<String, String> getConnectorConfigs(ServletRequest servletRequest) throws Exception {

        String tenantDomain = servletRequest.getParameter("tenantDomain");
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        Map<String, String> connectorConfigs;
        connectorConfigs = identityGovernanceService.getConfiguration(new String[]{
                        CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.ENABLE,
                        CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_DESTINATIONS,
                        CONNECTOR_NAME + CaptchaConstants.ReCaptchaConnectorPropertySuffixes.SECURED_PAGES},
                tenantDomain);

        return connectorConfigs;
    }

}
