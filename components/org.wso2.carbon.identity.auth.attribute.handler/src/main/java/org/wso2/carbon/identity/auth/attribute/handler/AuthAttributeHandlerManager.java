/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.auth.attribute.handler;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.AuthenticationStep;
import org.wso2.carbon.identity.application.common.model.LocalAuthenticatorConfig;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.internal.AuthAttributeHandlerServiceDataHolder;
import org.wso2.carbon.identity.auth.attribute.handler.model.AuthAttributeHolder;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND;
import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR;

/**
 * Service class responsible for handling Auth Attribute Handlers.
 * Capabilities provided by the Auth Attribute Handlers are accessed through this class.
 */
public class AuthAttributeHandlerManager {

    private static final Log LOG = LogFactory.getLog(AuthAttributeHandlerManager.class);
    private static final AuthAttributeHandlerManager instance = new AuthAttributeHandlerManager();

    private AuthAttributeHandlerManager() {

    }

    /**
     * Return the instance of AuthAttributeHandlerManager.
     *
     * @return Returns the instance of AuthAttributeHandlerManager.
     */
    public static AuthAttributeHandlerManager getInstance() {

        return instance;
    }

    /**
     * Get a list of auth attribute holders for a given application id.
     * The auth attribute holders will be derived based on the configured
     * authenticators of the application which have bound auth attribute
     * handlers. The auth attribute holders will contain the metadata of the
     * auth attribute handler and the required auth attributes.
     *
     * @param appId Identifier of the application.
     * @return A list of Auth Attribute Holders.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    public List<AuthAttributeHolder> getAvailableAuthAttributeHolders(String appId) throws
            AuthAttributeHandlerException {

        List<String> authenticators = getConfiguredAuthenticators(appId);
        List<AuthAttributeHandler> authAttributeHandlers =
                AuthAttributeHandlerServiceDataHolder.getInstance().getAuthAttributeHandlers();
        List<AuthAttributeHolder> selectedAuthAttributeHolders = new ArrayList<>();
        for (AuthAttributeHandler authAttributeHandler : authAttributeHandlers) {
            if (authAttributeHandler.getBindingType() == AuthAttributeHandlerBindingType.AUTHENTICATOR
                    && authenticators.contains(authAttributeHandler.getBoundIdentifier())) {
                selectedAuthAttributeHolders.add(authAttributeHandler.getAuthAttributeData());
            }
        }

        return selectedAuthAttributeHolders;
    }

    /**
     * This method is used to engage the validations related to the
     * selected auth attribute handler.
     *
     * @param authAttributeHandlerName Name of the auth attribute handler.
     * @param attributeMap             Set of attributes to be validated.
     * @return A ValidationResult object containing the validation status.
     * If there are validation failures it will contain a validation
     * failure reason for any of the validation failed attributes.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    public ValidationResult validateAuthAttributes(String authAttributeHandlerName, Map<String, String> attributeMap)
            throws AuthAttributeHandlerException {

        List<AuthAttributeHandler> authAttributeHandlers =
                AuthAttributeHandlerServiceDataHolder.getInstance().getAuthAttributeHandlers();

        AuthAttributeHandler authAttributeHandler = null;
        for (AuthAttributeHandler handler : authAttributeHandlers) {
            if (StringUtils.equals(handler.getName(), authAttributeHandlerName)) {
                authAttributeHandler = handler;
                break;
            }
        }

        if (authAttributeHandler == null) {
            throw new AuthAttributeHandlerClientException(ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getCode(),
                    String.format(ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getMessage(), authAttributeHandlerName));
        }

        return authAttributeHandler.validateAttributes(attributeMap);
    }

    private ApplicationManagementService getApplicationManagementService() {

        return AuthAttributeHandlerServiceDataHolder.getInstance().getApplicationManagementService();
    }

    private List<String> getConfiguredAuthenticators(String appId)
            throws AuthAttributeHandlerException {

        List<String> authenticators = new ArrayList<>();
        AuthenticationStep[] authenticationSteps;
        try {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Attempting to retrieve configured authenticators for appId: " + appId);
            }

            authenticationSteps = getApplicationManagementService().getConfiguredAuthenticators(appId);
        } catch (IdentityApplicationManagementException e) {
            throw new AuthAttributeHandlerException(ERROR_CODE_UNEXPECTED_ERROR.getCode(),
                    "Error while retrieving configured authenticators for appId: " + appId, e);
        }
        if (authenticationSteps != null) {
            for (AuthenticationStep authenticationStep : authenticationSteps) {
                LocalAuthenticatorConfig[] configs = authenticationStep.getLocalAuthenticatorConfigs();
                if (configs != null) {
                    for (LocalAuthenticatorConfig config : configs) {
                        if (!authenticators.contains(config.getName())) {
                            authenticators.add(config.getName());
                        }
                    }
                }
            }
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug(String.format("Configured authenticators for appId: %s : %s", appId,
                    StringUtils.join(authenticators, ",")));
        }

        return authenticators;
    }
}
