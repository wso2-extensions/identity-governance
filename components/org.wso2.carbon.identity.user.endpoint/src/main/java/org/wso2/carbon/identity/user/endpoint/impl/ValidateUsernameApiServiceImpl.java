/*
 *  Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.wso2.carbon.identity.user.endpoint.impl;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.mgt.constants.SelfRegistrationStatusCodes;
import org.wso2.carbon.identity.mgt.endpoint.util.IdentityManagementEndpointConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.ValidateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidateInfoResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.List;
import javax.ws.rs.core.Response;

public class ValidateUsernameApiServiceImpl extends ValidateUsernameApiService {

    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);
    private static final String SKIP_SIGN_UP_ENABLE_CHECK_KEY = "skipSignUpEnableCheck";

    @Override
    public Response validateUsernamePost(UsernameValidationRequestDTO user) {

        if (StringUtils.isEmpty(user.getUsername())) {
            return Response.status(Response.Status.BAD_REQUEST).entity("Username cannot be empty.").build();
        }

        String fullyQualifiedUsername = user.getUsername();
        try {
            String tenantDomain = resolveTenantDomain(user);
            String tenantAwareUsername = MultitenantUtils.getTenantAwareUsername(user.getUsername());
            fullyQualifiedUsername = UserCoreUtil.addTenantDomainToEntry(tenantAwareUsername, tenantDomain);
            List<PropertyDTO> propertyDTOList = user.getProperties();

            boolean skipSelfSignUpEnabledCheck = false;

            if (CollectionUtils.isNotEmpty(propertyDTOList)) {
                for (PropertyDTO propertyDTO : propertyDTOList) {
                    if (SKIP_SIGN_UP_ENABLE_CHECK_KEY.equalsIgnoreCase(propertyDTO.getKey())) {
                        skipSelfSignUpEnabledCheck = Boolean.parseBoolean(propertyDTO.getValue());
                    }
                }
            }
            UserSelfRegistrationManager userSelfRegistrationManager = Utils.getUserSelfRegistrationManager();
            if (LOG.isDebugEnabled()) {
                LOG.debug(String.format("Validating username for user %s", fullyQualifiedUsername));
            }
            UsernameValidateInfoResponseDTO responseDTO = new UsernameValidateInfoResponseDTO();

            if (IdentityUtil.isEmailUsernameEnabled() &&
                    StringUtils.containsNone(user.getUsername(), "@")) {
                logDebug("Username is invalid. Username should be in email format.");
                responseDTO.setStatusCode(Integer.parseInt(
                        SelfRegistrationStatusCodes.ERROR_CODE_INVALID_EMAIL_USERNAME));
            } else if (!userSelfRegistrationManager.isValidTenantDomain(tenantDomain)) {
                logDebug(String.format("%s is an invalid tenant domain. Hence returning code %s: ", tenantDomain,
                        SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT));
            } else if (!skipSelfSignUpEnabledCheck && !userSelfRegistrationManager.isSelfRegistrationEnabled(tenantDomain)) {
                logDebug(String.format("Self registration is not enabled for tenant domain : %s . Hence returning " +
                                "code %s", tenantDomain,
                        SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
                responseDTO.setStatusCode(
                        Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
            } else if (userSelfRegistrationManager.isUsernameAlreadyTaken(fullyQualifiedUsername, tenantDomain)) {
                logDebug(String.format("username : %s is an already taken. Hence returning code %s: ",
                        fullyQualifiedUsername, SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS));
            } else if (!userSelfRegistrationManager.isMatchUserNameRegex(tenantDomain, tenantAwareUsername)) {
                logDebug(String.format("%s is an invalid user name. Hence returning code %s: ",
                        fullyQualifiedUsername, SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID));
            } else {
                logDebug(String.format("username : %s is available for self registration. Hence returning code %s: ",
                        fullyQualifiedUsername, SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
            }
            return Response.ok().entity(responseDTO).build();
        } catch (IdentityRecoveryException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Error while checking username validity for user " + fullyQualifiedUsername, e);
            }
            if (e instanceof IdentityRecoveryClientException) {
                if (StringUtils.isNotBlank(e.getErrorDescription())) {
                    return Response.status(Response.Status.BAD_REQUEST).entity(e.getErrorDescription()).build();
                }
                return Response.status(Response.Status.BAD_REQUEST).entity("Invalid Request").build();
            }
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity("Error while checking user " +
                    "existence").build();
        }
    }

    private void logDebug(String message) {

        if (LOG.isDebugEnabled()) {
            LOG.debug(message);
        }
    }

    /**
     * Resolve the tenant domain.
     *
     * @param usernameValidationRequestDTO UsernameValidationRequestDTO object.
     * @return Tenant domain.
     * @throws IdentityRecoveryClientException If the tenant domain in the UsernameValidationRequestDTO is not same
     *                                         as the tenant domain in the request.
     */
    private String resolveTenantDomain(UsernameValidationRequestDTO usernameValidationRequestDTO)
            throws IdentityRecoveryClientException {

        String tenantDomain;
        String usernameInTheRequest = usernameValidationRequestDTO.getUsername();

        // Added to maintain the backward compatibility.
        if (!Boolean.parseBoolean(IdentityUtil.getProperty(IdentityRecoveryConstants.ENABLE_TENANT_QUALIFIED_URLS))) {
            return MultitenantUtils.getTenantDomain(usernameInTheRequest);
        }

        String tenantDomainFromContext = (String) IdentityUtil.threadLocalProperties.get()
                .get(Constants.TENANT_NAME_FROM_CONTEXT);
        if (StringUtils.isNotBlank(tenantDomainFromContext)) {
            tenantDomain = tenantDomainFromContext.toLowerCase();
        } else {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }
        String tenantAwareUsernameInTheRequest = MultitenantUtils.getTenantAwareUsername(usernameInTheRequest);

        /*
        This means that the user has not specified a tenant domain in the username of the validation request.
        Therefore, accept the tenant domain in the request url.
         */
        if (usernameInTheRequest.equals(tenantAwareUsernameInTheRequest)) {
            return tenantDomain;
        }
        // Since the above condition is false, there is a tenant domain specified in the username of the request.
        String tenantDomainInValidationRequest = MultitenantUtils.getTenantDomain(usernameInTheRequest);

        // Tenant domain is specified and it is NOT EQUAL to the one specified in the request.
        if (!tenantDomain.equals(tenantDomainInValidationRequest)) {
            throw new IdentityRecoveryClientException(String.format("Tenant domain in the request: %s does not match " +
                    "with the domain specified in the URL: %s", tenantDomainInValidationRequest, tenantDomain));
        }
        return tenantDomain;
    }
}
