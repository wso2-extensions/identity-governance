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
import org.wso2.carbon.CarbonException;
import org.wso2.carbon.core.util.AnonymousSessionUtil;
import org.wso2.carbon.identity.mgt.constants.SelfRegistrationStatusCodes;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.ValidateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidateInfoResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;

import java.util.List;
import javax.ws.rs.core.Response;

public class ValidateUsernameApiServiceImpl extends ValidateUsernameApiService {

    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);
    private static final String SKIP_SIGN_UP_ENABLE_CHECK_KEY = "skipSignUpEnableCheck";
    private static final String USERNAME_JAVA_REG_EX_VIOLATION_ERROR_MSG = "UsernameJavaRegExViolationErrorMsg";
    private static final String DISABLE_USER_NAME_VALIDITY_API_ERROR_RESPONSES =
            "DisableUserNameValidityApiErrorResponses";

    @Override
    public Response validateUsernamePost(UsernameValidationRequestDTO user) {

        // Checking the property DisableUserNameValidityApiErrorResponses in identity.xml to decide whether to use the
        // newly improved method or the old one.

        if (isUserNameValidityApiErrorResponsesDisabled()) {
            return checkUserNameValidity(user);
        } else {
            return checkUserNameValidityStatus(user);
        }
    }

    /**
     * This is the old way of checking username validity, where we return 200 ok for any instance. Keeping this to
     * preserve backward compatibility. Need to use {@link #checkUserNameValidityStatus(UsernameValidationRequestDTO)}
     * instead.
     *
     * @param user Username validation request.
     * @return Response object.
     */
    private Response checkUserNameValidity(UsernameValidationRequestDTO user) {

        if (StringUtils.isEmpty(user.getUsername())) {
            return Response.status(Response.Status.BAD_REQUEST).entity("Username cannot be empty.").build();
        }
        try {
            String tenantDomain = MultitenantUtils.getTenantDomain(user.getUsername());
            List<PropertyDTO> propertyDTOList = user.getProperties();
            boolean skipSelfSignUpEnabledCheck = false;
            if (CollectionUtils.isNotEmpty(propertyDTOList)) {
                for (PropertyDTO propertyDTO : propertyDTOList) {
                    if (SKIP_SIGN_UP_ENABLE_CHECK_KEY.equalsIgnoreCase(propertyDTO.getKey())) {
                        skipSelfSignUpEnabledCheck = Boolean.parseBoolean(propertyDTO.getValue());
                    }
                }
            }
            UserSelfRegistrationManager userSelfRegistrationManager = Utils
                    .getUserSelfRegistrationManager();
            if (LOG.isDebugEnabled()) {
                LOG.debug(String.format("Validating username for user %s", user.getUsername()));
            }
            UsernameValidateInfoResponseDTO responseDTO = new UsernameValidateInfoResponseDTO();
            if (!userSelfRegistrationManager.isValidTenantDomain(tenantDomain)) {
                logDebug(String.format("%s is an invalid tenant domain. Hence returning code %s: ", tenantDomain,
                        SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT));
            } else if (!skipSelfSignUpEnabledCheck &&
                    !userSelfRegistrationManager.isSelfRegistrationEnabled(tenantDomain)) {
                logDebug(String.format("Self registration is not enabled for tenant domain: %s . Hence returning code" +
                        ": %s", tenantDomain, SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
                responseDTO.setStatusCode(
                        Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
            } else if (userSelfRegistrationManager.isUsernameAlreadyTaken(user.getUsername())) {
                logDebug(String.format("username : %s is an already taken. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS));
            } else if (!userSelfRegistrationManager.isMatchUserNameRegex(tenantDomain, user.getUsername())) {
                logDebug(String.format("%s is an invalid user name. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID));
            } else {
                logDebug(String.format("username : %s is available for self registration. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
            }
            return Response.ok().entity(responseDTO).build();
        } catch (IdentityRecoveryException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Error while checking username validity for user " + user.getUsername(), e);
            }
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity("Error while checking user " +
                    "existence").build();
        }
    }

    private Response checkUserNameValidityStatus(UsernameValidationRequestDTO user) {

        if (StringUtils.isEmpty(user.getUsername())) {
            return Response.status(Response.Status.BAD_REQUEST).entity("Username cannot be empty.").build();
        }

        try {
            String tenantDomain = MultitenantUtils.getTenantDomain(user.getUsername());
            List<PropertyDTO> propertyDTOList = user.getProperties();
            boolean skipSelfSignUpEnabledCheck = false;

            if (CollectionUtils.isNotEmpty(propertyDTOList)) {
                for (PropertyDTO propertyDTO : propertyDTOList) {
                    if (SKIP_SIGN_UP_ENABLE_CHECK_KEY.equalsIgnoreCase(propertyDTO.getKey())) {
                        skipSelfSignUpEnabledCheck = Boolean.parseBoolean(propertyDTO.getValue());
                    }
                }
            }
            UserSelfRegistrationManager userSelfRegistrationManager = Utils
                    .getUserSelfRegistrationManager();
            if (LOG.isDebugEnabled()) {
                LOG.debug(String.format("Validating username for user %s", user.getUsername()));
            }
            UsernameValidateInfoResponseDTO responseDTO = new UsernameValidateInfoResponseDTO();
            ErrorDTO errorDTO = new ErrorDTO();
            if (!userSelfRegistrationManager.isValidTenantDomain(tenantDomain)) {
                logDebug(String.format("%s is an invalid tenant domain. Hence returning code %s: ", tenantDomain,
                        SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT));
                errorDTO.setCode(SelfRegistrationStatusCodes.ERROR_CODE_INVALID_TENANT);
                return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
            } else if (!skipSelfSignUpEnabledCheck &&
                    !userSelfRegistrationManager.isSelfRegistrationEnabled(tenantDomain)) {
                logDebug(String.format("Self registration is not enabled for tenant domain: %s. Hence returning code:" +
                        " %s", tenantDomain, SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
                errorDTO.setCode(SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED);
                return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
            } else if (userSelfRegistrationManager.isUsernameAlreadyTaken(user.getUsername())) {
                logDebug(String.format("username : %s is an already taken. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS));
                errorDTO.setCode(SelfRegistrationStatusCodes.ERROR_CODE_USER_ALREADY_EXISTS);
                return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
            } else if (!userSelfRegistrationManager.isMatchUserNameRegex(tenantDomain, user.getUsername())) {
                logDebug(String.format("%s is an invalid user name. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID));
                errorDTO.setCode(SelfRegistrationStatusCodes.CODE_USER_NAME_INVALID);
                errorDTO.setMessage(getRegexViolationErrorMsg(user, tenantDomain));
                return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
            } else {
                logDebug(String.format("username : %s is available for self registration. Hence returning code %s: ",
                        user.getUsername(), SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
                responseDTO.setStatusCode(Integer.parseInt(SelfRegistrationStatusCodes.CODE_USER_NAME_AVAILABLE));
                return Response.ok().entity(responseDTO).build();
            }
        } catch (IdentityRecoveryException | CarbonException | UserStoreException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Error while checking username validity for user " + user.getUsername(), e);
            }
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity("Error while checking user " +
                    "existence").build();
        }
    }

    private String getRegexViolationErrorMsg(UsernameValidationRequestDTO user, String tenantDomain)
            throws CarbonException, UserStoreException {

        String userDomain = IdentityUtil.extractDomainFromName(user.getUsername());
        UserRealm userRealm = getUserRealm(tenantDomain);
        RealmConfiguration realmConfiguration = userRealm.getUserStoreManager().getSecondaryUserStoreManager
                (userDomain).getRealmConfiguration();
        String errorMsg = realmConfiguration
                .getUserStoreProperty(USERNAME_JAVA_REG_EX_VIOLATION_ERROR_MSG);
        if (StringUtils.isNotEmpty(errorMsg)) {
            return errorMsg;
        } else {
            return user.getUsername() + " is an invalid user name. Please pick a valid username.";
        }
    }

    /**
     * If the property DisableUserNameValidityApiErrorResponses is set to True in identity.xml, username validity API
     * will not return error response for invalid usernames. All responses will be 200 OK with specific error codes.
     */
    public static boolean isUserNameValidityApiErrorResponsesDisabled() {

        String enableUserNameValidationApiResponseProperty =
                IdentityUtil.getProperty(DISABLE_USER_NAME_VALIDITY_API_ERROR_RESPONSES);
        if (StringUtils.isNotBlank(enableUserNameValidationApiResponseProperty)) {
            return Boolean.parseBoolean(enableUserNameValidationApiResponseProperty);
        }
        return false;
    }

    private UserRealm getUserRealm(String tenantDomain) throws CarbonException {

        return AnonymousSessionUtil.getRealmByTenantDomain(IdentityRecoveryServiceDataHolder.getInstance()
                        .getRegistryService(), IdentityRecoveryServiceDataHolder.getInstance().getRealmService(),
                tenantDomain);
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
