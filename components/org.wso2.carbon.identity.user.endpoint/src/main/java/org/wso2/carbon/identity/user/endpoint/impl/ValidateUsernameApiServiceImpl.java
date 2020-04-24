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
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.mgt.constants.SelfRegistrationStatusCodes;
import org.wso2.carbon.identity.mgt.endpoint.util.IdentityManagementEndpointConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.ValidateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidateInfoResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import javax.ws.rs.core.Response;
import java.util.List;

public class ValidateUsernameApiServiceImpl extends ValidateUsernameApiService {

    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);
    private static final String SKIP_SIGN_UP_ENABLE_CHECK_KEY = "skipSignUpEnableCheck";

    @Override
    public Response validateUsernamePost(UsernameValidationRequestDTO user) {

        if (StringUtils.isEmpty(user.getUsername())) {
            return Response.status(Response.Status.BAD_REQUEST).entity("Username cannot be empty.").build();
        }

        try {
            List<PropertyDTO> propertyDTOList = user.getProperties();
            String tenantDomain = MultitenantUtils.getTenantDomain(user.getUsername());
            boolean skipSelfSignUpEnabledCheck = false;

            if (CollectionUtils.isNotEmpty(propertyDTOList)) {
                for (PropertyDTO propertyDTO : propertyDTOList) {
                    if (SKIP_SIGN_UP_ENABLE_CHECK_KEY.equalsIgnoreCase(propertyDTO.getKey())) {
                        skipSelfSignUpEnabledCheck = Boolean.parseBoolean(propertyDTO.getValue());
                    } else if (StringUtils.equals(IdentityManagementEndpointConstants.TENANT_DOMAIN,
                            propertyDTO.getKey())) {
                        tenantDomain = propertyDTO.getValue();
                    }
                }
            }
            UserSelfRegistrationManager userSelfRegistrationManager = Utils
                    .getUserSelfRegistrationManager();
            if (LOG.isDebugEnabled()) {
                LOG.debug(String.format("Validating username for user %s", user.getUsername()));
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
                logDebug(String.format("Self registration is not enabled for tenant domain : %s . Hence returning code",
                        tenantDomain, SelfRegistrationStatusCodes.ERROR_CODE_SELF_REGISTRATION_DISABLED));
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
                logDebug(String.format("username : %s is avilable for self registration. Hence returning code %s: ",
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

    private void logDebug(String message) {

        if (LOG.isDebugEnabled()) {
            LOG.debug(message);
        }
    }
}
