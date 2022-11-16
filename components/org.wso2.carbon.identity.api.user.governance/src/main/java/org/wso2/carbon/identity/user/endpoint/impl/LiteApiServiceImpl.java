/*
 *  Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.LiteApiService;
import org.wso2.carbon.identity.user.endpoint.dto.*;

import org.wso2.carbon.identity.user.endpoint.dto.SuccessfulUserCreationDTO;

import org.wso2.carbon.identity.user.endpoint.util.Utils;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response;

import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_LITE_USER_EMAIL_CONFIRM;
import static org.wso2.carbon.identity.recovery.RecoveryScenarios.LITE_SIGN_UP;
import static org.wso2.carbon.identity.recovery.RecoverySteps.CONFIRM_LITE_SIGN_UP;

public class LiteApiServiceImpl extends LiteApiService {

    private static final Log LOG = LogFactory.getLog(LiteApiServiceImpl.class);

    // Default value for enabling API response.
    private static final boolean ENABLE_DETAILED_API_RESPONSE = false;

    @Override
    public Response litePost(LiteUserRegistrationRequestDTO liteUserRegistrationRequestDTO) {

        //reject if username is not present.
        if (liteUserRegistrationRequestDTO == null ||
                (StringUtils.isBlank(liteUserRegistrationRequestDTO.getEmail())
                        && StringUtils.isBlank(liteUserRegistrationRequestDTO.getMobile())) ){
            Utils.handleBadRequest(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_BAD_LITE_REGISTER_REQUEST.getMessage(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_BAD_LITE_REGISTER_REQUEST.getCode());
        }

        String tenantFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);
        List<PropertyDTO> properties = new ArrayList<>();
        User user = new User();
        user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
        user.setUserName(liteUserRegistrationRequestDTO.getEmail());

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey(IdentityRecoveryConstants.IS_LITE_SIGN_UP);
        propertyDTO.setValue("true");
        properties.add(propertyDTO);

        if (StringUtils.isNotBlank(liteUserRegistrationRequestDTO.getRealm())) {
            user.setUserStoreDomain(liteUserRegistrationRequestDTO.getRealm());
        }

        if (StringUtils.isNotBlank(tenantFromContext)) {
            user.setTenantDomain(tenantFromContext);
        }

        UserSelfRegistrationManager userSelfRegistrationManager = Utils.getUserSelfRegistrationManager();
        NotificationResponseBean notificationResponseBean = null;
        properties.addAll(liteUserRegistrationRequestDTO.getProperties());
        try {
            notificationResponseBean = userSelfRegistrationManager
                    .registerLiteUser(user,
                            Utils.getClaims(liteUserRegistrationRequestDTO.getClaims()),
                            Utils.getProperties(properties));
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while self registering lite user ", e);
            }
            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS.getCode().equals(e.getErrorCode())) {
                try {
                    boolean isResendVerificationEnabledOnUserExistence =
                            Boolean.parseBoolean(org.wso2.carbon.identity.recovery.util.Utils.getConnectorConfig(
                                    LITE_REGISTRATION_RESEND_VERIFICATION_ON_USER_EXISTENCE, user.getTenantDomain()));
                    if (isResendVerificationEnabledOnUserExistence) {
                        try {
                            ResendConfirmationManager resendConfirmationManager = Utils.getResendConfirmationManager();
                            notificationResponseBean =
                                    resendConfirmationManager.resendConfirmationCode(user, LITE_SIGN_UP.toString(),
                                            CONFIRM_LITE_SIGN_UP.toString(),
                                            NOTIFICATION_TYPE_RESEND_LITE_USER_EMAIL_CONFIRM, null);
                        } catch (IdentityRecoveryException ex) {
                            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, ex);
                        }
                    } else {
                        Utils.handleConflict(e.getMessage(), e.getErrorCode());
                    }
                } catch (IdentityEventException ex) {
                    Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, ex);
                }
            } else {
                Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
            }
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return buildSuccessfulAPIResponse(notificationResponseBean);
    }

    /**
     * Build response for a successful user self registration.
     *
     * @param notificationResponseBean NotificationResponseBean {@link NotificationResponseBean}
     * @return Response
     */
    private Response buildSuccessfulAPIResponse(NotificationResponseBean notificationResponseBean) {

        // Check whether detailed api responses are enabled.
        if (isDetailedResponseBodyEnabled()) {
            String notificationChannel = notificationResponseBean.getNotificationChannel();
            if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
                // Handle response when the notifications are externally managed.
                SuccessfulUserCreationExternalResponseDTO successfulUserCreationDTO =
                        buildSuccessResponseForExternalChannel(notificationResponseBean);
                return Response.status(Response.Status.CREATED).entity(successfulUserCreationDTO).build();
            }
            SuccessfulUserCreationDTO successfulUserCreationDTO =
                    buildSuccessResponseForInternalChannels(notificationResponseBean);
            return Response.status(Response.Status.CREATED).entity(successfulUserCreationDTO).build();
        } else {
            if (notificationResponseBean != null) {
                String notificationChannel = notificationResponseBean.getNotificationChannel();
                /*If the notifications are required in the form of legacy response, and notifications are externally
                 managed, the recoveryId should be in the response as text*/
                if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
                    return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getRecoveryId())
                            .build();
                }
            }
            return Response.status(Response.Status.CREATED).build();
        }
    }

    /**
     * Build the successResponseDTO for successful user identification and channel retrieve when the notifications
     * are managed externally.
     *
     * @param notificationResponseBean NotificationResponseBean
     * @return SuccessfulUserCreationExternalResponseDTO
     */
    private SuccessfulUserCreationExternalResponseDTO buildSuccessResponseForExternalChannel(
            NotificationResponseBean notificationResponseBean) {

        SuccessfulUserCreationExternalResponseDTO successDTO = new SuccessfulUserCreationExternalResponseDTO();
        successDTO.setCode(notificationResponseBean.getCode());
        successDTO.setMessage(notificationResponseBean.getMessage());
        successDTO.setNotificationChannel(notificationResponseBean.getNotificationChannel());
        successDTO.setConfirmationCode(notificationResponseBean.getRecoveryId());
        return successDTO;
    }

    /**
     * Reads configurations from the identity.xml and return whether the detailed response is enabled or not.
     *
     * @return True if the legacy response is enabled.
     */
    private boolean isDetailedResponseBodyEnabled() {

        String enableDetailedResponseConfig = IdentityUtil
                .getProperty(Constants.ENABLE_DETAILED_API_RESPONSE);
        if (StringUtils.isEmpty(enableDetailedResponseConfig)) {
            // Return false if the user has not enabled the detailed response body.
            return ENABLE_DETAILED_API_RESPONSE;
        } else {
            return Boolean.parseBoolean(enableDetailedResponseConfig);
        }
    }

    /**
     * Build the successResponseDTO for successful user identification and channel retrieve when the notifications
     * are managed internally.
     *
     * @param notificationResponseBean NotificationResponseBean
     * @return SuccessfulUserCreationDTO
     */
    private SuccessfulUserCreationDTO buildSuccessResponseForInternalChannels(
            NotificationResponseBean notificationResponseBean) {

        SuccessfulUserCreationDTO successDTO = new SuccessfulUserCreationDTO();
        successDTO.setCode(notificationResponseBean.getCode());
        successDTO.setMessage(notificationResponseBean.getMessage());
        successDTO.setNotificationChannel(notificationResponseBean.getNotificationChannel());
        return successDTO;
    }

}
