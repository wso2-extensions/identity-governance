/*
 *
 *  Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.MeApiService;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeCodeValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SuccessfulUserCreationDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SuccessfulUserCreationExternalResponseDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.ws.rs.core.Response;

import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_BAD_SELF_REGISTER_REQUEST;

/**
 * Class which contains the implementation of MeApiService.
 */
public class MeApiServiceImpl extends MeApiService {

    private static final Log LOG = LogFactory.getLog(MeApiServiceImpl.class);
    private static final String RECOVERY_SCENARIO_KEY = "RecoveryScenario";

    // Default value for enabling API response.
    private static final boolean ENABLE_DETAILED_API_RESPONSE = false;

    @Override
    public Response getMe() {

        String username = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        String userStoreDomain = UserCoreUtil.extractDomainFromName(username);
        username = UserCoreUtil.removeDomainFromName(username);
        int tenantId = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantId();
        Map userAttributes;
        try {
            userAttributes = Utils.getUserInformationService().getRetainedUserInformation(username, userStoreDomain,
                    tenantId);
        } catch (UserExportException e) {
            ErrorDTO errorDTO = new ErrorDTO();
            errorDTO.setRef(Utils.getCorrelation());
            errorDTO.setMessage(e.getMessage());
            return Response.serverError().entity(errorDTO).build();
        }
        return Response.ok().status(Response.Status.OK).entity(userAttributes).build();
    }

    @Override
    public Response mePost(SelfUserRegistrationRequestDTO selfUserRegistrationRequestDTO) {

        String tenantFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);

        if (selfUserRegistrationRequestDTO == null) {
            Utils.handleBadRequest("Invalid data for self-registration.",
                    ERROR_CODE_BAD_SELF_REGISTER_REQUEST.getCode());
        }

        if (StringUtils.isNotBlank(tenantFromContext)) {
            selfUserRegistrationRequestDTO.getUser().setTenantDomain(tenantFromContext);
        }

        if (selfUserRegistrationRequestDTO != null && StringUtils.isBlank(selfUserRegistrationRequestDTO.getUser().getRealm())) {
            selfUserRegistrationRequestDTO.getUser().setRealm(IdentityUtil.getPrimaryDomainName());
        }

        UserSelfRegistrationManager userSelfRegistrationManager = Utils
                .getUserSelfRegistrationManager();
        NotificationResponseBean notificationResponseBean = null;
        try {
            notificationResponseBean = userSelfRegistrationManager
                    .registerUser(Utils.getUser(selfUserRegistrationRequestDTO.getUser()),
                            selfUserRegistrationRequestDTO.getUser().getPassword(),
                            Utils.getClaims(selfUserRegistrationRequestDTO.getUser().getClaims()),
                            Utils.getProperties(selfUserRegistrationRequestDTO.getProperties()));
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while registering self up user ", e);
            }
            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS.getCode().equals(e.getErrorCode())) {
                Utils.handleConflict(e.getMessage(), e.getErrorCode());
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

    @Override
    public Response meResendCodePost(MeResendCodeRequestDTO meResendCodeRequestDTO) {

        ResendCodeRequestDTO resendCodeRequestDTO = convertToResendCodeRequest(meResendCodeRequestDTO);
        NotificationResponseBean notificationResponseBean = null;

        String recoveryScenario = getRecoveryScenarioFromProperties(resendCodeRequestDTO.getProperties());
        if (!StringUtils.isBlank(recoveryScenario)) {
            notificationResponseBean = doResendConfirmationCode(recoveryScenario, notificationResponseBean,
                    resendCodeRequestDTO);
        }

        if (notificationResponseBean == null) {
            ErrorDTO errorDTO = Utils.getErrorDTO(Constants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_SCENARIO.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_SCENARIO.getMessage());
            return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
        }
        return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getKey()).build();
    }

    @Override
    public Response meValidateCodePost(MeCodeValidationRequestDTO meCodeValidationRequestDTO) {

        UserSelfRegistrationManager userSelfRegistrationManager = Utils.getUserSelfRegistrationManager();
        try {
            // Get the map of properties in the request.
            HashMap<String, String> propertyMap = Utils.getPropertiesMap(meCodeValidationRequestDTO.getProperties());
            // Confirm verification code.
            userSelfRegistrationManager.confirmVerificationCodeMe(meCodeValidationRequestDTO.getCode(), propertyMap);
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client error while confirming verification code.", e);
            }
            Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return Response.accepted().build();
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

    private String getRecoveryScenarioFromProperties(List<PropertyDTO> propertyDTOS) {

        String recoveryScenario = null;
        Map<String, List<PropertyDTO>> filteredList =
                propertyDTOS.stream().collect(Collectors.groupingBy(PropertyDTO::getKey));

        if (!filteredList.containsKey(RECOVERY_SCENARIO_KEY) || filteredList.get(RECOVERY_SCENARIO_KEY).size() > 1) {
            return recoveryScenario;
        } else {
            recoveryScenario = filteredList.get(RECOVERY_SCENARIO_KEY).get(0).getValue();
        }

        if (RecoveryScenarios.ASK_PASSWORD.toString().equals(recoveryScenario) ||
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString().equals(recoveryScenario) ||
                RecoveryScenarios.SELF_SIGN_UP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) ||
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario)) {
            return recoveryScenario;
        }

        return recoveryScenario;
    }

    private NotificationResponseBean doResendConfirmationCode(String recoveryScenario,
                                                              NotificationResponseBean notificationResponseBean,
                                                              ResendCodeRequestDTO resendCodeRequestDTO) {

        UserRecoveryData userRecoveryData = null;
        // Currently this me/resend-code API supports resend code during mobile verification scenario only.
        if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario)) {
            userRecoveryData = Utils.getUserRecoveryData(resendCodeRequestDTO, recoveryScenario);
        }
        if (userRecoveryData == null) {
            return notificationResponseBean;
        }

        ResendConfirmationManager resendConfirmationManager = Utils.getResendConfirmationManager();
        if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_MOBILE_NUMBER.equals(userRecoveryData.getRecoveryStep())) {

            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE,
                    resendCodeRequestDTO);
        }

        return notificationResponseBean;
    }

    private NotificationResponseBean setNotificationResponseBean(ResendConfirmationManager resendConfirmationManager,
                                                                 String recoveryScenario, String recoveryStep,
                                                                 String notificationType,
                                                                 ResendCodeRequestDTO resendCodeRequestDTO) {

        NotificationResponseBean notificationResponseBean = null;
        try {
            notificationResponseBean =
                    resendConfirmationManager.resendConfirmationCode(Utils.getUser(resendCodeRequestDTO.getUser()),
                            recoveryScenario, recoveryStep, notificationType,
                            Utils.getProperties(resendCodeRequestDTO.getProperties()));
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        }

        return notificationResponseBean;
    }

    /**
     * Converts MeResendCodeRequestDTO to ResendCodeRequestDTO with user details from context.
     *
     * @param meResendCodeRequestDTO meResendCodeRequestDTO.
     * @return resendCodeRequestDTO.
     */
    private ResendCodeRequestDTO convertToResendCodeRequest(MeResendCodeRequestDTO meResendCodeRequestDTO) {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        if (meResendCodeRequestDTO != null) {
            resendCodeRequestDTO.setProperties(meResendCodeRequestDTO.getProperties());
        }
        resendCodeRequestDTO.setUser(getUser());
        return resendCodeRequestDTO;
    }

    /**
     * Form UserDTO using username and tenant from context.
     *
     * @return userDTO.
     */
    private UserDTO getUser() {

        String usernameFromContext = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        String tenantFromContext = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        UserDTO userDTO = new UserDTO();
        userDTO.setUsername(UserCoreUtil.removeDomainFromName(usernameFromContext));
        userDTO.setRealm(UserCoreUtil.extractDomainFromName(usernameFromContext));
        userDTO.setTenantDomain(tenantFromContext);
        return userDTO;
    }
}

