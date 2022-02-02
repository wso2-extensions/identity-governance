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
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.ResendCodeApiService;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.ws.rs.core.Response;

public class ResendCodeApiServiceImpl extends ResendCodeApiService {
    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);
    private static final String RECOVERY_SCENARIO_KEY = "RecoveryScenario";

    @Override
    public Response resendCodePost(ResendCodeRequestDTO resendCodeRequestDTO) {

        // Remove any empty properties if exists.
        List<PropertyDTO> properties = resendCodeRequestDTO.getProperties();
        properties.removeIf(property -> StringUtils.isEmpty(property.getKey()));

        String tenantFromContext = getTenantDomainFromContext();
        if (StringUtils.isNotBlank(tenantFromContext)) {
            resendCodeRequestDTO.getUser().setTenantDomain(tenantFromContext);
        }
        NotificationResponseBean notificationResponseBean = null;

        String recoveryScenario = getRecoveryScenarioFromProperties(resendCodeRequestDTO.getProperties());
        if (StringUtils.isBlank(recoveryScenario)) {
            notificationResponseBean = doResendConfirmationCodeForSelfSignUp(notificationResponseBean,
                    resendCodeRequestDTO);
        } else {
            notificationResponseBean = doResendConfirmationCode(recoveryScenario, notificationResponseBean,
                    resendCodeRequestDTO);
        }

        if (notificationResponseBean == null || StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.status(Response.Status.CREATED).build();
        }
        return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getKey()).build();
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
                RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD.toString().equals(recoveryScenario)) {
            return recoveryScenario;
        }

        return recoveryScenario;
    }

    private NotificationResponseBean doResendConfirmationCode(String recoveryScenario,
                                                              NotificationResponseBean notificationResponseBean,
                                                              ResendCodeRequestDTO resendCodeRequestDTO) {

        UserRecoveryData userRecoveryData = Utils.getUserRecoveryData(resendCodeRequestDTO, recoveryScenario);
        if (userRecoveryData == null) {
            return notificationResponseBean;
        }

        ResendConfirmationManager resendConfirmationManager = Utils.getResendConfirmationManager();
        if (RecoveryScenarios.ASK_PASSWORD.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ASK_PASSWORD.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ASK_PASSWORD.toString(), RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD, resendCodeRequestDTO);
        } else if (RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString().equals(recoveryScenario) &&
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_PASSWORD_RESET, resendCodeRequestDTO);
        } else if (RecoveryScenarios.SELF_SIGN_UP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.SELF_SIGN_UP.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.CONFIRM_SIGN_UP.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.SELF_SIGN_UP.toString(), RecoverySteps.CONFIRM_SIGN_UP.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM, resendCodeRequestDTO);
        } else if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.
                equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ADMIN_FORCED_PASSWORD_RESET,
                    resendCodeRequestDTO);
        } else if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.
                equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP,
                    resendCodeRequestDTO);
        } else if (RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD.toString().equals(recoveryScenario) &&
                RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
                notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD.toString(), RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_TENANT_REGISTRATION_CONFIRMATION, resendCodeRequestDTO);
        } else if (RecoveryScenarios.LITE_SIGN_UP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.LITE_SIGN_UP.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.CONFIRM_LITE_SIGN_UP.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.LITE_SIGN_UP.toString(), RecoverySteps.CONFIRM_LITE_SIGN_UP.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_LITE_USER_EMAIL_CONFIRM, resendCodeRequestDTO);
        } else if (RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_EMAIL.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString(), RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_VERIFY_EMAIL_ON_UPDATE, resendCodeRequestDTO);
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

    private NotificationResponseBean doResendConfirmationCodeForSelfSignUp(
            NotificationResponseBean notificationResponseBean, ResendCodeRequestDTO resendCodeRequestDTO) {

        UserSelfRegistrationManager userSelfRegistrationManager = Utils
                .getUserSelfRegistrationManager();
        try {
            notificationResponseBean = userSelfRegistrationManager.resendConfirmationCode(
                    Utils.getUser(resendCodeRequestDTO.getUser()),
                    Utils.getProperties(resendCodeRequestDTO.getProperties()));
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while resending self sign-up confirmation code ", e);
            }
            Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }

        return notificationResponseBean;
    }

    private String getTenantDomainFromContext() {

        String tenantDomain = null;
        if (IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT) != null) {
            tenantDomain = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);
        }

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        return tenantDomain;
    }

}
