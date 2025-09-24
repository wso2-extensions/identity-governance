/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.user.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.ResendCodeApiService;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_SEND_OTP;
import static org.wso2.carbon.identity.recovery.util.Utils.getRecoveryConfigs;

/**
 * This class contains the implementation of Resend Code API Service.
 */
public class ResendCodeApiServiceImpl extends ResendCodeApiService {

    private static final List<String> askPasswordRecoveryScenarios =
            Arrays.asList(RecoveryScenarios.ASK_PASSWORD.toString(), RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP
                            .toString(), RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString());
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

        // Resolve the username using the login attribute when multi attribute login is enabled.
        ResolvedUserResult resolvedUserResult = FrameworkUtils.processMultiAttributeLoginIdentification(
                resendCodeRequestDTO.getUser().getUsername(), resendCodeRequestDTO.getUser().getTenantDomain());
        if (ResolvedUserResult.UserResolvedStatus.SUCCESS.equals(resolvedUserResult.getResolvedStatus())) {
            resendCodeRequestDTO.getUser().setUsername(
                    UserCoreUtil.removeDomainFromName(resolvedUserResult.getUser().getUsername()));
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

        if (notificationResponseBean == null) {
            ErrorDTO errorDTO = new ErrorDTO();
            errorDTO.setRef(Utils.getCorrelation());
            errorDTO.setMessage("User recovery data is not found. Please re-initiate the recovery flow.");
            return Response.status(Response.Status.BAD_REQUEST).entity(errorDTO).build();
        }

        //when notifications internally managed key might not be set.
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
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
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString().equals(recoveryScenario) ||
                RecoveryScenarios.SELF_SIGN_UP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.toString().equals(recoveryScenario) ||
                RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD.toString().equals(recoveryScenario)) {
            return recoveryScenario;
        }

        return recoveryScenario;
    }

    private NotificationResponseBean doResendConfirmationCode(String recoveryScenario,
                                                              NotificationResponseBean notificationResponseBean,
                                                              ResendCodeRequestDTO resendCodeRequestDTO) {

        UserRecoveryData userRecoveryData = Utils.getUserRecoveryData(resendCodeRequestDTO, recoveryScenario);
        ResendConfirmationManager resendConfirmationManager;

        if (userRecoveryData == null) {
            // If the recovery scenario is ASK_PASSWORD and the user's account state is pending ask password,
            // reinitiate "Ask password" flow even though recovery data is absent.
            if (askPasswordRecoveryScenarios.contains(recoveryScenario)
                    && isUserInPendingAskPasswordState(resendCodeRequestDTO.getUser())) {
                String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD;
                RecoverySteps recoveryStep = RecoverySteps.UPDATE_PASSWORD;
                if (RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString().equals(recoveryScenario)) {
                    notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_RESEND_EMAIL_OTP;
                    recoveryStep = RecoverySteps.SET_PASSWORD;
                } else if (RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString().equals(recoveryScenario)) {
                    notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_RESEND_SMS_OTP;
                    recoveryStep = RecoverySteps.SET_PASSWORD;
                }
                resendConfirmationManager = Utils.getResendConfirmationManager();
                notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                        recoveryScenario,
                        recoveryStep.toString(),
                        notificationType,
                        resendCodeRequestDTO);
            }
            // If the recovery scenario is EMAIL_VERIFICATION and the user's account state is pending email
            // verification, re-initiate "Email Verification" flow even though recovery data is absent.
            else if (RecoveryScenarios.EMAIL_VERIFICATION.toString().equals(recoveryScenario)
                    && isUserInPendingEmailVerificationState(resendCodeRequestDTO.getUser())) {
                resendConfirmationManager = Utils.getResendConfirmationManager();
                notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                        RecoveryScenarios.EMAIL_VERIFICATION.toString(),
                        RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.toString(),
                        IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM,
                        resendCodeRequestDTO);

                // Initial email verification flow can be triggered with SELF_SIGN_UP scenario.
                // Hence, remove user recovery data if it exists for SELF_SIGN_UP scenario.
                Utils.invalidateUserRecoveryData(resendCodeRequestDTO, RecoveryScenarios.SELF_SIGN_UP);
            } else if (RecoveryScenarios.EMAIL_VERIFICATION_OTP.toString().equals(recoveryScenario)
                    && isUserInPendingEmailVerificationState(resendCodeRequestDTO.getUser())) {
                resendConfirmationManager = Utils.getResendConfirmationManager();
                notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                        RecoveryScenarios.EMAIL_VERIFICATION_OTP.toString(),
                        RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.toString(),
                        IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM_OTP,
                        resendCodeRequestDTO);
            }

            return notificationResponseBean;
        }

        resendConfirmationManager = Utils.getResendConfirmationManager();
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

            boolean isSelfRegistrationEmailOTPEnabled;
            String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM;
            try {
                isSelfRegistrationEmailOTPEnabled = Boolean.parseBoolean(Utils.getSignUpConfigs(
                                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE,
                                resendCodeRequestDTO.getUser().getTenantDomain()));
                if (isSelfRegistrationEmailOTPEnabled) {
                    notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM_EMAIL_OTP;
                }
            } catch (IdentityRecoveryException e){
                LOG.error("Error while getting self registration send OTP in email configuration", e);
            }
            notificationResponseBean =  setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.SELF_SIGN_UP.toString(), RecoverySteps.CONFIRM_SIGN_UP.toString(),
                    notificationType, resendCodeRequestDTO);
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
        } else if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.equals(userRecoveryData.getRecoveryScenario())
                && RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.toString(),
                    RecoverySteps.UPDATE_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ADMIN_FORCED_PASSWORD_RESET_SMS_OTP,
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
        } else if (RecoveryScenarios.EMAIL_VERIFICATION.toString().equals(recoveryScenario) &&
                RecoveryScenarios.EMAIL_VERIFICATION.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.EMAIL_VERIFICATION.toString(),
                    RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM, resendCodeRequestDTO);
        } else if (RecoveryScenarios.EMAIL_VERIFICATION_OTP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.EMAIL_VERIFICATION_OTP.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.EMAIL_VERIFICATION_OTP.toString(),
                    RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM_OTP, resendCodeRequestDTO);
        } else if (RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_EMAIL.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString(), RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_VERIFY_EMAIL_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE
                        .equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_EMAIL.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                    RecoverySteps.VERIFY_EMAIL.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_VERIFY_EMAIL_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_MOBILE_NUMBER.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString().equals(recoveryScenario) &&
                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE
                        .equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_MOBILE_NUMBER.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE.toString()
                        .equals(recoveryScenario) &&
                RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE
                        .equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_MOBILE_NUMBER.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString()
                        .equals(recoveryScenario) &&
                RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE
                        .equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.VERIFY_MOBILE_NUMBER.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString(),
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE, resendCodeRequestDTO);
        } else if (RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.equals(userRecoveryData.getRecoveryScenario()) &&
                RecoverySteps.SET_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.toString(),
                    RecoverySteps.SET_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_RESEND_EMAIL_OTP,
                    resendCodeRequestDTO);
        } else if (RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString().equals(recoveryScenario) &&
                RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(userRecoveryData.getRecoveryScenario())
                && RecoverySteps.SET_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            notificationResponseBean = setNotificationResponseBean(resendConfirmationManager,
                    RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.toString(),
                    RecoverySteps.SET_PASSWORD.toString(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_RESEND_SMS_OTP,
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

    private NotificationResponseBean doResendConfirmationCodeForSelfSignUp(
            NotificationResponseBean notificationResponseBean, ResendCodeRequestDTO resendCodeRequestDTO) {

        UserSelfRegistrationManager userSelfRegistrationManager = Utils.getUserSelfRegistrationManager();
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
            Utils.handleInternalServerError(Constants.SERVER_ERROR,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
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

    /**
     * Determines if a user's account is in a state where they need to set a password
     * through the ask password flow.
     *
     * @param user User object containing user information.
     * @return true if the user is in pending ask password state, false otherwise.
     */
    private boolean isUserInPendingAskPasswordState(UserDTO user) {

        String domainQualifiedUsername = UserCoreUtil.addDomainToName(user.getUsername(), user.getRealm());
        String accountState = Utils.getAccountState(domainQualifiedUsername, user.getTenantDomain());
        if (StringUtils.isBlank(accountState)) {
            return false;
        }
        return IdentityRecoveryConstants.PENDING_ASK_PASSWORD.equals(accountState);
    }

    /**
     * Determines if a user's account is in a state where they need to verify their email.
     *
     * @param user User object containing user information.
     * @return true if the user is in pending email verification state, false otherwise.
     */
    private boolean isUserInPendingEmailVerificationState(UserDTO user) {

        String domainQualifiedUsername = UserCoreUtil.addDomainToName(user.getUsername(), user.getRealm());
        String accountState = Utils.getAccountState(domainQualifiedUsername, user.getTenantDomain());
        if (StringUtils.isBlank(accountState)) {
            return false;
        }
        return IdentityRecoveryConstants.PENDING_EMAIL_VERIFICATION.equals(accountState);
    }
}
