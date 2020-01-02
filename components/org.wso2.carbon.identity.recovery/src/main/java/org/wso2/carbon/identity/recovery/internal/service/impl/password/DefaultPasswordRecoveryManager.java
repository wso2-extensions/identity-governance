/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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
package org.wso2.carbon.identity.recovery.internal.service.impl.password;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.dto.PasswordRecoverDTO;
import org.wso2.carbon.identity.recovery.dto.PasswordResetCodeDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryChannelInfoDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.ResendConfirmationDTO;
import org.wso2.carbon.identity.recovery.dto.SuccessfulPasswordResetDTO;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.services.password.PasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;

import java.util.ArrayList;
import java.util.Map;

/**
 * Class that implements the PasswordRecoveryManager.
 */
public class DefaultPasswordRecoveryManager implements PasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(DefaultPasswordRecoveryManager.class);

    /**
     * Get the username recovery information with available verified channel details.
     *
     * @param claims       User Claims
     * @param tenantDomain Tenant domain
     * @param properties   Meta properties
     * @return RecoveryInformationDTO {@link RecoveryInformationDTO} object that contains
     * recovery information of a  verified user
     * @throws IdentityRecoveryException Error while initiating password recovery
     */
    @Override
    public RecoveryInformationDTO initiate(Map<String, String> claims, String tenantDomain,
                                           Map<String, String> properties) throws IdentityRecoveryException {

        tenantDomain = resolveTenantDomain(tenantDomain);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        boolean isQuestionBasedRecoveryEnabled = isQuestionBasedRecoveryEnabled(tenantDomain);
        boolean isNotificationBasedRecoveryEnabled = isNotificationBasedRecoveryEnabled(tenantDomain);

        if (!isNotificationBasedRecoveryEnabled && !isQuestionBasedRecoveryEnabled) {
            if (log.isDebugEnabled()) {
                log.debug("User password recovery is not enabled for the tenant: " + tenantDomain);
            }
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_RECOVERY_NOT_ENABLED,
                    null);
        }
        // Get recovery channel information.
        RecoveryChannelInfoDTO recoveryChannelInfoDTO = userAccountRecoveryManager
                .retrieveUserRecoveryInformation(claims, tenantDomain, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY,
                        properties);
        RecoveryInformationDTO recoveryInformationDTO = new RecoveryInformationDTO();
        recoveryInformationDTO.setUsername(recoveryChannelInfoDTO.getUsername());
        // Do not add recovery channel information if Notification based recovery is not enabled.
        recoveryInformationDTO.setNotificationBasedRecoveryEnabled(isNotificationBasedRecoveryEnabled);
        if (isNotificationBasedRecoveryEnabled) {
            recoveryInformationDTO.setRecoveryChannelInfoDTO(recoveryChannelInfoDTO);
        }
        recoveryInformationDTO.setQuestionBasedRecoveryEnabled(isQuestionBasedRecoveryEnabled);
        recoveryInformationDTO.setNotificationBasedRecoveryEnabled(isNotificationBasedRecoveryEnabled);
        return recoveryInformationDTO;
    }

    /**
     * Verify the recovery code and send recovery information via channel which matches the given channel id.
     *
     * @param recoveryCode RecoveryId of the user
     * @param channelId    Channel Id of the user
     * @param tenantDomain Tenant Domain
     * @param properties   Meta properties in the recovery request
     * @return UsernameRecoverDTO {@link PasswordRecoverDTO} object that contains notified
     * channel details and success status code
     * @throws IdentityRecoveryException Error while notifying user
     */
    @Override
    public PasswordRecoverDTO notify(String recoveryCode, String channelId, String tenantDomain,
                                     Map<String, String> properties) throws IdentityRecoveryException {

        tenantDomain = resolveTenantDomain(tenantDomain);
        validateConfigurations(tenantDomain);
        int channelIDCode = validateChannelID(channelId);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();

        // Get Recovery data.
        UserRecoveryData userRecoveryData = userAccountRecoveryManager
                .getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION);
        String notificationChannel = extractNotificationChannelDetails(userRecoveryData.getRemainingSetIds(),
                channelIDCode);
        // Resolve notify status according to the notification channel of the user.
        boolean manageNotificationsInternally = true;
        if (IdentityRecoveryConstants.EXTERNAL_NOTIFICATION_CHANNEL.equals(notificationChannel)) {
            manageNotificationsInternally = false;
        }
        NotificationResponseBean notificationResponseBean = notifyUser(userRecoveryData.getUser(), notificationChannel,
                manageNotificationsInternally, properties);
        String secretKey = notificationResponseBean.getKey();
        String resendCode = generateResendCode(notificationChannel, userRecoveryData);
        return buildPasswordRecoveryResponseDTO(notificationChannel, secretKey, resendCode);
    }

    /**
     * Validate the confirmation code given for password recovery and return the password reset code.
     *
     * @param confirmationCode Confirmation code
     * @param tenantDomain     Tenant domain
     * @param properties       Meta properties in the confirmation request
     * @return PasswordResetCodeDTO {@link PasswordResetCodeDTO} object which contains password reset code
     * @throws IdentityRecoveryException Error while confirming password recovery
     */
    @Override
    public PasswordResetCodeDTO confirm(String confirmationCode, String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException {

        tenantDomain = resolveTenantDomain(tenantDomain);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        // Get Recovery data.
        UserRecoveryData userRecoveryData = userAccountRecoveryManager
                .getUserRecoveryData(confirmationCode, RecoverySteps.UPDATE_PASSWORD);
        if (!tenantDomain.equals(userRecoveryData.getUser().getTenantDomain())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_TENANT_DOMAIN_MISS_MATCH_WITH_CONTEXT,
                    tenantDomain);
        }
        String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(),
                userRecoveryData.getUser().getUserStoreDomain());
        if (log.isDebugEnabled()) {
            log.debug("Valid confirmation code for user: " + domainQualifiedName);
        }
        return buildPasswordResetCodeDTO(confirmationCode);
    }

    /**
     * Reset the password for password recovery, if the password reset code is valid.
     *
     * @param resetCode  Password reset code
     * @param password   New password
     * @param properties Properties
     * @return SuccessfulPasswordResetDTO {@link SuccessfulPasswordResetDTO} object which contain the information
     * for a successful password update
     * @throws IdentityRecoveryException Error while resetting the password
     */
    @Override
    public SuccessfulPasswordResetDTO reset(String resetCode, char[] password, Map<String, String> properties)
            throws IdentityRecoveryException {

        // Validate the password.
        if (ArrayUtils.isEmpty(password)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_PASSWORD_IN_REQUEST.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_PASSWORD_IN_REQUEST.getMessage(), null);
        }
        String newPassword = String.valueOf(password);
        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = NotificationPasswordRecoveryManager
                .getInstance();
        Property[] metaProperties = buildPropertyList(null, properties);
        try {
            notificationPasswordRecoveryManager.updatePassword(resetCode, newPassword, metaProperties);
        } catch (IdentityRecoveryServerException e) {
            String errorCode = Utils.prependOperationScenarioToErrorCode(e.getErrorCode(),
                    IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO);
            throw Utils.handleServerException(errorCode, e.getMessage(), null);
        } catch (IdentityRecoveryClientException e) {
            throw mapClientExceptionWithImprovedErrorCodes(e);
        } catch (IdentityEventException e) {
            if (log.isDebugEnabled()) {
                log.debug("DefaultPasswordRecoveryManager: Error while resetting password ", e);
            }
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_PASSWORD_RESET.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_PASSWORD_RESET.getMessage(),
                    null);
        }
        return buildSuccessfulPasswordUpdateDTO();
    }

    /**
     * Resend the password recovery information to the user via user specified channel.
     *
     * @param tenantDomain Tenant Domain
     * @param resendCode   Resend code
     * @param properties   Meta properties
     * @return ResendConfirmationDTO {@link ResendConfirmationDTO} which wraps the information for a successful
     * recovery information resend
     * @throws IdentityRecoveryException Error while sending recovery information
     */
    @Override
    public ResendConfirmationDTO resend(String tenantDomain, String resendCode, Map<String, String> properties)
            throws IdentityRecoveryException {

        tenantDomain = resolveTenantDomain(tenantDomain);
        Property[] metaProperties = buildPropertyList(null, properties);
        ResendConfirmationManager resendConfirmationManager = ResendConfirmationManager.getInstance();
        try {
            return resendConfirmationManager.resendConfirmation(tenantDomain, resendCode,
                    RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name(), RecoverySteps.UPDATE_PASSWORD.name(),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_PASSWORD_RESET, metaProperties);
        } catch (IdentityRecoveryException e) {
            e.setErrorCode(Utils.prependOperationScenarioToErrorCode(e.getErrorCode(),
                    IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO));
            throw e;
        }
    }

    /**
     * Map the client exceptions with the new API error codes and the scenarios.
     *
     * @param exception IdentityRecoveryClientException
     * @return IdentityRecoveryClientException
     */
    private IdentityRecoveryClientException mapClientExceptionWithImprovedErrorCodes(
            IdentityRecoveryClientException exception) {

        if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getCode()
                .equals(exception.getErrorCode())) {
            exception.setErrorCode(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_CODE.getCode());
        } else if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode()
                .equals(exception.getErrorCode())) {
            exception.setErrorCode(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_RECOVERY_CODE.getCode());
        } else if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_HISTORY_VIOLATE.getCode()
                .equals(exception.getErrorCode())) {
            exception.setErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_HISTORY_VIOLATION.getCode());
        } else if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode()
                .equals(exception.getErrorCode())) {
            exception.setErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_POLICY_VIOLATION.getCode());
        } else {
            exception.setErrorCode(Utils.prependOperationScenarioToErrorCode(exception.getErrorCode(),
                    IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO));
        }
        return Utils.handleClientException(exception.getErrorCode(), exception.getMessage(), null);
    }

    /**
     * Send recovery information to the user.
     *
     * @param user                         User
     * @param notificationChannel          Notification Channel
     * @param manageNotificationInternally Manage notifications internally
     * @param properties                   Meta properties
     * @return NotificationResponseBean
     * @throws IdentityRecoveryException Error while sending notifications
     */
    private NotificationResponseBean notifyUser(User user, String notificationChannel,
                                                boolean manageNotificationInternally, Map<String, String> properties)
            throws IdentityRecoveryException {

        Property[] metaProperties = buildPropertyList(notificationChannel, properties);
        NotificationResponseBean notificationResponseBean;
        try {
            notificationResponseBean = NotificationPasswordRecoveryManager
                    .getInstance().
                            sendRecoveryNotification(user, null, manageNotificationInternally, metaProperties);
        } catch (IdentityRecoveryException exception) {
            if (StringUtils.isNotEmpty(exception.getErrorCode())) {
                String errorCode = exception.getErrorCode();
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID.getCode()
                        .equals(errorCode)) {
                    exception.setErrorCode(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CALLBACK_PASSWORD_RESET
                                    .getCode());
                } else if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED.getCode()
                        .equals(errorCode)) {
                    exception.setErrorCode(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_PASSWORD_RESET
                                    .getCode());
                }
                exception.setErrorCode(Utils.prependOperationScenarioToErrorCode(exception.getErrorCode(),
                        IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO));
            }
            throw exception;
        }
        if (notificationResponseBean == null) {
            if (log.isDebugEnabled()) {
                log.debug("Empty Response while notifying password recovery information for user : " + user
                        .getUserName());
            }
            throw Utils
                    .handleServerException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_PASSWORD_RESET, null);
        }
        return notificationResponseBean;
    }

    /**
     * Build Property list using the meta properties map.
     *
     * @param notificationChannel Notification channel
     * @param properties          Map of properties
     * @return List of properties
     */
    private Property[] buildPropertyList(String notificationChannel, Map<String, String> properties) {

        ArrayList<Property> propertyArrayList = new ArrayList<>();
        // Add already existing meta properties.
        if (MapUtils.isNotEmpty(properties)) {
            for (String key : properties.keySet()) {
                if (StringUtils.isNotEmpty(key)) {
                    propertyArrayList.add(buildProperty(key, properties.get(key)));
                }
            }
        }
        // Add the notification channel property.
        if (StringUtils.isNotEmpty(notificationChannel)) {
            propertyArrayList.add(buildProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL_PROPERTY_KEY,
                    notificationChannel));
        }
        // Add the verified user property since the user is already verified and no need to validate the existence
        // again.
        propertyArrayList
                .add(buildProperty(IdentityRecoveryConstants.VERIFIED_USER_PROPERTY_KEY, Boolean.toString(true)));
        return propertyArrayList.toArray(new Property[0]);
    }

    /**
     * Build a property object.
     *
     * @param key   Key
     * @param value Value
     * @return Property object
     */
    private Property buildProperty(String key, String value) {

        Property property = new Property();
        property.setKey(key);
        property.setValue(value);
        return property;
    }

    /**
     * Build SuccessfulPasswordResetDTO response for a successful password update.
     *
     * @return SuccessfulPasswordResetDTO object
     */
    private SuccessfulPasswordResetDTO buildSuccessfulPasswordUpdateDTO() {

        SuccessfulPasswordResetDTO successfulPasswordResetDTO = new SuccessfulPasswordResetDTO();
        successfulPasswordResetDTO.setSuccessCode(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_SUCCESSFUL_PASSWORD_UPDATE.getCode());
        successfulPasswordResetDTO.setMessage(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_SUCCESSFUL_PASSWORD_UPDATE.getMessage());
        return successfulPasswordResetDTO;
    }

    /**
     * Validate the channel Id given in the reqest.
     *
     * @param channelId Channel Id in the request.
     * @return Channel Id
     * @throws IdentityRecoveryClientException Invalid channel Id
     */
    private int validateChannelID(String channelId) throws IdentityRecoveryClientException {

        int id;
        // Check whether the channel Id is an int.
        try {
            id = Integer.parseInt(channelId);
        } catch (NumberFormatException e) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        // Channel id needs to be larger than 0.
        if (id < 1) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        return id;
    }

    /**
     * Build the PasswordResetCodeDTO for successful verification.
     *
     * @param resetCode Password reset code
     * @return PasswordResetCodeDTO
     */
    private PasswordResetCodeDTO buildPasswordResetCodeDTO(String resetCode) {

        PasswordResetCodeDTO passwordResetCodeDTO = new PasswordResetCodeDTO();
        passwordResetCodeDTO.setPasswordResetCode(resetCode);
        return passwordResetCodeDTO;
    }

    /**
     * Build PasswordRecoverDTO for notifying password recovery details successfully.
     *
     * @param notificationChannel Notified channel
     * @param confirmationCode    Confirmation code for confirm recovery
     * @param resendCode          Code to resend recovery confirmation code
     * @return PasswordRecoverDTO object
     */
    private PasswordRecoverDTO buildPasswordRecoveryResponseDTO(String notificationChannel, String confirmationCode,
                                                                String resendCode) {

        PasswordRecoverDTO passwordRecoverDTO = new PasswordRecoverDTO();
        passwordRecoverDTO.setNotificationChannel(notificationChannel);
        if (IdentityRecoveryConstants.EXTERNAL_NOTIFICATION_CHANNEL.equals(notificationChannel)) {
            passwordRecoverDTO.setConfirmationCode(confirmationCode);
            passwordRecoverDTO.setCode(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_EXTERNALLY_NOTIFIED
                            .getCode());
            passwordRecoverDTO.setMessage(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_EXTERNALLY_NOTIFIED
                            .getMessage());
        } else {
            passwordRecoverDTO.setConfirmationCode(StringUtils.EMPTY);
            passwordRecoverDTO.setResendCode(resendCode);
            passwordRecoverDTO.setCode(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_INTERNALLY_NOTIFIED
                            .getCode());
            passwordRecoverDTO.setMessage(
                    IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_PASSWORD_RECOVERY_INTERNALLY_NOTIFIED
                            .getMessage());
        }
        return passwordRecoverDTO;
    }

    /**
     * Extract the channel that matches the channelId from the channels stored in recovery data.
     *
     * @param recoveryChannels All available recovery channels
     * @param channelId        User preferred channelId
     * @throws IdentityRecoveryException Invalid channelId
     */
    private String extractNotificationChannelDetails(String recoveryChannels, int channelId)
            throws IdentityRecoveryException {

        String[] channels = recoveryChannels.split(IdentityRecoveryConstants.NOTIFY_CHANNEL_LIST_SEPARATOR);
        if (channels.length < channelId) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null);
        }
        String notificationChannel = channels[channelId - 1];
        String[] channelDetails = notificationChannel.split(IdentityRecoveryConstants.CHANNEL_ATTRIBUTE_SEPARATOR);
        return channelDetails[0];
    }

    /**
     * Check whether recovery is enabled.
     *
     * @param tenantDomain Tenant domain
     * @throws IdentityRecoveryException Error with the configurations
     */
    private void validateConfigurations(String tenantDomain) throws IdentityRecoveryException {

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                        tenantDomain));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_RECOVERY_WITH_NOTIFICATIONS_NOT_ENABLED,
                    null);
        }
    }

    /**
     * Resolve the tenant domain in the request.
     *
     * @param tenantDomain Tenant domain in the request
     * @return Resolved tenant domain
     */
    private String resolveTenantDomain(String tenantDomain) {

        if (StringUtils.isBlank(tenantDomain)) {
            if (log.isDebugEnabled()) {
                log.debug("Tenant domain is not in the request. Therefore, domain set to : "
                        + MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            }
            return MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }
        return tenantDomain;
    }

    /**
     * Check whether challenge question based recovery is enabled for the tenant.
     *
     * @param tenantDomain Tenant domain
     * @return True if challenge question based recovery is enabled
     * @throws IdentityRecoveryServerException Error reading configs for the tenant
     */
    private boolean isQuestionBasedRecoveryEnabled(String tenantDomain) throws IdentityRecoveryServerException {

        // Check whether the challenge question based recovery is enabled.
        try {
            return Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                            tenantDomain));
        } catch (IdentityRecoveryServerException e) {
            // Prepend scenario to the thrown exception.
            String errorCode = Utils
                    .prependOperationScenarioToErrorCode(IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO,
                            e.getErrorCode());
            throw Utils.handleServerException(errorCode, e.getMessage(), null);
        }
    }

    /**
     * Check whether notification based recovery is enabled for the tenant.
     *
     * @param tenantDomain Tenant domain
     * @return True if challenge question based recovery is enabled
     * @throws IdentityRecoveryServerException Error reading configs for the tenant
     */
    private boolean isNotificationBasedRecoveryEnabled(String tenantDomain) throws IdentityRecoveryServerException {

        // Check whether the challenge question based recovery is enabled.
        try {
            return Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                            tenantDomain));
        } catch (IdentityRecoveryServerException e) {
            // Prepend scenario to the thrown exception.
            String errorCode = Utils
                    .prependOperationScenarioToErrorCode(IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO,
                            e.getErrorCode());
            throw Utils.handleServerException(errorCode, e.getMessage(), null);
        }
    }

    /**
     * Get the resend code.
     *
     * @param notificationChannel Notification channel
     * @param userRecoveryData    User Recovery data
     * @return Resend code
     * @throws IdentityRecoveryServerException Error while adding the resend code
     */
    private String generateResendCode(String notificationChannel, UserRecoveryData userRecoveryData)
            throws IdentityRecoveryServerException {

        String resendCode = UUIDGenerator.generateUUID();
        User user = userRecoveryData.getUser();
        addRecoveryDataObject(user.getUserName(), user.getTenantDomain(), resendCode, notificationChannel);
        return resendCode;
    }

    /**
     * Add the notification channel recovery data to the store.
     *
     * @param userName     Username
     * @param tenantDomain Tenant domain
     * @param secretKey    RecoveryId
     * @param recoveryData Data to be stored as mata which are needed to evaluate the recovery data object
     * @throws IdentityRecoveryServerException Error storing recovery data
     */
    private void addRecoveryDataObject(String userName, String tenantDomain, String secretKey, String recoveryData)
            throws IdentityRecoveryServerException {

        // Create a user object.
        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(IdentityUtil.extractDomainFromName(userName));
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.RESEND_CONFIRMATION_CODE);
        // Store available channels in remaining setIDs.
        recoveryDataDO.setRemainingSetIds(recoveryData);
        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_STORING_RECOVERY_DATA,
                    "Error Storing Recovery Data", e);
        }
    }
}
