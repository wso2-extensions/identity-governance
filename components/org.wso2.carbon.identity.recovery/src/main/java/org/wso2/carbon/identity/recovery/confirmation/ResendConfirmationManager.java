/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.confirmation;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.dto.ResendConfirmationDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;

import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.util.HashMap;

/**
 * Generic Manager class which can be used to resend confirmation code for any recovery scenario and self registration
 * using a notification.
 */
public class ResendConfirmationManager {

    private static final Log log = LogFactory.getLog(ResendConfirmationManager.class);
    private static ResendConfirmationManager instance = new ResendConfirmationManager();

    private ResendConfirmationManager() {
    }

    public static ResendConfirmationManager getInstance() {
        return instance;
    }

    /**
     * Generic method to resend confirmation email for account recovery and self registration
     *
     * @param user             User who receives the confirmation email
     * @param recoveryScenario name of the recovery scenario (mandatory)
     *                         scenarios in org.wso2.carbon.identity.recovery.RecoveryScenarios
     * @param recoveryStep     name of the recovery step (mandatory)
     *                         steps in org.wso2.carbon.identity.recovery.RecoverySteps
     * @param notificationType email notification type (mandatory)
     *                         Ex: org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET
     * @param properties       other properties
     * @return notificationResponseBean
     * @throws org.wso2.carbon.identity.recovery.IdentityRecoveryException
     */
    public NotificationResponseBean resendConfirmationCode(User user, String recoveryScenario,
                                                           String recoveryStep, String notificationType,
                                                           Property[] properties) throws IdentityRecoveryException {

        return resendAccountRecoveryNotification(user, null, recoveryScenario, recoveryStep, notificationType,
                properties);
    }

    /**
     * Generic method to resend confirmation email for account recovery and self registration with provided
     * confirmation code validation
     *
     * @param user             User who receives the confirmation email
     * @param code             Previously issued confirmation code (mandatory)
     * @param recoveryScenario name of the recovery scenario (mandatory)
     *                         scenarios in org.wso2.carbon.identity.recovery.RecoveryScenarios
     * @param recoveryStep     name of the recovery step (mandatory)
     *                         steps in org.wso2.carbon.identity.recovery.RecoverySteps
     * @param notificationType email notification type (mandatory)
     *                         Ex: org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET
     * @param properties       other properties
     * @return notificationResponseBean
     * @throws IdentityRecoveryException
     */
    public NotificationResponseBean resendConfirmationCode(User user, String code, String recoveryScenario,
                                                           String recoveryStep, String notificationType,
                                                           Property[] properties) throws IdentityRecoveryException {

        if (StringUtils.isBlank(code)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CONFIRMATION_CODE_NOT_PROVIDED,
                    user.getUserName());
        }
        return resendAccountRecoveryNotification(user, code, recoveryScenario, recoveryStep, notificationType,
                properties);
    }

    /**
     * Resend confirmation information for the user bound to the resend code. The user will be notified via a channel
     * recovered from the recovery data of the user.
     *
     * @param tenantDomain         Tenant domain
     * @param resendCode           Previously issued confirmation code
     * @param recoveryScenario     Name of the recovery scenario
     *                             {@link org.wso2.carbon.identity.recovery.RecoveryScenarios}
     * @param recoveryStep         Name of the recovery step {@link org.wso2.carbon.identity.recovery.RecoverySteps}
     * @param notificationScenario Notification template name related to the recovery scenario (Eg: org.wso2.carbon
     *                             .identity.recovery.IdentityRecoveryConstants
     *                             .NOTIFICATION_TYPE_RESEND_PASSWORD_RESET
     * @param properties           Meta properties
     * @return ResendConfirmationDTO {@link ResendConfirmationDTO} bean resend operation information
     * @throws IdentityRecoveryException Error while sending confirmation info
     */
    public ResendConfirmationDTO resendConfirmation(String tenantDomain, String resendCode, String recoveryScenario,
                                                    String recoveryStep, String notificationScenario,
                                                    Property[] properties) throws IdentityRecoveryException {

        RecoverySteps step = RecoverySteps.getRecoveryStep(recoveryStep);
        RecoveryScenarios scenario = RecoveryScenarios.getRecoveryScenario(recoveryScenario);
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();

        // Get Recovery data.
        UserRecoveryData userRecoveryData = userAccountRecoveryManager
                .getUserRecoveryData(resendCode, RecoverySteps.RESEND_CONFIRMATION_CODE);
        User user = userRecoveryData.getUser();

        // Validate the tenant domain and the recovery scenario in the request.
        validateRequestAttributes(user, scenario, userRecoveryData.getRecoveryScenario(), tenantDomain, resendCode);
        validateCallback(properties, user.getTenantDomain());
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        String notificationChannel = validateNotificationChannel(userRecoveryData.getRemainingSetIds());

        String confirmationCode;
        UserRecoveryData confirmationCodeRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                scenario, step);
        /* Checking whether the existing confirmation code can be used based on the email confirmation code tolerance
           and the existing recovery details. */
        if (Utils.reIssueExistingConfirmationCode(confirmationCodeRecoveryData, notificationChannel)) {
            confirmationCode = confirmationCodeRecoveryData.getSecret();
        } else {
            userRecoveryDataStore.invalidate(user);
            confirmationCode = Utils.generateSecretKey(notificationChannel, user.getTenantDomain(), recoveryScenario);
            // Store new confirmation code.
            addRecoveryDataObject(confirmationCode, notificationChannel, scenario, step, user);
        }
        ResendConfirmationDTO resendConfirmationDTO = new ResendConfirmationDTO();

        // Notification needs to trigger if the notification channel is not equal to EXTERNAL.
        if (!NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
            String eventName = Utils.resolveEventName(notificationChannel);
            triggerNotification(user, notificationChannel, notificationScenario, confirmationCode, eventName,
                    properties);
        } else {
            resendConfirmationDTO.setExternalConfirmationCode(confirmationCode);
        }

        resendCode = generateResendCode(notificationChannel, scenario, userRecoveryData);
        resendConfirmationDTO.setNotificationChannel(notificationChannel);
        resendConfirmationDTO.setResendCode(resendCode);
        resendConfirmationDTO.setSuccessCode(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE.getCode());
        resendConfirmationDTO.setSuccessMessage(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE.getMessage());
        return resendConfirmationDTO;
    }

    /**
     * Validate the tenant domain and the recovery scenario in the request.
     *
     * @param recoveredUser         User recovered using the resend code
     * @param scenarioInRequest     Recovery scenario in the resend request
     * @param recoveredScenario     Recovery scenario related to the resend code
     * @param tenantDomainInRequest Tenant domain in the request
     * @param resendCode            Resend code
     * @throws IdentityRecoveryClientException Error in the resend request
     */
    private void validateRequestAttributes(User recoveredUser, RecoveryScenarios scenarioInRequest,
                                           Enum recoveredScenario,
                                           String tenantDomainInRequest, String resendCode)
            throws IdentityRecoveryClientException {

        if (!StringUtils.equals(tenantDomainInRequest, recoveredUser.getTenantDomain())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_TENANT_DOMAIN_MISS_MATCH_WITH_CONTEXT,
                    tenantDomainInRequest);
        }
        if (!scenarioInRequest.equals(recoveredScenario)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RESEND_CODE,
                    resendCode);
        }
    }

    /**
     * Trigger notification to send userName recovery information.
     *
     * @param user                User
     * @param notificationChannel Notification channel (Eg: EMAIL or SMS)
     * @param templateName        Notification Template name
     * @param code                Secret key
     * @param eventName           Event name
     * @param metaProperties      Meta properties to be send with the notification.
     * @throws IdentityRecoveryException Error while triggering notification.
     */
    private void triggerNotification(User user, String notificationChannel, String templateName, String code,
                                     String eventName, Property[] metaProperties) throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        if (StringUtils.isBlank(notificationChannel)) {
            notificationChannel = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        }
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, templateName);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    /**
     * Get the resend code.
     *
     * @param notificationChannel Notification channel
     * @param scenario            Recovery scenario
     * @param userRecoveryData    User Recovery data
     * @return Resend code
     * @throws IdentityRecoveryServerException Error while adding the resend code
     */
    private String generateResendCode(String notificationChannel, RecoveryScenarios scenario,
                                      UserRecoveryData userRecoveryData) throws IdentityRecoveryServerException {

        String resendCode = UUIDGenerator.generateUUID();
        /* Checking whether the existing confirmation code issued time is in the tolerance period. If so this code
           updates the existing RESEND_CONFIRMATION_CODE with the new one by not changing the TIME_CREATED. */
        if (Utils.reIssueExistingConfirmationCode(getResendConfirmationCodeData(userRecoveryData.getUser()),
                notificationChannel)){
            invalidateResendConfirmationCode(resendCode, notificationChannel, userRecoveryData);
            return resendCode;
        }
        addRecoveryDataObject(resendCode, notificationChannel, scenario, RecoverySteps.RESEND_CONFIRMATION_CODE,
                userRecoveryData.getUser());
        return resendCode;
    }


    /**
     * Retrieves the existing confirmation code details.
     *
     * @param user User object of the corresponding user that needs to resend the confirmation code.
     * @return An UserRecoveryData object with the confirmation data.
     * @throws IdentityRecoveryServerException will be thrown if there is any error.
     */
    private UserRecoveryData getResendConfirmationCodeData(User user) throws IdentityRecoveryServerException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        try {
            return userRecoveryDataStore.loadWithoutCodeExpiryValidation(
                    user, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY,
                    RecoverySteps.RESEND_CONFIRMATION_CODE);
        } catch (IdentityRecoveryException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_RETRIEVING_RECOVERY_DATA,
                    "Error Retrieving Recovery Data", e);
        }
    }

    /**
     * Invalidates the existing resend code and add the new resend code by not changing the existing resend code's
     * time created.
     *
     * @param resendCode New resend code that needs to be sent.
     * @param notificationChannel Channel that needs to send the recovery information.
     * @param userRecoveryData Existing resend code details.
     * @throws IdentityRecoveryServerException Will be thrown if there is any error.
     */
    private void invalidateResendConfirmationCode(String resendCode, String notificationChannel,
                                                  UserRecoveryData userRecoveryData)
            throws IdentityRecoveryServerException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        try {
            userRecoveryDataStore.invalidateWithoutChangeTimeCreated(userRecoveryData.getSecret(), resendCode,
                    RecoverySteps.RESEND_CONFIRMATION_CODE, notificationChannel);
        } catch (IdentityRecoveryException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_UPDATING_RECOVERY_DATA,
                    "Error Updating Recovery Data : RESEND_CONFIRMATION_CODE", e);
        }
    }

    /**
     * Add the notification channel recovery data to the store.
     *
     * @param secretKey        RecoveryId
     * @param recoveryData     Data to be stored as mata which are needed to evaluate the recovery data object
     * @param recoveryScenario Recovery scenario
     * @param recoveryStep     Recovery step
     * @param user             User object
     * @throws IdentityRecoveryServerException Error storing recovery data
     */
    private void addRecoveryDataObject(String secretKey, String recoveryData, RecoveryScenarios recoveryScenario,
                                       RecoverySteps recoveryStep, User user)
            throws IdentityRecoveryServerException {

        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

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

    /**
     * Validate the callback Url.
     *
     * @param properties   Properties
     * @param tenantDomain Tenant Domain
     * @throws IdentityRecoveryServerException Error validating the callback
     */
    private void validateCallback(Property[] properties, String tenantDomain) throws IdentityRecoveryServerException {

        String callbackURL = null;
        try {
            callbackURL = Utils.getCallbackURL(properties);
            if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, tenantDomain,
                    IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX)) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
            }
        } catch (URISyntaxException | UnsupportedEncodingException | IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }
    }

    /**
     * Validate the channel name in the recovery data.
     *
     * @param channel Channel in the user recovery data
     * @return Server supported channel name
     * @throws IdentityRecoveryClientException Invalid channel type
     */
    private String validateNotificationChannel(String channel) throws IdentityRecoveryClientException {

        try {
            return NotificationChannels.getNotificationChannel(channel).getChannelType();
        } catch (NotificationChannelManagerException e) {
            if (log.isDebugEnabled()) {
                log.debug("Unsupported Notification channel : " + channel, e);
            }
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_NOTIFICATION_CHANNEL, channel);
        }
    }

    /**
     * Resend account recovery information to the user.
     *
     * @param user             User object
     * @param code             Previous confirmation code
     * @param recoveryScenario Recovery scenario
     * @param recoveryStep     Recovery step
     * @param notificationType Notification type
     * @param properties       Event properties
     * @return NotificationResponseBean
     * @throws IdentityRecoveryException If an error occurred while sending notifications.
     */
    private NotificationResponseBean resendAccountRecoveryNotification(User user, String code, String recoveryScenario,
                                                                       String recoveryStep, String notificationType,
                                                                       Property[] properties)
            throws IdentityRecoveryException {

        validateRequestParameters(user, recoveryScenario, recoveryStep, notificationType);
        // Resolve the tenant domain and the userstore domain name of the user.
        resolveUserAttributes(user);

        boolean notificationInternallyManage = isNotificationInternallyManage(user, recoveryScenario);

        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.getRecoveryScenario(recoveryScenario));

        // Validate the previous confirmation code with the data retrieved by the user recovery information.
        validateWithOldConfirmationCode(code, recoveryScenario, recoveryStep, userRecoveryData);

        // Get the notification channel details stored in the remainingSetIds.
        String storedNotificationChannel = userRecoveryData.getRemainingSetIds();

        String preferredChannel = StringUtils.EMPTY;
        /* Having a not supported storedNotificationChannel implies that the particular recovery scenario does not store
        the notification channel in remainingSetIds column. In that case the notification channel should be EMAIL.*/
        if (isServerSupportedNotificationChannel(storedNotificationChannel)) {
            preferredChannel = storedNotificationChannel;
            if (!notificationInternallyManage) {
                preferredChannel = NotificationChannels.EXTERNAL_CHANNEL.getChannelType();
            }
        }
        if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario)) {
            preferredChannel = NotificationChannels.SMS_CHANNEL.getChannelType();
        }

        String secretKey;
        if (Utils.reIssueExistingConfirmationCode(userRecoveryData, preferredChannel)) {
            secretKey = userRecoveryData.getSecret();
        } else {
            // Invalid previous confirmation code.
            userRecoveryDataStore.invalidate(userRecoveryData.getSecret());
            secretKey = Utils.generateSecretKey(preferredChannel, user.getTenantDomain(), recoveryScenario);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                    .getRecoveryScenario(recoveryScenario), RecoverySteps.getRecoveryStep(recoveryStep));
            /* Notified channel is stored in remaining setIds for recovery purposes. Having a EMPTY preferred channel
            states that the notification channel should not be stored. */
            if (StringUtils.isNotBlank(preferredChannel)) {
                recoveryDataDO.setRemainingSetIds(preferredChannel);
                notificationResponseBean.setNotificationChannel(preferredChannel);
            }

            if (RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                    RecoverySteps.VERIFY_EMAIL.toString().equals(recoveryStep)) {
                String verificationPendingEmailClaimValue = userRecoveryData.getRemainingSetIds();
                properties = new Property[]{new Property(IdentityRecoveryConstants.SEND_TO,
                        verificationPendingEmailClaimValue)};
                recoveryDataDO.setRemainingSetIds(verificationPendingEmailClaimValue);
            } else if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario) &&
                    RecoverySteps.VERIFY_MOBILE_NUMBER.toString().equals(recoveryStep)) {
                String verificationPendingMobileNumber = userRecoveryData.getRemainingSetIds();
                properties = new Property[]{new Property(IdentityRecoveryConstants.SEND_TO,
                        verificationPendingMobileNumber)};
                recoveryDataDO.setRemainingSetIds(verificationPendingMobileNumber);
            }

            userRecoveryDataStore.store(recoveryDataDO);
        }

        if (notificationInternallyManage) {
            String eventName = resolveEventName(preferredChannel, user.getUserName(), user.getUserStoreDomain(),
                    user.getTenantDomain());
            triggerNotification(user, preferredChannel, notificationType, secretKey, eventName, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }
        return notificationResponseBean;
    }

    /**
     * Resolve the event name according to the notification channel.
     *
     * @param preferredChannel User preferred notification channel
     * @param userName         Username
     * @param domainName       Domain name
     * @param tenantDomain     Tenant domain name
     * @return Resolved event name
     */
    private String resolveEventName(String preferredChannel, String userName, String domainName, String tenantDomain) {

        String eventName;
        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(preferredChannel)) {
            eventName = IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + preferredChannel
                    + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX;
        } else {
            eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        }
        if (log.isDebugEnabled()) {
            String message = String
                    .format("For user : %1$s in domain : %2$s, notifications were sent from the event : %3$s",
                            domainName + CarbonConstants.DOMAIN_SEPARATOR + userName, tenantDomain, eventName);
            log.debug(message);
        }
        return eventName;
    }

    /**
     * Checks whether the given value is a sever supported notification channel.
     *
     * @param value Value
     * @return TRUE if the value is a sever supported notification channel.
     */
    private boolean isServerSupportedNotificationChannel(String value) {

        try {
            return StringUtils.isNotBlank(NotificationChannels.getNotificationChannel(value).getChannelType());
        } catch (NotificationChannelManagerException e) {
            if (log.isDebugEnabled()) {
                log.debug("The given value : " + value + " is not a supported notification channel", e);
            }
            return false;
        }
    }

    /**
     * Validate the input parameters in the request.
     *
     * @param user             User
     * @param recoveryScenario Recovery scenario
     * @param recoveryStep     Recovery step
     * @param notificationType Notification type
     * @throws IdentityRecoveryClientException If resend request does not have a recovery scenario or a recovery step.
     */
    private void validateRequestParameters(User user, String recoveryScenario, String recoveryStep,
                                           String notificationType)
            throws IdentityRecoveryClientException {

        if (user == null) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_OBJECT_NOT_FOUND.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_OBJECT_NOT_FOUND.getMessage(), null);
        }
        if (StringUtils.isBlank(recoveryScenario)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_RECOVERY_SCENARIO_NOT_PROVIDED,
                    user.getUserName());
        }
        if (StringUtils.isBlank(recoveryStep)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_RECOVERY_STEP_NOT_PROVIDED,
                    user.getUserName());
        }
        if (StringUtils.isBlank(notificationType)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_TYPE_NOT_PROVIDED,
                    user.getUserName());
        }
    }

    private boolean isNotificationInternallyManage(User user) throws IdentityRecoveryServerException {

        return Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                        user.getTenantDomain()));
    }

    private boolean isNotificationInternallyManage(User user, String recoveryScenario)
            throws IdentityRecoveryServerException {

        if (RecoveryScenarios.ASK_PASSWORD.toString().equals(recoveryScenario)) {
            return Boolean.parseBoolean(Utils.getSignUpConfigs
                    (IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                            user.getTenantDomain()));
        } else if (RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString().equals(recoveryScenario)) {
            return Boolean.parseBoolean(Utils.getSignUpConfigs
                    (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            user.getTenantDomain()));
        } else if (RecoveryScenarios.LITE_SIGN_UP.toString().equals(recoveryScenario)) {
            return Boolean.parseBoolean(Utils.getSignUpConfigs
                    (IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                            user.getTenantDomain()));
        } else if (RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario)) {
            // We manage the notifications internally for EMAIL_VERIFICATION_ON_UPDATE.
            return true;
        } else if (RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE.toString().equals(recoveryScenario)) {
            // We manage the notifications internally for MOBILE_VERIFICATION_ON_UPDATE.
            return true;
        } else {
            // To maintain the backward compatibility for the non-configurable flows.
            return isNotificationInternallyManage(user);
        }
    }

    /**
     * Resolve the tenant domain and the userstore domain of the user object.
     *
     * @param user User
     */
    private void resolveUserAttributes(User user) {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
            if (StringUtils.isBlank(tenantDomain)) {
                tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
            }
            user.setTenantDomain(tenantDomain);
            if (log.isDebugEnabled()) {
                log.debug("Tenant domain is not in the request. Set super tenant domain for user : " +
                        user.getUserName());
            }
        }
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            if (log.isDebugEnabled()) {
                log.debug("User store domain is not in the request. Set primary user store domain for user : " +
                        user.getUserName());
            }
        }
    }

    /**
     * Validate the previously issued code, recovery scenario and recovery step with the data in the UserRecoveryData
     * object.
     *
     * @param code             Previously issued code
     * @param recoveryScenario Recovery scenario
     * @param recoveryStep     Recovery step
     * @param userRecoveryData UserRecoveryData which holds recovery information.
     * @throws IdentityRecoveryClientException If code or recovery scenario or recovery step not matched with the
     *                                         data in UserRecoveryData object.
     */
    private void validateWithOldConfirmationCode(String code, String recoveryScenario, String recoveryStep,
                                                 UserRecoveryData userRecoveryData)
            throws IdentityRecoveryClientException {

        if (userRecoveryData == null || StringUtils.isBlank(userRecoveryData.getSecret()) ||
                !recoveryScenario.equals(userRecoveryData.getRecoveryScenario().toString()) ||
                !recoveryStep.equals(userRecoveryData.getRecoveryStep().toString())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_OLD_CODE_NOT_FOUND,
                    null);
        }
        // Validate the provided confirmation code with previously issued code.
        if (code != null && !userRecoveryData.getSecret().equals(code)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PROVIDED_CONFIRMATION_CODE_NOT_VALID, code);
        }
    }
}
