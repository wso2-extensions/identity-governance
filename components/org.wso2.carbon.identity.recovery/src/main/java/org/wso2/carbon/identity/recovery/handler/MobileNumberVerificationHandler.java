/*
 * Copyright (c) 2020-2025, WSO2 LLC. (http://www.wso2.com).
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
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.wso2.carbon.identity.recovery.util.Utils.maskIfRequired;

/**
 * This event handler is used to send a verification SMS when a claim update event to update the mobile number
 * is triggered.
 */
public class MobileNumberVerificationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(MobileNumberVerificationHandler.class);

    public String getName() {

        return "userMobileVerification";
    }

    public String getFriendlyName() {

        return "User Mobile Number Verification";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String eventName = event.getEventName();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.
                EventProperty.USER_STORE_MANAGER);
        User user = getUser((String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME),
                (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN),
                userStoreManager.getRealmConfiguration().
                        getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));

        if (IdentityEventConstants.Event.PRE_DELETE_USER_CLAIM.equals(eventName)) {
            /*
             * The `mobile` claim stores the user's primary mobile number, while `phoneVerified` flags
             * whether that mobile number has been verified. If the primary mobile number is deleted, its
             * verification flag should be removed as well.
             */
            String claim = (String) eventProperties.get(IdentityEventConstants.EventProperty.CLAIM_URI);
            if (IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM.equals(claim)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("Primary mobile claim removed for user '%s'. Removing " +
                            "corresponding 'phoneVerified' claim.", maskIfRequired(user.getUserName())));
                }
                setUserClaim(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM, StringUtils.EMPTY, userStoreManager,
                        user);
            }
            return;
        }

        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);
        if (claims == null) {
            claims = new HashMap<>();
        }

        boolean supportMultipleMobileNumbers =
                Utils.isMultiMobileNumbersPerUserEnabled(user.getTenantDomain(), user.getUserStoreDomain());

        boolean enable = isMobileVerificationOnUpdateEnabled(user.getTenantDomain());

        if (!supportMultipleMobileNumbers) {
            // Multiple mobile numbers per user support is disabled.
            log.debug("Supporting multiple mobile numbers per user is disabled.");
            claims.remove(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
            claims.remove(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
        }

        if (!enable) {
            // Mobile Number Verification feature is disabled.
            if (log.isDebugEnabled()) {
                log.debug("Mobile number verification handler is disabled in tenant: " + user.getTenantDomain() +
                        " for event: " + eventName);
            }
            /* We need to empty 'MOBILE_NUMBER_PENDING_VALUE_CLAIM' because having a value in that claim implies
            a verification is pending. But verification is not enabled anymore. */
            if (claims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM) ||
                    claims.containsKey(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM)) {
                invalidatePendingMobileVerification(user, userStoreManager, claims);
            }
            claims.remove(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM);
            claims.remove(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
            return;
        }

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            Utils.unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated();
            if (supportMultipleMobileNumbers && !claims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM)) {
                Utils.setThreadLocalIsOnlyVerifiedMobileNumbersUpdated(true);
            }
            preSetUserClaimOnMobileNumberUpdate(claims, userStoreManager, user);
            claims.remove(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM);
        }

        if (IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {
            postSetUserClaimOnMobileNumberUpdate(user, userStoreManager);
            claims.remove(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM);
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        int priority = super.getPriority(messageContext);
        if (priority == -1) {
            return 50;
        }
        return priority;
    }

    /**
     * Store verification details in the recovery data store and initiate notification.
     *
     * @param user                            User.
     * @param verificationPendingMobileNumber Updated mobile number that is pending verification.
     * @throws IdentityEventException
     */
    private void initNotificationForMobileNumberVerificationOnUpdate(User user, String verificationPendingMobileNumber)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        try {
            String otpTriggeredClaim;
            RecoveryScenarios recoveryScenario;
            boolean isProgressiveProfileVerification = Boolean.parseBoolean(String.valueOf(
                    IdentityUtil.threadLocalProperties.get().remove(
                            FrameworkConstants.IS_PROGRESSIVE_PROFILE_VERIFICATION)));
            String secretKey = Utils.generateSecretKey(NotificationChannels.SMS_CHANNEL.getChannelType(),
                    String.valueOf(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE), user.getTenantDomain(),
                    "UserClaimUpdate");

            UserRecoveryData recoveryDataDO;
            if (Utils.getThreadLocalIsOnlyVerifiedMobileNumbersUpdated()) {
                recoveryScenario = RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE;
                if (isProgressiveProfileVerification) {
                    recoveryScenario =
                            RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE;
                }
                userRecoveryDataStore.invalidate(user, recoveryScenario, RecoverySteps.VERIFY_MOBILE_NUMBER);
                recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario,
                        RecoverySteps.VERIFY_MOBILE_NUMBER);
                otpTriggeredClaim = IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM;
            } else {
                recoveryScenario = RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE;
                if (isProgressiveProfileVerification) {
                    recoveryScenario =
                            RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE;
                }
                userRecoveryDataStore.invalidate(user, recoveryScenario, RecoverySteps.VERIFY_MOBILE_NUMBER);
                recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario,
                        RecoverySteps.VERIFY_MOBILE_NUMBER);
                otpTriggeredClaim = IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM;
            }

            /* Mobile number is persisted in remaining set ids to maintain context information about the mobile number
            associated with the verification code generated. */
            recoveryDataDO.setRemainingSetIds(verificationPendingMobileNumber);
            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, secretKey, Utils.getArbitraryProperties(), verificationPendingMobileNumber,
                    recoveryDataDO);
            // Set the otp triggered claim to be used in the authentication flow.
            IdentityUtil.threadLocalProperties.get().put(
                    IdentityRecoveryConstants.CLAIM_FOR_PENDING_OTP_VERIFICATION, otpTriggeredClaim);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending notification to user: " +
                    user.toFullQualifiedUsername() + " for mobile verification on update.", e);
        }
    }

    /**
     * Trigger the SMS notification.
     *
     * @param user                            User.
     * @param code                            SMS OTP.
     * @param props                           Other properties.
     * @param verificationPendingMobileNumber Mobile number to which the SMS should be sent.
     * @throws IdentityRecoveryException
     */
    private void triggerNotification(User user, String code, Property[] props, String
            verificationPendingMobileNumber, UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {

        String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE;

        if (log.isDebugEnabled()) {
            log.debug("Sending: " + notificationType + " notification to user: " + user.toFullQualifiedUsername());
        }

        String eventName = Utils.resolveEventName(NotificationChannels.SMS_CHANNEL.getChannelType());
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL,
                NotificationChannels.SMS_CHANNEL.getChannelType());
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, notificationType);
        if (StringUtils.isNotBlank(verificationPendingMobileNumber)) {
            properties.put(IdentityRecoveryConstants.SEND_TO, verificationPendingMobileNumber);
        }
        if (props != null && props.length > 0) {
            for (Property prop : props) {
                properties.put(prop.getKey(), prop.getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
            properties.put(IdentityRecoveryConstants.OTP_TOKEN_STRING, code);
        }

        if (recoveryDataDO != null) {
            properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO,
                    recoveryDataDO.getRecoveryScenario().name());
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.toFullQualifiedUsername(), e);
        }
    }

    /**
     * Form User object from username, tenant domain, and user store domain.
     *
     * @param userName        UserName.
     * @param tenantDomain    Tenant Domain.
     * @param userStoreDomain User Domain.
     * @return User.
     */
    private User getUser(String userName, String tenantDomain, String userStoreDomain) {

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(userStoreDomain);
        return user;
    }

    /**
     * If the mobile claim is updated, set it to the 'MOBILE_NUMBER_PENDING_VALUE_CLAIM' claim.
     * Set thread local state to skip sending verification notification in inapplicable claim update scenarios.
     *
     * @param claims           Map of claims to be updated.
     * @param userStoreManager User store manager.
     * @param user             User.
     * @throws IdentityEventException
     */
    private void preSetUserClaimOnMobileNumberUpdate(Map<String, String> claims, UserStoreManager userStoreManager,
                                                     User user) throws IdentityEventException {

        if (MapUtils.isEmpty(claims)) {
            // Not required to handle in this handler.
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        if (IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
        (Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate())) {
            invalidatePendingMobileVerification(user, userStoreManager, claims);
            return;
        }

        /*
        Within the SMS OTP flow, the mobile number is updated in the user profile after successfully verifying the
        OTP. Therefore, the mobile number is already verified & no need to verify it again.
        */
        if (IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_SMS_OTP_FLOW.toString().
                equals(Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate())) {
            invalidatePendingMobileVerification(user, userStoreManager, claims);
            return;
        }

        if (Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate() != null) {
            Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        }

        boolean supportMultipleMobileNumbers =
                Utils.isMultiMobileNumbersPerUserEnabled(user.getTenantDomain(), user.getUserStoreDomain());

        // Update multiple mobile numbers only if they’re in the claims map.
        // This avoids issues with updating the primary mobile number due to user store limitations on multiple
        // mobile numbers.
        boolean shouldUpdateMultiMobilesRelatedClaims =
                claims.containsKey(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM) ||
                        claims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
        String multiAttributeSeparator = FrameworkUtils.getMultiAttributeSeparator();

        String mobileNumber = claims.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);

        /*
         * If the `mobile` claim (the primary number) is emptied — indicating its removal — the`phoneVerified` claim
         * (which flags verification) should also be cleared so the user profile does not display a deleted number
         * as verified.
         */
        if (StringUtils.EMPTY.equals(mobileNumber)) {
            claims.put(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM, StringUtils.EMPTY);
        }

        List<String> updatedVerifiedNumbersList = new ArrayList<>();
        List<String> updatedAllNumbersList;

        if (supportMultipleMobileNumbers) {
            List<String> exisitingVerifiedNumbersList = Utils.getMultiValuedClaim(userStoreManager, user,
                    IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
            updatedVerifiedNumbersList = claims.containsKey(IdentityRecoveryConstants.
                    VERIFIED_MOBILE_NUMBERS_CLAIM) ? getListOfMobileNumbersFromString(claims.get(
                    IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM)) : exisitingVerifiedNumbersList;

            List<String> exisitingAllNumbersList = Utils.getMultiValuedClaim(userStoreManager, user,
                    IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
            updatedAllNumbersList = claims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM) ?
                    getListOfMobileNumbersFromString(claims.get(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM)) :
                    exisitingAllNumbersList;

            /*
            Finds the verification pending mobile number and remove it from the verified numbers list in the payload.
            */
            if (mobileNumber == null && CollectionUtils.isNotEmpty(updatedVerifiedNumbersList)) {
                mobileNumber = getVerificationPendingMobileNumber(exisitingVerifiedNumbersList,
                        updatedVerifiedNumbersList);
                updatedVerifiedNumbersList.remove(mobileNumber);
            } else {
                /*
                 * When both primary mobile number and verified mobile numbers are provided, give the primary‑mobile
                 * number change the precedence; leave the updated verified‑mobile numbers list exactly as it exists
                 * in the user store.
                 */
                updatedVerifiedNumbersList = exisitingVerifiedNumbersList;
            }

            /*
            Finds the removed numbers from the existing mobile numbers list and remove them from the verified numbers
            list. As verified numbers list should not contain numbers that are not in the mobile numbers list.
            */
            if (updatedAllNumbersList != null) {
                updatedVerifiedNumbersList.removeIf(number -> !updatedAllNumbersList.contains(number));
            }

            if (shouldUpdateMultiMobilesRelatedClaims) {
                claims.put(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM,
                        String.join(multiAttributeSeparator, updatedAllNumbersList));
                claims.put(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM,
                        String.join(multiAttributeSeparator, updatedVerifiedNumbersList));
            }
        } else {
            updatedAllNumbersList = new ArrayList<>();
            claims.remove(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
            claims.remove(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
        }

        if (StringUtils.isBlank(mobileNumber)) {
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        String existingMobileNumber;
        String username = user.getUserName();
        try {
            existingMobileNumber = userStoreManager.getUserClaimValue(username, IdentityRecoveryConstants.
                    MOBILE_NUMBER_CLAIM, null);
        } catch (UserStoreException e) {
            String error = String.format("Error occurred while retrieving existing mobile number for user: %s in " +
                    "domain: %s and user store: %s", username, user.getTenantDomain(), user.getUserStoreDomain());
            throw new IdentityEventException(error, e);
        }

        if (supportMultipleMobileNumbers && updatedVerifiedNumbersList.contains(mobileNumber)) {
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                    IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                            .SKIP_ON_ALREADY_VERIFIED_MOBILE_NUMBERS.toString());
            claims.put(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM, Boolean.TRUE.toString());
            invalidatePendingMobileVerification(user, userStoreManager, claims);
            return;
        }

        if (StringUtils.equals(mobileNumber, existingMobileNumber)) {

            if (supportMultipleMobileNumbers && shouldUpdateMultiMobilesRelatedClaims &&
                    !updatedAllNumbersList.contains(existingMobileNumber)) {
                updatedAllNumbersList.add(existingMobileNumber);
                claims.put(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM,
                        String.join(multiAttributeSeparator, updatedAllNumbersList));
            }

            if (isPrimaryMobileVerified(userStoreManager, user)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The mobile number to be updated: %s is same as the existing mobile " +
                                    "number for user: %s in domain: %s and user store: %s. Hence an SMS OTP " +
                                    "verification will not be triggered.", maskIfRequired(mobileNumber),
                            maskIfRequired(username), user.getTenantDomain(), user.getUserStoreDomain()));
                }
                Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_EXISTING_MOBILE_NUM.toString());
                invalidatePendingMobileVerification(user, userStoreManager, claims);

                if (supportMultipleMobileNumbers && shouldUpdateMultiMobilesRelatedClaims &&
                        !updatedVerifiedNumbersList.contains(existingMobileNumber)) {
                    updatedVerifiedNumbersList.add(existingMobileNumber);
                    claims.put(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM,
                            String.join(multiAttributeSeparator, updatedVerifiedNumbersList));
                }
                return;
            }

            if (log.isDebugEnabled()) {
                log.debug(String.format("The mobile number to be updated: %s is same as the existing mobile " +
                                "number for user: %s in domain: %s and user store: %s. Yet the mobile number is not " +
                                "verified. Hence an SMS OTP verification will be triggered.",
                        maskIfRequired(mobileNumber), maskIfRequired(username), user.getTenantDomain(),
                        user.getUserStoreDomain()));
            }
        }
        /*
        When 'UseVerifyClaim' is enabled, the verification should happen only if the 'verifyMobile'
        temporary claim exists as 'true' in the claim list. If 'UseVerifyClaim' is disabled, no need to
        check for 'verifyMobile' claim.
         */
        if (Utils.isUseVerifyClaimEnabled() && !isVerifyMobileClaimAvailable(claims)) {
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            invalidatePendingMobileVerification(user, userStoreManager, claims);
            return;
        }
        claims.remove(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);

        if (isVerificationPendingMobileClaimConfigAvailable(user.getTenantDomain())) {
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, mobileNumber);
        }
    }

    /**
     * Convert comma separated list of mobile numbers to a list.
     *
     * @param mobileNumbers Comma separated list of mobile numbers.
     * @return List of mobile numbers.
     */
    private List<String> getListOfMobileNumbersFromString(String mobileNumbers) {

        String multiAttributeSeparator = FrameworkUtils.getMultiAttributeSeparator();
        return StringUtils.isBlank(mobileNumbers) ? new ArrayList<>() : new ArrayList<>(Arrays.asList(
                mobileNumbers.split(multiAttributeSeparator))).stream().map(String::trim).collect(Collectors.toList());
    }

    /**
     * Get the mobile number that is pending verification.
     *
     * @param existingVerifiedNumbersList List of existing verified mobile numbers.
     * @param updatedVerifiedNumbersList  List of updated verified mobile numbers.
     * @return Mobile number that is pending verification.
     */
    private String getVerificationPendingMobileNumber(List<String> existingVerifiedNumbersList,
                                                      List<String> updatedVerifiedNumbersList) throws
            IdentityEventException {

        Set<String> existingVerifiedNumbersSet = new HashSet<>(existingVerifiedNumbersList);
        String mobileNumber = null;

        for (String verificationPendingNumber : updatedVerifiedNumbersList) {
            if (!existingVerifiedNumbersSet.contains(verificationPendingNumber)) {
                if (mobileNumber == null) {
                    mobileNumber = verificationPendingNumber;
                } else {
                    throw new IdentityEventClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_VERIFY_MULTIPLE_MOBILE_NUMBERS.getCode(),
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_VERIFY_MULTIPLE_MOBILE_NUMBERS.getMessage());
                }
            }
        }
        return mobileNumber;
    }

    /**
     * Initiate notification sending process if the thread local is not set to skip verification process.
     *
     * @param user             User.
     * @param userStoreManager User store manager.
     * @throws IdentityEventException
     */
    private void postSetUserClaimOnMobileNumberUpdate(User user, UserStoreManager userStoreManager) throws
            IdentityEventException {

        try {
            String skipMobileNumVerificationOnUpdateState =
                    Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
            if (!IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                    (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants.
                    SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_EXISTING_MOBILE_NUM.toString().equals
                            (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString().equals
                            (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_SMS_OTP_FLOW.toString().equals
                            (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants.
                    SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_ALREADY_VERIFIED_MOBILE_NUMBERS.toString().
                    equals(skipMobileNumVerificationOnUpdateState)) {

                String verificationPendingMobileNumClaim = getVerificationPendingMobileNumValue(userStoreManager, user);

                if (StringUtils.isNotBlank(verificationPendingMobileNumClaim)) {
                    initNotificationForMobileNumberVerificationOnUpdate(user, verificationPendingMobileNumClaim);
                }
            }
        } finally {
            Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
            Utils.unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated();
        }
    }

    /**
     * Get the 'http://wso2.org/claims/identity/mobileNumber.pendingValue' claim value.
     *
     * @param userStoreManager User store manager.
     * @param user             User.
     * @return Claim value.
     * @throws IdentityEventException
     */
    private String getVerificationPendingMobileNumValue(UserStoreManager userStoreManager, User user) throws
            IdentityEventException {

        Map<String, String> verificationPendingMobileNumClaimMap;
        try {
            verificationPendingMobileNumClaimMap = userStoreManager.getUserClaimValues(user.getUserName(), new String[]{
                    IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM}, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error while retrieving verification pending mobile number claim value " +
                    "for user: " + user.toFullQualifiedUsername(), e);
        }

        if (MapUtils.isEmpty(verificationPendingMobileNumClaimMap)) {
            return null;
        }

        for (Map.Entry<String, String> entry : verificationPendingMobileNumClaimMap.entrySet()) {
            String pendingVerificationMobileNumClaimURI = entry.getKey();
            if (IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM
                    .equals(pendingVerificationMobileNumClaimURI)) {
                return entry.getValue();
            }
        }
        return null;
    }

    /**
     * Check whether mobile verification on update feature is enabled via connector configuration.
     *
     * @param userTenantDomain Tenant domain of the user.
     * @return True if the feature is enabled, false otherwise.
     * @throws IdentityEventException
     */
    private boolean isMobileVerificationOnUpdateEnabled(String userTenantDomain) throws IdentityEventException {

        return Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, userTenantDomain));
    }

    /**
     * Invalidate pending mobile number verification.
     *
     * @param user             User.
     * @param userStoreManager User store manager.
     * @param claims           User claims.
     * @throws IdentityEventException
     */
    private void invalidatePendingMobileVerification(User user, UserStoreManager userStoreManager,
                                                     Map<String, String> claims) throws IdentityEventException {

        if (isVerificationPendingMobileClaimConfigAvailable(user.getTenantDomain()) &&
                StringUtils.isNotBlank(getVerificationPendingMobileNumValue(userStoreManager, user))) {
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
            try {
                UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                userRecoveryDataStore.invalidate(user, RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER);
                userRecoveryDataStore.invalidate(user,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER);
            } catch (IdentityRecoveryException e) {
                throw new IdentityEventException("Error while invalidating previous mobile verification data " +
                        "from recovery store for user: " + user.toFullQualifiedUsername(), e);
            }
        }
    }

    /**
     * Check if the claims contain the temporary claim 'verifyMobile' and it is set to true.
     *
     * @param claims User claims.
     * @return True if 'verifyMobile' claim is available as true, false otherwise.
     */
    private boolean isVerifyMobileClaimAvailable(Map<String, String> claims) {

        return (claims.containsKey(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM) &&
                Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM)));
    }

    private boolean isVerificationPendingMobileClaimConfigAvailable(String tenantDomain) {

        UserRealm userRealm = getUserRealm(tenantDomain);
        ClaimManager claimManager = null;

        if (userRealm != null) {
            // Get claim manager for manipulating attributes.
            claimManager = getClaimManager(userRealm);
        }

        try {
            if (claimManager != null) {
                Claim claim = claimManager.getClaim(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM);
                if (claim != null) {
                    return true;
                }
            }
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            log.error("Error while looking for the pendingMobileNumber claim from claim manager " +
                    "in tenant: " + tenantDomain, e);
            return false;
        }
        return false;
    }

    private UserRealm getUserRealm(String tenantDomain) {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        if (realmService != null) {
            // Get tenant's user realm.
            try {
                int tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
                return realmService.getTenantUserRealm(tenantId);
            } catch (org.wso2.carbon.user.api.UserStoreException e) {
                log.error("Error while retrieving user realm in mobile verification handler for tenant domain: "
                        + tenantDomain, e);
            }
        }
        return null;
    }

    private ClaimManager getClaimManager(UserRealm userRealm) {

        try {
            return userRealm.getClaimManager();
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            log.error("Error while retrieving claim manager.", e);
        }
        return null;
    }

    /**
     * Sets the user claim value.
     *
     * @param claimName        Claim URI.
     * @param claimValue       Claim value to be set.
     * @param userStoreManager User store manager.
     * @param user             User.
     * @throws IdentityEventException If an error occurs while setting the user claim value.
     */
    private void setUserClaim(String claimName, String claimValue, UserStoreManager userStoreManager, User user)
            throws IdentityEventException {

        HashMap<String, String> userClaims = new HashMap<>();
        userClaims.put(claimName, claimValue);
        try {
            userStoreManager.setUserClaimValues(user.getUserName(), userClaims, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error while setting user claim value for user: %s",
                            maskIfRequired(user.getUserName())), e);
        }
    }

    /**
     * Check if the user's primary mobile is verified by checking the phoneVerified claim value.
     *
     * @param userStoreManager User store manager.
     * @param user             User.
     * @return true if the primary mobile number is verified, false otherwise.
     * @throws IdentityEventException if there is an error checking the claim value.
     */
    private boolean isPrimaryMobileVerified(UserStoreManager userStoreManager, User user)
            throws IdentityEventException {

        return Boolean.parseBoolean(
                Utils.getUserClaim(userStoreManager, user, IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM));
    }
}
