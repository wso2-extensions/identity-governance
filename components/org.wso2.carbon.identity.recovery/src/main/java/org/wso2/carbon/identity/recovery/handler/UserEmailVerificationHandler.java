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
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_SEND_OTP;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages
        .ERROR_CODE_VERIFICATION_EMAIL_NOT_FOUND;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.FLOW_TYPE;
import static org.wso2.carbon.identity.recovery.util.Utils.getRecoveryConfigs;
import static org.wso2.carbon.identity.recovery.util.Utils.maskIfRequired;

public class UserEmailVerificationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserEmailVerificationHandler.class);

    private static final Random RANDOM = new SecureRandom();

    public String getName() {

        return "userEmailVerification";
    }

    public String getFriendlyName() {

        return "User Email Verification";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String eventName = event.getEventName();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(
                IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        User user = getUser(eventProperties, userStoreManager);

        if (IdentityEventConstants.Event.PRE_DELETE_USER_CLAIM.equals(eventName)) {
            /*
             * The `emailAddress` claim stores the user’s primary email address, while `emailVerified` flags
             * whether that address has been verified. If the primary email address is deleted, its verification flag
             * should be removed as well.
             */
            String claim = (String) eventProperties.get(IdentityEventConstants.EventProperty.CLAIM_URI);
            if (IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM.equals(claim)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("Primary email address claim removed for user '%s'. Removing " +
                            "corresponding 'emailVerified' claim.", maskIfRequired(user.getUserName())));
                }
                setUserClaim(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, StringUtils.EMPTY, userStoreManager, user);
            }
            return;
        }

        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);
        if (claims == null) {
            claims = new HashMap<>();
        }

        boolean supportMultipleEmails =
                Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(user.getTenantDomain(), user.getUserStoreDomain());

        boolean enable = false;

        if (IdentityEventConstants.Event.PRE_ADD_USER.equals(eventName) ||
                IdentityEventConstants.Event.POST_ADD_USER.equals(eventName)) {
            enable = Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                    .ENABLE_EMAIL_VERIFICATION, user.getTenantDomain()));
        } else if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName) ||
                IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {

            enable = isEmailVerificationOnUpdateEnabled(user.getTenantDomain());

            if (!supportMultipleEmails) {
                if (log.isDebugEnabled()) {
                    log.debug("Supporting multiple email addresses per user is disabled.");
                }
                claims.remove(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
                claims.remove(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
            }

            if (!enable) {
                /* We need to empty 'EMAIL_ADDRESS_PENDING_VALUE_CLAIM' because having a value in that claim implies
                a verification is pending. But verification is not enabled anymore. */
                if (claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM)) {
                    if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
                        sendNotificationToExistingEmailOnEmailUpdate(
                                user, userStoreManager, claims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM),
                                IdentityRecoveryConstants.NOTIFICATION_TYPE_NOTIFY_EMAIL_UPDATE_WITHOUT_VERIFICATION);
                    }
                    invalidatePendingEmailVerification(user, userStoreManager, claims);
                }
                claims.remove(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
                claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
            }
        }

        if (!enable) {
            // Email Verification feature is disabled.
            if (log.isDebugEnabled()) {
                log.debug("Email verification Handler is disabled in tenant: " + user.getTenantDomain() + "for " +
                        "event: " + eventName);
            }
            return;
        }

        String[] roleList = (String[]) eventProperties.get(IdentityEventConstants.EventProperty.ROLE_LIST);

        if (roleList != null) {
            List<String> roles = Arrays.asList(roleList);
            if (roles.contains(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                //This is a self signup request. Will be handled in self signup handler
                return;
            }
        }

        if (IdentityEventConstants.Event.PRE_ADD_USER.equals(eventName)) {
            Utils.clearEmailVerifyTemporaryClaim();
            if (claims == null || claims.isEmpty()) {
                // Not required to handle in this handler.
                return;
            } else if (claims.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM) &&
                    Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM))) {
                if (!claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM)
                        || StringUtils.isBlank(claims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM))) {
                    throw new IdentityEventClientException(ERROR_CODE_VERIFICATION_EMAIL_NOT_FOUND.getCode(),
                            ERROR_CODE_VERIFICATION_EMAIL_NOT_FOUND.getMessage());
                }
                Claim claim = new Claim();
                claim.setClaimUri(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
                claim.setValue(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));
                Utils.setEmailVerifyTemporaryClaim(claim);
                claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
                Utils.publishRecoveryEvent(eventProperties, IdentityEventConstants.Event.PRE_VERIFY_EMAIL_CLAIM,
                        null);
            } else {
                return;
                // Not required to handle in this handler.
            }
        }

        if (IdentityEventConstants.Event.POST_ADD_USER.equals(eventName)) {

            boolean isAccountLockOnCreation = Boolean.parseBoolean(Utils.getConnectorConfig
                    (IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION, user.getTenantDomain()));
            boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getConnectorConfig
                    (IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                            user.getTenantDomain()));

            Claim claim = Utils.getEmailVerifyTemporaryClaim();
            boolean isAccountClaimExist = Utils.isAccountStateClaimExisting(user.getTenantDomain());

            if (claim == null) {
                return;
                // Not required to handle in this handler.
            } else if (IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM.equals(claim.getClaimUri())) {
                String confirmationCode = UUID.randomUUID().toString();
                if (isNotificationInternallyManage) {
                    if (isAccountClaimExist) {
                        setUserClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                                IdentityRecoveryConstants.PENDING_EMAIL_VERIFICATION, userStoreManager, user);
                    }
                    String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM;
                    RecoveryScenarios recoveryScenario = RecoveryScenarios.SELF_SIGN_UP;
                    RecoverySteps recoveryStep = RecoverySteps.CONFIRM_SIGN_UP;
                    try {
                        boolean isSendEmailOTPEnabled =
                                Boolean.parseBoolean(getRecoveryConfigs(EMAIL_VERIFICATION_SEND_OTP,
                                        user.getTenantDomain()));
                        if (isSendEmailOTPEnabled) {
                            notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM_OTP;
                            recoveryScenario = RecoveryScenarios.EMAIL_VERIFICATION_OTP;
                            recoveryStep = RecoverySteps.CONFIRM_PENDING_EMAIL_VERIFICATION;
                        }
                    } catch (IdentityRecoveryServerException e) {
                        throw new IdentityEventException("Error while checking send OTP in email for email " +
                                "verification.", e);
                    }
                    initNotification(user, recoveryScenario, recoveryStep, notificationType);
                }

                // Need to lock user account.
                if (isAccountLockOnCreation) {
                    lockAccount(user, userStoreManager);
                    setUserClaim(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                            IdentityMgtConstants.LockedReason.PENDING_EMAIL_VERIFICATION.toString(),
                            userStoreManager, user);
                }
                Utils.publishRecoveryEvent(eventProperties, IdentityEventConstants.Event.POST_VERIFY_EMAIL_CLAIM,
                        confirmationCode);
            }
            Utils.clearEmailVerifyTemporaryClaim();
        }

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            Utils.unsetThreadLocalIsOnlyVerifiedEmailAddressesUpdated();
            if (supportMultipleEmails && !claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM)) {
                Utils.setThreadLocalIsOnlyVerifiedEmailAddressesUpdated(true);
            }
            preSetUserClaimsOnEmailUpdate(claims, userStoreManager, user);
            claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        }

        if (IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {
            postSetUserClaimsOnEmailUpdate(user, userStoreManager);
            claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        }
    }

    private boolean isEmailVerificationOnUpdateEnabled(String tenantDomain) throws IdentityEventException {

        return Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_EMAIL_VERIFICATION_ON_UPDATE, tenantDomain));
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 65;
    }

    public void lockAccount(User user, UserStoreManager userStoreManager) throws IdentityEventException {

        if (log.isDebugEnabled()) {
            log.debug("Locking user account:" + user.getUserName());
        }
        setUserClaim(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString(), userStoreManager, user);
    }

    protected void initNotification(User user, Enum recoveryScenario, Enum recoveryStep, String notificationType)
            throws IdentityEventException {

        try {
            String secretKey = Utils.generateSecretKey(NotificationChannels.EMAIL_CHANNEL.getChannelType(),
                    recoveryScenario.name(), user.getTenantDomain(), "EmailVerification");
            initNotification(user, recoveryScenario, recoveryStep, notificationType, secretKey);
        } catch (IdentityRecoveryServerException e) {
            throw new IdentityEventException("Error while fetching the OTP pattern ", e);
        }
    }

    protected void initNotification(User user, Enum recoveryScenario, Enum recoveryStep, String notificationType,
                                    String secretKey) throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, notificationType, secretKey, Utils.getArbitraryProperties(), recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending  notification ", e);
        }
    }

    private void initNotificationForEmailVerificationOnUpdate(String verificationPendingEmailAddress, User user)
            throws IdentityEventException {

        try {
            String secretKey = Utils.generateSecretKey(NotificationChannels.EMAIL_CHANNEL.getChannelType(),
                    RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE.name(), user.getTenantDomain(),
                    "UserClaimUpdate");
            initNotificationForEmailVerificationOnUpdate(user, secretKey, verificationPendingEmailAddress);
        } catch (IdentityRecoveryServerException e) {
            throw new IdentityEventException("Error while fetching the OTP pattern ", e);
        }
    }

    private void initNotificationForEmailVerificationOnUpdate(User user, String secretKey,
                                                              String verificationPendingEmailAddress)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            UserRecoveryData recoveryDataDO;
            if (Utils.getThreadLocalIsOnlyVerifiedEmailAddressesUpdated()) {
                userRecoveryDataStore.invalidate(user, RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_EMAIL);
                recoveryDataDO = new UserRecoveryData(user, secretKey,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL);
            } else {
                userRecoveryDataStore.invalidate(user, RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_EMAIL);
                recoveryDataDO = new UserRecoveryData(user, secretKey,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL);
            }
            /* Email address persisted in remaining set ids to maintain context information about the email address
            associated with the verification code generated. */
            recoveryDataDO.setRemainingSetIds(verificationPendingEmailAddress);
            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE, secretKey,
                    Utils.getArbitraryProperties(), verificationPendingEmailAddress, recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending notification for user: " +
                    user.toFullQualifiedUsername(), e);
        }
    }

    protected void invalidateRecoveryData(User user)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while invalidate recovery data for user :" + user.toString(), e);
        }
    }

    protected void setRecoveryData(User user, Enum recoveryScenario, Enum recoveryStep, String secretKey)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while setting recovery data for user ", e);
        }
    }

    protected UserRecoveryData getRecoveryData(User user) throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData recoveryData;
        try {
            recoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while loading recovery data for user ", e);
        }
        return recoveryData;
    }

    protected void setUserClaim(String claimName, String claimValue, UserStoreManager userStoreManager, User user)
            throws IdentityEventException {

        HashMap<String, String> userClaims = new HashMap<>();
        userClaims.put(claimName, claimValue);
        try {
            userStoreManager.setUserClaimValues(user.getUserName(), userClaims, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error while setting user claim value :" + user.getUserName(), e);
        }

    }

    protected void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

        triggerNotification(user, type, code, props, null);
    }

    protected void triggerNotification(User user, String type, String code, Property[] props,
                                       UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {

        triggerNotification(user, type, code, props, null, recoveryDataDO);
    }

    private void triggerNotification(User user, String type, String code, Property[] props, String
            verificationPendingEmailAddress, UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Sending : " + type + " notification to user : " + user.toString());
        }

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        String serviceProviderUUID = (String) IdentityUtil.threadLocalProperties.get()
                .get(IdentityRecoveryConstants.Consent.SERVICE_PROVIDER_UUID);

        HashMap<String, Object> properties = new HashMap<>();
        if (serviceProviderUUID != null && !serviceProviderUUID.isEmpty()) {
            properties.put(IdentityRecoveryConstants.Consent.SERVICE_PROVIDER_UUID, serviceProviderUUID);
        }
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (StringUtils.isNotBlank(verificationPendingEmailAddress)) {
            properties.put(IdentityRecoveryConstants.SEND_TO, verificationPendingEmailAddress);
        }

        if (props != null && props.length > 0) {
            for (Property prop : props) {
                properties.put(prop.getKey(), prop.getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        try {
            String selectedNotificationType = type;
            if (recoveryDataDO != null) {
                String recoveryScenario = recoveryDataDO.getRecoveryScenario().name();
                properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);

                if (RecoveryScenarios.ASK_PASSWORD.toString().equals(recoveryScenario)) {
                    selectedNotificationType = getNotificationTypeForAskPassword(user, type, recoveryScenario, properties);
                }
            }
            properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, selectedNotificationType);
            Event identityMgtEvent = new Event(eventName, properties);
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
    }

    private static String getNotificationTypeForAskPassword(User user, String type, String recoveryScenario,
                                                            HashMap<String, Object> properties)
            throws IdentityEventException {

        Boolean isDynamicAskPwdEnabled;
        try {
            isDynamicAskPwdEnabled = FlowMgtConfigUtils.getFlowConfig(Flow.Name.INVITED_USER_REGISTRATION.name(),
                    user.getTenantDomain()).getIsEnabled();
        } catch (FlowMgtServerException e) {
            throw new IdentityEventException("Error while retrieving the flow configuration for " +
                    "INVITED_USER_REGISTRATION flow.", e);
        }
        if (isDynamicAskPwdEnabled) {
            type = IdentityRecoveryConstants.NOTIFICATION_TYPE_ORCHESTRATED_ASK_PASSWORD;
            properties.put(FLOW_TYPE, Flow.Name.INVITED_USER_REGISTRATION.name());
        }
        return type;
    }

    protected User getUser(Map eventProperties, UserStoreManager userStoreManager) {

        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(domainName);
        return user;
    }

    private void preSetUserClaimsOnEmailUpdate(Map<String, String> claims, UserStoreManager userStoreManager,
                                               User user) throws IdentityEventException {

        if (MapUtils.isEmpty(claims)) {
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants.
                    SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        if (IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                (Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate())) {
            // Not required to handle in this handler.
            return;
        }

        /*
        Within the Email OTP flow, the email address is updated in the user profile after successfully verifying the
        OTP. Therefore, the email is already verified & no need to verify it again.
         */
        if (IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_EMAIL_OTP_FLOW.toString().equals
                (Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate())) {
            invalidatePendingEmailVerification(user, userStoreManager, claims);
            return;
        }

        if (claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM)) {
            sendNotificationToExistingEmailOnEmailUpdate(
                    user, userStoreManager, claims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM),
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_NOTIFY_EMAIL_ON_UPDATE);
        }

        if (Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate() != null) {
            Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
        }

        boolean supportMultipleEmails =
                Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(user.getTenantDomain(), user.getUserStoreDomain());
        // Update multiple email address related claims only if they’re in the claims map.
        // This avoids issues with updating the primary email address due to user store limitations on multiple
        // email addresses.
        boolean shouldUpdateMultiMobilesRelatedClaims =
                claims.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM) ||
                        claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);

        String multiAttributeSeparator = FrameworkUtils.getMultiAttributeSeparator();

        String emailAddress = claims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);

        /*
         * If the `emailaddress` claim (the primary email) is emptied — indicating its removal — the`emailVerified`
         * claim (which flags verification) should also be cleared so the user profile does not display a deleted
         * email as verified.
         */
        if (StringUtils.EMPTY.equals(emailAddress)) {
            claims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, StringUtils.EMPTY);
        }

        List<String> updatedVerifiedEmailAddresses = new ArrayList<>();
        List<String> updatedAllEmailAddresses;
        String primaryEmail = getEmailClaimValue(user, userStoreManager);

        // Handle email addresses and verified email addresses claims.
        if (supportMultipleEmails) {
            List<String> existingVerifiedEmailAddresses = Utils.getMultiValuedClaim(userStoreManager, user,
                IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
            List<String> existingAllEmailAddresses = Utils.getMultiValuedClaim(userStoreManager, user,
                    IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);

            if (isPrimaryEmailVerified(userStoreManager, user)) {
                // Add primary email address to verifiedEmailAddress claim if not available.
                if (existingAllEmailAddresses.contains(primaryEmail) &&
                        !existingVerifiedEmailAddresses.contains(primaryEmail)) {
                    if (log.isDebugEnabled()) {
                        log.debug("Added primary email to verifiedEmailAddresses claim for user: " +
                                maskIfRequired(user.getUserName()));
                    }
                    existingVerifiedEmailAddresses.add(primaryEmail);
                }
            }

            updatedVerifiedEmailAddresses = claims.containsKey(IdentityRecoveryConstants.
                    VERIFIED_EMAIL_ADDRESSES_CLAIM) ? getListOfEmailAddressesFromString(claims.get(
                    IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM)) : existingVerifiedEmailAddresses;
            updatedAllEmailAddresses = claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM) ?
                    getListOfEmailAddressesFromString(claims.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM)) :
                    existingAllEmailAddresses;

            if (updatedAllEmailAddresses == null) {
                updatedAllEmailAddresses = new ArrayList<>();
            }
            if (updatedVerifiedEmailAddresses == null) {
                updatedVerifiedEmailAddresses = new ArrayList<>();
            }

            // Find the verification pending email address and remove it from verified email addresses in the payload.
            if (emailAddress == null && CollectionUtils.isNotEmpty(updatedVerifiedEmailAddresses)) {
                emailAddress = getVerificationPendingEmailAddress(existingVerifiedEmailAddresses,
                        updatedVerifiedEmailAddresses);
                updatedVerifiedEmailAddresses.remove(emailAddress);
            } else {
                /*
                 * When both primary email address and verified email addresses are provided, give the primary‑email
                 * change the precedence; leave the updated verified‑emails list exactly as it exists in the
                 * user store.
                 */
                updatedVerifiedEmailAddresses = existingVerifiedEmailAddresses;
            }

            /*
            Find the removed numbers from the existing email addresses list and remove them from the verified email
            addresses list, as verified email addresses list should not contain email addresses that are not in the
            email addresses list.
            */
            final List<String> tempUpdatedAllEmailAddresses = new ArrayList<>(updatedAllEmailAddresses);
            updatedVerifiedEmailAddresses.removeIf(number -> !tempUpdatedAllEmailAddresses.contains(number));

            if (shouldUpdateMultiMobilesRelatedClaims) {
                claims.put(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM,
                        StringUtils.join(updatedVerifiedEmailAddresses, multiAttributeSeparator));
                claims.put(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM,
                        StringUtils.join(updatedAllEmailAddresses, multiAttributeSeparator));
            }
        } else {
            /*
            email addresses and verified email addresses should not be updated when support for multiple email
            addresses is disabled.
             */
            claims.remove(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
            claims.remove(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
            updatedAllEmailAddresses = new ArrayList<>();
        }

        if (StringUtils.isBlank(emailAddress)) {
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        if (supportMultipleEmails && updatedVerifiedEmailAddresses.contains(emailAddress)) {
            if (log.isDebugEnabled()) {
                log.debug(String.format("The email address to be updated: %s is already verified and contains" +
                        " in the verified email addresses list for user: %s in domain %s. Hence an email " +
                        "verification will not be triggered.", maskIfRequired(emailAddress),
                        maskIfRequired(user.getUserName()), user.getTenantDomain()));
            }
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_ALREADY_VERIFIED_EMAIL_ADDRESSES.toString());
            claims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
            invalidatePendingEmailVerification(user, userStoreManager, claims);
            return;
        }

        if (emailAddress.equals(primaryEmail)) {

            if (supportMultipleEmails && shouldUpdateMultiMobilesRelatedClaims &&
                    !updatedAllEmailAddresses.contains(primaryEmail)) {
                updatedAllEmailAddresses.add(primaryEmail);
                claims.put(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM,
                        StringUtils.join(updatedAllEmailAddresses, multiAttributeSeparator));
            }

            if (isPrimaryEmailVerified(userStoreManager, user)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The email address to be updated: %s is same as the existing email " +
                            "address for user: %s in domain %s. Hence an email verification will not be " +
                            "triggered.", maskIfRequired(emailAddress), maskIfRequired(user.getUserName()),
                            user.getTenantDomain()));
                }
                Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipEmailVerificationOnUpdateStates.SKIP_ON_EXISTING_EMAIL.toString());
                invalidatePendingEmailVerification(user, userStoreManager, claims);

                if (supportMultipleEmails && shouldUpdateMultiMobilesRelatedClaims &&
                        !updatedVerifiedEmailAddresses.contains(primaryEmail)) {
                    updatedVerifiedEmailAddresses.add(primaryEmail);
                    claims.put(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM,
                            StringUtils.join(updatedVerifiedEmailAddresses, multiAttributeSeparator));
                }
                return;
            }

            if (log.isDebugEnabled()) {
                log.debug(String.format("The email address to be updated: %s is same as the existing email " +
                        "address for user: %s in domain %s. Yet the email is not verified. " +
                        "Hence an email verification will be triggered.", maskIfRequired(emailAddress),
                        maskIfRequired(user.getUserName()), user.getTenantDomain()));
            }
        }

        /*
        When 'UseVerifyClaim' is enabled, the verification should happen only if the 'verifyEmail' temporary
        claim exists as 'true' in the claim list. If 'UseVerifyClaim' is disabled, no need to check for
        'verifyEmail' claim.
         */
        if (Utils.isUseVerifyClaimEnabled() && !isVerifyEmailClaimAvailable(claims)) {
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            invalidatePendingEmailVerification(user, userStoreManager, claims);
            return;
        }
        claims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM, emailAddress);
        claims.remove(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);
    }

    /**
     * Get the email address that is pending verification.
     *
     * @param existingVerifiedEmailAddresses List of existing verified email addresses.
     * @param updatedVerifiedEmailAddresses  List of updated verified email addresses.
     * @return email address that is pending verification.
     */
    private String getVerificationPendingEmailAddress(List<String> existingVerifiedEmailAddresses,
                                                      List<String> updatedVerifiedEmailAddresses) throws
            IdentityEventException {

        String emailAddress = null;
        for (String verificationPendingEmailAddress : updatedVerifiedEmailAddresses) {
            if (existingVerifiedEmailAddresses.stream().noneMatch(email ->
                    email.trim().equalsIgnoreCase(verificationPendingEmailAddress.trim()))) {
                if (emailAddress == null) {
                    emailAddress = verificationPendingEmailAddress;
                } else {
                    throw new IdentityEventClientException(IdentityRecoveryConstants.ErrorMessages.
                            ERROR_CODE_VERIFY_MULTIPLE_EMAILS.getCode(), IdentityRecoveryConstants.
                            ErrorMessages.ERROR_CODE_VERIFY_MULTIPLE_EMAILS.getMessage());
                }
            }
        }
        return emailAddress;
    }

    /**
     * Convert comma separated list of email addresses to a list.
     *
     * @param emails Comma separated list of mobile numbers.
     * @return List of email addresses.
     */
    private List<String> getListOfEmailAddressesFromString(String emails) {

        String multiAttributeSeparator = FrameworkUtils.getMultiAttributeSeparator();
        return emails != null ? new LinkedList<>(Arrays.asList(emails.split(multiAttributeSeparator))).stream()
                .map(String::trim).collect(Collectors.toList()) : new ArrayList<>();
    }

    private void postSetUserClaimsOnEmailUpdate(User user, UserStoreManager userStoreManager) throws
            IdentityEventException {

        try {
            String skipEmailVerificationOnUpdateState = Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate();
            if (!IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                    (skipEmailVerificationOnUpdateState) && !IdentityRecoveryConstants.
                    SkipEmailVerificationOnUpdateStates.SKIP_ON_EXISTING_EMAIL.toString().equals
                            (skipEmailVerificationOnUpdateState) && !IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString().equals
                            (skipEmailVerificationOnUpdateState) && !IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_EMAIL_OTP_FLOW.toString().equals
                            (skipEmailVerificationOnUpdateState) && !IdentityRecoveryConstants.
                    SkipEmailVerificationOnUpdateStates.SKIP_ON_ALREADY_VERIFIED_EMAIL_ADDRESSES.toString().equals(
                            skipEmailVerificationOnUpdateState)) {

                String pendingVerificationEmailClaimValue = getPendingVerificationEmailValue(userStoreManager, user);

                if (StringUtils.isNotBlank(pendingVerificationEmailClaimValue)) {
                    initNotificationForEmailVerificationOnUpdate(pendingVerificationEmailClaimValue, user);
                }
            }
        } finally {
            Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
            Utils.unsetThreadLocalIsOnlyVerifiedEmailAddressesUpdated();
        }
    }

    private String getPendingVerificationEmailValue(UserStoreManager userStoreManager, User user) throws
            IdentityEventException {

        Map<String, String> verificationPendingEmailClaimMap;
        try {
            verificationPendingEmailClaimMap = userStoreManager.getUserClaimValues(user.getUserName(), new String[]{
                    IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM}, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error while retrieving verification pending email claim value for user: "
                    + user.toFullQualifiedUsername(), e);
        }

        if (MapUtils.isEmpty(verificationPendingEmailClaimMap)) {
            return null;
        }

        for (Map.Entry<String, String> entry : verificationPendingEmailClaimMap.entrySet()) {
            String pendingVerificationEmailClaimURI = entry.getKey();
            if (pendingVerificationEmailClaimURI
                    .equals(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM)) {
                return entry.getValue();
            }
        }
        return null;
    }

    /**
     * Invalidate pending email verification.
     *
     * @param user             User.
     * @param userStoreManager User store manager.
     * @param claims           User claims.
     * @throws IdentityEventException
     */
    private void invalidatePendingEmailVerification(User user, UserStoreManager userStoreManager,
                                                    Map<String, String> claims) throws IdentityEventException {

        if (StringUtils.isNotBlank(getPendingVerificationEmailValue(userStoreManager, user))) {
            claims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
            try {
                UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                userRecoveryDataStore.invalidate(user, RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_EMAIL);
            } catch (IdentityRecoveryException e) {
                throw new IdentityEventException("Error while invalidating previous email verification data " +
                        "from recovery store for user: " + user.toFullQualifiedUsername(), e);
            }
        }
    }

    /**
     * Check if the claims contain the temporary claim 'verifyEmail' and it is set to true.
     *
     * @param claims User claims.
     * @return True if 'verifyEmail' claim is available as true, false otherwise.
     */
    private boolean isVerifyEmailClaimAvailable(Map<String, String> claims) {

        return (claims.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM) &&
                Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM)));
    }

    /**
     * Send an email notification to existing email when a request is made to update the email address with either
     * email verification on update feature enabled or not.
     *
     * @param user             User.
     * @param userStoreManager UserStoreManager.
     * @param newEmailAddress  The new email address provided the user to update the existing email address.
     *                         When the email verification on update feature is enabled, this variable contains the
     *                         verification pending email.
     * @param templateType     Email template type.
     * @throws IdentityEventException IdentityEventException.
     */
    private void sendNotificationToExistingEmailOnEmailUpdate(User user, UserStoreManager userStoreManager,
                                                              String newEmailAddress, String templateType) throws IdentityEventException {
        boolean enable = Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_NOTIFICATION_ON_EMAIL_UPDATE, user.getTenantDomain()));
        if (!enable) {
            if (log.isDebugEnabled()) {
                log.debug("Notify existing email on update feature is disabled for tenant: " + user.getTenantDomain());
            }
            return;
        }
        // Get existing email address.
        String existingEmail = getEmailClaimValue(user, userStoreManager);
        if (StringUtils.isBlank(existingEmail) || existingEmail.equals(newEmailAddress)) {
            log.debug("Old email in not available for user : " + user.toFullQualifiedUsername() + ". " +
                    "Terminated the notification sending process to existing email.");
            return;
        }
        HashMap<String, String> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        if (IdentityRecoveryConstants.NOTIFICATION_TYPE_NOTIFY_EMAIL_ON_UPDATE.equals(templateType)) {
            properties.put(IdentityRecoveryConstants.VERIFICATION_PENDING_EMAIL, newEmailAddress);
        } else if (IdentityRecoveryConstants.NOTIFICATION_TYPE_NOTIFY_EMAIL_UPDATE_WITHOUT_VERIFICATION.
                equals(templateType)) {
            properties.put(IdentityRecoveryConstants.NEW_EMAIL_ADDRESS, newEmailAddress);
        }
        triggerEmailNotificationToExistingEmail(existingEmail, templateType, user, properties);
    }

    /**
     * Get user email claim value.
     *
     * @param user             User.
     * @param userStoreManager UserStoreManager.
     * @return String user email address.
     * @throws IdentityRecoveryException Error retrieving email address.
     */
    private String getEmailClaimValue(User user, UserStoreManager userStoreManager) throws IdentityEventException {

        String email = StringUtils.EMPTY;
        if (user != null && userStoreManager != null) {
            String username = user.getUserName();
            if (StringUtils.isNotBlank(user.getUserStoreDomain())) {
                username = IdentityUtil.addDomainToName(username, user.getUserStoreDomain());
            }
            try {
                email = userStoreManager.getUserClaimValue(username, IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM,
                        null);
            } catch (UserStoreException e) {
                String error = String.format("Error occurred while retrieving existing email address for user: " +
                        "%s in tenant domain : %s", username, user.getTenantDomain());
                throw new IdentityEventException(error, e);
            }
        }
        return email;
    }

    /**
     * Trigger a notification to the existing email address when the user attempts to update the existing email
     * address.
     *
     * @param sendTo       Send to email address.
     * @param templateType Email template type.
     * @param user         User.
     * @param props        Other properties.
     * @throws IdentityEventException IdentityEventException while sending notification to user.
     */
    private void triggerEmailNotificationToExistingEmail(String sendTo, String templateType, User user, Map<String,
            String> props) throws IdentityEventException {

        if (log.isDebugEnabled()) {
            log.debug("Sending : " + templateType + " notification to user : " + user.toFullQualifiedUsername());
        }
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityRecoveryConstants.SEND_TO, sendTo);
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, templateType);

        if (CollectionUtils.size(props) > 0) {
            properties.putAll(props);
        }
        Event identityMgtEvent = new Event(IdentityEventConstants.Event.TRIGGER_NOTIFICATION, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw new IdentityEventException("Error while sending notification for user: " +
                    user.toFullQualifiedUsername(), e);
        }
    }

    /**
     * Check if the user's primary email is verified by checking the emailVerified claim value.
     *
     * @param userStoreManager The user store manager.
     * @param user             The user.
     * @return true if the primary email is verified, false otherwise.
     * @throws IdentityEventException if there is an error checking the claim value.
     */
    protected boolean isPrimaryEmailVerified(UserStoreManager userStoreManager, User user)
            throws IdentityEventException {

        return Boolean.parseBoolean(
                Utils.getUserClaim(userStoreManager, user, IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM));
    }
}
