/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
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
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UserEmailVerificationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserEmailVerificationHandler.class);

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

        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);

        boolean enable = false;
        if (IdentityEventConstants.Event.PRE_ADD_USER.equals(eventName) ||
                IdentityEventConstants.Event.POST_ADD_USER.equals(eventName)) {
            enable = Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                    .ENABLE_EMAIL_VERIFICATION, user.getTenantDomain()));
        } else if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName) ||
                IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {
            enable = Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                    .ENABLE_EMAIL_VERIFICATION_ON_UPDATE, user.getTenantDomain()));
            if (!enable) {
                /* We need to empty 'EMAIL_ADDRESS_PENDING_VALUE_CLAIM' because having a value in that claim implies
                a verification is pending. But verification is not enabled anymore. */
                if (claims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM)) {
                    invalidatePendingEmailVerification(user, userStoreManager, claims);
                }
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
            } else if (claims.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM) && Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM))) {
                Claim claim = new Claim();
                claim.setClaimUri(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
                claim.setValue(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));
                Utils.setEmailVerifyTemporaryClaim(claim);
                claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);

            } else if (claims.containsKey(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM) && Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM))) {
                Claim claim = new Claim();
                claim.setClaimUri(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);
                claim.setValue(claims.get(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM));
                Utils.setEmailVerifyTemporaryClaim(claim);
                claims.remove(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);

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
                if (isNotificationInternallyManage) {
                    if (isAccountClaimExist) {
                        setUserClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                                IdentityRecoveryConstants.PENDING_EMAIL_VERIFICATION, userStoreManager, user);
                    }
                    initNotification(user, RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP, IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM.toString());
                }

                // Need to lock user account.
                if (isAccountLockOnCreation) {
                    lockAccount(user, userStoreManager);
                    setUserClaim(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                            IdentityMgtConstants.LockedReason.PENDING_EMAIL_VERIFICATION.toString(),
                            userStoreManager, user);
                }
            } else if (IdentityRecoveryConstants.ASK_PASSWORD_CLAIM.equals(claim.getClaimUri())) {
                if (isNotificationInternallyManage) {
                    if (isAccountClaimExist) {
                        setUserClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                                IdentityRecoveryConstants.PENDING_ASK_PASSWORD, userStoreManager, user);
                    }
                    initNotification(user, RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD,
                            IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD.toString());
                }
                // Need to lock user account.
                if (isAccountLockOnCreation) {
                    lockAccount(user, userStoreManager);
                    setUserClaim(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                            IdentityMgtConstants.LockedReason.PENDING_ASK_PASSWORD.toString(),
                            userStoreManager, user);
                }
            }
        }

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            preSetUserClaimsOnEmailUpdate(claims, userStoreManager, user);
            claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        }

        if (IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {
            postSetUserClaimsOnEmailUpdate(user, userStoreManager);
            claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        }
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

        String secretKey = UUIDGenerator.generateUUID();
        initNotification(user, recoveryScenario, recoveryStep, notificationType, secretKey);
    }

    protected void initNotification(User user, Enum recoveryScenario, Enum recoveryStep, String notificationType,
                                    String secretKey) throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, notificationType, secretKey, Utils.getArbitraryProperties());
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending  notification ", e);
        }
    }

    private void initNotificationForEmailVerificationOnUpdate(String verificationPendingEmailAddress, User user)
            throws IdentityEventException {

        String secretKey = UUIDGenerator.generateUUID();
        initNotificationForEmailVerificationOnUpdate(user, secretKey, verificationPendingEmailAddress);
    }

    private void initNotificationForEmailVerificationOnUpdate(User user, String secretKey,
                                                              String verificationPendingEmailAddress)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user, RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                    RecoverySteps.VERIFY_EMAIL);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                    RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL);
            /* Email address persisted in remaining set ids to maintain context information about the email address
            associated with the verification code generated. */
            recoveryDataDO.setRemainingSetIds(verificationPendingEmailAddress);
            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_EMAIL_ON_UPDATE, secretKey,
                    Utils.getArbitraryProperties(), verificationPendingEmailAddress);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending notification for user: " +
                    user.toFullQualifiedUsername(), e);
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

    private void triggerNotification(User user, String type, String code, Property[] props, String
            verificationPendingEmailAddress) throws IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Sending : " + type + " notification to user : " + user.toString());
        }

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
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
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
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

        if (IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                (Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate())) {
            // Not required to handle in this handler.
            return;
        }

        if (Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate() != null) {
            Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
        }

        if (MapUtils.isEmpty(claims)) {
            // Not required to handle in this handler.
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants.
                    SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        String emailAddress = claims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM);

        if (StringUtils.isNotBlank(emailAddress)) {

            String existingEmail;
            String username = user.getUserName();
            try {
                existingEmail = userStoreManager.getUserClaimValue(username,
                        IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, null);
            } catch (UserStoreException e) {
                String error = String.format("Error occurred while retrieving existing email address for user: %s in " +
                        "domain : %s", username, user.getTenantDomain());
                throw new IdentityEventException(error, e);
            }

            if (emailAddress.equals(existingEmail)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The email address to be updated: %s is same as the existing email " +
                            "address for user: %s in domain %s. Hence an email verification will not be " +
                            "triggered.", emailAddress, username, user.getTenantDomain()));
                }
                Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipEmailVerificationOnUpdateStates.SKIP_ON_EXISTING_EMAIL.toString());
                invalidatePendingEmailVerification(user, userStoreManager, claims);
                return;
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
        } else {
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
        }
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
                            (skipEmailVerificationOnUpdateState)) {

                String pendingVerificationEmailClaimValue = getPendingVerificationEmailValue(userStoreManager, user);

                if (StringUtils.isNotBlank(pendingVerificationEmailClaimValue)) {
                    initNotificationForEmailVerificationOnUpdate(pendingVerificationEmailClaimValue, user);

                }
            }
        } finally {
            Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
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
     * @param user              User.
     * @param userStoreManager  User store manager.
     * @param claims            User claims.
     * @throws IdentityEventException
     */
    private void invalidatePendingEmailVerification(User user, UserStoreManager userStoreManager,
                                                    Map<String, String> claims ) throws IdentityEventException {

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
     * @param claims    User claims.
     * @return True if 'verifyEmail' claim is available as true, false otherwise.
     */
    private boolean isVerifyEmailClaimAvailable(Map<String, String> claims) {

        return (claims.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM) &&
                Boolean.parseBoolean(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM)));
    }
}
