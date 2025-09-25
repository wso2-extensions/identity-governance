/*
 * Copyright (c) 2017-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.security.SecureRandom;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class AdminForcedPasswordResetHandler extends UserEmailVerificationHandler {

    private static final Log log = LogFactory.getLog(AdminForcedPasswordResetHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();
        if (log.isDebugEnabled()) {
            log.debug("Handling event : " + eventName);
        }
        Map<String, Object> eventProperties = event.getEventProperties();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants
                .EventProperty.USER_STORE_MANAGER);

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            handleClaimUpdate(eventProperties, userStoreManager);
        }

        if (IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN.equals(eventName)) {
            handleUpdateCredentialsByAdmin(eventProperties, userStoreManager);
        }
    }

    private void handleUpdateCredentialsByAdmin(Map<String, Object> eventProperties, UserStoreManager userStoreManager)
            throws IdentityEventException {

        User user = getUser(eventProperties, userStoreManager);
        String maskedUsername = Utils.maskIfRequired(user.getUserName());

        if (log.isDebugEnabled()) {
            log.debug("PostUpdateCredentialsByAdmin - AdminForcedPasswordResetHandler for user : "
                    + maskedUsername);
        }

        UserRecoveryData userRecoveryData = getRecoveryData(user);
        if (userRecoveryData != null) {
            invalidateRecoveryData(user);
            if (log.isDebugEnabled()) {
                log.debug("PostUpdateCredentialsByAdmin - invalidate Recovery Data for user : "
                        + maskedUsername);
            }
        }

        handleAccountUnlockOnAdminPasswordReset(user, userStoreManager);
    }

    /**
     * Handles the account unlock process after an admin password reset.
     *
     * @param user             User object.
     * @param userStoreManager UserStoreManager instance.
     * @throws IdentityEventException If an error occurs while handling account unlock.
     */
    private void handleAccountUnlockOnAdminPasswordReset(User user, UserStoreManager userStoreManager)
            throws IdentityEventException {

        try {
            // Retrieve the user's account lock-related claims (locked status, locked reason, and account state).
            Map<String, String> currentClaims = userStoreManager.getUserClaimValues(
                    user.getUserName(),
                    new String[]{
                            IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                            IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                            IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI
                    },
                    null
            );

            if (currentClaims == null) {
                return;
            }

            boolean isAccountLocked =
                    Boolean.parseBoolean(currentClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM));
            String accountState = currentClaims.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
            String lockedReason = currentClaims.get(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM);

            // Prepare a map for claim updates.
            Map<String, String> claimMap = new HashMap<>();

            if (isAccountLocked) {
                // Scenario 1:
                // If the account is locked and the locked reason is valid and qualifies for unlocking criteria
                // (pending password update), proceed with unlocking.
                if (!isAccountUnlockRequiredAfterAdminPasswordReset(lockedReason)) {
                    return;
                }
                claimMap.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
                claimMap.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM, StringUtils.EMPTY);
                if (Utils.isAccountStateClaimExisting(user.getTenantDomain())) {
                    claimMap.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                            IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
                }
            } else {
                // Scenario 2:
                // If the account is not locked but its state indicates that it is awaiting the user to set
                // a new password (i.e., the Ask Password flow), update the account state to 'unlocked'.
                if (!IdentityRecoveryConstants.PENDING_ASK_PASSWORD.equals(accountState)) {
                    return;
                }
                claimMap.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
                claimMap.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                        IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
            }

            // Update the user's claims to unlock the account and set the appropriate state.
            userStoreManager.setUserClaimValues(user.getUserName(), claimMap, null);
            if (log.isDebugEnabled()) {
                log.debug(String.format("Account unlocked for user: %s after admin password update.",
                        Utils.maskIfRequired(user.getUserName())));
            }
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error while handling account unlock on admin password update for user: %s",
                            Utils.maskIfRequired(user.getUserName())), e);
        }
    }

    private boolean isAccountUnlockRequiredAfterAdminPasswordReset(String lockedReason) {

        return IdentityMgtConstants.LockedReason.PENDING_ASK_PASSWORD.toString().equals(lockedReason)
                || IdentityMgtConstants.LockedReason.PENDING_ADMIN_FORCED_USER_PASSWORD_RESET.toString()
                .equals(lockedReason);
    }

    protected void handleClaimUpdate(Map<String, Object> eventProperties, UserStoreManager userStoreManager) throws
            IdentityEventException {

        User user = getUser(eventProperties, userStoreManager);

        if (log.isDebugEnabled()) {
            log.debug("PreAuthenticate - AdminForcedPasswordResetHandler for : " + user.toString());
        }

        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);

        boolean adminPasswordResetOffline = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE,
                user.getTenantDomain()));

        boolean adminPasswordResetEmailOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_EMAIL_OTP,
                user.getTenantDomain()));

        boolean adminPasswordResetSMSOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_SMS_OTP,
                user.getTenantDomain()));

        boolean adminPasswordResetRecoveryLink = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK,
                user.getTenantDomain()));

        boolean isAdminPasswordReset = adminPasswordResetOffline || adminPasswordResetEmailOTP ||
                adminPasswordResetRecoveryLink || adminPasswordResetSMSOTP;

        if (isAdminPasswordReset && Boolean.valueOf(claims.get(IdentityRecoveryConstants
                .ADMIN_FORCED_PASSWORD_RESET_CLAIM))) {

            if (log.isDebugEnabled()) {
                log.debug(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM + " update request.");
            }
            Utils.publishRecoveryEvent(eventProperties, IdentityEventConstants.Event.PRE_FORCE_PASSWORD_RESET_BY_ADMIN,
                    null);
            // Remove claim to prevent persisting this temporary claim
            claims.remove(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM);

            String OTP = generateOTPValue();
            String notificationType = "";
            Enum recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP;

            if (adminPasswordResetOffline) {
                if (claims.containsKey(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM)) {
                    claims.remove(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM);
                }
                setUserClaim(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM, OTP, userStoreManager, user);
            }

            if (adminPasswordResetEmailOTP) {
                notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP;
            }

            if (adminPasswordResetSMSOTP) {
                notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_SMS_OTP;
                recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP;
            }

            if (adminPasswordResetRecoveryLink) {
                OTP = UUID.randomUUID().toString();
                recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK;
                notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET;
            }

            claims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
            setRecoveryData(user, recoveryScenario, RecoverySteps.UPDATE_PASSWORD, OTP);
            lockAccountOnAdminPasswordReset(user, claims);

            if (adminPasswordResetEmailOTP || adminPasswordResetRecoveryLink || adminPasswordResetSMSOTP) {
                try {
                    if (adminPasswordResetSMSOTP) {
                        String mobileNumber = userStoreManager.getUserClaimValue(user.getUserName(),
                                IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, null);
                        triggerSmsNotification(user, notificationType, OTP, mobileNumber);
                    } else {
                        triggerNotification(user, notificationType, OTP, Utils.getArbitraryProperties(),
                                new UserRecoveryData(user, OTP, recoveryScenario, RecoverySteps.UPDATE_PASSWORD));
                    }
                    Utils.publishRecoveryEvent(eventProperties, IdentityEventConstants.Event.POST_FORCE_PASSWORD_RESET_BY_ADMIN,
                            OTP);
                } catch (IdentityRecoveryException e) {
                    throw new IdentityEventException("Error while sending  notification ", e);
                } catch (UserStoreException e) {
                    throw new IdentityEventException("Error while getting user claim value.", e);
                }
            }

        }
    }

    protected void triggerSmsNotification(User user, String notificationType, String OTP, String mobileNumber)
            throws IdentityRecoveryServerException {

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
        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, OTP);
        if (StringUtils.isNotBlank(mobileNumber)) {
            properties.put(IdentityRecoveryConstants.SEND_TO, mobileNumber);
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.toFullQualifiedUsername(), e);
        }
    }

    private void lockAccountOnAdminPasswordReset(User user, Map<String, String> claims) {

        if (log.isDebugEnabled()) {
            log.debug("Locking user account on admin forced password reset: " + user.getUserName());
        }
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
        claims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                IdentityMgtConstants.LockedReason.PENDING_ADMIN_FORCED_USER_PASSWORD_RESET.toString());
        claims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                IdentityMgtConstants.AccountStates.PENDING_ADMIN_FORCED_USER_PASSWORD_RESET);
    }

    protected void setUserClaims(Map<String, String> userClaims, User user, UserStoreManager userStoreManager)
            throws IdentityEventException {

        try {
            userStoreManager.setUserClaimValues(user.getUserName(), userClaims, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error while setting user claim value :" + user.getUserName(), e);
        }
    }

    @Override
    public String getName() {
        return "adminForcedPasswordReset";
    }

    @Override
    public String getFriendlyName() {
        return "Admin Forced Password Reset";
    }

    @Override
    public int getPriority(MessageContext messageContext) {
        return 27;
    }

    private String generateOTPValue() {
        char[] chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toCharArray();
        SecureRandom rnd = new SecureRandom();
        StringBuilder sb = new StringBuilder("");
        for (int i = 0; i < 6; i++) {
            sb.append(chars[rnd.nextInt(chars.length)]);
        }
        return sb.toString();
    }
}
