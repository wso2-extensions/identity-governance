/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
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

        if (IdentityEventConstants.Event.PRE_AUTHENTICATION.equals(eventName)) {
            handleAuthenticate(eventProperties, userStoreManager);
        }
        if (IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN.equals(eventName)) {
            handleUpdateCredentialsByAdmin(eventProperties, userStoreManager);
        }
    }

    private void handleUpdateCredentialsByAdmin(Map<String, Object> eventProperties, UserStoreManager userStoreManager)
            throws IdentityEventException {

        User user = getUser(eventProperties, userStoreManager);
        if (log.isDebugEnabled()) {
            log.debug("PostUpdateCredentialsByAdmin - AdminForcedPasswordResetHandler for user : "
                    + user.toString());
        }

        UserRecoveryData userRecoveryData = getRecoveryData(user);
        if (userRecoveryData != null) {
            invalidateRecoveryData(user);
            if (log.isDebugEnabled()) {
                log.debug("PostUpdateCredentialsByAdmin - invalidate Recovery Data for user : "
                        + user.toString());
            }
        }

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

    private void triggerSmsNotification(User user, String notificationType, String OTP, String mobileNumber)
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

    protected void handleAuthenticate(Map<String, Object> eventProperties, UserStoreManager userStoreManager) throws
            IdentityEventException {
        User user = getUser(eventProperties, userStoreManager);

        if (log.isDebugEnabled()) {
            log.debug("PreAuthenticate - AdminForcedPasswordResetHandler for user : " + user.toString());
        }

        UserRecoveryData userRecoveryData = getRecoveryData(user);
        if (userRecoveryData != null) {

            Enum recoveryScenario = userRecoveryData.getRecoveryScenario();

            if (log.isDebugEnabled()) {
                log.debug("Handling recovery scenario : " + recoveryScenario.toString() + " for user : " + user.toString
                        ());
            }

            String errorCode = null;
            String errorMsg = "User : " + user.toString();
            boolean isForcedPasswordReset = false;

            if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(recoveryScenario)) {
                errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_EMAIL_LINK_ERROR_CODE;
                errorMsg = errorMsg + " needs to reset the password using the given link in email";
                isForcedPasswordReset = true;

            } else if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(recoveryScenario) ||
                    RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.equals(recoveryScenario)) {
                String credential = (String) eventProperties.get(IdentityEventConstants.EventProperty.CREDENTIAL);
                isForcedPasswordReset = true;

                if (userRecoveryData.getSecret().equals(credential)) {
                    errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_OTP_ERROR_CODE;
                    errorMsg = errorMsg + " has given correct OTP";
                } else {
                    errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_OTP_MISMATCHED_ERROR_CODE;
                    errorMsg = errorMsg + " has given in-correct OTP";
                }
            }

            if (isForcedPasswordReset) {
                if (log.isDebugEnabled()) {
                    log.debug(errorMsg);
                }

                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(errorCode);
                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                throw new IdentityEventException(errorMsg);
            }

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
