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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.util.Map;
import java.util.Random;

/**
 * Admin Forced Password Reset Handler.
 */
public class AdminForcedPasswordResetHandler extends UserEmailVerificationHandler {

    private static final Log log = LogFactory.getLog(AdminForcedPasswordResetHandler.class);

    @Override
    public void handleEvent(Event event) throws EventException {

        String eventName = event.getEventName();
        if (log.isDebugEnabled()) {
            log.debug("Handling event : " + eventName);
        }
        Map<String, Object> eventProperties = event.getEventProperties();
        IdentityStore identityStore =
                (IdentityStore) eventProperties.get(EventConstants.EventProperty.USER_STORE_MANAGER);

        if (EventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            handleClaimUpdate(eventProperties, identityStore);
        }

        if (EventConstants.Event.PRE_AUTHENTICATION.equals(eventName)) {
            handleAuthenticate(eventProperties, identityStore);
            handleClaimUpdate(eventProperties, identityStore);
        }

    }

    protected void handleClaimUpdate(Map<String, Object> eventProperties, IdentityStore identityStore)
            throws EventException {

        User user = getUser(eventProperties, identityStore);

        if (log.isDebugEnabled()) {
            log.debug("PreAuthenticate - AdminForcedPasswordResetHandler for : " + user.toString());
        }

        Map<String, String> claims =
                (Map<String, String>) eventProperties.get(EventConstants.EventProperty.USER_CLAIMS);

        boolean adminPasswordResetOffline = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE));

        boolean adminPasswordResetOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP));

        boolean adminPasswordResetRecoveryLink = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK));

        boolean isAdminPasswordReset =
                adminPasswordResetOffline | adminPasswordResetOTP | adminPasswordResetRecoveryLink;

        if (isAdminPasswordReset &&
            Boolean.valueOf(claims.get(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM))) {

            if (log.isDebugEnabled()) {
                log.debug(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM + " update request.");
            }
            // Remove claim to prevent persisting this temporary claim
            claims.remove(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM);

            String otp = generateOTPValue();
            String notificationType = "";
            Enum recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP;

            if (adminPasswordResetOffline) {
                if (claims.containsKey(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM)) {
                    claims.remove(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM);
                }
                try {
                    Utils.setClaimInIdentityStore(user, IdentityRecoveryConstants.OTP_PASSWORD_CLAIM, otp);
                } catch (IdentityStoreException e) {
                    throw new EventException("Error while setting claims", e);
                } catch (UserNotFoundException e) {
                    throw new EventException("Error while setting claims", e);
                }
            }

            if (adminPasswordResetOTP) {
                notificationType = IdentityRecoveryConstants
                        .NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP;
            }

            if (adminPasswordResetRecoveryLink) {
                otp = Utils.generateUUID();
                recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK;
                notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET;
            }

            if (claims.containsKey(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM)) {
                claims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
            }
            setRecoveryData(user, recoveryScenario, RecoverySteps.UPDATE_PASSWORD, otp);
            lockAccount(user, identityStore);

            if (adminPasswordResetOTP | adminPasswordResetRecoveryLink) {
                try {
                    triggerNotification(user, notificationType, otp, Utils.getArbitraryProperties());
                } catch (IdentityRecoveryException e) {
                    throw new EventException("Error while sending  notification ", e);
                }
            }

        }
    }

    protected void handleAuthenticate(Map<String, Object> eventProperties, IdentityStore identityStore)
            throws EventException {
        User user = getUser(eventProperties, identityStore);

        if (log.isDebugEnabled()) {
            log.debug("PreAuthenticate - AdminForcedPasswordResetHandler for user : " + user.toString());
        }

        UserRecoveryData userRecoveryData = getRecoveryData(user);
        if (userRecoveryData != null) {

            Enum recoveryScenario = userRecoveryData.getRecoveryScenario();

            if (log.isDebugEnabled()) {
                log.debug("Handling recovery scenario : " + recoveryScenario.toString() + " for user : "
                          + user.toString());
            }

//            String errorCode = null;
            String errorMsg = "User : " + user.toString();
            boolean isForcedPasswordReset = false;

            if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(recoveryScenario)) {
//                errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_EMAIL_LINK_ERROR_CODE;
                errorMsg = errorMsg + " needs to reset the password using the given link in email";
                isForcedPasswordReset = true;

            } else if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(recoveryScenario)) {
                String credential = (String) eventProperties.get(EventConstants.EventProperty.CREDENTIAL);
                isForcedPasswordReset = true;

                if (userRecoveryData.getSecret().equals(credential)) {
//                    errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_OTP_ERROR_CODE;
                    errorMsg = errorMsg + " has given correct OTP";
                } else {
//                    errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_OTP_MISMATCHED_ERROR_CODE;
                    errorMsg = errorMsg + " has given in-correct OTP";
                }
            }

            if (isForcedPasswordReset) {
                if (log.isDebugEnabled()) {
                    log.debug(errorMsg);
                }

//                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(errorCode);
//                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                throw new EventException(errorMsg);
            }

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

    private String generateOTPValue() {
        char[] chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toCharArray();
        Random rnd = new Random();
        StringBuilder sb = new StringBuilder("");
        for (int i = 0; i < 6; i++) {
            sb.append(chars[rnd.nextInt(chars.length)]);
        }
        return sb.toString();
    }

}
