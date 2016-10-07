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
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;
import java.util.Random;

public class AdminForcedPasswordResetHandler extends UserEmailVerificationHandler {

    private static final Log log = LogFactory.getLog(AdminForcedPasswordResetHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        User user = getUser(eventProperties, userStoreManager);

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(event.getEventName())) {

            Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                    .USER_CLAIMS);

            boolean adminPasswordResetOffline = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE, user.getTenantDomain()));
            boolean adminPasswordResetOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP, user.getTenantDomain()));

            boolean adminPasswordResetRecoveryLink = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK, user.getTenantDomain()));

            boolean isAdminPasswordReset = adminPasswordResetOffline | adminPasswordResetOTP | adminPasswordResetRecoveryLink;

            if (Boolean.valueOf(claims.get(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM)) && isAdminPasswordReset) {
                claims.remove(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM);
                String OTP = generateOTPValue();
                String notificationType = "";
                Enum recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP;
                if (adminPasswordResetOffline) {
                    setUserClaim(IdentityRecoveryConstants.OTP_PASSWORD_CLAIM, OTP, userStoreManager, user);
                }
                if (adminPasswordResetOTP) {
                    notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET_WITH_OTP.toString();
                }
                if (adminPasswordResetRecoveryLink) {
                    OTP = UUIDGenerator.generateUUID();
                    recoveryScenario = RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK;
                    notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_ADMIN_FORCED_PASSWORD_RESET.toString();
                }
                if (adminPasswordResetOTP | adminPasswordResetRecoveryLink) {
                    try {
                        triggerNotification(user, notificationType, OTP, Utils.getArbitraryProperties());
                    } catch (IdentityRecoveryException e) {
                        throw new IdentityEventException("Error while sending  notification ", e);
                    }
                }
                if (claims.containsKey(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM)) {
                    claims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
                }
                setRecoveryData(user, recoveryScenario, RecoverySteps.UPDATE_PASSWORD, OTP);
                lockAccount(user, userStoreManager);
            }
        }

        if (IdentityEventConstants.Event.PRE_AUTHENTICATION.equals(event.getEventName())) {
            if (log.isDebugEnabled()) {
                log.debug("PreAuthenticate");
            }
            UserRecoveryData userRecoveryData = getRecoveryData(user);
            if (userRecoveryData != null) {
                String errorCode = null;
                String errorMsg = "User : " + user.getUserName() + " has given correct OTP";
                if(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(userRecoveryData.getRecoveryScenario())){
                    errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_EMAIL_LINK_ERROR_CODE;
                } else if(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(userRecoveryData.getRecoveryScenario())){
                    String credential = (String) eventProperties.get(IdentityEventConstants.EventProperty.CREDENTIAL);
                    if(userRecoveryData.getSecret().equals(credential)){
                        errorCode = IdentityCoreConstants.ADMIN_FORCED_USER_PASSWORD_RESET_VIA_OTP_ERROR_CODE;
                    }
                }

                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(errorCode);
                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                throw new IdentityEventException(errorMsg);
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

    @Override
    public int getPriority(MessageContext messageContext) {
        return 27;
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
