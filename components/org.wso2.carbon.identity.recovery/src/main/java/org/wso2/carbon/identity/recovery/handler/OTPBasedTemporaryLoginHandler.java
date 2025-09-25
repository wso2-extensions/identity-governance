/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;

/**
 * This handler handles the OTP based temporary login scenarios.
 */
public class OTPBasedTemporaryLoginHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(OTPBasedTemporaryLoginHandler.class);

    @Override
    public String getName() {

        return "otpBasedTemporaryLoginHandler";
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 227;
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();
        if (!IdentityEventConstants.Event.PRE_AUTHENTICATION.equals(eventName)) {
            return;
        }

        if (log.isDebugEnabled()) {
            log.debug("Handling event : " + eventName);
        }
        Map<String, Object> eventProperties = event.getEventProperties();

        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        boolean askPasswordEmailOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_EMAIL_OTP, tenantDomain));
        boolean askPasswordSmsOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_SEND_SMS_OTP, tenantDomain));
        boolean forcedPasswordResetEmailOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_EMAIL_OTP, tenantDomain));
        boolean forcedPasswordResetSmsOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_SMS_OTP, tenantDomain));

        // If none of the OTP based temporary login scenarios are enabled, skip the handler.
        if (!askPasswordEmailOTP && !askPasswordSmsOTP && !forcedPasswordResetEmailOTP && !forcedPasswordResetSmsOTP) {
            return;
        }

        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants
                .EventProperty.USER_STORE_MANAGER);
        User user = getUser(eventProperties, userStoreManager);

        if (log.isDebugEnabled()) {
            log.debug("PreAuthenticate - AdminForcedPasswordResetHandler for user : " + user);
        }

        UserRecoveryData userRecoveryData = getRecoveryData(user);
        if (userRecoveryData != null) {
            Enum recoveryScenario = userRecoveryData.getRecoveryScenario();
            if (log.isDebugEnabled()) {
                log.debug("Handling recovery scenario : " + recoveryScenario.toString() + " for user : " + user);
            }

            String errorCode = null;
            String errorMsg = "User : " + user;
            boolean isForcedPasswordReset = false;
            boolean isAskPasswordBasedPasswordSet = false;

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
            } else if (RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.equals(recoveryScenario) ||
                    RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(recoveryScenario)) {
                /* Handle the Ask Password via OTP scenario to improve the performance instead of going to another
                   handler. */
                String credential = (String) eventProperties.get(IdentityEventConstants.EventProperty.CREDENTIAL);
                isAskPasswordBasedPasswordSet = true;
                if (userRecoveryData.getSecret().equals(credential)) {
                    errorCode = IdentityCoreConstants.ASK_PASSWORD_SET_PASSWORD_VIA_OTP_ERROR_CODE;
                    errorMsg = errorMsg + " has given correct OTP";
                } else {
                    errorCode = IdentityCoreConstants.ASK_PASSWORD_SET_PASSWORD_VIA_OTP_MISMATCHED_ERROR_CODE;
                    errorMsg = errorMsg + " has given in-correct OTP";
                }
            }

            if (isForcedPasswordReset || isAskPasswordBasedPasswordSet) {
                if (log.isDebugEnabled()) {
                    log.debug(errorMsg);
                }

                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(errorCode);
                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                throw new IdentityEventException(errorMsg);
            }
        }
    }

    private User getUser(Map eventProperties, UserStoreManager userStoreManager) {

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

    private UserRecoveryData getRecoveryData(User user) throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData recoveryData;
        try {
            recoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while loading recovery data for user ", e);
        }
        return recoveryData;
    }
}
