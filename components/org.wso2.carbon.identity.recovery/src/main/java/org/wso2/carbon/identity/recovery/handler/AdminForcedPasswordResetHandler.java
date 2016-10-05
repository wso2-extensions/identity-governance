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
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;

public class AdminForcedPasswordResetHandler extends UserEmailVerificationHandler {

    private static final Log log = LogFactory.getLog(AdminForcedPasswordResetHandler.class);

    @Override
    public void handleEvent(Event event) throws IdentityEventException {
        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(event.getEventName())) {
            Map<String, Object> eventProperties = event.getEventProperties();
            UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
            User user = getUser(eventProperties, userStoreManager);

            Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                    .USER_CLAIMS);

            boolean adminPassResetOffline = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_OFFLINE, user.getTenantDomain()));
            boolean adminPassResetOTP = Boolean.parseBoolean(Utils.getConnectorConfig(
                                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_OTP, user.getTenantDomain()));

            boolean adminPassResetRecoveryLink = Boolean.parseBoolean(Utils.getConnectorConfig(
                    IdentityRecoveryConstants.ConnectorConfig.ENABLE_ADMIN_PASSWORD_RESET_WITH_RECOVERY_LINK, user.getTenantDomain()));

            if (adminPassResetRecoveryLink) {
                if (Boolean.valueOf(claims.get(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM))) {
                    initNotification(user, RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET, RecoverySteps.UPDATE_PASSWORD, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET.toString());
                    claims.remove(IdentityRecoveryConstants.ADMIN_FORCED_PASSWORD_RESET_CLAIM);
                    lockAccount(user, userStoreManager);
                }
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
        return 70;
    }


}
