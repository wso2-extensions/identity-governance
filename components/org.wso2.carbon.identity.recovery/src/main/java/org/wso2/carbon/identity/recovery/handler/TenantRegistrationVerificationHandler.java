/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
 * limitations under the License.
 */
package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;

/**
 * This handler is used to send password reset link for tenant admin during tenant registration.
 */
public class TenantRegistrationVerificationHandler extends UserEmailVerificationHandler {

    private static final Log log = LogFactory.getLog(TenantRegistrationVerificationHandler.class);

    public String getName() {

        return "tenantRegistrationVerificationHandler";
    }

    public String getFriendlyName() {

        return "Tenant registration Email Verification";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();

        if (log.isDebugEnabled()) {
            log.debug("Handling event : " + eventName);
        }

        Map<String, Object> eventProperties = event.getEventProperties();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants
                .EventProperty.USER_STORE_MANAGER);

        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);

        if (isTenantRegistrationRequest(claims, eventName)) {
            handleClaimUpdate(eventProperties, userStoreManager, claims);
        }
    }

    private boolean isTenantRegistrationRequest(Map<String, String> claims, String eventName)
            throws IdentityEventException {

        return IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName) && Boolean.valueOf(claims.
                get(IdentityRecoveryConstants.TENANT_ADMIN_ASK_PASSWORD_CLAIM));
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 26;
    }

    protected void handleClaimUpdate(Map<String, Object> eventProperties, UserStoreManager userStoreManager,
                                     Map<String, String> claims) throws IdentityEventException {

        User user = getUser(eventProperties, userStoreManager);

        if (log.isDebugEnabled()) {
            log.debug("PreSetUserClaim - TenantRegistrationVerificationHandler for : " + user.toString() +
                    ". This is an update request for the claim:  " + IdentityRecoveryConstants.
                    TENANT_ADMIN_ASK_PASSWORD_CLAIM);
        }

        // Remove claim to prevent persisting this temporary claim.
        claims.remove(IdentityRecoveryConstants.TENANT_ADMIN_ASK_PASSWORD_CLAIM);

        String uuid = UUIDGenerator.generateUUID();
        String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_TENANT_REGISTRATION_CONFIRMATION;

        if (claims.containsKey(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM)) {
            claims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM);
        }

        setRecoveryData(user, RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD, uuid);
        lockAccount(user, userStoreManager);
        try {
            triggerNotification(user, notificationType, uuid, Utils.getArbitraryProperties());
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending  notification.", e);
        }
    }
}
