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
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.common.base.handler.InitConfig;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.AbstractEventHandler;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.Map;

/**
 * Account Confirmation Validation Handler
 */
public class AccountConfirmationValidationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(AccountConfirmationValidationHandler.class);

    public static final String ACCOUNT_LOCKED_CLAIM = "http://wso2.org/claims/identity/accountLocked";

    public String getName() {
        return "accountConfirmationValidation";
    }

    public String getFriendlyName() {
        return "Account Confirmation Validation";
    }

    @Override
    public void handleEvent(Event event) throws EventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(EventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(EventConstants.EventProperty
                .USER_STORE_MANAGER);

        String tenantDomain = (String) eventProperties.get(EventConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig
                .PROPERTY_DOMAIN_NAME);
        String usernameWithDomain = UserCoreUtil.addDomainToName(userName, domainName);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(domainName);

        if (EventConstants.Event.PRE_AUTHENTICATION.equals(event.getEventName())) {
                if (log.isDebugEnabled()) {
                    log.debug("PreAuthenticate");
                }
            boolean isAccountLocked = true;
            try {
                isAccountLocked = Boolean.parseBoolean(userStoreManager.getUserClaimValue(userName,
                        ACCOUNT_LOCKED_CLAIM, null));
            } catch (UserStoreException e) {
                throw new EventException("Error while retrieving account lock claim value", e);
            }
            if (isAccountLocked && !isUserAccountConfirmed(user)) {
                if (!isUserAccountConfirmed(user)) {
                    IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(
                            IdentityCoreConstants.USER_ACCOUNT_NOT_CONFIRMED_ERROR_CODE);
                    IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                    throw new EventException("User : " + userName + " not confirmed yet.");
                }
            }
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {
        return 50;
    }


    /**
     * Check whether user is already confirmed or not.
     *
     * @param user
     * @return
     * @throws EventException
     */
    private boolean isUserAccountConfirmed(User user) throws EventException {
        boolean userConfirmed = false;
        try {
            userConfirmed = UserSelfRegistrationManager.getInstance().isUserConfirmed(user);
        } catch (IdentityRecoveryException e) {
            throw new EventException("Error occurred while checking whether this user is confirmed or not, " +
                    e.getMessage(), e);
        }
        return userConfirmed;
    }
}
