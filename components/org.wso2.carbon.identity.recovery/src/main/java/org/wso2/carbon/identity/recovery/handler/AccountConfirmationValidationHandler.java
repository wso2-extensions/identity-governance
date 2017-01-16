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
import org.wso2.carbon.identity.common.base.exception.IdentityRuntimeException;
import org.wso2.carbon.identity.common.base.handler.InitConfig;
import org.wso2.carbon.identity.event.AbstractEventHandler;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.util.Map;

/**
 * Account Confirmation Validation Handler.
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
        String uniqueUserID = (String) eventProperties.get(EventConstants.EventProperty.USER_NAME);
        IdentityStore identityStore =
                (IdentityStore) eventProperties.get(EventConstants.EventProperty.USER_STORE_MANAGER);
        User user;
        
        if (EventConstants.Event.PRE_AUTHENTICATION.equals(event.getEventName())) {
                if (log.isDebugEnabled()) {
                    log.debug("PreAuthenticate");
                }
            boolean isAccountLocked;
            try {
                user = identityStore.getUser(uniqueUserID);
                isAccountLocked = Boolean.parseBoolean(
                        Utils.getClaimFromIdentityStore(user, ACCOUNT_LOCKED_CLAIM));
            } catch (IdentityStoreException e) {
                throw new EventException("Error while retrieving account lock claim value", e);
            } catch (UserNotFoundException e) {
                throw new EventException("User not found " + uniqueUserID, e);
            }
            if (isAccountLocked && !isUserAccountConfirmed(user)) {
//                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(
//                        IdentityCoreConstants.USER_ACCOUNT_NOT_CONFIRMED_ERROR_CODE);
//                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                throw new EventException("User : " + uniqueUserID + " not confirmed yet.");
            }
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }


    /**
     * Check whether user is already confirmed or not.
     *
     * @param user
     * @return
     * @throws EventException
     */
    private boolean isUserAccountConfirmed(User user) throws EventException {
        boolean userConfirmed;
        try {
            userConfirmed = UserSelfRegistrationManager.getInstance().isUserConfirmed(user);
        } catch (IdentityRecoveryException e) {
            throw new EventException("Error occurred while checking whether this user is confirmed or not, " +
                    e.getMessage(), e);
        }
        return userConfirmed;
    }
}
