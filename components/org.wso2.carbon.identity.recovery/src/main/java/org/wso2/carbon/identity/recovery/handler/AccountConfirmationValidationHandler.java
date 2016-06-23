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
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.EventMgtException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.Map;

public class AccountConfirmationValidationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(AccountConfirmationValidationHandler.class);

    public String getName() {
        return "accountConfirmationValidation";
    }

    public String getFriendlyName() {
        return "Account Confirmation Validation";
    }

    @Override
    public boolean handleEvent(Event event) throws EventMgtException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(EventMgtConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(EventMgtConstants.EventProperty.USER_STORE_MANAGER);

        String tenantDomain = (String) eventProperties.get(EventMgtConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        String usernameWithDomain = UserCoreUtil.addDomainToName(userName, domainName);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(domainName);


        if (EventMgtConstants.Event.PRE_AUTHENTICATION.equals(event.getEventName())) {
                if(log.isDebugEnabled()){
                    log.debug("PreAuthenticate");
                }
                if(!isUserAccountConfirmed(user)){
                    IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(
                            IdentityCoreConstants.USER_ACCOUNT_NOT_CONFIRMED);
                    IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
                    throw new EventMgtException("User : " + userName + " not confirmed yet.");
                }
        }
        return true;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }


    private boolean isUserAccountConfirmed(User user){
        return false ;
    }

}
