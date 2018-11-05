/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.bean.IdentityEventMessageContext;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;

/**
 * This class is used to invalidate the recover codes.
 */
public class CodeInvalidationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(CodeInvalidationHandler.class);

    public String getName() {

        return "confirmationCodesInvalidate";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        String eventName = event.getEventName();
        if (log.isDebugEnabled()) {
            log.debug("Handling event : " + eventName);
        }

        if (IdentityEventConstants.Event.POST_DELETE_USER.equals(event.getEventName())) {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            Map<String, Object> eventProperties = event.getEventProperties();
            String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
            UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.
                    EventProperty.USER_STORE_MANAGER);

            String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
            String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants.
                    RealmConfig.PROPERTY_DOMAIN_NAME);

            User user = new User();
            user.setUserName(userName);
            user.setTenantDomain(tenantDomain);
            user.setUserStoreDomain(domainName);
            try {
                userRecoveryDataStore.invalidate(user);
            } catch (IdentityRecoveryException e) {
                throw new IdentityEventException("Error while invalidating codes.", e);
            }
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        int priority = super.getPriority(messageContext);
        if (priority == -1) {
            priority = 70;
        }
        return priority;
    }

    public boolean canHandle(MessageContext messageContext) throws IdentityRuntimeException {

        Event event = ((IdentityEventMessageContext) messageContext).getEvent();
        String eventName = event.getEventName();
        if (IdentityEventConstants.Event.POST_DELETE_USER.equals(eventName)) {
            return true;
        }
        return false;
    }

    public boolean isAssociationAsync(String eventName) throws IdentityEventException {

        return false;
    }
}
