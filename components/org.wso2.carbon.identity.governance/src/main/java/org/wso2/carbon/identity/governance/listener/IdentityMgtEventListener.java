/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.listener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.EventMgtException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.internal.EventMgtServiceDataHolder;
import org.wso2.carbon.identity.event.services.EventMgtService;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

/**
 * This is an implementation of UserOperationEventListener. This defines
 * additional operations
 * for some of the core user management operations
 */
public class IdentityMgtEventListener extends AbstractIdentityUserOperationEventListener {



    private static final Log log = LogFactory.getLog(IdentityMgtEventListener.class);
    EventMgtService eventMgtService = EventMgtServiceDataHolder.getInstance().getEventMgtService();

    @Override
    public int getExecutionOrderId() {
        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 95;
    }

    /**
     * This method checks if the user account exist or is locked. If the account is
     * locked, the authentication process will be terminated after this method
     * returning false.
     */
    @Override
    public boolean doPreAuthenticate(String userName, Object credential,
                                     UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre authenticator is called in IdentityMgtEventListener");
        }
        try {
            String eventName = EventMgtConstants.Event.PRE_AUTHENTICATION;

            HashMap<String, Object> properties = new HashMap<>();
            properties.put(EventMgtConstants.EventProperty.USER_NAME, userName);
            properties.put(EventMgtConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(EventMgtConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantDomain());

            Event identityMgtEvent = new Event(eventName, properties);

            eventMgtService.handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            throw new UserStoreException("Error when authenticating user", e);
        }

        return true;
    }

    @Override
    public boolean doPostAuthenticate(String userName, boolean authenticated, UserStoreManager userStoreManager)
            throws UserStoreException {

        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post authenticator is called in IdentityMgtEventListener");
        }
        try {
            String eventName = EventMgtConstants.Event.POST_AUTHENTICATION;

            HashMap<String, Object> properties = new HashMap<>();
            properties.put(EventMgtConstants.EventProperty.USER_NAME, userName);
            properties.put(EventMgtConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(EventMgtConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantDomain());
            properties.put(EventMgtConstants.EventProperty.OPERATION_STATUS, authenticated);
            Event identityMgtEvent = new Event(eventName, properties);

            eventMgtService.handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            throw new UserStoreException("Error when authenticating user", e);
        }

        return true;
    }

    @Override
    public boolean doPreSetUserClaimValues(String userName, Map<String, String> claims, String profileName, UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre set claims is called in IdentityMgtEventListener");
        }
        try {
            String eventName = EventMgtConstants.Event.PRE_SET_USER_CLAIMS;

            HashMap<String, Object> properties = new HashMap<>();
            properties.put(EventMgtConstants.EventProperty.USER_NAME, userName);
            properties.put(EventMgtConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(EventMgtConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantDomain());
            properties.put(EventMgtConstants.EventProperty.USER_CLAIMS, claims);
            properties.put(EventMgtConstants.EventProperty.PROFILE_NAME, profileName);
            Event identityMgtEvent = new Event(eventName, properties);

            eventMgtService.handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            throw new UserStoreException("Error when setting user claims.", e);
        }

        return true;
    }

    @Override
    public boolean doPostSetUserClaimValues(String userName, Map<String, String> claims, String profileName,
                                          UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Post set claims is called in IdentityMgtEventListener");
        }
        try {
            String eventName = EventMgtConstants.Event.POST_SET_USER_CLAIMS;

            HashMap<String, Object> properties = new HashMap<>();
            properties.put(EventMgtConstants.EventProperty.USER_NAME, userName);
            properties.put(EventMgtConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(EventMgtConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantDomain());
            properties.put(EventMgtConstants.EventProperty.USER_CLAIMS, claims);
            properties.put(EventMgtConstants.EventProperty.PROFILE_NAME, profileName);
            Event identityMgtEvent = new Event(eventName, properties);

            eventMgtService.handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            throw new UserStoreException("Error when setting user claims.", e);
        }

        return true;
    }
}
