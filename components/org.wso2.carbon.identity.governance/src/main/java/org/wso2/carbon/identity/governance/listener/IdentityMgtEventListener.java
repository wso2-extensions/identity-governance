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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.api.Permission;
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
    IdentityEventService eventMgtService = IdentityMgtServiceDataHolder.getInstance().getIdentityEventService();

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
        String eventName = IdentityEventConstants.Event.PRE_AUTHENTICATION;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
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
        String eventName = IdentityEventConstants.Event.POST_AUTHENTICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.OPERATION_STATUS, authenticated);

        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }

    @Override
    public boolean doPreSetUserClaimValues(String userName, Map<String, String> claims, String
            profileName, UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre set claims is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_SET_USER_CLAIMS;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, userStoreManager, eventName, properties);
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
        String eventName = IdentityEventConstants.Event.POST_SET_USER_CLAIMS;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPreAddUser(String userName, Object credential, String[] roleList,
                                Map<String, String> claims, String profile,
                                UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("Pre add user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_ADD_USER;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        properties.put(IdentityEventConstants.EventProperty.ROLE_LIST, roleList);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profile);
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);

        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostAddUser(String userName, Object credential, String[] roleList,
                                 Map<String, String> claims, String profile,
                                 UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post add user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_ADD_USER;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        properties.put(IdentityEventConstants.EventProperty.ROLE_LIST, roleList);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profile);
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPreUpdateCredential(String userName, Object newCredential,
                                         Object oldCredential,
                                         UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre update credential is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_UPDATE_CREDENTIAL;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, newCredential);
        properties.put(IdentityEventConstants.EventProperty.OLD_CREDENTIAL, oldCredential);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostUpdateCredential(String userName, Object credential, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post update credential is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }

    public boolean doPreUpdateCredentialByAdmin(String userName, Object newCredential,
                                                UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre update credential by admin is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_UPDATE_CREDENTIAL_BY_ADMIN;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, newCredential);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostUpdateCredentialByAdmin(String userName, Object credential,
                                                 UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post update credential by admin is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPreDeleteUser(String userName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre delete user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_DELETE_USER;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPostDeleteUser(String userName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post delete user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_DELETE_USER;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPreSetUserClaimValue(String userName, String claimURI, String claimValue,
                                          String profileName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre set user claim value is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_SET_USER_CLAIM;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CLAIM_URI, claimURI);
        properties.put(IdentityEventConstants.EventProperty.CLAIM_VALUE, claimValue);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostSetUserClaimValue(String userName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post set user claim value is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_SET_USER_CLAIM;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPreDeleteUserClaimValues(String userName, String[] claims, String profileName,
                                              UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre delete user claim values is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_DELETE_USER_CLAIMS;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostDeleteUserClaimValues(String userName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post delete user claim values is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_DELETE_USER_CLAIMS;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPreDeleteUserClaimValue(String userName, String claimURI, String profileName,
                                             UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre delete user claim value is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_DELETE_USER_CLAIM;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CLAIM_URI, claimURI);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    public boolean doPostDeleteUserClaimValue(String userName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post delete user claim value is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_DELETE_USER_CLAIM;
        handleEvent(userName, userStoreManager, eventName, new HashMap<String, Object>());
        return true;
    }

    public boolean doPreAddRole(String roleName, String[] userList, Permission[] permissions,
                                UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre add role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_ADD_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_LIST, userList);
        properties.put(IdentityEventConstants.EventProperty.PERMISSIONS, permissions);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPostAddRole(String roleName, String[] userList, Permission[] permissions,
                                 UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post add role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_ADD_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_LIST, userList);
        properties.put(IdentityEventConstants.EventProperty.PERMISSIONS, permissions);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPreDeleteRole(String roleName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre delete role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_DELETE_ROLE;
        handleEvent(null, userStoreManager, eventName, roleName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPostDeleteRole(String roleName, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post delete role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_DELETE_ROLE;
        handleEvent(null, userStoreManager, eventName, roleName, new HashMap<String, Object>());
        return true;
    }


    public boolean doPreUpdateRoleName(String roleName, String newRoleName,
                                       UserStoreManager userStoreManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre update role name is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_UPDATE_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.NEW_ROLE_NAME, newRoleName);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPostUpdateRoleName(String roleName, String newRoleName,
                                        UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post update role name is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_UPDATE_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.NEW_ROLE_NAME, newRoleName);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPreUpdateUserListOfRole(String roleName, String deletedUsers[],
                                             String[] newUsers, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre update user list of role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_UPDATE_USER_LIST_OF_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.DELETED_USERS, deletedUsers);
        properties.put(IdentityEventConstants.EventProperty.NEW_USERS, newUsers);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPostUpdateUserListOfRole(String roleName, String deletedUsers[],
                                              String[] newUsers, UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post update user list of role is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_UPDATE_USER_LIST_OF_ROLE;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.DELETED_USERS, deletedUsers);
        properties.put(IdentityEventConstants.EventProperty.NEW_USERS, newUsers);
        handleEvent(null, userStoreManager, eventName, roleName, properties);
        return true;
    }


    public boolean doPreUpdateRoleListOfUser(String userName, String[] deletedRoles,
                                             String[] newRoles,
                                             UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("pre update role list of user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.PRE_UPDATE_ROLE_LIST_OF_USER;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.DELETED_ROLES, deletedRoles);
        properties.put(IdentityEventConstants.EventProperty.NEW_ROLES, newRoles);

        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }

    public boolean doPostUpdateRoleListOfUser(String userName, String[] deletedRoles,
                                              String[] newRoles,
                                              UserStoreManager userStoreManager)
            throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post update role list of user is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_UPDATE_ROLE_LIST_OF_USER;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.DELETED_ROLES, deletedRoles);
        properties.put(IdentityEventConstants.EventProperty.NEW_ROLES, newRoles);

        handleEvent(userName, userStoreManager, eventName, properties);
        return true;
    }


    private void handleEvent(String userName, UserStoreManager userStoreManager, String eventName, HashMap<String, Object> properties)
            throws UserStoreException {
        handleEvent(userName, userStoreManager, eventName, null, properties);
    }

    private void handleEvent(String userName, UserStoreManager userStoreManager, String eventName, String roleName, HashMap<String, Object> properties)
            throws UserStoreException {

        try {
            if (StringUtils.isNotBlank(userName)) {
                properties.put(IdentityEventConstants.EventProperty.USER_NAME, userName);
            }
            if (StringUtils.isNotBlank(roleName)) {
                properties.put(IdentityEventConstants.EventProperty.ROLE_NAME, roleName);
            }
            properties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(IdentityEventConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantDomain());

            Event identityMgtEvent = new Event(eventName, properties);

            eventMgtService.handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw new UserStoreException("Error when handling event : " + eventName, e);
        }
    }

}
