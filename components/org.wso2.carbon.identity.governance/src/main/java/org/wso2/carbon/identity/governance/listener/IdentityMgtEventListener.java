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
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.tenant.mgt.util.TenantMgtUtil;
import org.wso2.carbon.user.api.Permission;
import org.wso2.carbon.user.api.TenantManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This is an implementation of UserOperationEventListener. This defines
 * additional operations
 * for some of the core user management operations
 */
public class IdentityMgtEventListener extends AbstractIdentityUserOperationEventListener {


    private static final Log log = LogFactory.getLog(IdentityMgtEventListener.class);
    IdentityEventService eventMgtService = IdentityMgtServiceDataHolder.getInstance().getIdentityEventService();
    private static String RE_CAPTCHA_USER_DOMAIN = "user-domain-recaptcha";

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
        IdentityUtil.clearIdentityErrorMsg();
        IdentityUtil.threadLocalProperties.get().remove(RE_CAPTCHA_USER_DOMAIN);

        // This is used set domain of the user when authentication is failed for an existing user. This is required
        // for re-captcha feature.
        IdentityUtil.threadLocalProperties.get().put(RE_CAPTCHA_USER_DOMAIN,
                IdentityGovernanceUtil.getUserStoreDomainName(userStoreManager));
        String eventName = IdentityEventConstants.Event.PRE_AUTHENTICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);
        handleEvent(userName, userStoreManager, eventName, properties);
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
        if (!isUserExistsInDomain(userStoreManager, userName, authenticated)){
            if (log.isDebugEnabled()) {
                log.debug("IdentityMgtEventListener returns since user: " + userName + " not available in current " +
                        "user store domain: " + userStoreManager.getRealmConfiguration().getUserStoreProperty
                        (UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME) );
            }
            return true;
        }
        IdentityUtil.threadLocalProperties.get().remove(IdentityCoreConstants.USER_ACCOUNT_STATE);
        String eventName = IdentityEventConstants.Event.POST_AUTHENTICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.OPERATION_STATUS, authenticated);

        handleEvent(userName, userStoreManager, eventName, properties);

        // This is not required for authenticated users.
        if (authenticated) {
            IdentityUtil.threadLocalProperties.get().remove(RE_CAPTCHA_USER_DOMAIN);
        }
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
        IdentityUtil.threadLocalProperties.get().remove(IdentityCoreConstants.USER_ACCOUNT_STATE);
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
        IdentityUtil.threadLocalProperties.get().remove(IdentityCoreConstants.USER_ACCOUNT_STATE);
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

            int tenantId = userStoreManager.getTenantId();
            String userTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
            try {
                RealmService realmService = IdentityMgtServiceDataHolder.getInstance().getRealmService();
                TenantManager tenantManager = realmService.getTenantManager();
                userTenantDomain = tenantManager.getDomain(tenantId);
            } catch (org.wso2.carbon.user.api.UserStoreException e) {
                    log.error("Unable to get the get the domain from realmService for tenant: " + tenantId, e);
            }

            properties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
            properties.put(IdentityEventConstants.EventProperty.TENANT_ID, PrivilegedCarbonContext
                    .getThreadLocalCarbonContext().getTenantId());
            properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, userTenantDomain);

            Event identityMgtEvent = new Event(eventName, properties);

            if (!TenantMgtUtil.isTenantAdminCreationOperation()) {
                eventMgtService.handleEvent(identityMgtEvent);
            }
        } catch (IdentityEventException e) {
            String errorCode = e.getErrorCode();

            if (StringUtils.isNotEmpty(errorCode)) {
                //This error code 22001 means user password history is violated.
                if (StringUtils.equals(errorCode, "22001")|| StringUtils.equals(errorCode, "40001")
                        || StringUtils.equals(errorCode, "40002")
                        || UserCoreConstants.ErrorCode.USER_IS_LOCKED.equals(errorCode)
                        || IdentityCoreConstants.USER_ACCOUNT_DISABLED_ERROR_CODE.equals(errorCode)
                        || IdentityCoreConstants.USER_ACCOUNT_NOT_CONFIRMED_ERROR_CODE.equals(errorCode)) {
                    throw new UserStoreException(e.getMessage(), e);
                }
            }
            throw new UserStoreException("Error when handling event : " + eventName, e);
        }
    }

    public boolean doPostGetUserClaimValue(String userName, String claim, List<String> claimValue, String profileName, UserStoreManager storeManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post get user claim value is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_GET_USER_CLAIM;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CLAIM_URI, claim);
        properties.put(IdentityEventConstants.EventProperty.CLAIM_VALUE, claimValue);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, storeManager, eventName, properties);
        return true;
    }


    public boolean doPostGetUserClaimValues(String userName, String[] claims, String profileName, Map<String, String>
            claimMap, UserStoreManager storeManager) throws UserStoreException {
        if (!isEnable()) {
            return true;
        }
        if (log.isDebugEnabled()) {
            log.debug("post get user claim values is called in IdentityMgtEventListener");
        }
        String eventName = IdentityEventConstants.Event.POST_GET_USER_CLAIMS;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claimMap);
        properties.put(IdentityEventConstants.EventProperty.PROFILE_NAME, profileName);
        handleEvent(userName, storeManager, eventName, properties);
        return true;
    }

    private boolean isUserExistsInDomain(UserStoreManager userStoreManager, String userName) throws UserStoreException {
        boolean isExists = false;
        if (userStoreManager.isExistingUser(userName)) {
            isExists = true;
        }
        return isExists;
    }

    private boolean isUserExistsInDomain(UserStoreManager userStoreManager, String userName,
                                         boolean authenticated) throws UserStoreException {
        boolean isExists = false;
        if (authenticated) {
            String userDomain = UserCoreUtil.getDomainFromThreadLocal();
            String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants
                    .RealmConfig.PROPERTY_DOMAIN_NAME);

            if (userDomain != null) {
                if (userDomain.equalsIgnoreCase(userStoreDomain)) {
                    isExists = true;
                }
            } else if (IdentityUtil.getPrimaryDomainName().equalsIgnoreCase(userStoreDomain)) {
                isExists = true;
            }
        } else {
            isExists = isUserExistsInDomain(userStoreManager, userName);
        }
        return isExists;
    }
}
