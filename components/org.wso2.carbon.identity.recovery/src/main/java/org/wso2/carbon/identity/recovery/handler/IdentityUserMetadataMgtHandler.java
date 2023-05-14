/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.listener.IdentityStoreEventListener;
import org.wso2.carbon.identity.mgt.constants.IdentityMgtConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.jdbc.JDBCUserStoreManager;
import org.wso2.carbon.user.core.jdbc.UniqueIDJDBCUserStoreManager;
import org.wso2.carbon.user.core.listener.UserOperationEventListener;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * This event handler is used to handle events related to user metadata updates.
 */
public class IdentityUserMetadataMgtHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(IdentityUserMetadataMgtHandler.class);
    private static final String POST_AUTHENTICATION = "post_authentication";
    private static final String POST_CREDENTIAL_UPDATE = "post_credential_update";
    private static final String ENABLE_IDENTITY_USER_METADATA_MGT_HANDLER = "identityUserMetadataMgtHandler.enable";
    private static final String USE_DAO_FOR_USER_METADATA_UPDATE = "identityUserMetadataMgtHandler.storeWithDAO";

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        UserStoreManager userStoreManager = (UserStoreManager)
                eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);

        boolean enable = Boolean.parseBoolean(configs.getModuleProperties().getProperty(
                ENABLE_IDENTITY_USER_METADATA_MGT_HANDLER));
        boolean enableDao = Boolean.parseBoolean(configs.getModuleProperties().getProperty(
                USE_DAO_FOR_USER_METADATA_UPDATE));
        if (!enable) {
            if (log.isDebugEnabled()) {
                log.debug("Identity User Metadata Management handler is not enabled.");
            }
            return;
        }

        String username = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        // Skip the tenant association manager user since,
        // POST_AUTHENTICATION event is triggered for this user frequently.
        if (username.contains(IdentityRecoveryConstants.TENANT_ASSOCIATION_MANAGER)) {
            return;
        }

        if (IdentityEventConstants.Event.POST_AUTHENTICATION.equals(event.getEventName())) {
            handlePostAuthenticate(eventProperties, userStoreManager, username, enableDao);
        } else if (IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL.equals(event.getEventName()) ||
                IdentityEventConstants.Event.POST_UPDATE_CREDENTIAL_BY_ADMIN.equals(event.getEventName())) {
            handleCredentialUpdate(userStoreManager, username);
        }
    }

    private void handlePostAuthenticate(Map<String, Object> eventProperties, UserStoreManager userStoreManager,
                                        String username, boolean enableDao) throws IdentityEventException {

        if (log.isDebugEnabled()) {
            log.debug("Start handling post authentication event.");
        }
        if ((Boolean) eventProperties.get(IdentityEventConstants.EventProperty.OPERATION_STATUS)) {
            String lastLoginTime = Long.toString(System.currentTimeMillis());
            if (enableDao) {
                setUserClaimByDAO(userStoreManager, username, IdentityMgtConstants.LAST_LOGIN_TIME,
                        lastLoginTime, POST_AUTHENTICATION);
                return;
            }
            setUserClaim(userStoreManager, username, IdentityMgtConstants.LAST_LOGIN_TIME,
                    lastLoginTime, POST_AUTHENTICATION);
        }
    }

    private void handleCredentialUpdate(UserStoreManager userStoreManager, String username)
            throws IdentityEventException {

        if (log.isDebugEnabled()) {
            log.debug("Start handling post credential update event.");
        }
        String lastPasswordUpdateTime = Long.toString(System.currentTimeMillis());
        setUserClaim(userStoreManager, username, IdentityMgtConstants.LAST_PASSWORD_UPDATE_TIME,
                lastPasswordUpdateTime, POST_CREDENTIAL_UPDATE);
    }

    private void setUserClaim(UserStoreManager userStoreManager, String username,
                              String claimURI, String claimValue, String eventName) throws IdentityEventException {

        Map<String, String> userClaims = new HashMap<>();
        userClaims.put(claimURI, claimValue);
        try {
            userStoreManager.setUserClaimValues(username, userClaims, null);
            if (log.isDebugEnabled()) {
                log.debug(String.format("Successfully updated the user claims related to %s event.", eventName));
            }
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while updating user claims related to %s event.", eventName), e);
        }
    }

    /**
     * Store user attributes in the database by directly contacting the DAO layer.
     *
     * @param userStoreManager  User store manager.
     * @param username          Username.
     * @param claimURI          Claim URI.
     * @param claimValue        Claim value.
     * @param eventName         Event name.
     */
    private void setUserClaimByDAO(UserStoreManager userStoreManager, String username,
                                   String claimURI, String claimValue, String eventName) throws IdentityEventException {

        Map<String, String> userClaims = new HashMap<>();
        userClaims.put(claimURI, claimValue);

        try {
            // Storing attribute values of Asgardeo users into user store database.
            if (isAsgardeoUser(userStoreManager) && isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
                if (userStoreManager instanceof UniqueIDJDBCUserStoreManager) {
                    String userId = ((UniqueIDJDBCUserStoreManager) userStoreManager).getUserIDFromUserName(username);
                    if (StringUtils.isEmpty(userId)) {
                        throw new IdentityEventException(
                                String.format("Error occurred while retrieving user's ID related to %s event. "
                                        , eventName));
                    }
                    ((UniqueIDJDBCUserStoreManager) userStoreManager).doSetUserClaimValuesWithID(userId, userClaims,
                            null);
                    return;
                }
                throw new IdentityEventException(
                        String.format("Error occurred while updating user claims related to %s event.", eventName));
            }
            // Storing attribute values of business users into identity store database.
            for (UserOperationEventListener listener : getUserStoreManagerListeners()) {
                if (listener instanceof IdentityStoreEventListener) {
                    listener.doPreSetUserClaimValues(username, userClaims, null, userStoreManager);
                }
            }
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while updating user claims related to %s event.", eventName), e);
        }
    }

    /**
     * Check whether the given user is Asgardeo user.
     *
     * @param userStoreManager  User Store manager.
     * @return                  Whether the user store is Asgardeo user store or not.
     */
    private boolean isAsgardeoUser(UserStoreManager userStoreManager) {

        String userStoreDomain = userStoreManager.getRealmConfiguration().
                getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);
        return IdentityRecoveryConstants.ASGARDEO_USER_DOMAIN_NAME.equals(userStoreDomain);
    }

    /**
     * Check whether the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager  User Store manager.
     * @return                  Whether identity claims are stored in user store or not.
     */
    private boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(IdentityRecoveryConstants.STORE_IDENTITY_CLAIMS));
    }

    private Collection<UserOperationEventListener> getUserStoreManagerListeners() {

        Map<Integer, UserOperationEventListener> userOperationEventListeners =
                IdentityRecoveryServiceDataHolder.getInstance().getUserOperationEventListeners();
        return userOperationEventListeners.values();
    }

    @Override
    public String getName() {

        return "identityUserMetadataMgtHandler";
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 50;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    public String getFriendlyName() {

        return "Identity User Metadata Management Handler";
    }
}
