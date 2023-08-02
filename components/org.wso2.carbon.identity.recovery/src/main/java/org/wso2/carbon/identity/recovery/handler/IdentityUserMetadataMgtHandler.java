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

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.mgt.constants.IdentityMgtConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.exception.FederatedAssociationManagerException;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreConfigConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.Map;

/**
 * This event handler is used to handle events related to user meta data updates.
 */
public class IdentityUserMetadataMgtHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(IdentityUserMetadataMgtHandler.class);
    private static final String ENABLE_IDENTITY_USER_METADATA_MGT_HANDLER = "identityUserMetadataMgtHandler.enable";
    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();

        boolean enable = Boolean.parseBoolean(configs.getModuleProperties().getProperty(
                ENABLE_IDENTITY_USER_METADATA_MGT_HANDLER));
        if (!enable) {
            if (log.isDebugEnabled()) {
                log.debug("Identity User Metadata Management handler is not enabled.");
            }
            return;
        }

        if (IdentityEventConstants.EventName.AUTHENTICATION_SUCCESS.name().equals(event.getEventName())) {
            AuthenticationContext context = (AuthenticationContext) eventProperties.get(
                    IdentityEventConstants.EventProperty.CONTEXT);
            handleUserMetadataUpdate(context);
        }
    }


    /**
     * Method to handle user metadata update during AUTHENTICATION_SUCCESS event.
     *
     * @param authenticationContext     Authentication context.
     * @throws IdentityEventException   Identity Event Exception.
     */
    private void handleUserMetadataUpdate(AuthenticationContext authenticationContext) throws IdentityEventException {

        if (log.isDebugEnabled()) {
            log.debug("Start handling authentication success event.");
        }
        // Check whether the authentication is passive.
        if (authenticationContext.isPassiveAuthenticate()) {
            return;
        }
        AuthenticatedUser authenticatedUser = authenticationContext.getSequenceConfig().getAuthenticatedUser();
        if (authenticatedUser == null) {
            return;
        }
        if (authenticatedUser.isFederatedUser()) {
            AuthenticatedUser associatedUser = getAssociatedUser(authenticatedUser);
            if (associatedUser != null) {
                authenticatedUser = associatedUser;
            } else {
                // If the user is federated and not associated with a local user,
                // skip storing user metadata values.
                return;
            }
        }
        try {
            UserStoreManager userStoreManager = getUserStoreManager(authenticatedUser);
            if (userStoreManager == null) {
                if (log.isDebugEnabled()) {
                    log.debug("User store manager is null for username: " + authenticatedUser.getUserName() +
                            " in tenant domain: " + authenticatedUser.getTenantDomain());
                }
                return;
            }
            String lastLoginTime = Long.toString(System.currentTimeMillis());
            Map<String, String> userClaims = new HashMap<>();
            userClaims.put(IdentityMgtConstants.LAST_LOGIN_TIME, lastLoginTime);
            setUserMetadataValues(userStoreManager, authenticatedUser, userClaims,
                    IdentityEventConstants.EventName.AUTHENTICATION_SUCCESS.name());
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            IdentityRecoveryConstants.ErrorMessages error = IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER;
            throw new IdentityEventException(error.getCode(), error.getMessage(), e);
        }
    }

    private AuthenticatedUser getAssociatedUser(AuthenticatedUser authenticatedUser) throws IdentityEventException {

        String associatesUserName;
        AuthenticatedUser associatedUserObj = null;
        try {
            String subjectIdentifierValue = authenticatedUser.getUserName();
            FederatedAssociationManager federatedAssociationManager =
                    IdentityRecoveryServiceDataHolder.getInstance().getFederatedAssociationManager();
            associatesUserName = federatedAssociationManager.getUserForFederatedAssociation(
                    authenticatedUser.getTenantDomain(), authenticatedUser.getFederatedIdPName(),
                    subjectIdentifierValue);
            if (associatesUserName != null) {
                associatedUserObj = new AuthenticatedUser();
                associatedUserObj.setUserName(UserCoreUtil.removeDomainFromName(associatesUserName));
                associatedUserObj.setUserStoreDomain(UserCoreUtil.extractDomainFromName(associatesUserName));
                associatedUserObj.setTenantDomain(authenticatedUser.getTenantDomain());
            }
        } catch (FederatedAssociationManagerException e) {
            IdentityRecoveryConstants.ErrorMessages error = IdentityRecoveryConstants.ErrorMessages
                    .ERROR_RETRIEVING_ASSOCIATED_USER;
            throw new IdentityEventException(error.getCode(), error.getMessage(), e);
        }
        return associatedUserObj;
    }

    /**
     * Get user store manager.
     *
     * @param authenticatedUser Authenticated user.
     * @return                  User store manager.
     * @throws UserStoreException Error when getting the user store manager.
     */
    private UserStoreManager getUserStoreManager(AuthenticatedUser authenticatedUser)
            throws org.wso2.carbon.user.api.UserStoreException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        String userStoreDomainName = authenticatedUser.getUserStoreDomain();
        if (StringUtils.isBlank(authenticatedUser.getUserStoreDomain())) {
            if (log.isDebugEnabled()) {
                log.error("User store domain is not found for the user: " + authenticatedUser.getUserName() +
                        " in tenant domain: " + authenticatedUser.getTenantDomain());
            }
            return null;
        }
        int tenantId = IdentityTenantUtil.getTenantId(authenticatedUser.getTenantDomain());
        UserRealm userRealm;
        if (UserStoreConfigConstants.PRIMARY.equals(userStoreDomainName)) {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
            return userRealm.getUserStoreManager();
        } else {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
            return userRealm.getUserStoreManager().getSecondaryUserStoreManager(
                    authenticatedUser.getUserStoreDomain());
        }
    }

    /**
     * Store the user metadata values into the database.
     *
     * @param userStoreManager  User Store manager.
     * @param authenticatedUser Authenticated user.
     * @param userClaims        User claims.
     * @param eventName         Event name.
     */
    private void setUserMetadataValues(UserStoreManager userStoreManager, AuthenticatedUser authenticatedUser,
                                       Map<String, String> userClaims, String eventName)
            throws IdentityEventException {

        try {
            // Storing attribute values of users into identity store database.
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityDataStoreService().storeInIdentityDataStore(
                    authenticatedUser.getUserName(), userStoreManager, PRE_SET_USER_CLAIM_VALUES, userClaims);
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while updating user claims related to %s event.", eventName), e);
        }
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
