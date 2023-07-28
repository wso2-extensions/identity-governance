/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.identity.event.bean.Subscription;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.exception.UserIdNotFoundException;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedIdPData;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.exception.FederatedAssociationManagerException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.jdbc.UniqueIDJDBCUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.ORGANIZATION_AUTHENTICATOR;
import static org.wso2.carbon.user.core.UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;


/**
 * This event handler is used to handle events related to user metadata updates in Asgardeo.
 */
public class UserMetadataMgtHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserMetadataMgtHandler.class);
    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        if (!isUserMetadataMgtHandlerEnabled()) {
            if (log.isDebugEnabled()) {
                log.debug("User Metadata Management handler is not enabled.");
            }
            return;
        }

        Map<String, Object> eventProperties = event.getEventProperties();
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
        if (isOrganizationLogin(authenticationContext)) {
            // Skip storing user metadata values for organization login since the metadata is already stored
            // during the sub organization login flow.
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
            userClaims.put(IdentityRecoveryConstants.LAST_LOGIN_TIME, lastLoginTime);
            setUserMetadataValues(userStoreManager, authenticatedUser, userClaims,
                    IdentityEventConstants.EventName.AUTHENTICATION_SUCCESS.name());
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            IdentityRecoveryConstants.ErrorMessages error = IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER;
            throw new IdentityEventException(error.getCode(), error.getMessage(), e);
        }
    }

    private boolean isOrganizationLogin(AuthenticationContext context) {

        for (Map.Entry<String, AuthenticatedIdPData> authenticatedIdPDataMap
                : context.getCurrentAuthenticatedIdPs().entrySet()) {
            for (AuthenticatorConfig authenticatorConfig : authenticatedIdPDataMap.getValue().getAuthenticators()) {
                if (ORGANIZATION_AUTHENTICATOR.equals(authenticatorConfig.getName())) {
                    return true;
                }
            }
        }
        return false;
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
        UserRealm userRealm;
        if (PRIMARY_DEFAULT_DOMAIN_NAME.equals(userStoreDomainName)) {
            int tenantId = IdentityTenantUtil.getTenantId(authenticatedUser.getTenantDomain());
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
            return userRealm.getUserStoreManager();
        } else if (IdentityRecoveryConstants.ASGARDEO_USER_DOMAIN_NAME.equals(userStoreDomainName)) {
            // This will be an asgardeo user. Need to get the realm of the super tenant.
            userRealm = (UserRealm) realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID);
            return userRealm.getUserStoreManager().getSecondaryUserStoreManager(
                    IdentityRecoveryConstants.ASGARDEO_USER_DOMAIN_NAME);
        } else {
            int tenantId = IdentityTenantUtil.getTenantId(authenticatedUser.getTenantDomain());
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
            // Storing attribute values of Asgardeo users into user store database.
            if (isAsgardeoUser(userStoreManager) && isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
                if (userStoreManager instanceof UniqueIDJDBCUserStoreManager) {
                    String userId;
                    if (StringUtils.isNotBlank(authenticatedUser.getUserId())) {
                        userId = authenticatedUser.getUserId();
                    } else {
                        userId = ((UniqueIDJDBCUserStoreManager) userStoreManager).
                                getUserIDFromUserName(authenticatedUser.getUserName());
                    }
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
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityDataStoreService().storeInIdentityDataStore(
                    authenticatedUser.getUserName(), userStoreManager, PRE_SET_USER_CLAIM_VALUES, userClaims);
        } catch (UserStoreException e) {
            throw new IdentityEventException(
                    String.format("Error occurred while updating user claims related to %s event.", eventName), e);
        } catch (UserIdNotFoundException e) {
            IdentityRecoveryConstants.ErrorMessages error = IdentityRecoveryConstants
                    .ErrorMessages.ERROR_USER_ID_NOT_FOUND;
            throw new IdentityEventException(error.getCode(), error.getMessage(), e);
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

    /**
     * Check whether the user metadata management handler is enabled via event handler configuration.
     *
     * @return True if the feature is enabled, false otherwise.
     */
    private boolean isUserMetadataMgtHandlerEnabled() {

        String enablePropertyKey = IdentityRecoveryConstants.ENABLE_USER_METADATA_HANDLER;
        return this.configs != null && this.configs.getModuleProperties() != null &&
                Boolean.parseBoolean(configs.getModuleProperties().getProperty(enablePropertyKey));
    }

    public String getName() {

        return IdentityRecoveryConstants.USER_METADATA_MGT_HANDLER_NAME;
    }

    public String getFriendlyName() {

        return IdentityRecoveryConstants.USER_METADATA_MGT_HANDLER_FRIENDLY_NAME;
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 100;
    }
}
