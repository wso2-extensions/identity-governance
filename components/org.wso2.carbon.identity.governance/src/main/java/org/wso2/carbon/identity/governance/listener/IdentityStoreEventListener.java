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
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserStoreBasedIdentityDataStore;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.UserClaimSearchEntry;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class IdentityStoreEventListener extends AbstractIdentityUserOperationEventListener {

    private static final Log log = LogFactory.getLog(IdentityStoreEventListener.class);
    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";
    private static final String PRE_USER_ADD_CLAIM_VALUES = "PreAddUserClaimValues";
    private static final String USER_OPERATION_EVENT_LISTENER_TYPE = "org.wso2.carbon.user.core.listener" +
            ".UserOperationEventListener";
    private static final String DATA_STORE_PROPERTY_NAME = "Data.Store";
    private UserIdentityDataStore identityDataStore;
    private static final String INVALID_OPERATION = "InvalidOperation";
    private static final String USER_IDENTITY_CLAIMS = "UserIdentityClaims";

    public IdentityStoreEventListener() throws IllegalAccessException, InstantiationException, ClassNotFoundException {

        String storeClassName = IdentityUtil.readEventListenerProperty(USER_OPERATION_EVENT_LISTENER_TYPE,
                this.getClass().getName()).getProperties().get(DATA_STORE_PROPERTY_NAME).toString();
        Class clazz = Class.forName(storeClassName.trim());
        identityDataStore = (UserIdentityDataStore) clazz.newInstance();
    }

    @Override
    public int getExecutionOrderId() {

        int orderId = getOrderId();
        if (orderId != IdentityCoreConstants.EVENT_LISTENER_ORDER_ID) {
            return orderId;
        }
        return 100;
    }

    /**
     * In this method we temporarily hold the Identity Claim data related to the user being added by storing it in a
     * thread local. Upon successful addition of the user these claims will be persisted to the IdentityDataStore.
     *
     * @param userName
     * @param credential
     * @param roleList
     * @param claims
     * @param profile
     * @param userStoreManager
     * @return
     * @throws UserStoreException
     */
    @Override
    public boolean doPreAddUser(String userName,
                                Object credential,
                                String[] roleList,
                                Map<String, String> claims,
                                String profile,
                                UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPreAddUser executed in the IdentityStoreEventListener for user: " + userName);
        }

        // clear the existing thread local
        IdentityUtil.threadLocalProperties.get().remove(USER_IDENTITY_CLAIMS);

        Map<String, String> userDataMap = new HashMap<>();

        Iterator<Map.Entry<String, String>> it = claims.entrySet().iterator();
        while (it.hasNext()) {

            Map.Entry<String, String> claim = it.next();
            if (claim.getKey().contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)
                    && !(identityDataStore instanceof UserStoreBasedIdentityDataStore)) {
                // add the identity claim to temp map
                userDataMap.put(claim.getKey(), claim.getValue());
                // we remove the identity claims to prevent it from getting stored in user store
                // before the user is successfully added
                if (log.isDebugEnabled()) {
                    log.debug(claim.getKey() + " claim added to thread local for user: " + userName + " in preUserAdd");
                }
                it.remove();
            }
        }

        UserIdentityClaim userIdentityClaim = new UserIdentityClaim(userName, userDataMap);
        userIdentityClaim.setTenantId(userStoreManager.getTenantId());
        // Add the identity claims to to thread local, these claims will be stored to the identityDataStore to the
        // in the PostAddUser method
        IdentityUtil.threadLocalProperties.get().put(USER_IDENTITY_CLAIMS, userIdentityClaim);
        return true;

    }

    /**
     * Persist the Identity Claims we added to thread local in doPreAddUser() operation after the user was
     * successfully added.
     *
     * @param userName
     * @param credential
     * @param roleList
     * @param claims
     * @param profile
     * @param userStoreManager
     * @return
     * @throws UserStoreException
     */
    @Override
    public boolean doPostAddUser(String userName,
                                 Object credential,
                                 String[] roleList,
                                 Map<String, String> claims,
                                 String profile,
                                 UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPostAddUser executed in the IdentityStoreEventListener for user: " + userName);
        }

        try {
            // read the thread local for temporarily stored Identity Claims
            UserIdentityClaim userIdentityClaims =
                    (UserIdentityClaim) IdentityUtil.threadLocalProperties.get().get(USER_IDENTITY_CLAIMS);

            Map<String, String> userIdentityDataMap;
            if (userIdentityClaims == null) {
                userIdentityDataMap = new HashMap<>();
            } else {
                userIdentityDataMap = userIdentityClaims.getUserIdentityDataMap();
                claims.putAll(userIdentityDataMap);
            }

            return storeInIdentityDataStore(userName, userStoreManager, PRE_USER_ADD_CLAIM_VALUES, userIdentityDataMap);
        } finally {
            // clear the thread local used to store identity claims
            IdentityUtil.threadLocalProperties.get().remove(USER_IDENTITY_CLAIMS);
        }
    }

    /**
     * As in the above method the user account lock claim, primary challenges
     * claim will be separately handled. Identity claims will be removed from
     * the claim set before adding claims to the user store.
     */
    @Override
    public boolean doPreSetUserClaimValues(String userName, Map<String, String> claims,
                                           String profileName, UserStoreManager userStoreManager)
            throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPreSetUserClaimValues executed in the IdentityStoreEventListener for user: " + userName);
        }

        boolean accountLocked = Boolean.parseBoolean(claims.get(UserIdentityDataStore.ACCOUNT_LOCK));
        if (accountLocked) {
            IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(UserCoreConstants
                    .ErrorCode.USER_IS_LOCKED);
            IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
        }

        return storeInIdentityDataStore(userName, userStoreManager, PRE_SET_USER_CLAIM_VALUES, claims);
    }

    @Override
    public boolean doPostGetUserClaimValues(String userName, String[] claims, String profileName,
                                            Map<String, String> claimMap,
                                            UserStoreManager storeManager) {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPostGetUserClaimValues getting executed in the IdentityStoreEventListener for user: " +
                    userName);
        }

        // No need to separately handle if identity `data store is user store based
        if (identityDataStore instanceof UserStoreBasedIdentityDataStore) {
            return true;
        }

        if (claimMap == null) {
            claimMap = new HashMap<>();
        }
        // check if there are identity claims
        boolean containsIdentityClaims = false;
        for (String claim : claims) {
            if (claim.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
                containsIdentityClaims = true;
                break;
            }
        }
        // if there are no identity claims, let it go
        if (!containsIdentityClaims) {
            return true;
        }
        // there is/are identity claim/s . load the dto

        UserIdentityClaim identityDTO = identityDataStore.load(userName, storeManager);
        // if no user identity data found, just continue
        if (identityDTO == null) {
            return true;
        }
        // data found, add the values for security questions and identity claims
        String value;
        for (String claim : claims) {
            if (identityDTO.getUserIdentityDataMap().containsKey(claim)
                    && StringUtils.isNotBlank(value = identityDTO.getUserIdentityDataMap().get(claim))) {
                claimMap.put(claim, value);
            }
        }
        return true;
    }

    public boolean doPreGetUserClaimValue(String userName,
                                          String claim,
                                          String profileName,
                                          UserStoreManager storeManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        //This operation is not supported for Identity Claims
        if (StringUtils.isNotBlank(claim) && claim.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
            throw new UserStoreException(INVALID_OPERATION + " This operation is not supported for Identity claims");
        }
        return true;
    }

    public boolean doPreSetUserClaimValue(String userName,
                                          String claimURI,
                                          String claimValue,
                                          String profileName,
                                          UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }
        //This operation is not supported for Identity Claims
        if (StringUtils.isNotBlank(claimURI) && claimURI.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
            throw new UserStoreException(INVALID_OPERATION + " This operation is not supported for Identity claims");
        }
        return true;
    }

    @Override
    public boolean doPreGetUserList(String claimUri, String claimValue, List<String> returnUserNameList,
                                    UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (!StringUtils.contains(claimUri, UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPreGetUserList executed in the IdentityStoreEventListener for claim URI: " + claimUri +
                    " and claim value: " + claimValue);
        }

        try {
            List<String> userIds = identityDataStore.list(claimUri, claimValue, userStoreManager);

            // If this is the primary domain, all the users will be retrieved since the primary domain is not appended
            // to the user name in the IDN table. So we have to filter users belongs to primary in Java level.
            String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());
            if (StringUtils.equalsIgnoreCase(userStoreDomain, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME)) {
                for (String userId : userIds) {
                    if (!StringUtils.contains(userId, UserCoreConstants.DOMAIN_SEPARATOR) ||
                            StringUtils.startsWith(userId, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME +
                                    UserCoreConstants.DOMAIN_SEPARATOR)) {
                        returnUserNameList.add(userId);
                    }
                }
            } else {
                returnUserNameList.addAll(userIds);
            }

            if (log.isDebugEnabled()) {
                log.debug("Retrieved " + userIds.size() + " users for claim: " + claimUri);
            }
            return true;
        } catch (IdentityException e) {
            throw new UserStoreException("Error while listing the users for given claim: " + claimUri, e);
        }
    }

    /**
     * Remove identity claims related to the user being deleted from the IdentityDataStore.
     *
     * @param userName
     * @param userStoreManager
     * @return
     * @throws UserStoreException
     */
    @Override
    public boolean doPostDeleteUser(String userName, UserStoreManager userStoreManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPostDeleteUser executed in the IdentityStoreEventListener for user: " + userName);
        }

        // remove identity claims of user deleted from the identity store
        try {
            if (log.isDebugEnabled()) {
                log.debug("Removed Identity Claims of user: " + userName + " from IdentityDataStore.");
            }
            identityDataStore.remove(userName, userStoreManager);
            return true;
        } catch (IdentityException e) {
            throw new UserStoreException("Error while removing user: " + userName + " from identity data store", e);
        }
    }

    /**
     * Store identity claims in the IdentityDataStore
     *
     * @param userName
     * @param userStoreManager
     * @param operationType
     * @param claims
     * @return
     * @throws UserStoreException
     */
    private boolean storeInIdentityDataStore(String userName,
                                             UserStoreManager userStoreManager,
                                             String operationType,
                                             Map<String, String> claims) throws UserStoreException {

        // No need to separately handle if data identityDataStore is user store based
        if (identityDataStore instanceof UserStoreBasedIdentityDataStore) {
            return true;
        }

        // Top level try and finally blocks are used to unset thread local variables
        try {
            if (!IdentityUtil.threadLocalProperties.get().containsKey(operationType)) {
                IdentityUtil.threadLocalProperties.get().put(operationType, true);

                UserIdentityClaim userIdentityClaim = null;
                if (!StringUtils.equalsIgnoreCase(operationType, PRE_USER_ADD_CLAIM_VALUES)) {
                    // we avoid loading claims for pre user add operations
                    userIdentityClaim = identityDataStore.load(userName, userStoreManager);
                }

                if (userIdentityClaim == null) {
                    userIdentityClaim = new UserIdentityClaim(userName);
                }

                Iterator<Map.Entry<String, String>> it = claims.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry<String, String> claim = it.next();
                    String key = claim.getKey();
                    String value = claim.getValue();
                    if (key.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
                        userIdentityClaim.setUserIdentityDataClaim(key, value);
                        it.remove();
                    }
                }

                // storing the identity claims and challenge questions
                try {
                    identityDataStore.store(userIdentityClaim, userStoreManager);
                } catch (IdentityException e) {
                    throw new UserStoreException(
                            "Error while saving user identityDataStore data for user : " + userName, e);
                }
            }
            return true;
        } finally {
            // Remove thread local variable
            IdentityUtil.threadLocalProperties.get().remove(operationType);
        }

    }

    @Override
    public boolean doPostGetUsersClaimValues(String[] userNames, String[] claims, String profileName,
                                             UserClaimSearchEntry[] userClaimSearchEntries) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("Method doPostGetUsersClaimValues getting executed in the IdentityStoreEventListener.");
        }

        // No need to separately handle if identity data store is user store based.
        if (identityDataStore instanceof UserStoreBasedIdentityDataStore) {
            return true;
        }

        // Check if there are identity claims.
        boolean containsIdentityClaims = false;
        for (String claim : claims) {
            if (claim.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI)) {
                containsIdentityClaims = true;
                break;
            }
        }

        // If there are no identity claims, let it go.
        if (!containsIdentityClaims) {
            return true;
        }

        // Pulling the UserStoreManager using the realm service as it is not passed to the listener.
        UserStoreManager userStoreManager = getUserStoreManager();

        for (UserClaimSearchEntry userClaimSearchEntry : userClaimSearchEntries) {

            String username = userClaimSearchEntry.getUserName();

            if (username == null) {
                if (log.isDebugEnabled()) {
                    log.debug("Username found to be null while method doPostGetUsersClaimValues getting executed in " +
                            "the IdentityStoreEventListener.");
                }
                continue;
            }

            if (log.isDebugEnabled()) {
                log.debug("Method doPostGetUsersClaimValues getting executed in the IdentityStoreEventListener for " +
                        "user: " + username);
            }

            if (userClaimSearchEntry.getClaims() == null) {
                userClaimSearchEntry.setClaims(new HashMap<String, String>());
            }

            // There is/are identity claim/s load the dto.
            UserIdentityClaim identityDTO = identityDataStore.load(userClaimSearchEntry.getUserName(), userStoreManager
                    .getSecondaryUserStoreManager(UserCoreUtil.extractDomainFromName(username)));

            // If no user identity data found, just continue.
            if (identityDTO == null) {
                continue;
            }

            // Data found, add the values for security questions and identity claims.
            for (String claim : claims) {
                if (identityDTO.getUserIdentityDataMap().containsKey(claim)) {
                    userClaimSearchEntry.getClaims().put(claim, identityDTO.getUserIdentityDataMap().get(claim));
                }
            }
        }
        return true;
    }

    private UserStoreManager getUserStoreManager() throws UserStoreException {

        RealmService realmService = IdentityMgtServiceDataHolder.getInstance().getRealmService();
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();

        UserRealm userRealm;
        try {
            int tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw new UserStoreException("Error occurred while retrieving user realm.", e);
        }
        return userRealm.getUserStoreManager();
    }
}
