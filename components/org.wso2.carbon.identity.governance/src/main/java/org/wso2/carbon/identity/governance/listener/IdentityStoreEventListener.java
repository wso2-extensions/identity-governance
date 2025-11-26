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
import org.apache.commons.lang.mutable.MutableBoolean;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.AbstractIdentityUserOperationEventListener;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.model.ExpressionOperation;
import org.wso2.carbon.user.core.model.OperationalCondition;
import org.wso2.carbon.user.core.model.UserClaimSearchEntry;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.governance.util.IdentityDataStoreUtil.isManagedInIdentityDataStoreByClaimConfig;
import static org.wso2.carbon.identity.governance.util.IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled;
import static org.wso2.carbon.identity.governance.util.IdentityDataStoreUtil.maskIfRequired;

public class IdentityStoreEventListener extends AbstractIdentityUserOperationEventListener {

    private static final Log log = LogFactory.getLog(IdentityStoreEventListener.class);
    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";
    private static final String PRE_USER_ADD_CLAIM_VALUES = "PreAddUserClaimValues";
    private static final String USER_OPERATION_EVENT_LISTENER_TYPE = "org.wso2.carbon.user.core.listener" +
            ".UserOperationEventListener";
    private static final String DATA_STORE_PROPERTY_NAME = "Data.Store";
    private static final String ENABLE_HYBRID_DATA_STORE_PROPERTY_NAME = "EnableHybridDataStore";
    private UserIdentityDataStore identityDataStore;
    private IdentityDataStoreService identityDataStoreService;
    private boolean isHybridDataStoreEnable = false;
    private static final String INVALID_OPERATION = "InvalidOperation";
    private static final String USER_IDENTITY_CLAIMS = "UserIdentityClaims";
    private static final String USER_IDENTITY_CLAIMS_MAP = "UserIdentityClaimsMap";

    public IdentityStoreEventListener() {

        identityDataStoreService = IdentityMgtServiceDataHolder.getInstance().getIdentityDataStoreService();
        Object hybridDataStoreEnableObject =
                IdentityUtil.readEventListenerProperty(USER_OPERATION_EVENT_LISTENER_TYPE, this.getClass().getName())
                        .getProperties().get(ENABLE_HYBRID_DATA_STORE_PROPERTY_NAME);
        if (hybridDataStoreEnableObject != null) {
            isHybridDataStoreEnable = Boolean.parseBoolean(hybridDataStoreEnableObject.toString());
        }
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

        boolean isUserStoreBasedIdentityDataStore = isUserStoreBasedIdentityDataStore();
        boolean isStoreIdentityClaimsInUserStoreEnabled =
                isStoreIdentityClaimsInUserStoreEnabled(userStoreManager);
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());

        if (!isUserStoreBasedIdentityDataStore && !isStoreIdentityClaimsInUserStoreEnabled) {
            Iterator<Map.Entry<String, String>> it = claims.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<String, String> claim = it.next();

                boolean storeInIdentityDataStore =
                        isManagedInIdentityDataStoreByClaimConfig(claim.getKey(), tenantDomain, userStoreDomain);
                if (storeInIdentityDataStore) {
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

            return identityDataStoreService.storeInIdentityDataStore(userName, userStoreManager,
                    PRE_USER_ADD_CLAIM_VALUES, userIdentityDataMap);
        } finally {
            // clear the thread local used to store identity claims
            IdentityUtil.threadLocalProperties.get().remove(USER_IDENTITY_CLAIMS);
            IdentityUtil.threadLocalProperties.get().remove(USER_IDENTITY_CLAIMS_MAP);
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

        return identityDataStoreService.storeInIdentityDataStore(userName, userStoreManager,
                PRE_SET_USER_CLAIM_VALUES, claims);
    }

    @Override
    public boolean doPreGetUserClaimValues(String userName, String[] claims, String profileName,
                                           Map<String, String> claimMap,
                                           UserStoreManager storeManager) {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPreGetUserClaimValues getting executed in the IdentityStoreEventListener for user: " +
                    userName);
        }

        // If hybrid data store is enabled, we need to send all claims to user store
        if (isHybridDataStoreEnable) {
            return true;
        }

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(storeManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = UserCoreUtil.getDomainName(storeManager.getRealmConfiguration());

        removeClaimsManagedInIdentityDataStore(claims, tenantDomain, userStoreDomain);
        return true;
    }

    /**
     * Removes the claims which are managed in identity data store from the given claims array.
     *
     * @param claims           Array of claims to be filtered.
     * @param tenantDomain     Tenant domain.
     * @param userStoreDomain  User store domain.
     */
    private static void removeClaimsManagedInIdentityDataStore(String[] claims, String tenantDomain,
                                                               String userStoreDomain) {

        int validCount = 0;

        for (int i = 0; i < claims.length; i++) {
            boolean shouldBeManagedInIdentityDataStore = isManagedInIdentityDataStoreByClaimConfig(claims[i],
                    tenantDomain, userStoreDomain);

            if (claims[i] != null && !shouldBeManagedInIdentityDataStore) {
                claims[validCount++] = claims[i];
            }
        }

        // Set the remaining elements to null.
        while (validCount < claims.length) {
            claims[validCount++] = null;
        }
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

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(storeManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        if (claimMap == null) {
            claimMap = new HashMap<>();
        }

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = UserCoreUtil.getDomainName(storeManager.getRealmConfiguration());

        if (!isHybridDataStoreEnable) {
            /*
            If hybrid data store is disabled, we need to use the identity claim value only from the identity data store.
            Hence, we need to remove the identity claim values from the claimMap to avoid use of values from user store
            for identity claims.
             */
            claimMap.entrySet().removeIf(
                    entry -> isManagedInIdentityDataStoreByClaimConfig(entry.getKey(),
                            tenantDomain, userStoreDomain));
        }

        // check if there are identity claims
        boolean containsIdentityClaims = false;
        for (String claim : claims) {
            if (isManagedInIdentityDataStoreByClaimConfig(claim, tenantDomain, userStoreDomain)) {
                containsIdentityClaims = true;
                break;
            }
        }
        // if there are no identity claims, let it go
        if (!containsIdentityClaims) {
            return true;
        }
        // there is/are identity claim/s . load the dto

        UserIdentityClaim identityDTO = identityDataStoreService.getIdentityClaimData(userName, storeManager);
        // if no user identity data found, just continue
        if (identityDTO == null) {
            return true;
        }
        // data found, add the values for security questions and identity claims
        String value;
        for (String claim : claims) {
            if (identityDTO.getUserIdentityDataMap().containsKey(claim)
                    && StringUtils.isNotBlank(value = identityDTO.getUserIdentityDataMap().get(claim))
                    && isManagedInIdentityDataStoreByClaimConfig(claim, tenantDomain, userStoreDomain)
            ) {
                claimMap.put(claim, value);
            }
        }
        return true;
    }

    @Override
    public boolean doPostGetUserClaimValue(String userName, String claim, List<String> claimValue, String profileName,
                                           UserStoreManager storeManager) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPostGetUserClaimValue getting executed in the IdentityStoreEventListener for user: " +
                    maskIfRequired(userName) + " and claim: " + claim);
        }

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(storeManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = UserCoreUtil.getDomainName(storeManager.getRealmConfiguration());
        if (!isManagedInIdentityDataStoreByClaimConfig(claim, tenantDomain, userStoreDomain)) {
            if (log.isDebugEnabled()) {
                log.debug("Claim: " + claim + " is not managed in identity data store. Hence returning.");
            }
            return true;
        }

        if (!isHybridDataStoreEnable) {
            /*
            If hybrid data store is disabled, we need to use the identity claim value only from the identity data store.
            Hence, we need to clear the claim value from user store to avoid use of values from user store
            for identity claims.
             */
            if (log.isDebugEnabled()) {
                log.debug("Clearing the claim value retrieved from user store for identity claim: " + claim);
            }
            claimValue.clear();
        }

        UserIdentityClaim identityDTO = identityDataStoreService.getIdentityClaimData(userName, storeManager);
        // If no user identity data found, just continue.
        if (identityDTO == null) {
            if (log.isDebugEnabled()) {
                log.debug("No identity claim data found in identity data store for user: " + maskIfRequired(userName));
            }
            return true;
        }

        // Data found, add the value for the identity claim.
        if (identityDTO.getUserIdentityDataMap().containsKey(claim)) {
            String value = identityDTO.getUserIdentityDataMap().get(claim);
            if (StringUtils.isNotBlank(value)) {
                if (log.isDebugEnabled()) {
                    log.debug("Setting the identity claim value retrieved from identity data store " +
                            "for claim: " + claim);
                }
                claimValue.clear();
                claimValue.add(value);
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
        if (StringUtils.isNotBlank(claim) && claim.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI_PREFIX)) {
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
        if (StringUtils.isNotBlank(claimURI) && claimURI.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI_PREFIX)) {
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

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());
        if (!isManagedInIdentityDataStoreByClaimConfig(claimUri, tenantDomain, userStoreDomain)) {
            return true;
        }

        if (log.isDebugEnabled()) {
            log.debug("doPreGetUserList executed in the IdentityStoreEventListener for claim URI: " + claimUri +
                    " and claim value: " + claimValue);
        }

        try {
            List<String> userIds = identityDataStoreService.listUsersByClaimURIAndValue(claimUri,
                    claimValue, userStoreManager);

            // If this is the primary domain, all the users will be retrieved since the primary domain is not appended
            // to the user name in the IDN table. So we have to filter users belongs to primary in Java level.
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
     * Filter users that match the identity claims specified in the filter condition.
     *
     * @param condition            Condition to be considered for filtering.
     * @param filteredUserNameList Username list to be updated and returned.
     * @param userStoreManager     UserStoreManager.
     * @param domain               User store domain.
     * @return
     * @throws UserStoreException
     */
    @Override
    public boolean doPreGetUserList(Condition condition, List<String> filteredUserNameList, UserStoreManager
            userStoreManager, String domain) throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        MutableBoolean isFirstClaimFilter = new MutableBoolean(true);
        filterUsers(condition, userStoreManager, domain, filteredUserNameList, isFirstClaimFilter);
        return true;
    }

    /**
     * Recursively search within the condition for expression conditions that contain identity claims and filter users
     * matched with each such claims. After filtering users for each claim, the common set of users will be retained in
     * the final user list to be returned.
     *
     * @param condition            Condition to be considered for filtering.
     * @param userManager          UserStoreManager.
     * @param domain               User store domain.
     * @param filteredUserNameList Username list to be returned from the listener.
     * @param isFirstClaimFilter   Whether this is the first claim being filtered. This is used to decide whether to
     *                             add or retain username list to the final username list.
     * @throws UserStoreException
     */
    private void filterUsers(Condition condition, UserStoreManager userManager, String domain,
                             List<String> filteredUserNameList, MutableBoolean isFirstClaimFilter)
            throws UserStoreException {

        if (condition instanceof ExpressionCondition) {
            ExpressionCondition expressionCondition = (ExpressionCondition) condition;
            String claimUri = expressionCondition.getAttributeName();

            String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
            String userStoreDomain = UserCoreUtil.getDomainName(userManager.getRealmConfiguration());

            if (isManagedInIdentityDataStoreByClaimConfig(claimUri, tenantDomain, userStoreDomain)) {
                String claimValue = expressionCondition.getAttributeValue();
                String operation = expressionCondition.getOperation();

                try {
                    List<String> usernames;
                   if (ExpressionOperation.NE.toString().equals(operation)) {
                       usernames = identityDataStoreService.getUserNamesByClaimURINotEqualValue(condition, claimUri,
                               claimValue, userManager);
                   } else if (ExpressionOperation.GE.toString().equals(operation)) {
                       int tenantId = userManager.getTenantId();
                       String domainName = UserCoreUtil.getDomainName(userManager.getRealmConfiguration());
                       usernames = identityDataStoreService
                               .getUserNamesMoreThanProvidedClaimValue(claimUri, claimValue, tenantId);
                       removeDomainNotMatchedUsers(usernames, domainName);
                   } else if (ExpressionOperation.LE.toString().equals(operation)) {
                       int tenantId = userManager.getTenantId();
                       String domainName = UserCoreUtil.getDomainName(userManager.getRealmConfiguration());
                       usernames = identityDataStoreService
                               .getUserNamesLessThanProvidedClaimValue(claimUri, claimValue, tenantId);
                       removeDomainNotMatchedUsers(usernames, domainName);
                   } else {
                        usernames = identityDataStoreService.listUsersByClaimURIAndValue(claimUri,
                                getClaimValueForOperation(operation, claimValue), userManager);
                   }
                    updateUserList(usernames, filteredUserNameList, domain, isFirstClaimFilter);

                    if (log.isDebugEnabled()) {
                        log.debug("Retrieved " + usernames.size() + " users for claim: " + claimUri);
                    }
                } catch (IdentityException e) {
                    throw new UserStoreException("Error while listing the users for given claim: " + claimUri, e);
                }

                // Remove expression conditions with identity claims from the condition.
                ((ExpressionCondition) condition).setAttributeName(null);
                ((ExpressionCondition) condition).setAttributeValue(null);
                ((ExpressionCondition) condition).setOperation(null);
            }
        } else if (condition instanceof OperationalCondition) {
            Condition leftCondition = ((OperationalCondition) condition).getLeftCondition();
            filterUsers(leftCondition, userManager, domain, filteredUserNameList, isFirstClaimFilter);
            Condition rightCondition = ((OperationalCondition) condition).getRightCondition();
            filterUsers(rightCondition, userManager, domain, filteredUserNameList, isFirstClaimFilter);
        }
    }

    private void removeDomainNotMatchedUsers(List<String> usernames, String domainName) {

        String primaryDomainName = resolvePrimaryUserStoreDomainName();
        Iterator<String> iterator = usernames.iterator();
        while (iterator.hasNext()) {
            String username = iterator.next();
            if (StringUtils.equals(primaryDomainName, domainName)) {
                if (username.contains(UserCoreConstants.DOMAIN_SEPARATOR)) {
                    iterator.remove();
                }
            } else {
                if (!username.startsWith(domainName + UserCoreConstants.DOMAIN_SEPARATOR)) {
                    iterator.remove();
                }
            }
        }
    }

    /**
     * Update the username list to be returned, with each identity claim filtering result.
     *
     * @param usernames            Usernames returned for an identity claim filter.
     * @param filteredUserNameList Username list to be returned from the listener.
     * @param userStoreDomain      User store domain.
     * @param isFirstClaimFilter   Whether this is the first claim being filtered. This is used to decide whether to
     *                             add or retain username list to the final username list.
     */
    private void updateUserList(List<String> usernames, List<String> filteredUserNameList, String userStoreDomain,
                                MutableBoolean isFirstClaimFilter) {

         /* If this is the primary domain, all the users will be retrieved since the primary domain is
         not appended to the user name in the IDN table. So we have to filter users belongs to primary
         in Java level. */
        if (StringUtils.equalsIgnoreCase(userStoreDomain, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME)) {
            List<String> usersInPrimaryDomain = new ArrayList<>();
            for (String username : usernames) {
                if (!StringUtils.contains(username, UserCoreConstants.DOMAIN_SEPARATOR) ||
                        StringUtils.startsWith(username, UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME +
                                UserCoreConstants.DOMAIN_SEPARATOR)) {
                    usersInPrimaryDomain.add(username);
                }
            }
            usernames = usersInPrimaryDomain;
        }
        usernames = Arrays.asList(usernames.toArray(new String[0]));

        /* If this is the first claim that was filtered, all users are added to the final user list.
         Otherwise, final user list is updated with common users for all claims filtered so far. */
        if (isFirstClaimFilter.booleanValue()) {
            filteredUserNameList.addAll(usernames);
            isFirstClaimFilter.setValue(false);
        } else {
            filteredUserNameList.retainAll(usernames);
        }
    }

    /**
     * Add wildcards to the claim value to match the specified operation for filtering.
     *
     * @param operation  Filtering operation specified.
     * @param claimValue Attribute value.
     * @return attribute value with wildcards.
     */
    private String getClaimValueForOperation(String operation, String claimValue) {

        if (ExpressionOperation.EW.toString().equals(operation)) {
            claimValue = "%" + claimValue;
        } else if (ExpressionOperation.CO.toString().equals(operation)) {
            claimValue = "%" + claimValue + "%";
        } else if (ExpressionOperation.SW.toString().equals(operation)) {
            claimValue = claimValue + "%";
        }
        return claimValue;
    }

    public boolean doPreGetPaginatedUserList(Condition condition, List<String> identityClaimFilteredUserNames,
                                             String domain, UserStoreManager userStoreManager, int limit, int offset)
            throws UserStoreException {

        if (!isEnable()) {
            return true;
        }

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
            return true;
        }

        List<ExpressionCondition> identityClaimFilterConditions = new ArrayList<>();
        try {
            // Extract identity Claim filter-conditions from the given conditions.
            extractIdentityClaimFilterConditions(condition, identityClaimFilterConditions, domain);
            if (!identityClaimFilterConditions.isEmpty()) {
                identityDataStoreService.listPaginatedUsersByClaimURIAndValue(identityClaimFilterConditions,
                        identityClaimFilteredUserNames, domain, userStoreManager, limit, offset);
            }
        } catch (IdentityException e) {
            throw new UserStoreException("Error while listing the users for identity claim filters with pagination " +
                    "parameters.", e);
        }
        return true;
    }

    private void extractIdentityClaimFilterConditions(Condition condition,
                                                      List<ExpressionCondition> expressionConditions,
                                                      String userStoreDomain) {

        if (condition instanceof ExpressionCondition) {
            ExpressionCondition expressionCondition = (ExpressionCondition) condition;
            String claimUri = expressionCondition.getAttributeName();
            String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();

            if (isManagedInIdentityDataStoreByClaimConfig(claimUri, tenantDomain, userStoreDomain)) {
                ExpressionCondition expressionConditionWithIdentityClaimFilter =
                        new ExpressionCondition(expressionCondition.getOperation(),
                                expressionCondition.getAttributeName(), expressionCondition.getAttributeValue());

                // Adding a copy of expression condition.
                expressionConditions.add(expressionConditionWithIdentityClaimFilter);

                // Remove expression conditions with identity claims from the condition.
                 expressionCondition.setAttributeName(null);
                 expressionCondition.setAttributeValue(null);
                 expressionCondition.setOperation(null);
            }

        } else if (condition instanceof OperationalCondition) {
            Condition leftCondition = ((OperationalCondition) condition).getLeftCondition();
            extractIdentityClaimFilterConditions(leftCondition, expressionConditions, userStoreDomain);
            Condition rightCondition = ((OperationalCondition) condition).getRightCondition();
            extractIdentityClaimFilterConditions(rightCondition, expressionConditions, userStoreDomain);
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
            identityDataStoreService.removeIdentityClaims(userName, userStoreManager);
            return true;
        } catch (IdentityException e) {
            throw new UserStoreException("Error while removing user: " + userName + " from identity data store", e);
        }
    }

    /**
     * Store identity claims in the IdentityDataStore.
     *
     * @param userName
     * @param userStoreManager
     * @param operationType
     * @param claims
     * @return
     * @throws UserStoreException
     */
    @Deprecated
    private boolean storeInIdentityDataStore(String userName,
                                             UserStoreManager userStoreManager,
                                             String operationType,
                                             Map<String, String> claims) throws UserStoreException {

        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            log.debug("All claims are managed in user store. Hence no need to filter identity claims.");
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

                String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
                String userStoreDomain = UserCoreUtil.getDomainName(userStoreManager.getRealmConfiguration());

                Iterator<Map.Entry<String, String>> it = claims.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry<String, String> claim = it.next();
                    String key = claim.getKey();
                    String value = claim.getValue();
                    if (isManagedInIdentityDataStoreByClaimConfig(key, tenantDomain, userStoreDomain)) {
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

        // Pulling the UserStoreManager using the realm service as it is not passed to the listener.
        UserStoreManager userStoreManager = getUserStoreManager();
        if (isUserStoreBasedIdentityDataStore()) {
            return true;
        }

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();

        // Check if there are identity claims.
        boolean containsIdentityStoreManagedClaims = false;
        for (String claim : claims) {
            if (isManagedInIdentityDataStoreByClaimConfig(claim, tenantDomain, null)) {
                containsIdentityStoreManagedClaims = true;
                break;
            }
        }

        // If there are no identity claims, let it go.
        if (!containsIdentityStoreManagedClaims) {
            return true;
        }

        for (UserClaimSearchEntry userClaimSearchEntry : userClaimSearchEntries) {

            String username = userClaimSearchEntry.getUserName();

            if (username == null) {
                if (log.isDebugEnabled()) {
                    log.debug("Username found to be null while method doPostGetUsersClaimValues getting executed in " +
                            "the IdentityStoreEventListener.");
                }
                continue;
            }

            if (isStoreIdentityClaimsInUserStoreEnabled(userStoreManager
                    .getSecondaryUserStoreManager(UserCoreUtil.extractDomainFromName(username)))) {
                continue;
            }

            if (log.isDebugEnabled()) {
                log.debug("Method doPostGetUsersClaimValues getting executed in the IdentityStoreEventListener for " +
                        "user: " + username);
            }

            if (userClaimSearchEntry.getClaims() == null) {
                userClaimSearchEntry.setClaims(new HashMap<String, String>());
            }

            // Get user store domain for this specific user.
            String userStoreDomain = UserCoreUtil.extractDomainFromName(username);

            if (!isHybridDataStoreEnable) {
                /*
                If hybrid data store is disabled, we need to use the identity claim value only from the identity data
                store. Hence, we need to remove the identity claim values from the claimMap to avoid use of values from
                user store for identity claims.
                 */
                userClaimSearchEntry.getClaims().entrySet().removeIf(
                        entry -> isManagedInIdentityDataStoreByClaimConfig(entry.getKey(),
                                tenantDomain, userStoreDomain));
            }

            // There is/are identity claim/s load the dto.
            UserIdentityClaim identityDTO = identityDataStoreService
                    .getIdentityClaimData(userClaimSearchEntry.getUserName(), userStoreManager
                    .getSecondaryUserStoreManager(UserCoreUtil.extractDomainFromName(username)));

            // If no user identity data found, just continue.
            if (identityDTO == null) {
                continue;
            }

            // Data found, add the values for security questions and identity claims.
            for (String claim : claims) {
                if (identityDTO.getUserIdentityDataMap().containsKey(claim) &&
                        isManagedInIdentityDataStoreByClaimConfig(claim, tenantDomain, userStoreDomain)) {
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

    private boolean isUserStoreBasedIdentityDataStore() {

        return identityDataStoreService.isUserStoreBasedIdentityDataStore();
    }

    private String resolvePrimaryUserStoreDomainName() {

        RealmConfiguration realmConfiguration = IdentityMgtServiceDataHolder.getInstance().getRealmService().
                getBootstrapRealmConfiguration();
        if (realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME) != null) {
            return realmConfiguration.getUserStoreProperty(
                    UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME).toUpperCase();
        }
        return UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;
    }
}
