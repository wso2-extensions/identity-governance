/*
 * Copyright (c) 2023-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance.service;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.model.IdentityErrorMsgContext;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.store.JDBCIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserStoreBasedIdentityDataStore;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;

import java.util.*;

/**
 * Implementation of the IdentityDataStoreService.
 */
public class IdentityDataStoreServiceImpl implements IdentityDataStoreService {

    private UserIdentityDataStore identityDataStore;
    private ClaimMetadataManagementService claimMetadataManagementService;

    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";
    private static final String PRE_USER_ADD_CLAIM_VALUES = "PreAddUserClaimValues";
    private static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";
    private static final String IDENTITY_DATA_STORE_TYPE = "IdentityDataStore.DataStoreType";
    private static final String DEFAULT_JDBC_IDENTITY_DATA_STORE =
            "org.wso2.carbon.identity.governance.store.JDBCIdentityDataStore";
    private static final String DEFAULT_USER_STORE_BASED_IDENTITY_DATA_STORE =
            "org.wso2.carbon.identity.governance.store.UserStoreBasedIdentityDataStore";

    public IdentityDataStoreServiceImpl() throws ClassNotFoundException, InstantiationException,
            IllegalAccessException {

        String storeClassName = IdentityUtil.getProperty(IDENTITY_DATA_STORE_TYPE);
        if (DEFAULT_JDBC_IDENTITY_DATA_STORE.equals(storeClassName)) {
            identityDataStore =  new JDBCIdentityDataStore();
        } else if (DEFAULT_USER_STORE_BASED_IDENTITY_DATA_STORE.equals(storeClassName)) {
            identityDataStore = new UserStoreBasedIdentityDataStore();
        } else {
            Class clazz = Class.forName(storeClassName.trim());
            identityDataStore = (UserIdentityDataStore) clazz.newInstance();
        }
        claimMetadataManagementService = IdentityMgtServiceDataHolder.getInstance()
                .getClaimMetadataManagementService();
    }

    @Override
    public boolean storeInIdentityDataStore(String userName, UserStoreManager userStoreManager, String operationType,
                                            Map<String, String> claims) throws UserStoreException {

        // Check if the user is locked.
        if (PRE_SET_USER_CLAIM_VALUES.equals(operationType)) {
            boolean isAccountLocked = Boolean.parseBoolean(claims.get(UserIdentityDataStore.ACCOUNT_LOCK));
            if (isAccountLocked) {
                IdentityErrorMsgContext customErrorMessageContext = new IdentityErrorMsgContext(UserCoreConstants
                        .ErrorCode.USER_IS_LOCKED);
                IdentityUtil.setIdentityErrorMsg(customErrorMessageContext);
            }
        }

        // Extract claims that have a configured store location.
        Iterator<Map.Entry<String, String>> claimIterator = claims.entrySet().iterator();
        String storeLocation ;
        // This map will hold the claims that have a configured store location.
        Map<String, List<String>> storeLocationConfiguredClaims = new HashMap<>();

        //Retrieve tenant domain from user.
         int tenantId = IdentityTenantUtil.getTenantIdOfUser(userName);
        String tenantDomain = IdentityTenantUtil.getTenantDomain(tenantId);

        // Iterate through the claimMap to find claims with a configured store location.
        while (claimIterator.hasNext()) {
            Map.Entry<String, String> claim = claimIterator.next();
            String key = claim.getKey();
            String value = claim.getValue();
            if (StringUtils.isEmpty(key)) {
                continue;
            }
            try {
                storeLocation = claimMetadataManagementService.getLocalClaim(key, tenantDomain).get().getClaimProperty(ClaimConstants.SKIP_USER_STORE);
            } catch (ClaimMetadataException e) {
                throw new UserStoreException("Error while retrieving claim metadata for claim: " + key, e);
            }
            if (!StringUtils.isEmpty(storeLocation)){
                // If the claim has a configured storing location, we remove the claim from the claimMap
                // and add it to the storeLocationConfiguredClaims map with store location as the second value.
                storeLocationConfiguredClaims.put(key, Arrays.asList(value, storeLocation));
                claimIterator.remove();
            }
        }

        // When user store–based identity data storage is enabled at the server level,
        // attributes (claims) can be handled in one of two ways:
        // 1. All claims are stored in the user store.
        // 2. Specific claims can be configured with a storage location and stored in the identity data store.
        if (isUserStoreBasedIdentityDataStore() || isStoreIdentityClaimsInUserStoreEnabled(userStoreManager)) {
            if (storeLocationConfiguredClaims.isEmpty()){
                return true;
            }
        }

        // Top level try and finally blocks are used to unset thread local variables.
        try {
            if (!IdentityUtil.threadLocalProperties.get().containsKey(operationType)) {
                IdentityUtil.threadLocalProperties.get().put(operationType, true);

                UserIdentityClaim userIdentityClaim = null;
                if (!StringUtils.equalsIgnoreCase(operationType, PRE_USER_ADD_CLAIM_VALUES)) {
                    // We avoid loading claims for pre user add operations.
                    userIdentityClaim = identityDataStore.load(userName, userStoreManager);
                }

                if (userIdentityClaim == null) {
                    userIdentityClaim = new UserIdentityClaim(userName);
                }

                // Process claims that do not have a configured store location.
                Iterator<Map.Entry<String, String>> claimsIterator = claims.entrySet().iterator();

                while (claimsIterator.hasNext()) {
                    Map.Entry<String, String> claim = claimsIterator.next();
                    String key = claim.getKey();
                    String value = claim.getValue();
                    if (StringUtils.isEmpty(key)) {
                        continue;
                    }
                    if (key.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI_PREFIX)) {
                        userIdentityClaim.setUserIdentityDataClaim(key, value);
                        claimsIterator.remove();
                    }
                }

                // Process claims that have a configured store location.
                if (!storeLocationConfiguredClaims.isEmpty()) {
                    for (Map.Entry<String, List<String>> entry : storeLocationConfiguredClaims.entrySet()) {
                        String key = entry.getKey();
                        List<String> valueAndStoreLocation = entry.getValue();
                        String value = valueAndStoreLocation.get(0);
                        storeLocation = valueAndStoreLocation.get(1);
                        if (Boolean.parseBoolean(storeLocation)) {
                            userIdentityClaim.setUserIdentityDataClaim(key, value);
                        }else{
                            // If the store location is not IDENTITY_DB, we assume it is a user store claim.
                            // Add the claim back to claims to be stored in the user store.
                            claims.put(key, value);
                        }
                    }
                }

                // Storing the identity claims and challenge questions.
                try {
                    identityDataStore.store(userIdentityClaim, userStoreManager);
                } catch (IdentityException e) {
                    throw new UserStoreException(
                            "Error while saving user identityDataStore data for user : " + userName, e);
                }
            }
            return true;
        } finally {
            // Remove thread local variable.
            IdentityUtil.threadLocalProperties.get().remove(operationType);
        }
    }

    @Override
    public UserIdentityClaim getIdentityClaimData(String username, UserStoreManager userStoreManager) {

        return identityDataStore.load(username, userStoreManager);
    }

    @Override
    public List<String> listUsersByClaimURIAndValue(String claimURI, String claimValue,
                                                    UserStoreManager userStoreManager) throws IdentityException {

        return identityDataStore.list(claimURI, claimValue, userStoreManager);
    }

    @Override
    public List<String> listPaginatedUsersByClaimURIAndValue(List<ExpressionCondition> expressionConditions,
                                                             List<String> identityClaimFilteredUserNames, String domain,
                                                             UserStoreManager userStoreManager, int limit,
                                                             int offset) throws IdentityException {

        return identityDataStore.listPaginatedUsersNames(expressionConditions, identityClaimFilteredUserNames, domain,
                userStoreManager, limit, offset);
    }

    @Override
    public void removeIdentityClaims(String username, UserStoreManager userStoreManager) throws IdentityException {

        identityDataStore.remove(username, userStoreManager);
    }

    @Override
    public List<String> getUserNamesByClaimURINotEqualValue(Condition condition, String claimURI, String claimValue,
                                                            UserStoreManager userStoreManager)
            throws IdentityException {

        return identityDataStore.getUserNamesByClaimURINotEqualValue(condition, claimURI, claimValue, userStoreManager);
    }

    @Override
    public List<String> getUserNamesLessThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        return identityDataStore.getUserNamesLessThanProvidedClaimValue(claimURI, claimValue, tenantId);
    }

    @Override
    public List<String> getUserNamesMoreThanProvidedClaimValue(String claimURI, String claimValue, int tenantId)
            throws IdentityException {

        return identityDataStore.getUserNamesMoreThanProvidedClaimValue(claimURI, claimValue, tenantId);
    }

    @Override
    public List<String> getUserNamesBetweenProvidedClaimValues(String claimURI, String startValue, String endValue,
                                                               int tenantId) throws IdentityException {

        return identityDataStore.getUserNamesBetweenProvidedClaimValues(claimURI, startValue, endValue, tenantId);
    }

    @Override
    public List<String> getUserNamesLessThanClaimWithNestedClaim(String claimURI,
                                                                 String claimValue,
                                                                 String nestedClaimURI,
                                                                 String nestedClaimValue,
                                                                 int tenantId,
                                                                 boolean isIncluded)
            throws IdentityException {

        return identityDataStore.getUserNamesLessThanClaimWithNestedClaim(claimURI, claimValue,
                nestedClaimURI, nestedClaimValue, tenantId, isIncluded);
    }

    @Override
    public List<String> getUserNamesBetweenGivenClaimsWithNestedClaim(String claimURI, String startValue,
                                                                    String endValue,
                                                                    String nestedClaimURI,
                                                                    String nestedClaimValue, int tenantId,
                                                                    boolean isIncluded)
            throws IdentityException {

        return identityDataStore.getUserNamesBetweenGivenClaimsWithNestedClaim(claimURI, startValue,
                endValue, nestedClaimURI, nestedClaimValue, tenantId, isIncluded);
    }

    @Override
    public boolean isUserStoreBasedIdentityDataStore() {

        return identityDataStore instanceof UserStoreBasedIdentityDataStore;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager User Store manager.
     * @return Weather identity claims are stored in user store or not.
     */
    private boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(STORE_IDENTITY_CLAIMS));
    }
}
