/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;

import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Utility class for Identity Claim Management related operations.
 */
public class IdentityMgtUtil {

    private static final Log log = LogFactory.getLog(IdentityMgtUtil.class);
    private static final ClaimMetadataManagementService claimMetadataManagementService = IdentityMgtServiceDataHolder
            .getInstance().getClaimMetadataManagementService();


    /**
     * Extract the user store persistency configured claims.
     *
     * @param claimMap          Map of claims to be filtered.
     * @param userStoreManager UserStoreManager instance.
     * @return Pair of HashMaps containing user store persisted and identity db persisted claims respectively.
     */
    public static Pair<Map<String, String>, Map<String, String>> getCustomPersistenceEnabledClaims(
            Map<String, String> claimMap, UserStoreManager userStoreManager) throws UserStoreException {

        // Extract claims that have a configured store location.
        Iterator<Map.Entry<String, String>> claimIterator = claimMap.entrySet().iterator();

        // This map will hold the claims that have a configured store location.
        Map<String, String> userStorePersistentClaims = new HashMap<>();
        Map<String, String> identityStorePersistentClaims = new HashMap<>();
        String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants
                .RealmConfig.PROPERTY_DOMAIN_NAME);

        // Retrieve tenant domain from user.
        int tenantId;
        try {
            tenantId = userStoreManager.getTenantId();
        } catch (UserStoreException e) {
            throw new UserStoreException ("Error while retrieving tenant ID for user store: " + userStoreDomain , e);
        }
        String tenantDomain = IdentityTenantUtil.getTenantDomain(tenantId);


        // Iterate through the claimMap to find claims with a custom configured store location.
        while (claimIterator.hasNext()) {
            Map.Entry<String, String> claim = claimIterator.next();
            String key = claim.getKey();
            String value = claim.getValue();
            if (StringUtils.isEmpty(key)) {
                continue;
            }
            Boolean isExcluded = isUserStoreExcludedForClaim(key, tenantDomain, userStoreDomain);
            if (isExcluded  == null) {
                // If the claim does not have a custom persistence configured, we skip it.
                continue;
            }

             /*
             If the claim has a configured custom persistent location, we remove the claim from the claimMap
             and add it to the respective map.
             */
            if (!isExcluded) {
                // If the claim isn't excluded from current user store, we add it to the userStorePersistentClaims map.
                userStorePersistentClaims.put(key, value);
                claimIterator.remove();
            } else {
                // If the claim is excluded from current user store, we add it to the identityStorePersistentClaims map.
                identityStorePersistentClaims.put(key, value);
                claimIterator.remove();
            }
        }
        return Pair.of(userStorePersistentClaims, identityStorePersistentClaims);

    }


    /**
     * Extract the user store persistency configured claims.
     * This method is an overloaded version of the above method.
     * @param claimMap       Map of claims that has been retrieved from the user store.
     * @param claims         List of claims to be filtered.
     * @param userStoreManager UserStoreManager instance.
     * @return Pair of HashMaps containing user store persisted and identity db persisted claims respectively.
     */
    public static Pair<Map<String, String>, Map<String, String>> getCustomPersistenceEnabledClaims(
            Map<String, String> claimMap, String[] claims, UserStoreManager userStoreManager) throws UserStoreException {
        /*
         * The `claims` parameter represents the complete set of claim URIs to be evaluated for custom persistence.
         * The `claimMap` contains the subset of those claims that have been retrieved from the user store, along with their values.
         * Claims configured to persist in the user store but not present in `claimMap` are ignored.
         * Claims configured to persist in the identity store and not present in `claimMap` are added to the
         * `identityStorePersistentClaims` map.
         */

        // This map will hold the claims that have a configured store location.
        Map<String, String> userStorePersistentClaims = new HashMap<>();
        Map<String, String> identityStorePersistentClaims = new HashMap<>();
        String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants
                .RealmConfig.PROPERTY_DOMAIN_NAME);

        // Retrieve tenant domain from user.
        int tenantId;
        try {
            tenantId = userStoreManager.getTenantId();
        } catch (UserStoreException e) {
            throw new UserStoreException ("Error while retrieving tenant ID for user store: " + userStoreDomain , e);
        }
        String tenantDomain = IdentityTenantUtil.getTenantDomain(tenantId);

        // Iterate through the claims to find claims with a custom configured store location.
        String value;
        Boolean isExcluded;
        for (String claim: claims) {

            isExcluded = isUserStoreExcludedForClaim(claim, tenantDomain, userStoreDomain);
            if (isExcluded == null) {
                // If the claim does not have a custom persistence configured, we skip it.
                continue;
            }

            // If the claim has a configured custom persistent location, and add it to the respective map.
            if (!isExcluded && claimMap.containsKey(claim)) {
                /*
                * If the claim isn't excluded from current user store, and it is present in the claimMap,
                * we add it to the userStorePersistentClaims map.
                */
                value = claimMap.get(claim);
                userStorePersistentClaims.put(claim, value);
            } else if (isExcluded){
                // If the claim is excluded from current user store, we add it to the identityStorePersistentClaims map.
                identityStorePersistentClaims.put(claim, null);
            }
        }
        return Pair.of(userStorePersistentClaims, identityStorePersistentClaims);
    }

    /**
     * Extract the user store persistency configured claims.
     *
     * @param claims           Array of claims to be filtered.
     * @param userStoreManager UserStoreManager instance.
     * @return Pair of Lists containing user store persisted and identity db persisted claims respectively.
     */
    public static Pair<List<String>, List<String>> getCustomPersistenceEnabledClaims(
            String[] claims, UserStoreManager userStoreManager) throws UserStoreException {

        List<String> userStorePersistentClaims = new ArrayList<>();
        List<String> identityStorePersistentClaims = new ArrayList<>();

        // User store domain.
        String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(UserCoreConstants
                .RealmConfig.PROPERTY_DOMAIN_NAME);

        // Retrieve tenant domain from user.
        int tenantId;
        try {
            tenantId = userStoreManager.getTenantId();
        } catch (UserStoreException e) {
            throw new UserStoreException ("Error while retrieving tenant ID for user store: " + userStoreDomain , e);
        }
        String tenantDomain = IdentityTenantUtil.getTenantDomain(tenantId);

        // Iterate through the claims to find claims with a custom configured store location.
        Boolean isExcluded;
        for (String claim: claims) {

            isExcluded = isUserStoreExcludedForClaim(claim, tenantDomain, userStoreDomain);
            if (isExcluded == null) {
                // If the claim does not have a custom persistence configured, we skip it.
                continue;
            }

            // If the claim has a configured custom persistent location, and add it to the respective map.
            if (!isExcluded) {
                // If the claim isn't excluded from current user store, add it to the userStorePersistentClaims list.
                userStorePersistentClaims.add(claim);
            } else {
                // If the claim is excluded from current user store, we add it to the identityStorePersistentClaims map.
                identityStorePersistentClaims.add(claim);
            }
        }
        return Pair.of( userStorePersistentClaims, identityStorePersistentClaims );
    }


    /**
     * Check if the user store is excluded for claim storing.
     *
     * @return boolean value if excluded or not.
     */
    public static Boolean isUserStoreExcludedForClaim(String key, String tenantDomain, String userStoreDomain)
            throws UserStoreException {

        String excludedUserStores;
        try {
            // Check if the claim has a custom persistence enabled.
            boolean isCustomPersistenceEnabled = Boolean.parseBoolean(claimMetadataManagementService
                    .getLocalClaim(key, tenantDomain).get().getClaimProperty("IsCustomPersistenceEnabled"));

            if (!isCustomPersistenceEnabled){
                // If the claim does not have custom persistence enabled, we return null.
                return null;
            }

            Optional<LocalClaim> optionalClaim = claimMetadataManagementService.getLocalClaim(key, tenantDomain);

            if (optionalClaim.isPresent()) {
                excludedUserStores = optionalClaim.get().getClaimProperty("ExcludedUserStores");
            } else {
                // If the claim is not found, we return null.
                log.error("Claim property 'ExcludedUserStores' not found for key: " + key + " in tenant domain: "
                        + tenantDomain);
                return null;
            }

        } catch (ClaimMetadataException e) {
            throw new UserStoreException("Error while retrieving claim metadata for claim: " + key, e);
        }

        List<String> excludedUserStoresList = Arrays.asList(excludedUserStores.split(","));
        // Check if the user store domain is in the excluded list.
        return excludedUserStoresList.contains(userStoreDomain);
    }
}
