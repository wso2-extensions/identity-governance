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

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;

import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Utility class for Identity Claim Management related operations.
 */
public class IdentityMgtUtil {

    private static final ClaimMetadataManagementService claimMetadataManagementService = IdentityMgtServiceDataHolder
            .getInstance().getClaimMetadataManagementService();
    private static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";
    private static final String ENABLE_USER_STORE_PERSISTENCE = "enableUserStorePersistence";
    private static final String EXCLUDED_USER_STORES = "excludedUserStores";

    /**
     * Extract the user store persistency configured claims.
     *
     * @param claims           Array of claims to be filtered.
     * @param userStoreManager userStoreManager instance.
     * @return Pair of Lists containing user store persisted and identity db persisted claims respectively.
     */

    public static String[] filterIdentityDataStoreManagedClaims(String[] claims, UserStoreManager userStoreManager )
            throws UserStoreException {

        List<String> identityDataStoreManagedClaims = new ArrayList<>();

        // Retrieve tenant domain and user store domain.
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userStoreDomain = userStoreManager.getRealmConfiguration().getUserStoreProperty(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

        // Get IdentityDatStoreService.
        IdentityDataStoreService identityDataStoreService =
                IdentityMgtServiceDataHolder.getInstance().getIdentityDataStoreService();

        // Iterate through the claims to find claims with a custom configured store location.
        Optional<LocalClaim> localClaim;
        String excludedUserStores;
        for (String claim: claims) {
            localClaim = getLocalClaimInTenant(claim, tenantDomain);
            // Check if the claim has configured custom persistence.
            if (localClaim.isPresent() && Boolean.parseBoolean(localClaim.get()
                    .getClaimProperty(ENABLE_USER_STORE_PERSISTENCE))) {

                excludedUserStores = localClaim.get().getClaimProperty(EXCLUDED_USER_STORES);

                if (Arrays.asList(excludedUserStores.split(",")).contains(userStoreDomain)){
                    // If custom persistence is enabled and the user store is excluded, add to identity store claims.
                    identityDataStoreManagedClaims.add(claim);
                } else if (!identityDataStoreService.isUserStoreBasedIdentityDataStore() &&
                        !isStoreIdentityClaimsInUserStoreEnabled(userStoreManager) &&
                        claim.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI_PREFIX)){
                    /*
                    If neither server level identity data store is enabled nor the user store level is configured,
                    and the claim is an IDENTITY_CLAIM, add to identity data store managed claims.
                     */
                    identityDataStoreManagedClaims.add(claim);
                }
            }
        }
        return identityDataStoreManagedClaims.toArray(new String[0]);
    }

    /**
     * Check if the custom persistence is enabled for the claim.
     */
    private static Optional<LocalClaim> getLocalClaimInTenant(String claimURI, String tenantDomain)
            throws UserStoreException {

        Optional<LocalClaim> optionalClaim;
        try {
            optionalClaim = claimMetadataManagementService.getLocalClaim(claimURI, tenantDomain);
        } catch (ClaimMetadataException e) {
            throw new UserStoreException("Error while retrieving claim metadata for claim: " + claimURI, e);
        }

        // Return local claim.
        return optionalClaim;
    }

    /**
     * Extract local claims from the claim map that is presented in the requiredClaims list.
     */
    public static Map<String, String> extractRequiredClaimsFromClaimMap(Map<String, String> claimMap, List<String> requiredClaims) {

        Map<String, String> localClaims = new HashMap<>();
        for (Map.Entry<String, String> entry : claimMap.entrySet()) {
            String claimURI = entry.getKey();
            String claimValue = entry.getValue();

            // Check if the claim is in the list.
            if (requiredClaims.contains(claimURI)) {
                localClaims.put(claimURI, claimValue);
                claimMap.remove(claimURI);
            }
        }
        return localClaims;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager User Store manager.
     * @return Weather identity claims are stored in user store or not.
     */
    private static boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(STORE_IDENTITY_CLAIMS));
    }
}
