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

package org.wso2.carbon.identity.governance.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Utility class for identity data store related operations.
 */
public class IdentityDataStoreUtil {

    private static final Log log = LogFactory.getLog(IdentityDataStoreUtil.class);
    private static final String EXCLUDED_USER_STORE_DELIMITER = ",";

    public static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";

    private IdentityDataStoreUtil() {}

    /**
     * Determine whether the given claim needs to be managed in the identity data store.
     *
     * @param claimUri     Claim URI under evaluation.
     * @param tenantDomain Tenant domain.
     * @param userStoreDomain User store domain.
     * @param isUserStoreBasedIdentityDataStore Indicates whether the identity data store is user store based.
     * @param isStoreIdentityClaimsInUserStoreEnabled Indicates whether storing identity claims in user store is enabled.
     * @return {@code true} if the claim should be stored in the identity data store, {@code false} otherwise.
     */
    public static boolean isManagedInIdentityDataStore(String claimUri,
                                                           String tenantDomain,
                                                           String userStoreDomain,
                                                           boolean isUserStoreBasedIdentityDataStore,
                                                           boolean isStoreIdentityClaimsInUserStoreEnabled) {

        if (StringUtils.isBlank(claimUri) || StringUtils.isBlank(tenantDomain)) {
            return false;
        }

        if (isUserStoreBasedIdentityDataStore) {
            if (log.isDebugEnabled()) {
                log.debug("Identity data store is user store based. Hence, claim: " + claimUri +
                        " will be managed in user store.");
            }
            return false;
        }

        boolean isIdentityClaim = isIdentityClaim(claimUri);
        if (isIdentityClaim && isStoreIdentityClaimsInUserStoreEnabled) {
            if (log.isDebugEnabled()) {
                log.debug("Managing identity claim: " + claimUri + " in user store as storing identity claims " +
                        "in user store is enabled.");
            }
            return false;
        }

        Optional<LocalClaim> localClaim = getLocalClaim(claimUri, tenantDomain);
        if (!localClaim.isPresent()) {
            if (log.isDebugEnabled()) {
                log.debug("Local claim metadata not found for claim: " + claimUri +
                        ". Defaulting to managed based on claim type.");
            }
            return isIdentityClaim;
        }

        Map<String, String> properties = localClaim.get().getClaimProperties();
        boolean managedInUserStore;

        String managedInUserStoreValue = properties.get(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY);
        if (StringUtils.isBlank(managedInUserStoreValue)) {
            if (log.isDebugEnabled()) {
                log.debug("ManagedInUserStore property not defined for claim: " + claimUri +
                        ". Defaulting to managed based on claim type.");
            }
            return isIdentityClaim;
        } else {
            managedInUserStore = Boolean.parseBoolean(managedInUserStoreValue);
        }

        if (!managedInUserStore) {
            if (log.isDebugEnabled()) {
                log.debug("Claim: " + claimUri + " is marked to be managed in identity data store.");
            }
            return true;
        }

        if (StringUtils.isBlank(userStoreDomain)) {
            if (log.isDebugEnabled()) {
                log.debug("User store domain is not provided. Hence, claim: " + claimUri +
                        " will be managed in identity data store.");
            }
            return true;
        }

        String excludedUserStores = properties.get(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY);
        if (StringUtils.isBlank(excludedUserStores)) {
            if (log.isDebugEnabled()) {
                log.debug("No excluded user stores defined for claim: " + claimUri +
                        ". Managed in user store: " + userStoreDomain);
            }
            return false;
        }

        Set<String> excludedDomains = Arrays.stream(excludedUserStores.split(EXCLUDED_USER_STORE_DELIMITER))
                .map(String::trim)
                .filter(StringUtils::isNotBlank)
                .map(String::toUpperCase)
                .collect(Collectors.toSet());

        return excludedDomains.contains(userStoreDomain.toUpperCase());
    }

    public static boolean isManagedInUserStoreBasedIdentityDataStore(String claimUri, String tenantDomain) {

        if (StringUtils.isBlank(claimUri) || StringUtils.isBlank(tenantDomain)) {
            return false;
        }

        if (isIdentityClaim(claimUri)) {
            if (log.isDebugEnabled()) {
                log.debug("Claim: " + claimUri + " is an identity claim. Hence it is not managed in user store " +
                        "based identity data store.");
            }
            return true;
        }

        Optional<LocalClaim> localClaim = getLocalClaim(claimUri, tenantDomain);
        if (!localClaim.isPresent()) {
            if (log.isDebugEnabled()) {
                log.debug("Local claim metadata not found for claim: " + claimUri +
                        ". Defaulting to managed based on claim type.");
            }
            return false;
        }

        Map<String, String> properties = localClaim.get().getClaimProperties();
        boolean managedInUserStore;

        String managedInUserStoreValue = properties.get(ClaimConstants.MANAGED_IN_USER_STORE_PROPERTY);
        if (StringUtils.isBlank(managedInUserStoreValue)) {
            if (log.isDebugEnabled()) {
                log.debug("ManagedInUserStore property not defined for claim: " + claimUri +
                        ". Defaulting to managed based on claim type.");
            }
            return false;
        } else {
            managedInUserStore = Boolean.parseBoolean(managedInUserStoreValue);
        }

        if (!managedInUserStore) {
            if (log.isDebugEnabled()) {
                log.debug("Claim: " + claimUri + " is marked to be managed in user store based " +
                        "identity data store.");
            }
            return true;
        }
        return false;
    }

    /**
     * Retrieve the local claim for the given claim URI and tenant domain.
     *
     * @param claimUri     Claim URI under evaluation.
     * @param tenantDomain Tenant domain.
     * @return Local claim wrapped in an Optional.
     */
    private static Optional<LocalClaim> getLocalClaim(String claimUri, String tenantDomain) {

        ClaimMetadataManagementService claimMetadataManagementService =
                IdentityMgtServiceDataHolder.getInstance().getClaimMetadataManagementService();
        if (claimMetadataManagementService == null) {
            if (log.isDebugEnabled()) {
                log.debug("Claim metadata management service is not available. Defaulting to identity data store " +
                        "for claim: " + claimUri);
            }
            return Optional.empty();
        }

        Optional<LocalClaim> localClaim;
        try {
            localClaim = claimMetadataManagementService.getLocalClaim(claimUri, tenantDomain);
        } catch (ClaimMetadataException e) {
            log.error("Error while retrieving claim metadata for claim: " + claimUri +
                    ". Defaulting to managed based on claim type.", e);
            return Optional.empty();
        }
        return localClaim;
    }

    /**
     * Check weather the given user store has enabled the property "StoreIdentityClaims" to store identity claims
     * in the user store.
     *
     * @param userStoreManager User Store manager.
     * @return Weather identity claims are stored in user store or not.
     */
    public static boolean isStoreIdentityClaimsInUserStoreEnabled(UserStoreManager userStoreManager) {

        return Boolean.parseBoolean(userStoreManager.getRealmConfiguration().
                getUserStoreProperty(STORE_IDENTITY_CLAIMS));
    }

    /**
     * Check whether the given claim is an identity claim.
     *
     * @param claimUri Claim URI.
     * @return true if the claim is an identity claim, false otherwise.
     */
    public static boolean isIdentityClaim(String claimUri) {

        return claimUri.contains(UserCoreConstants.ClaimTypeURIs.IDENTITY_CLAIM_URI_PREFIX);
    }
}
