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
package org.wso2.carbon.identity.user.export.core.service.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.internal.service.impl.AbstractUserInformationProvider;
import org.wso2.carbon.identity.user.export.core.utils.Utils;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimMapping;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This is used to provide the profile information of user in the user profile export API.
 */
public class UserProfileInformationProvider extends AbstractUserInformationProvider {

    private static final Log LOG = LogFactory.getLog(UserProfileInformationProvider.class);
    private static final String ROLES_URIS_CLAIM = "roles";

    @Override
    public UserInformationDTO getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        Claim[] userClaimValues;
        UserStoreManager userStoreManager;
        try {
            userStoreManager = getUserStoreManager(tenantId, userStoreDomain);
            userClaimValues = userStoreManager.getUserClaimValues(username, null);
        } catch (UserStoreException e) {
            throw new UserExportException("Error while getting user claims", e);
        }

        if (ArrayUtils.isEmpty(userClaimValues)) {
            return new UserInformationDTO();
        }

        Map<String, String> userProfileAttributes = new HashMap<>();
        List<String> claimsToInclude = getClaimsToInclude(tenantId);
        List<String> claimsToExclude = Utils.getRestrictedClaims();
        for (Claim claim : userClaimValues) {
            String claimURI = claim.getClaimUri();
            String claimValue = claim.getValue();
            String claimName;
            if (!claimsToInclude.contains(claimURI) || claimsToExclude.contains(claimURI)) {
                // If the claim is not configured as supported by default in the org level or configured in the toml
                // config,
                // or configured as a restricted claim,
                // those won't be exposed in the profile API.
                continue;
            }
            if (!claimURI.contains(WSO2_IDENTITY_CLAIM_URI)) {
                claimName = claimURI.replace(WSO2_CLAIM_URI, "");
            } else {
                claimName = claimURI.replace(WSO2_IDENTITY_CLAIM_URI, "");
            }
            userProfileAttributes.put(claimName, claimValue);
        }
        return new UserInformationDTO(userProfileAttributes);
    }


    @Override
    public String getType() {

        return "user_profile";
    }

    /**
     * Get all the claims to include in the user profile export API. This will return the list of the claims that
     * are enabled via the toml config and user profile.
     *
     * @param tenantId tenant Id
     * @return list of all the claims to include in the user profile export API
     */
    private List<String> getClaimsToInclude(int tenantId) {

        List<String> supportedClaims = getSupportedClaims(tenantId);
        List<String> additionalClaimsToInclude = Utils.getAdditionalClaimsToInclude();

        return Stream.concat(supportedClaims.stream(), additionalClaimsToInclude.stream())
                .distinct()
                .collect(Collectors.toList());
    }

    /**
     * Get the claims that are enabled in the user profile.
     *
     * @param tenantId tenant Id.
     * @return List of the claims that are enabled in the user profile.
     */
    private List<String> getSupportedClaims(int tenantId) {

        ClaimMapping[] claims;
        List<String> supportedClaims = new ArrayList<>();

        try {
            claims = UserProfileExportDataHolder.getRealmService().getTenantUserRealm(tenantId).getClaimManager()
                    .getAllSupportClaimMappingsByDefault();
            for (ClaimMapping claimMapping : claims) {
                if (claimMapping.getClaim().getClaimUri().contains(WSO2_IDENTITY_CLAIM_URI) ||
                        claimMapping.getClaim().getClaimUri().contains(WSO2_RUN_TIME_CLAIM_URI)) {
                    if (LOG.isDebugEnabled()) {
                        LOG.debug("Claim URI: " + claimMapping.getClaim().getClaimUri() + " is either Identity claim" +
                                " or a runtime claim. It should be enabled via config. Hence ignoring this claim");
                    }
                    continue;
                }
                supportedClaims.add(claimMapping.getClaim().getClaimUri());
            }
            return supportedClaims;
        } catch (org.wso2.carbon.user.api.UserStoreException e) {
            throw new RuntimeException(e);
        }
    }
}
