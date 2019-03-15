/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationRuntimeException;
import org.wso2.carbon.identity.claim.verification.core.store.ClaimVerificationStore;
import org.wso2.carbon.identity.claim.verification.core.store.JDBCClaimVerificationStore;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.security.SecureRandom;
import java.util.List;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ErrorMessages;

/**
 * Contains utils used for claim verification.
 */
public class ClaimVerificationCoreUtils {

    private static final Log LOG = LogFactory.getLog(ClaimVerificationCoreUtils.class);

    private ClaimVerificationCoreUtils() {

    }

    /**
     * Get a type 4 (pseudo randomly generated) UUID confirmation code.
     *
     * @return Type 4 (pseudo randomly generated) UUID.
     */
    public static String getConfirmationCode() {

        return UUIDGenerator.generateUUID();
    }

    /**
     * Get a randomly generated confirmation code for a sent amount of characters.
     *
     * @param characterCount Number of character in the confirmation code. Minimum 6 characters.
     * @return Confirmation code.
     */
    public static String getConfirmationCode(int characterCount) {

        int minCharacterCount = 6;

        if (characterCount <= minCharacterCount) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Sent character count less than accepted minimum: " + minCharacterCount +
                        ". Defaulting to min character count.");
            }
            characterCount = minCharacterCount;
        }
        char[] chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toCharArray();
        SecureRandom rnd = new SecureRandom();
        StringBuilder sb = new StringBuilder("");
        for (int i = 0; i < characterCount; i++) {
            sb.append(chars[rnd.nextInt(chars.length)]);
        }
        return sb.toString();
    }

    /**
     * Retrieve the claim verification store.
     *
     * @return Instance of ClaimVerificationStore.
     */
    public static ClaimVerificationStore getClaimVerificationStore() {

        return JDBCClaimVerificationStore.getInstance();
    }

    /**
     * Used to get the list of users for a matching tenantId and username.
     *
     * @param tenantId TenantId of the users' tenant domain.
     * @param username Username of the user.
     * @return User list for the matching parameters passed.
     */
    public static String[] getUserList(int tenantId, String username, RealmService realmService) throws ClaimVerificationException {

        String[] userList;
        try {
            UserStoreManager userStoreManager = getUserStoreManager(tenantId, realmService);
            userList = userStoreManager.listUsers(username, 2);
            return userList;
        } catch (UserStoreException e) {
            LOG.error("Error retrieving the user-list for the tenant: " + tenantId + " and user: " + username, e);
            throw getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_USER_DATA, e);
        }
    }

//    /**
//     * Used to get claim metadata for a claim in a specific tenant domain.
//     *
//     * @param tenantId Tenant id of the tenant domain.
//     * @param claimUri A claim uri
//     * @return org.wso2.carbon.user.api.Claim object with claim metadata.
//     */
//    public static org.wso2.carbon.user.api.Claim getClaimMetaData(int tenantId, String claimUri,
//                                                                  RealmService realmService) throws
//            ClaimVerificationException {
//
//        org.wso2.carbon.user.api.Claim claimMetaData;
//
//        try {
//            ClaimManager claimManager = getClaimManager(tenantId, realmService);
//            claimMetaData = claimManager.getClaim(claimUri);
//            return claimMetaData;
//        } catch (UserStoreException e) {
//            LOG.error("Error retrieving the claim meta date for the tenant: " + tenantId + " and claim uri: "
//                    + claimUri, e);
//            throw getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_CLAIM_DATA, e);
//        }
//    }

    /**
     * Retrieve the user store manager for a tenant.
     *
     * @param tenantId Tenant id.
     * @return UserStoreManager.
     * @throws ClaimVerificationException
     */
    public static UserStoreManager getUserStoreManager(int tenantId, RealmService realmService) throws ClaimVerificationException {

        UserStoreManager userStoreManager;
        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (UserStoreManager) realmService.getTenantUserRealm(tenantId).getUserStoreManager();
                if (userStoreManager == null) {
                    throw new UserStoreException();
                }
                return userStoreManager;
            } else {
                throw new UserStoreException();
            }
        } catch (UserStoreException e) {
            LOG.error("Error retrieving UserStoreManager for tenantId : " + tenantId, e);
            throw getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_USER_DATA, e);
        }
    }

//    /**
//     * Retrieve the claim manager for a tenant.
//     *
//     * @param tenantId Tenant id.
//     * @return ClaimManager.
//     * @throws ClaimVerificationException
//     */
//    public static ClaimManager getClaimManager(int tenantId, RealmService realmService) throws ClaimVerificationException {
//
//        ClaimManager claimManager;
//        try {
//            if (realmService.getTenantUserRealm(tenantId) != null) {
//                claimManager = (ClaimManager) realmService.getTenantUserRealm(tenantId).getClaimManager();
//                if (claimManager == null) {
//                    throw new UserStoreException();
//                }
//                return claimManager;
//            } else {
//                throw new UserStoreException();
//            }
//        } catch (UserStoreException e) {
//            LOG.error("Error retrieving ClaimManager for tenant : " + tenantId, e);
//            throw getClaimVerificationException(ErrorMessages.ERROR_MSG_RETRIEVING_CLAIM_DATA, e);
//        }
//    }

    /**
     * Build a new ClaimVerificationException.
     *
     * @param details ErrorMessages enum.
     * @return ClaimVerificationException.
     */
    public static ClaimVerificationException getClaimVerificationException(ErrorMessages details) {

        return new ClaimVerificationException(details.getCode(), details.getMessage());
    }

    /**
     * Build a new ClaimVerificationException.
     *
     * @param details   ErrorMessages enum.
     * @param exception Throwable cause.
     * @return ClaimVerificationException.
     */
    public static ClaimVerificationException getClaimVerificationException(ErrorMessages details, Throwable exception) {

        return new ClaimVerificationException(details.getCode(), details.getMessage(), exception);
    }

    /**
     * Build a new ClaimVerificationBadRequestException.
     *
     * @param details ErrorMessages enum.
     * @return ClaimVerificationBadRequestException.
     */
    public static ClaimVerificationBadRequestException getClaimVerificationBadRequestException(
            ErrorMessages details) {

        return new ClaimVerificationBadRequestException(details.getCode(), details.getMessage());
    }

    /**
     * Build a new ClaimVerificationBadRequestException.
     *
     * @param details   ErrorMessages enum.
     * @param exception Throwable cause.
     * @return ClaimVerificationBadRequestException.
     */
    public static ClaimVerificationBadRequestException getClaimVerificationBadRequestException(
            ErrorMessages details, Throwable exception) {

        return new ClaimVerificationBadRequestException(details.getCode(), details.getMessage(), exception);
    }

    /**
     * Build a new {@link ClaimVerificationRuntimeException}.
     *
     * @param details   ErrorMessages enum.
     * @param exception Throwable cause.
     * @return ClaimVerificationBadRequestException.
     */
    public static ClaimVerificationRuntimeException getClaimVerificationRuntimeException(
            ErrorMessages details, Throwable exception) {

        return new ClaimVerificationRuntimeException(details.getCode(), details.getMessage(), exception);
    }

    /**
     * Build a new {@link ClaimVerificationRuntimeException}.
     *
     * @param details   ErrorMessages enum.
     * @return ClaimVerificationBadRequestException.
     */
    public static ClaimVerificationRuntimeException getClaimVerificationRuntimeException(
            ErrorMessages details) {

        return new ClaimVerificationRuntimeException(details.getCode(), details.getMessage());
    }

    /**
     * Retrieve {@link LocalClaim} for the given local claim URI from the claim meta-data service.
     *
     * @param claimMetadataManagementService {@link ClaimMetadataManagementService}.
     * @param tenantDomain                   Tenant domain.
     * @param localClaimUri                  URI of the local claim.
     * @return {@link LocalClaim} object.
     * @throws ClaimMetadataException
     */
    public static LocalClaim getLocalClaimFromService(ClaimMetadataManagementService claimMetadataManagementService,
                                                      String tenantDomain, String localClaimUri)
            throws ClaimMetadataException {

        List<LocalClaim> localClaims = claimMetadataManagementService.getLocalClaims(tenantDomain);

        for (LocalClaim localClaim : localClaims) {
            if (localClaim.getClaimURI().equals(localClaimUri)) {
                return localClaim;
            }
        }
        return null;
    }

    public static String getTenantDomainFromContext() {

        return PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
    }
}
