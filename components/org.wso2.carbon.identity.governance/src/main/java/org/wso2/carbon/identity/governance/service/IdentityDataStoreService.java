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

import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * This interface is used to store data in the identity data store.
 */
public interface IdentityDataStoreService {

    /**
     * Store identity claims data in the identity data store.
     *
     * @param userName              Username of the user.
     * @param userStoreManager      User store manager.
     * @param operationType         Operation type.
     * @param claims                Identity Claims.
     * @return                      True if the data is stored successfully.
     * @throws UserStoreException   User store exception.
     */
    boolean storeInIdentityDataStore(String userName, UserStoreManager userStoreManager, String operationType,
                                     Map<String, String> claims) throws UserStoreException;

    /**
     * Get identity claim data from the identity data store for a user.
     *
     * @param username          Username of the user.
     * @param userStoreManager  User store manager.
     * @return                  Identity claim data.
     */
    UserIdentityClaim getIdentityClaimData(String username, UserStoreManager userStoreManager);

    /**
     * Return an array of user who have the given claim URI and claim value.
     *
     * @param claimURI              Claim URI.
     * @param claimValue            Claim value.
     * @param userStoreManager      User store manager.
     * @return                      List of usernames.
     * @throws IdentityException    Identity exception.
     */
    List<String> listUsersByClaimURIAndValue(String claimURI, String claimValue, UserStoreManager userStoreManager)
            throws IdentityException;

    /**
     * Return an array of user who have the given claim URI and claim value and pagination parameters.
     *
     * @param expressionConditions              List of expression conditions.
     * @param identityClaimFilteredUserNames    List to hold filtered usernames.
     * @param domain                            Domain name.
     * @param userStoreManager                  User store manager.
     * @param limit                             Limit.
     * @param offset                            Offset.
     * @return                                  List of usernames.
     * @throws IdentityException                Identity exception.
     */
    List<String> listPaginatedUsersByClaimURIAndValue(List<ExpressionCondition> expressionConditions,
                                                      List<String> identityClaimFilteredUserNames,
                                                      String domain, UserStoreManager userStoreManager,
                                                      int limit, int offset) throws IdentityException;

    /**
     * Remove identity claims data of a user from the identity data store.
     *
     * @param username              Username of the user.
     * @param userStoreManager      User store manager.
     * @throws IdentityException    Identity exception.
     */
    void removeIdentityClaims(String username, UserStoreManager userStoreManager) throws IdentityException;

    /**
     * Get the list of usernames who either do not have a value configured for the given claim URI
     * or have a value that differs from the provided claim value.
     *
     * @param condition        Condition.
     * @param claimURI         Claim URI.
     * @param claimValue       Claim value.
     * @param userStoreManager UserStoreManager instance.
     * @return List of usernames.
     * @throws IdentityException Identity exception.
     */
    default List<String> getUserNamesByClaimURINotEqualValue(Condition condition, String claimURI, String claimValue,
                                                             UserStoreManager userStoreManager)
            throws IdentityException {

        return Collections.emptyList();
    }

    /**
     * Get the list of usernames who have the claim value less than the provided claim value for a given claim URI.
     *
     * @param claimURI              Claim URI.
     * @param claimValue            Claim value.
     * @param tenantId              Tenant ID.
     * @return                      List of usernames.
     * @throws IdentityException    Identity exception.
     */
    List<String> getUserNamesLessThanProvidedClaimValue(String claimURI, String claimValue,
                                                        int tenantId) throws IdentityException;

    /**
     * Get the list of usernames who have the claim value more than the provided claim value for a given claim URI.
     *
     * @param claimURI              Claim URI.
     * @param claimValue            Claim value.
     * @param tenantId              Tenant ID.
     * @return                      List of usernames.
     * @throws IdentityException    Identity exception.
     */
    List<String> getUserNamesMoreThanProvidedClaimValue(String claimURI, String claimValue,
                                                        int tenantId) throws IdentityException;

    /**
     * Get the list of usernames who have the claim value between the provided claim values for a given claim URI.
     * @param claimURI              Claim URI.
     * @param startValue            Start value.
     * @param endValue              End value.
     * @param tenantId              Tenant ID.
     * @return                      List of usernames.
     * @throws IdentityException    Identity exception.
     */
    List<String> getUserNamesBetweenProvidedClaimValues(String claimURI, String startValue, String endValue,
                                                        int tenantId) throws IdentityException;

    /**
     * Get the list of usernames who have the claim value less than the provided claim value for a given claim URI
     * and include or exclude the users with the boolean isIncluded
     * based on the nested claim value for a given nested claim URI.
     *
     * @param claimURI         Claim URI.
     * @param claimValue       Claim value.
     * @param nestedClaimURI   Nested claim URI.
     * @param nestedClaimValue Nested claim value.
     * @param tenantId         Tenant ID.
     * @param isIncluded       Include or exclude the users based on the nested claim.
     * @return List of usernames.
     * @throws IdentityException Identity exception.
     */
    default List<String> getUserNamesLessThanClaimWithNestedClaim(String claimURI,
                                                                  String claimValue,
                                                                  String nestedClaimURI,
                                                                  String nestedClaimValue,
                                                                  int tenantId,
                                                                  boolean isIncluded)
            throws IdentityException {

        return Collections.emptyList();
    }

    /**
     * Get the list of usernames who have the claim value between the provided claim values for a given claim URI
     * and include or exclude the users with the boolean isIncluded
     * based on the nested claim value for a given nested claim URI.
     *
     * @param claimURI         Claim URI.
     * @param startValue       Start value.
     * @param endValue         End value.
     * @param nestedClaimURI   Nested claim URI.
     * @param nestedClaimValue Nested claim value.
     * @param tenantId         Tenant ID.
     * @param isIncluded       Include or exclude the users based on the nested claim.
     * @return List of usernames.
     * @throws IdentityException Identity exception.
     */
    default List<String> getUserNamesBetweenGivenClaimsWithNestedClaim(String claimURI,
                                                                     String startValue,
                                                                     String endValue,
                                                                     String nestedClaimURI,
                                                                     String nestedClaimValue,
                                                                     int tenantId,
                                                                     boolean isIncluded)
            throws IdentityException {

        return Collections.emptyList();
    }

    /**
     * Check whether the identity data store is user store based.
     *
     * @return  True if the identity data store is user store based.
     */
    boolean isUserStoreBasedIdentityDataStore();
}
