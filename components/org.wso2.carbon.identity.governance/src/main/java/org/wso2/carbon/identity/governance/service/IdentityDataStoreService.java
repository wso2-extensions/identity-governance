package org.wso2.carbon.identity.governance.service;

import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.ExpressionCondition;

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
     * Check whether the identity data store is user store based.
     *
     * @return  True if the identity data store is user store based.
     */
    boolean isUserStoreBasedIdentityDataStore();
}
