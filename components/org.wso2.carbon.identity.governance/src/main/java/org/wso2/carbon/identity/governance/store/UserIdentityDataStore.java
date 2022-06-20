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

package org.wso2.carbon.identity.governance.store;

import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.model.ExpressionCondition;

import java.util.Collections;
import java.util.List;

/**
 * This interface provides to plug module for preferred persistence store.
 */
public abstract class UserIdentityDataStore {

    public static final String ONE_TIME_PASSWORD = "http://wso2.org/claims/identity/otp";
    public static final String PASSWORD_CHANGE_REQUIRED = "http://wso2.org/claims/identity/passwordChangeRequired";
    public static final String TEMPORARY_LOCK = "http://wso2.org/claims/identity/temporaryLock";
    public static final String LAST_FAILED_LOGIN_ATTEMPT_TIME = "http://wso2.org/claims/identity/lastFailedLoginAttemptTime";
    public static final String FAIL_LOGIN_ATTEMPTS = "http://wso2.org/claims/identity/failedLoginAttempts";
    public static final String LAST_LOGON_TIME = "http://wso2.org/claims/identity/lastLogonTime";
    public static final String UNLOCKING_TIME = "http://wso2.org/claims/identity/unlockTime";
    public static final String ACCOUNT_LOCK = "http://wso2.org/claims/identity/accountLocked";

    /**
     * Get all claim types that is need to persist in the store
     *
     * @return
     */
    public String[] getUserIdentityDataClaims() throws IdentityException {

        return new String[]{ONE_TIME_PASSWORD, PASSWORD_CHANGE_REQUIRED, TEMPORARY_LOCK,
                LAST_FAILED_LOGIN_ATTEMPT_TIME, FAIL_LOGIN_ATTEMPTS, LAST_LOGON_TIME,
                UNLOCKING_TIME, ACCOUNT_LOCK, UserCoreConstants.ClaimTypeURIs.CHALLENGE_QUESTION_URI};

    }

    /**
     * Stores data
     *
     * @param userIdentityDTO
     * @param userStoreManager
     */
    public abstract void store(UserIdentityClaim userIdentityDTO, UserStoreManager userStoreManager)
            throws IdentityException;

    /**
     * Loads
     *
     * @param userName
     * @param userStoreManager
     * @return
     */
    public abstract UserIdentityClaim load(String userName, UserStoreManager userStoreManager);


    /**
     * Removes
     *
     * @param userName
     * @param userStoreManager
     */
    public abstract void remove(String userName, UserStoreManager userStoreManager) throws IdentityException;

    /**
     * List users according to the given claim URI and value.
     * @param claimUri Claim URI.
     * @param claimValue Value for the given claim URI.
     * @param userStoreManager UserStoreManager instance.
     * @return List of user identifiers.
     * @throws IdentityException Identity Exception.
     */
    public List<String> list(String claimUri, String claimValue,
                             org.wso2.carbon.user.core.UserStoreManager userStoreManager) throws IdentityException {

        // This method should be overridden by the sub classes. Adding a non-abstract method to give backward
        // compatibility. Return an immutable empty list if sub classes do not have any overrides.
        return Collections.emptyList();
    }

    /**
     * List users according to the given claim URI and value and pagination parameters.
     *
     * @param expressionConditions           List of expression conditions.
     * @param userStoreManager               UserStoreManager instance.
     * @param identityClaimFilteredUserNames List to hold filtered usernames.
     * @return List of usernames.
     * @throws IdentityException Identity Exception.
     */
    public List<String> listPaginatedUsersNames(List<ExpressionCondition> expressionConditions,
                                                List<String> identityClaimFilteredUserNames,
                                                String domain,
                                                org.wso2.carbon.user.core.UserStoreManager userStoreManager, int limit,
                                                int offset) throws IdentityException {

        // Return an immutable empty list if sub classes do not have any overrides.
        return Collections.emptyList();
    }
}
