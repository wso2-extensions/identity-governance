/*
 * Copyright (c) 2014, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.store;

import org.apache.commons.lang.NotImplementedException;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

public interface UserRecoveryDataStore {
    void store(UserRecoveryData recoveryDataDO) throws IdentityRecoveryException;

    /*
     * returns UserRecoveryData if the code is validated. Otherwise returns an exception.
     */
    UserRecoveryData load(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws
            IdentityRecoveryException;

    UserRecoveryData load(String code) throws
            IdentityRecoveryException;

    UserRecoveryData load(User user) throws
            IdentityRecoveryException;

    /**
     * Load UserRecoveryData from the
     *
     * @param code                  Confirmation code.
     * @param skipExpiryValidation  Skip confirmation code validation.
     * @return UserRecoveryData     Data associated with the provided code, including related user and scenarios.
     * @throws IdentityRecoveryException If the functionality is not implemented.
     * @throws NotImplementedException   If an error occurred while getting the user recovery data.
     */
    default UserRecoveryData load(String code, boolean skipExpiryValidation) throws
            IdentityRecoveryException, NotImplementedException {

        throw new NotImplementedException("This functionality is not implemented");
    }

    UserRecoveryData loadWithoutCodeExpiryValidation(User user) throws
            IdentityRecoveryException;

    UserRecoveryData loadWithoutCodeExpiryValidation(User user, Enum recoveryScenario) throws
            IdentityRecoveryException;

    UserRecoveryData loadWithoutCodeExpiryValidation(User user, Enum recoveryScenario, Enum recoveryStep)
            throws IdentityRecoveryException;

    void invalidate(String code) throws
            IdentityRecoveryException;

    void invalidate(User user) throws
            IdentityRecoveryException;

    void invalidate(User user, Enum recoveryScenario, Enum recoveryStep) throws
            IdentityRecoveryException;

    /**
     * Delete all recovery data by tenant id
     *
     * @param tenantId Id of the tenant
     */
    default void deleteRecoveryDataByTenantId(int tenantId) throws IdentityRecoveryException {

    }

    /**
     * Update the existing recovery entry of the existing code and replace with a new code, recovery step and channel list
     * by not changing the existing code creation time.
     *
     * @param oldCode Existing code.
     * @param code Newly created code which replaces the existing code.
     * @param recoveryStep Recovery step that needs to be updated in the database.
     * @param channelList String which contains the list of channels to be updated.
     * @throws IdentityRecoveryException If an error occurred during the update operation.
     */
    void invalidateWithoutChangeTimeCreated(String oldCode, String code, Enum recoveryStep, String channelList)
            throws IdentityRecoveryException;
}
