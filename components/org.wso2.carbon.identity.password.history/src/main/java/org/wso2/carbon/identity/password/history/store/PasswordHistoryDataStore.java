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

package org.wso2.carbon.identity.password.history.store;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;

/**
 * This interface provides to plug module for preferred persistence store.
 */
public interface PasswordHistoryDataStore {


    /**
     * Stores data
     *
     * @param credential,user
     */
    void store(User user, Object credential) throws IdentityPasswordHistoryException;

    /**
     * Removes
     *
     * @param user
     */
    void remove(User user) throws IdentityPasswordHistoryException;

    /**
     * Delete password history data of a tenant.
     *
     * @param tenantId Id of the tenant
     * @throws IdentityPasswordHistoryException
     */
    default void deletePasswordHistoryData(int tenantId) throws IdentityPasswordHistoryException {

    }

    /**
     * validate
     *
     * @param user,credential
     *
     */
    boolean validate(User user, Object credential) throws IdentityPasswordHistoryException;
}
