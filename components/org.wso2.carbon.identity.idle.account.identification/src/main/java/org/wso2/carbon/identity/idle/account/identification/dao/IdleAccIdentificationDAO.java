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

package org.wso2.carbon.identity.idle.account.identification.dao;

import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Interface for IdleAccIdentificationDAO.
 */
public interface IdleAccIdentificationDAO {

    /**
     * Get inactive users from a specific date.
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, String tenantDomain)
            throws IdleAccIdentificationException;

    /**
     * Get inactive users from a specific date excluding the oldest inactive users.
     *
     * @param inactiveAfter date after which the user should be inactive.
     * @param excludeBefore date before which the user should be excluded.
     * @param tenantDomain  tenant domain.
     * @return              list of inactive users.
     * @throws IdleAccIdentificationException Exception when retrieving inactive users from database.
     */
    List<InactiveUserModel> getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter,
                            LocalDateTime excludeBefore, String tenantDomain) throws IdleAccIdentificationException;
}
