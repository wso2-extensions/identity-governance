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

import java.util.List;

/**
 * Interface for IdleAccIdentificationDAO.
 */
public interface IdleAccIdentificationDAO {

    /**
     * Get the list of inactive users.
     *
     * @param inactiveAfter                     Latest active date of login.
     * @param excludeBefore                     Date to exclude the oldest inactive users.
     * @param tenantDomain                      Tenant domain.
     * @return                                  List of inactive users.
     * @throws IdleAccIdentificationException   Exception thrown when getting inactive users.
     */
    List<InactiveUserModel> getInactiveUsers(String inactiveAfter, String excludeBefore, String tenantDomain)
            throws IdleAccIdentificationException;
}
