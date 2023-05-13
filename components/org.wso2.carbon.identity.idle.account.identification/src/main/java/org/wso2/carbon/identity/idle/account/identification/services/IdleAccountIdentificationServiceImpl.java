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

package org.wso2.carbon.identity.idle.account.identification.services;

import org.wso2.carbon.identity.idle.account.identification.dao.IdleAccIdentificationDAO;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Implementation of the service interface for idle account identification.
 */
public class IdleAccountIdentificationServiceImpl implements IdleAccountIdentificationService {

    IdleAccIdentificationDAO idleAccIdentificationDAO;

    /**
     * Constructor of the service implementation.
     *
     * @param idleAccIdentificationDAO DAO for idle account identification
     */
    public IdleAccountIdentificationServiceImpl(IdleAccIdentificationDAO idleAccIdentificationDAO) {

        this.idleAccIdentificationDAO = idleAccIdentificationDAO;
    }

    @Override
    public List<InactiveUserModel> getInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, String tenantDomain)
            throws IdleAccIdentificationException {

        return idleAccIdentificationDAO.getInactiveUsersFromSpecificDate(inactiveAfter, tenantDomain);
    }

    @Override
    public List<InactiveUserModel> getLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter,
                       LocalDateTime excludeBefore, String tenantDomain) throws IdleAccIdentificationException {

        return idleAccIdentificationDAO.getLimitedInactiveUsersFromSpecificDate(inactiveAfter,
                excludeBefore, tenantDomain);
    }
}
