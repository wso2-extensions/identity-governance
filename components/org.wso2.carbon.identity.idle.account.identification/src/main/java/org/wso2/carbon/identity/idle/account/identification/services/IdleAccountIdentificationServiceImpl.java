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

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.idle.account.identification.constants.IdleAccIdentificationConstants;
import org.wso2.carbon.identity.idle.account.identification.constants.IdleAccIdentificationConstants.ErrorMessages;
import org.wso2.carbon.identity.idle.account.identification.dao.IdleAccIdentificationDAO;
import org.wso2.carbon.identity.idle.account.identification.dao.impl.IdleAccIdentificationDAOImpl;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationClientException;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationException;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;
import org.wso2.carbon.identity.idle.account.identification.util.Utils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;

/**
 * Implementation of the service interface for idle account identification.
 */
public class IdleAccountIdentificationServiceImpl implements IdleAccountIdentificationService {

    IdleAccIdentificationDAO idleAccIdentificationDAO = new IdleAccIdentificationDAOImpl();

    @Override
    public List<InactiveUserModel> getInactiveUsers(String inactiveAfter, String excludeBefore, String tenantDomain)
            throws IdleAccIdentificationException {

        validateDates(inactiveAfter, excludeBefore);
        return idleAccIdentificationDAO.getInactiveUsers(inactiveAfter, excludeBefore, tenantDomain);
    }

    /**
     * Validate the dates.
     *
     * @param inactiveAfter date after which the user should be inactive
     * @param excludeBefore date before which the user should be inactive
     * @throws IdleAccIdentificationClientException Exception thrown when the dates are invalid
     */
    private void validateDates(String inactiveAfter, String excludeBefore) throws IdleAccIdentificationClientException {

        // Check if the required parameters are present.
        if (StringUtils.isEmpty(inactiveAfter)) {
            throw new IdleAccIdentificationClientException(
                    ErrorMessages.ERROR_REQUIRED_PARAMETER_MISSING.getCode(),
                    ErrorMessages.ERROR_REQUIRED_PARAMETER_MISSING.getMessage(),
                    String.format(ErrorMessages.ERROR_REQUIRED_PARAMETER_MISSING.getDescription(),
                            IdleAccIdentificationConstants.DATE_INACTIVE_AFTER));
        }

        // Check if the date format is valid.
        Utils.validateDateFormat(inactiveAfter, excludeBefore);

        try {
            LocalDateTime inactiveAfterObj = LocalDate.parse(inactiveAfter).atStartOfDay();
            if (StringUtils.isNotEmpty(excludeBefore)) {
                LocalDateTime excludeBeforeObj = LocalDate.parse(excludeBefore).atStartOfDay();
            }
        } catch (DateTimeParseException e) {
            if (inactiveAfter.equals(e.getParsedString())) {
                throw new IdleAccIdentificationClientException(
                        ErrorMessages.ERROR_INVALID_DATE.getCode(), ErrorMessages.ERROR_INVALID_DATE.getMessage(),
                        String.format(ErrorMessages.ERROR_INVALID_DATE.getDescription(),
                                IdleAccIdentificationConstants.DATE_INACTIVE_AFTER));
            }
            throw new IdleAccIdentificationClientException(
                    ErrorMessages.ERROR_INVALID_DATE.getCode(), ErrorMessages.ERROR_INVALID_DATE.getMessage(),
                    String.format(ErrorMessages.ERROR_INVALID_DATE.getDescription(),
                            IdleAccIdentificationConstants.DATE_EXCLUDE_BEFORE));
        }
    }
}
