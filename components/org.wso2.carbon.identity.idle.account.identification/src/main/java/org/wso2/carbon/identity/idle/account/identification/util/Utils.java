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

package org.wso2.carbon.identity.idle.account.identification.util;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.idle.account.identification.constants.IdleAccIdentificationConstants;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationClientException;
import org.wso2.carbon.identity.idle.account.identification.exception.IdleAccIdentificationServerException;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * Util class for idle account identification.
 */
public class Utils {

    /**
     * Convert date into epoch string.
     *
     * @param dateString date string
     * @return epoch
     * @throws IdleAccIdentificationServerException Exception thrown when converting date into epoch.
     */
    public static String convertDateIntoEpoch(String dateString) throws IdleAccIdentificationServerException {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = sdf.parse(dateString);
            long epoch = date.getTime();
            return Long.toString(epoch);
        } catch (ParseException e) {
            throw new IdleAccIdentificationServerException(
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_CONVERSION_TO_EPOCH.getCode(),
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_INVALID_DATE.getMessage());
        }
    }

    /**
     * Validate the date format.
     *
     * @param inactiveAfter date after which the user should be inactive
     * @param excludeBefore date before which the user should be inactive
     * @throws IdleAccIdentificationClientException Exception thrown when the date format is invalid
     */
    public static void validateDateFormat(String inactiveAfter, String excludeBefore)
            throws IdleAccIdentificationClientException {

        if (!Pattern.matches(IdleAccIdentificationConstants.DATE_FORMAT_REGEX, inactiveAfter)) {
            throw new IdleAccIdentificationClientException(
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getCode(),
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getMessage(),
                    String.format(IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getDescription(),
                            IdleAccIdentificationConstants.DATE_INACTIVE_AFTER));
        }
        if (StringUtils.isNotEmpty(excludeBefore) && !Pattern.matches(IdleAccIdentificationConstants.DATE_FORMAT_REGEX,
                excludeBefore)) {
            throw new IdleAccIdentificationClientException(
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getCode(),
                    IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getMessage(),
                    String.format(IdleAccIdentificationConstants.ErrorMessages.ERROR_DATE_REGEX_MISMATCH.getDescription(),
                            IdleAccIdentificationConstants.DATE_EXCLUDE_BEFORE));
        }
    }
}
