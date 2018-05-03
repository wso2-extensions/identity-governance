/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.user.endpoint.util;

import org.apache.commons.logging.Log;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.exceptions.BadRequestException;
import org.wso2.carbon.identity.user.endpoint.exceptions.ConflictException;
import org.wso2.carbon.identity.user.endpoint.exceptions.InternalServerErrorException;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfRegistrationUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.List;

public class Utils {

    public static UserSelfRegistrationManager getUserSelfRegistrationManager() {
        return (UserSelfRegistrationManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(UserSelfRegistrationManager.class, null);
    }

    /**
     * Logs the error, builds a internalServerErrorException with specified details and throws it
     *
     * @param msg error message
     * @param log Log instance
     * @throws InternalServerErrorException
     */
    public static void handleInternalServerError(String msg, String code, Log log, Throwable throwable)
            throws InternalServerErrorException {
        InternalServerErrorException internalServerErrorException = buildInternalServerErrorException(code);
        if (throwable == null) {
            log.error(msg);
        } else {
            log.error(msg, throwable);
        }
        throw internalServerErrorException;
    }


    /**
     * Returns a new InternalServerErrorException
     *
     * @return a new InternalServerErrorException with default details as a response DTO
     */
    public static InternalServerErrorException buildInternalServerErrorException(String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT, code,
                Constants.STATUS_INTERNAL_SERVER_ERROR_DESCRIPTION_DEFAULT);
        return new InternalServerErrorException(errorDTO);
    }


    /**
     * Logs the error, builds a BadRequestException with specified details and throws it
     *
     * @param msg  error message
     * @param code error code
     * @throws BadRequestException
     */
    public static void handleBadRequest(String msg, String code) throws BadRequestException {
        BadRequestException badRequestException = buildBadRequestException(msg, code);
        throw badRequestException;
    }

    /**
     * Returns a new BadRequestException
     *
     * @param description description of the exception
     * @return a new BadRequestException with the specified details as a response DTO
     */
    public static BadRequestException buildBadRequestException(String description, String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, code, description);
        return new BadRequestException(errorDTO);
    }

    /**
     * Logs the error, builds a ConflictException with specified details and throws it
     *
     * @param msg  error message
     * @param code error code
     * @throws ConflictException
     */
    public static void handleConflict(String msg, String code) throws ConflictException {
        ConflictException conflictException = buildConflictException(msg, code);
        throw conflictException;
    }

    /**
     * Returns a new ConflictException
     *
     * @param description description of the exception
     * @return a new ConflictException with the specified details as a response DTO
     */
    public static ConflictException buildConflictException(String description, String code) {
        ErrorDTO errorDTO = getErrorDTO(Constants.STATUS_CONFLICT_MESSAGE_DEFAULT, code, description);
        return new ConflictException(errorDTO);
    }

    /**
     * Returns a generic errorDTO
     *
     * @param message specifies the error message
     * @return A generic errorDTO with the specified details
     */
    public static ErrorDTO getErrorDTO(String message, String code, String description) {
        ErrorDTO errorDTO = new ErrorDTO();
        errorDTO.setCode(code);
        errorDTO.setMessage(message);
        errorDTO.setDescription(description);
        return errorDTO;
    }

    public static User getUser(UserDTO userDTO) {
        User user = new User();
        user.setTenantDomain(userDTO.getTenantDomain());
        if (userDTO.getRealm() == null) {
            userDTO.setRealm(IdentityUtil.getPrimaryDomainName());
        }
        user.setUserStoreDomain(userDTO.getRealm());
        user.setUserName(userDTO.getUsername());
        return user;
    }

    public static User getUser(SelfRegistrationUserDTO userDTO) {
        User user = new User();
        user.setTenantDomain(userDTO.getTenantDomain());
        user.setUserStoreDomain(userDTO.getRealm());
        user.setUserName(userDTO.getUsername());
        return user;
    }


    public static Claim[] getClaims(List<ClaimDTO> claimDTOs) {
        if (claimDTOs != null && claimDTOs.size() > 0) {
            Claim[] claims = new Claim[claimDTOs.size()];
            for (int i = 0; i < claimDTOs.size(); i++) {
                Claim claim = new Claim();
                claim.setClaimUri(claimDTOs.get(i).getUri());
                claim.setValue(claimDTOs.get(i).getValue());
                claims[i] = claim;
            }
            return claims;
        } else {
            return new Claim[0];
        }
    }

    public static String[] getRoles(List<String> roleList) {
        if (roleList == null) {
            return new String[0];
        }
        return roleList.toArray(new String[roleList.size()]);
    }


    public static Property[] getProperties(List<PropertyDTO> propertyDTOs) {
        if (propertyDTOs == null) {
            return new Property[0];
        }

        Property[] properties = new Property[propertyDTOs.size()];
        for (int i = 0; i < propertyDTOs.size(); i++) {
            Property property = new Property(propertyDTOs.get(i).getKey(), propertyDTOs.get(i).getValue());
            properties[i] = property;
        }
        return properties;
    }

    public static UserInformationService getUserInformationService() throws UserExportException {

        try {
            return (UserInformationService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(UserInformationService.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if the UserInformationService is not registered properly.
            throw new UserExportException("Error while retrieving UserInformationService.", e);
        }
    }

    public static RealmService getRealmService() throws UserExportException {

        try {
            return (RealmService) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .getOSGiService(RealmService.class, null);
        } catch (NullPointerException e) {
            // Catching NPE since getOSGiService can throw NPE if the RealmService is not registered properly.
            throw new UserExportException("Error while retrieving RealmService.", e);
        }
    }

}
