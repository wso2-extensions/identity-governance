/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.endpoint.impl.util;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.claim.verification.core.ClaimVerificationHandler;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.model.ValidationResponse;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.LinkDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ValidationResponseDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.VerificationInitiatingResponseDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.exception.BadRequestException;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.exception.InternalServerErrorException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility class for the claim verification endpoint.
 */
public class ClaimVerificationEndpointUtils {

    private static final Log LOG = LogFactory.getLog(ClaimVerificationEndpointUtils.class);

    /**
     * Used to get the claim verification internal service.
     *
     * @return Returns the ClaimVerificationHandler service.
     */
    public static ClaimVerificationHandler getClaimVerificationHandler() {

        return (ClaimVerificationHandler) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(ClaimVerificationHandler.class, null);
    }

    /**
     * Used to get a User object from a UserDTO.
     *
     * @param userDTO      User DTO.
     * @param tenantDomain Tenant domain of the user.
     * @return Returns a User object.
     */
    public static User getUser(UserDTO userDTO, String tenantDomain) {

        User user = new User();
        if (StringUtils.isNotBlank(tenantDomain)) {
            user.setTenantId(IdentityTenantUtil.getTenantId(tenantDomain));
        } else {
            user.setTenantId(MultitenantConstants.SUPER_TENANT_ID);
        }
        if (StringUtils.isNotBlank(userDTO.getRealm())) {
            user.setRealm(userDTO.getRealm());
        } else {
            user.setRealm(IdentityUtil.getPrimaryDomainName());
        }

        user.setUsername(userDTO.getUsername());
        return user;
    }

    /**
     * Used to get a Claim object from a ClaimDTO.
     *
     * @param claimDTO Claim DTO.
     * @return Returns a Claim object.
     */
    public static Claim getClaim(ClaimDTO claimDTO) {

        Claim claim = new Claim();
        claim.setClaimUri(claimDTO.getClaimUri());
        claim.setClaimValue(claimDTO.getValue());
        return claim;
    }

    /**
     * Used to concert a List of PropertyDTO to a map of properties with String key value pairs.
     *
     * @param propertyDTOList List of PropertyDTO.
     * @return Returns a map of properties with String key value pairs.
     */
    public static Map<String, String> getPropertiesToMap(List<PropertyDTO> propertyDTOList) {

        Map<String, String> properties = new HashMap<>();

        for (PropertyDTO propertyDTO :
                propertyDTOList) {
            properties.put(propertyDTO.getKey(), propertyDTO.getValue());
        }

        return properties;
    }

    /**
     * Used to get a ValidationResponseDTO.
     *
     * @param validationResponse             validationResponse ValidationResponse object consisting claim validation
     *                                       data.
     * @param isAdditionalValidationRequired boolean value stating whether further validation is required.
     * @return Returns a ValidationResponseDTO object.
     */
    public static ValidationResponseDTO getValidationResponse(ValidationResponse validationResponse,
                                                              boolean isAdditionalValidationRequired) {

        ValidationResponseDTO validationResponseDTO = new ValidationResponseDTO();

        if (!validationResponse.isValidationSuccess()) {
            validationResponseDTO.setStatus(validationResponse.getVerificationStatus());
            validationResponseDTO.setProperties(new ArrayList<>());
            validationResponseDTO.setLink(getLink(ClaimVerificationEndpointConstants.API_URI_EP_INIT_VERIFICATION));
        } else if (!isAdditionalValidationRequired) {
            validationResponseDTO.setStatus(validationResponse.getVerificationStatus());
            validationResponseDTO.setProperties(new ArrayList<>());
            validationResponseDTO.setLink(getEmptyLink());
        } else {
            validationResponseDTO.setStatus(validationResponse.getVerificationStatus());

            List<PropertyDTO> propertyList = new ArrayList<>();
            propertyList.add(getProperty("code", validationResponse.getCode()));

            validationResponseDTO.setProperties(propertyList);
            validationResponseDTO.setLink(getLink(ClaimVerificationEndpointConstants.API_URI_EP_CONFIRM));
        }

        return validationResponseDTO;
    }

    /**
     * Used to get a VerificationInitiatingResponseDTO.
     *
     * @param code conformation code.
     * @return Returns a VerificationInitiatingResponseDTO object.
     */
    public static VerificationInitiatingResponseDTO getInitVerificationResponse(String code) {

        VerificationInitiatingResponseDTO verificationInitiatingResponseDTO = new VerificationInitiatingResponseDTO();
        verificationInitiatingResponseDTO.setCode(code);
        verificationInitiatingResponseDTO.setLink(getLink(ClaimVerificationEndpointConstants.API_URI_EP_VALIDATE));

        return verificationInitiatingResponseDTO;
    }

    /**
     * Used to throw a BadRequestException with a default error message.
     * Sends a bad request response to the sender.
     *
     * @param code        Error code.
     * @param description Error description.
     */
    public static void handleBadRequest(String code, String description) {

        throw new BadRequestException(
                ClaimVerificationEndpointUtils.getErrorResponse(code,
                        ClaimVerificationEndpointConstants.STATUS_BAD_REQUEST_MESSAGE_DEFAULT, description)
        );
    }

    /**
     * Used to throw a InternalServerErrorException with a default error message.
     *
     * @param code        Error code.
     * @param description Error description.
     */
    public static void handleInternalServerError(String code, String description) {

        throw new InternalServerErrorException(
                ClaimVerificationEndpointUtils.getErrorResponse(code,
                        ClaimVerificationEndpointConstants.STATUS_INTERNAL_SERVER_ERROR_MESSAGE_DEFAULT, description)
        );
    }

    private static LinkDTO getLink(String rel) {

        LinkDTO linkDTO = new LinkDTO();
        linkDTO.setRel(rel);
        linkDTO.setUri(ClaimVerificationEndpointConstants.API_URI);

        return linkDTO;
    }

    private static LinkDTO getEmptyLink() {

        LinkDTO linkDTO = new LinkDTO();
        linkDTO.setRel(StringUtils.EMPTY);
        linkDTO.setUri(StringUtils.EMPTY);

        return linkDTO;
    }

    private static PropertyDTO getProperty(String key, String value) {

        PropertyDTO property = new PropertyDTO();
        property.setKey(key);
        property.setValue(value);

        return property;
    }

    private static ErrorDTO getErrorResponse(String code, String message, String description) {

        ErrorDTO errorDTO = new ErrorDTO();
        errorDTO.setCode(code);
        errorDTO.setMessage(message);
        errorDTO.setDescription(description);

        return errorDTO;
    }
}
