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

package org.wso2.carbon.identity.claim.verification.endpoint.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.model.ValidationResponse;
import org.wso2.carbon.identity.claim.verification.endpoint.ValidateApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ValidationRequestDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ValidationResponseDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointConstants;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointUtils;

import javax.ws.rs.core.Response;

/**
 * Implementation of the validate endpoint.
 */
public class ValidateApiServiceImpl extends ValidateApiService {

    private static final Log LOG = LogFactory.getLog(ValidateApiServiceImpl.class);

    @Override
    public Response validatePost(ValidationRequestDTO validationRequest) {

        ValidationResponse validationResponse = null;
        try {
            validationResponse = ClaimVerificationEndpointUtils.getClaimVerificationHandler().validateClaim(
                    validationRequest.getCode(),
                    ClaimVerificationEndpointUtils.getPropertiesToMap(validationRequest.getProperties()),
                    validationRequest.getRequireAdditionalValidation()
            );
        } catch (ClaimVerificationException e) {

            if (e instanceof ClaimVerificationBadRequestException) {
                if (LOG.isDebugEnabled()) {
                    String msg = "Malformed request received for claim verification validation. ";
                    LOG.debug(msg + e.getErrorCode() + ":" + e.getMessage(), e);
                }
                ClaimVerificationEndpointUtils.handleBadRequest(e.getErrorCode(), e.getMessage());
            } else {
                String msg = "Error while validating the claims";
                LOG.error(msg, e);
                ClaimVerificationEndpointUtils.handleInternalServerError(
                        ClaimVerificationEndpointConstants.ERROR_CODE_UNEXPECTED_ERROR, msg);

            }
        }
        ValidationResponseDTO validationResponseDTO =
                ClaimVerificationEndpointUtils.getValidationResponse(validationResponse,
                        validationRequest.getRequireAdditionalValidation());

        return Response.ok().entity(validationResponseDTO).build();
    }
}
