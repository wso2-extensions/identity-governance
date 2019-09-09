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
import org.wso2.carbon.identity.claim.verification.endpoint.ConfirmApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ConfirmationRequestDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointConstants;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointUtils;

import javax.ws.rs.core.Response;

/**
 * Implementation of the confirm endpoint.
 */
public class ConfirmApiServiceImpl extends ConfirmApiService {

    private static final Log LOG = LogFactory.getLog(ConfirmApiServiceImpl.class);

    @Override
    public Response confirmPost(ConfirmationRequestDTO confirmationRequest) {

        boolean isValidationSuccess = false;
        if (ClaimVerificationEndpointConstants.CLAIM_VALIDATION_SUCCESSFUL.equalsIgnoreCase(
                confirmationRequest.getStatus())) {
            isValidationSuccess = true;
        } else if (ClaimVerificationEndpointConstants.CLAIM_VALIDATION_FAILED.equalsIgnoreCase(
                confirmationRequest.getStatus())) {
            isValidationSuccess = false;
        } else {
            String msg = String.format("Sent validation status: %s is not a acceptable status. Use %s or %s .",
                    confirmationRequest.getStatus(), ClaimVerificationEndpointConstants.CLAIM_VALIDATION_SUCCESSFUL,
                    ClaimVerificationEndpointConstants.CLAIM_VALIDATION_FAILED);
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            ClaimVerificationEndpointUtils.handleBadRequest(
                    ClaimVerificationEndpointConstants.ERROR_CODE_NO_MATCHING_VALIDATION_STATUS_FOUND, msg);
        }

        try {
            ClaimVerificationEndpointUtils.getClaimVerificationHandler().confirmVerification(
                    confirmationRequest.getCode(), isValidationSuccess);
        } catch (ClaimVerificationBadRequestException e) {
            if (LOG.isDebugEnabled()) {
                String msg = "Malformed request received for claim verification confirmation. ";
                LOG.debug(msg + e.getErrorCode() + ":" + e.getMessage(), e);
            }
            ClaimVerificationEndpointUtils.handleBadRequest(e.getErrorCode(), e.getMessage());
        } catch (ClaimVerificationException e) {
            String msg = "Error while finalizing claim verification.";
            LOG.error(msg, e);
            ClaimVerificationEndpointUtils.handleInternalServerError(
                    ClaimVerificationEndpointConstants.ERROR_CODE_UNEXPECTED_ERROR, msg);
        }

        return Response.ok().build();
    }
}
