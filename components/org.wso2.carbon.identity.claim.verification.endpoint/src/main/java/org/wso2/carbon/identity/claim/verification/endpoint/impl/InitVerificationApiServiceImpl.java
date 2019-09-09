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
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.endpoint.InitVerificationApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.VerificationInitiatingRequestDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointConstants;
import org.wso2.carbon.identity.claim.verification.endpoint.impl.util.ClaimVerificationEndpointUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;

import javax.ws.rs.core.Response;

/**
 * Implementation of the init-verification endpoint.
 */
public class InitVerificationApiServiceImpl extends InitVerificationApiService {

    private static final Log LOG = LogFactory.getLog(InitVerificationApiServiceImpl.class);

    @Override
    public Response initVerificationPost(VerificationInitiatingRequestDTO verificationInitiatingRequest) {

        String tenantDomainFromContext =
                (String) IdentityUtil.threadLocalProperties.get().get(ClaimVerificationEndpointConstants
                        .TENANT_NAME_FROM_CONTEXT);

        User user = ClaimVerificationEndpointUtils.getUser(verificationInitiatingRequest.getUser(),
                tenantDomainFromContext);
        Claim claim = ClaimVerificationEndpointUtils.getClaim(verificationInitiatingRequest.getClaim());

        String confirmationCode = "";
        try {
            confirmationCode = ClaimVerificationEndpointUtils.getClaimVerificationHandler().initVerification(
                    user, claim, verificationInitiatingRequest.getVerificationMethod(),
                    ClaimVerificationEndpointUtils.getPropertiesToMap(verificationInitiatingRequest.getProperties()));
        } catch (ClaimVerificationBadRequestException e) {
            if (LOG.isDebugEnabled()) {
                String msg = "Malformed request received for claim verification initiation. ";
                LOG.debug(msg + e.getErrorCode() + ":" + e.getMessage(), e);
            }
            ClaimVerificationEndpointUtils.handleBadRequest(e.getErrorCode(), e.getMessage());
        } catch (ClaimVerificationException e) {
            String msg = "Error while initiating claim verification.";
            LOG.error(msg, e);
            ClaimVerificationEndpointUtils.handleInternalServerError(
                    ClaimVerificationEndpointConstants.ERROR_CODE_UNEXPECTED_ERROR, msg);
        }

        return Response.ok().entity(ClaimVerificationEndpointUtils.getInitVerificationResponse(confirmationCode))
                .build();
    }
}
