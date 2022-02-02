/*
 *
 *  Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.user.endpoint.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.dto.VerifiedChannelDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.user.endpoint.dto.CodeValidationRequestDTO;

import java.util.HashMap;
import javax.ws.rs.core.Response;

public class ValidateCodeApiServiceImpl extends ValidateCodeApiService {

    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);

    @Override
    public Response validateCodePost(CodeValidationRequestDTO codeValidationRequestDTO) {

        User user = null;
        UserSelfRegistrationManager userSelfRegistrationManager = Utils.getUserSelfRegistrationManager();
        try {
            // Get the map of properties in the request.
            HashMap<String, String> propertyMap = Utils.getPropertiesMap(codeValidationRequestDTO.getProperties());

            // Get externally verified channel information.
            VerifiedChannelDTO verifiedChannelDTO = codeValidationRequestDTO.getVerifiedChannel();
            String verifiedChannelType = null;
            String verifiedChannelClaim = null;

            // Handling verified channel details not in the request.
            if (verifiedChannelDTO != null) {
                verifiedChannelClaim = verifiedChannelDTO.getClaim();
                verifiedChannelType = verifiedChannelDTO.getType();
            }
            // Confirm self registration.
            user = userSelfRegistrationManager
                    .getConfirmedSelfRegisteredUser(codeValidationRequestDTO.getCode(), verifiedChannelType,
                            verifiedChannelClaim, propertyMap);
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while confirming self up user ", e);
            }
            Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return Response.accepted(Utils.getUserDTO(user)).build();
    }
}
