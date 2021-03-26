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

package org.wso2.carbon.identity.user.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.UpdateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameUpdateRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.rename.core.dto.UserDTO;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateClientException;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateException;
import org.wso2.carbon.identity.user.rename.core.service.UsernameUpdateService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Optional;
import javax.ws.rs.core.Response;

import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Error.ERROR_UNEXPECTED;

public class UpdateUsernameApiServiceImpl extends UpdateUsernameApiService {

    private static final Log log = LogFactory.getLog(UpdateUsernameApiServiceImpl.class);

    /**
     * Username update service method.
     *
     * @param user UsernameUpdateRequestDTO{@link UsernameUpdateRequestDTO} including update request parameters
     * @return Response status. Returns 200 upon successful update
     */
    @Override
    public Response updateUsernamePut(UsernameUpdateRequestDTO user) {

        try {

            UserDTO userDTO = new UserDTO();
            userDTO.setExistingUsername(user.getExistingUsername());
            userDTO.setNewUsername(user.getNewUsername());
            userDTO.setUserStoreDomain(user.getRealm());
            userDTO.setTenantDomain(getTenantDomainFromContext());

            UsernameUpdateService usernameUpdateService = Utils.getUsernameUpdateService();
            usernameUpdateService.updateUsername(userDTO);
        } catch (UsernameUpdateClientException e) {
            handleClientError(e);
        } catch (UsernameUpdateException e) {
            handleServerError(e);
        } catch (Throwable e) {
            handleUnexpectedServerError(e);
        }

        return Response.ok().status(Response.Status.OK).build();
    }

    private String getTenantDomainFromContext() {

        String tenantDomain = null;
        if (IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT) != null) {
            tenantDomain = (String) IdentityUtil.threadLocalProperties.get().get(Constants
                    .TENANT_NAME_FROM_CONTEXT);
        }

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        return tenantDomain;
    }

    private void handleClientError(UsernameUpdateClientException e) {

        if (log.isDebugEnabled()) {
            log.debug(e.getMessage(), e);
        }

        UsernameUpdateClientException.ErrorType errorType = e.getErrorType();
        if (UsernameUpdateClientException.ErrorType.BAD_REQUEST.equals(errorType)) {
            Utils.handleBadRequest(e.getMessage(), Optional.ofNullable(e.getErrorCode()).orElse(String.valueOf
                    (Response.Status.BAD_REQUEST.getStatusCode())));
        } else if (UsernameUpdateClientException.ErrorType.NOT_ACCEPTABLE.equals(errorType)) {
            Utils.handleNotAcceptable(e.getMessage(), Optional.ofNullable(e.getErrorCode()).orElse(String.valueOf
                    (Response.Status.NOT_ACCEPTABLE.getStatusCode())));
        } else if (UsernameUpdateClientException.ErrorType.NOT_FOUND.equals(errorType)) {
            Utils.handleNotFound(e.getMessage(), Optional.ofNullable(e.getErrorCode()).orElse(String.valueOf
                    (Response.Status.NOT_FOUND.getStatusCode())));
        }

        Utils.handleBadRequest(e.getMessage(), Optional.ofNullable(e.getErrorCode()).orElse(String.valueOf
                (Response.Status.BAD_REQUEST.getStatusCode())));
    }

    private void handleServerError(UsernameUpdateException e) {

        Utils.handleInternalServerError(e.getMessage(), Optional.ofNullable(e.getErrorCode()).orElse(String.valueOf
                (Response.Status.INTERNAL_SERVER_ERROR.getStatusCode())), log, e);
    }

    private void handleUnexpectedServerError(Throwable e) {

        Utils.handleInternalServerError(ERROR_UNEXPECTED.getMessage(), ERROR_UNEXPECTED.getCode(), log, e);
    }
}

