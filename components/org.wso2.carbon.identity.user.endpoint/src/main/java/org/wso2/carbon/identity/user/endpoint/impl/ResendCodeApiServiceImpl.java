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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.ResendCodeApiService;
import org.wso2.carbon.identity.user.endpoint.Util.Utils;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;

import javax.ws.rs.core.Response;

public class ResendCodeApiServiceImpl extends ResendCodeApiService {
    private static final Log LOG = LogFactory.getLog(ResendCodeApiServiceImpl.class);

    @Override
    public Response resendCodePost(ResendCodeRequestDTO resendCodeRequestDTO) {

        String tenantFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);

        if(StringUtils.isNotBlank(tenantFromContext)) {
            resendCodeRequestDTO.getUser().setTenantDomain(tenantFromContext);
        }

        UserSelfRegistrationManager userSelfRegistrationManager = Utils
                .getUserSelfRegistrationManager();
        NotificationResponseBean notificationResponseBean = null;
        try {
            notificationResponseBean = userSelfRegistrationManager.resendConfirmationCode(
                    Utils.getUser(resendCodeRequestDTO.getUser()),
                    Utils.getProperties(resendCodeRequestDTO.getProperties()));

        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while resending self sign-up confirmation code ", e);
            }
            Utils.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            Utils.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.status(Response.Status.CREATED).build();
        }
        return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getKey()).build();
    }
}
