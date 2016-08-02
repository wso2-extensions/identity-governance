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
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.MeApiService;
import org.wso2.carbon.identity.user.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;

import javax.ws.rs.core.Response;

public class MeApiServiceImpl extends MeApiService {

    private static final Log LOG = LogFactory.getLog(MeApiServiceImpl.class);

    @Override
    public Response mePost(SelfUserRegistrationRequestDTO selfUserRegistrationRequestDTO) {
        UserSelfRegistrationManager userSelfRegistrationManager = RecoveryUtil
                .getUserSelfRegistrationManager();
        NotificationResponseBean notificationResponseBean = null;
        try {
            notificationResponseBean = userSelfRegistrationManager.registerUser(
                    RecoveryUtil.getUser(selfUserRegistrationRequestDTO.getUser()), selfUserRegistrationRequestDTO.getUser().getPassword(),
                    RecoveryUtil.getClaims(selfUserRegistrationRequestDTO.getUser().getClaims()),
                    RecoveryUtil.getProperties(selfUserRegistrationRequestDTO.getProperties()),
                    RecoveryUtil.getRoles(selfUserRegistrationRequestDTO.getRoles()));

        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while registering self up user ", e);
            }
            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.status(Response.Status.CREATED).build();
        }
        return Response.status(Response.Status.CREATED).entity(notificationResponseBean.getKey()).build();
    }
}
