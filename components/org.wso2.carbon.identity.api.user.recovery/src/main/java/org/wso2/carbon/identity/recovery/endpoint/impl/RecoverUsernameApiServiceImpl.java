/*
 * Copyright (c) 2016-2024, WSO2 LLC. (http://www.wso2.com).
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
package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.RecoverUsernameApiService;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserClaimDTO;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;

import java.util.List;

import javax.ws.rs.core.Response;

public class RecoverUsernameApiServiceImpl extends RecoverUsernameApiService {

    private static final Log LOG = LogFactory.getLog(RecoverUsernameApiServiceImpl.class);

    @Override
    public Response recoverUsernamePost(List<UserClaimDTO> claim, String tenantDomain, Boolean notify) {

        if (IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT) != null) {
            tenantDomain = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);
        }

        NotificationUsernameRecoveryManager notificationBasedUsernameRecoveryManager = RecoveryUtil
                .getNotificationBasedUsernameRecoveryManager();
        String username = null;
        try {
            username = notificationBasedUsernameRecoveryManager.verifyUsername(RecoveryUtil.getUserClaims(claim),
                    tenantDomain, notify);
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug(e.getMessage(), e);
            }

            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND.getCode()
                    .equals(e.getErrorCode())) {
                return Response.noContent().build();
            }

            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            if (e.getCause() != null && StringUtils.equals(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND,
                    e.getCause().getMessage())) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug(e.getCause().getMessage());
                }
                return Response.ok().build();
            }
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            if (throwable != null && StringUtils.equals(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND,
                    throwable.getMessage())) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug(throwable.getMessage());
                }
                return Response.ok().build();
            }
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (StringUtils.isBlank(username)) {
            return Response.accepted().build();
        }
        return Response.accepted(username).build();
    }

}
