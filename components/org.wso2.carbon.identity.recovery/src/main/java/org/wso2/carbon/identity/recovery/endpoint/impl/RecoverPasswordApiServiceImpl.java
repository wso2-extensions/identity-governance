/*
 *
 *  Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.RecoverPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import javax.ws.rs.core.Response;

/**
 * Recover password api implementation
 */
public class RecoverPasswordApiServiceImpl extends RecoverPasswordApiService {

    private static final Log LOG = LogFactory.getLog(RecoverPasswordApiServiceImpl.class);
    private static final String USERNAME_CLAIM = "http://wso2.org/claims/username";

    @Override
    public Response recoverPasswordPost(RecoveryInitiatingRequestDTO recoveryInitiatingRequest, String type,
                                        Boolean notify) {
        String tenantDomainFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants
                .TENANT_NAME_FROM_CONTEXT);

        if (StringUtils.isNotBlank(tenantDomainFromContext)) {
            recoveryInitiatingRequest.getUser().setTenantDomain(tenantDomainFromContext);
        } else {
            recoveryInitiatingRequest.getUser().setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        }

        UserDTO user = recoveryInitiatingRequest.getUser();
        int tenantIdFromContext = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        if (StringUtils.isBlank(user.getRealm())) {
            String[] userList = RecoveryUtil.getUserList(tenantIdFromContext, user.getUsername());

            if (ArrayUtils.isEmpty(userList)) {
                String msg = "Unable to find an user with username: " + user.getUsername() + " in the system.";
                LOG.error(msg);
                RecoveryUtil.handleBadRequest(msg, Constants.ERROR_CODE_NO_USER_FOUND_FOR_RECOVERY);
            } else if (userList.length == 1) {
                recoveryInitiatingRequest.getUser().setRealm(IdentityUtil.extractDomainFromName(userList[0]));
            } else {
                String msg = "There are multiple users with username: " + user.getUsername() + " in the system, " +
                        "please send the correct user-store domain along with the username.";
                LOG.error(msg);
                RecoveryUtil.handleBadRequest(msg, Constants.ERROR_CODE_MULTIPLE_USERS_MATCHING);
            }
        }

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil
                .getNotificationBasedPwdRecoveryManager();
        NotificationResponseBean notificationResponseBean = null;

        try {
            notificationResponseBean = notificationPasswordRecoveryManager.sendRecoveryNotification(RecoveryUtil
                    .getUser(recoveryInitiatingRequest.getUser()), type, notify, RecoveryUtil.getProperties
                    (recoveryInitiatingRequest.getProperties()));

        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while sending recovery notification ", e);
            }
            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.ok().build();
        }
        return Response.ok(notificationResponseBean.getKey()).build();
    }
}
