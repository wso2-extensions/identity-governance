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
import org.wso2.carbon.identity.recovery.endpoint.ClaimsApiService;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import javax.ws.rs.core.Response;

public class ClaimsApiServiceImpl extends ClaimsApiService {
    private static final Log LOG = LogFactory.getLog(ClaimsApiServiceImpl.class);

    @Override
    public Response claimsGet(String tenantDomain) {

        if (IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT) != null) {
            tenantDomain = (String) IdentityUtil.threadLocalProperties.get().get(Constants.TENANT_NAME_FROM_CONTEXT);
        }

        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        } else if (!RecoveryUtil.isValidTenantDomain(tenantDomain)) {
            RecoveryUtil.handleBadRequest("Invalid tenant domain :" + tenantDomain, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_INVALID_TENANT.getCode());
        }
        String dialect = IdentityRecoveryConstants.WSO2CARBON_CLAIM_DIALECT;
        NotificationUsernameRecoveryManager notificationBasedUsernameRecoveryManager = RecoveryUtil
                .getNotificationBasedUsernameRecoveryManager();
        ClaimDTO[] claimDTOs = new ClaimDTO[0];
        try {
            Claim[] userClaims = notificationBasedUsernameRecoveryManager.getIdentitySupportedClaims(dialect, tenantDomain);
            claimDTOs = RecoveryUtil.getClaimDTOs(userClaims);

        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while getting all identity claims ", e);
            }
            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, null);
        } catch (Throwable throwable) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);

        }
        return Response.ok(claimDTOs).build();
    }
}
