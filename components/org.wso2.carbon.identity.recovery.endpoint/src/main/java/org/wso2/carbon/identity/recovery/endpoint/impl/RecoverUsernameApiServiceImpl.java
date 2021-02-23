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

import javax.ws.rs.core.Response;
import java.util.List;


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
