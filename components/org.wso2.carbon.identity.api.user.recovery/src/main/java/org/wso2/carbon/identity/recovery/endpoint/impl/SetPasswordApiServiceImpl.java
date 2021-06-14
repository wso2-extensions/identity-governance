package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.SetPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.ResetPasswordRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RetryErrorDTO;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import javax.ws.rs.core.Response;

public class SetPasswordApiServiceImpl extends SetPasswordApiService {

    private static final Log LOG = LogFactory.getLog(SetPasswordApiServiceImpl.class);
    private static final Log diagnosticLog = LogFactory.getLog("diagnostics");

    @Override
    public Response setPasswordPost(ResetPasswordRequestDTO resetPasswordRequest) {

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil
                .getNotificationBasedPwdRecoveryManager();
        User user = null;
        try {
            user = notificationPasswordRecoveryManager
                    .updateUserPassword(resetPasswordRequest.getKey(), resetPasswordRequest.getPassword(),
                            RecoveryUtil.getProperties(resetPasswordRequest.getProperties()));
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while resetting password ", e);
            }
            diagnosticLog.error("Client error while resetting the password. Error message: " + e.getMessage());

            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_HISTORY_VIOLATE.getCode()
                    .equals(e.getErrorCode())) {
                RetryErrorDTO errorDTO = new RetryErrorDTO();
                errorDTO.setCode(e.getErrorCode());
                errorDTO.setMessage(e.getMessage());
                errorDTO.setDescription(e.getMessage());
                errorDTO.setKey(resetPasswordRequest.getKey());
                return Response.status(Response.Status.PRECONDITION_FAILED).entity(errorDTO).build();
            }

            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());

        } catch (IdentityRecoveryException e) {
            diagnosticLog.error("Server error while resetting the passwor. Error message: " + e.getMessage());
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            diagnosticLog.error("Server error while resetting the passwor. Error message: " + throwable.getMessage());
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return Response.ok(RecoveryUtil.getUserDTO(user)).build();
    }
}
