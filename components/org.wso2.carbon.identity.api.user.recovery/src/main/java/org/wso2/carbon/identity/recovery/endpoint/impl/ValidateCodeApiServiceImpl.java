package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.recovery.endpoint.dto.CodeValidationRequestDTO;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import javax.ws.rs.core.Response;

/**
 * Implementation class of password reset confirmation code validation api.
 */
public class ValidateCodeApiServiceImpl extends ValidateCodeApiService {

    private static final Log LOG = LogFactory.getLog(ValidateCodeApiServiceImpl.class);

    @Override
    public Response validateCodePost(CodeValidationRequestDTO codeValidationRequestDTO) {

        User user = null;
        try {
            NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil
                    .getNotificationBasedPwdRecoveryManager();
            user = notificationPasswordRecoveryManager
                    .getValidatedUser(codeValidationRequestDTO.getCode(), codeValidationRequestDTO.getStep());
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while validating the confirmation code ", e);
            }
            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        return Response.accepted(RecoveryUtil.getUserDTO(user)).build();
    }
}
