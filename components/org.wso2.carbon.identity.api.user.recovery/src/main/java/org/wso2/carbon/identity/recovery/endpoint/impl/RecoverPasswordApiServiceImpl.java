package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.*;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.*;


import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;

import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.util.Utils;

import javax.ws.rs.core.Response;

public class RecoverPasswordApiServiceImpl extends RecoverPasswordApiService {

    private static final Log LOG = LogFactory.getLog(RecoverPasswordApiServiceImpl.class);
    private static final String USERNAME_CLAIM = "http://wso2.org/claims/username";

    @Override
    public Response recoverPasswordPost(RecoveryInitiatingRequestDTO recoveryInitiatingRequest, String type,
                                        Boolean notify){
        
        String tenantDomainFromContext = (String) IdentityUtil.threadLocalProperties.get().get(Constants
                .TENANT_NAME_FROM_CONTEXT);

        if (StringUtils.isNotBlank(tenantDomainFromContext)) {
            recoveryInitiatingRequest.getUser().setTenantDomain(tenantDomainFromContext);
        } else {
            recoveryInitiatingRequest.getUser().setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        }

        UserDTO user = recoveryInitiatingRequest.getUser();
        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil
                .getNotificationBasedPwdRecoveryManager();
        NotificationResponseBean notificationResponseBean = null;

        try {
            // If multi attribute login is enabled, resolve the user before sending recovery notification sending.
            if (IdentityRecoveryServiceDataHolder.getInstance().getMultiAttributeLoginService()
                    .isEnabled(user.getTenantDomain())) {
                ResolvedUserResult resolvedUserResult =
                        IdentityRecoveryServiceDataHolder.getInstance().getMultiAttributeLoginService()
                                .resolveUser(user.getUsername(), user.getTenantDomain());
                if (resolvedUserResult != null && ResolvedUserResult.UserResolvedStatus.SUCCESS.
                        equals(resolvedUserResult.getResolvedStatus())) {
                    User resolvedUser = new User();
                    resolvedUser.setUserName(resolvedUserResult.getUser().getUsername());
                    resolvedUser.setUserStoreDomain(resolvedUserResult.getUser().getUserStoreDomain());
                    resolvedUser.setTenantDomain(resolvedUserResult.getUser().getTenantDomain());
                    notificationResponseBean =
                            notificationPasswordRecoveryManager.sendRecoveryNotification(resolvedUser, type, notify,
                                    RecoveryUtil.getProperties(recoveryInitiatingRequest.getProperties()));
                } else {
                    /* If the user couldn't resolve, Check for NOTIFY_USER_EXISTENCE property. If the property is not
                    enabled, notify with an empty NotificationResponseBean.*/
                    boolean notifyUserExistence = Boolean.parseBoolean(
                            IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE));
                    if (notifyUserExistence) {
                        throw Utils.handleClientException(
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                                user.getUsername());
                    }
                    notificationResponseBean =
                            new NotificationResponseBean(RecoveryUtil.getUser(recoveryInitiatingRequest.getUser()));
                }
            } else {
                notificationResponseBean = notificationPasswordRecoveryManager.sendRecoveryNotification(
                        RecoveryUtil.getUser(recoveryInitiatingRequest.getUser()), type, notify,
                        RecoveryUtil.getProperties(recoveryInitiatingRequest.getProperties()));
            }
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while sending recovery notification ", e);
            }
            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FEDERATED_USER.getCode().equals(e.getErrorCode())) {
                return Response.accepted().build();
            }
            if (IdentityRecoveryConstants.ErrorMessages.INVALID_PASSWORD_RECOVERY_REQUEST.getCode().
                    equals(e.getErrorCode())) {
                return Response.accepted().build();
            }
            RecoveryUtil.handleBadRequest(e.getMessage(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            if (e.getCause() != null && StringUtils.equals(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND,
                    e.getCause().getMessage())) {
                LOG.error(e.getCause().getMessage(), e.getCause());
                RecoveryUtil.handleBadRequest(e.getCause().getMessage(), Constants.ERROR_CODE_EMAIL_NOT_FOUND);
            }
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, e.getErrorCode(), LOG, e);
        } catch (Throwable throwable) {
            if (throwable != null && StringUtils.equals(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND,
                    throwable.getMessage())) {
                LOG.error(throwable.getMessage(), throwable);
                RecoveryUtil.handleBadRequest(throwable.getMessage(), Constants.ERROR_CODE_EMAIL_NOT_FOUND);
            }
            RecoveryUtil.handleInternalServerError(Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode(), LOG, throwable);
        }
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.accepted().build();
        }
        return Response.accepted(notificationResponseBean.getKey()).build();
    }
}
