package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
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
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

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
        int tenantIdFromContext = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        ResolvedUserResult resolvedUserResult =
                FrameworkUtils.processMultiAttributeLoginIdentification(user.getUsername(), user.getTenantDomain());
        if (resolvedUserResult != null && ResolvedUserResult.UserResolvedStatus.SUCCESS.
                equals(resolvedUserResult.getResolvedStatus())) {
            user.setUsername(resolvedUserResult.getUser().getUsername());
            UserDTO userDTO = recoveryInitiatingRequest.getUser();
            userDTO.setUsername(user.getUsername());
            recoveryInitiatingRequest.setUser(userDTO);
        }
        if (StringUtils.isBlank(user.getRealm())) {
            String[] userList = RecoveryUtil.getUserList(tenantIdFromContext, user.getUsername());

            if (ArrayUtils.isEmpty(userList)) {
                String msg = "Unable to find an user with username: " + user.getUsername() + " in the system.";
                LOG.error(msg);
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
