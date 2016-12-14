package org.wso2.carbon.identity.recovery.endpoint.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
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

        UserDTO user = recoveryInitiatingRequest.getUser();
        if (StringUtils.isNotBlank(tenantDomainFromContext)) {
            recoveryInitiatingRequest.getUser().setTenantDomain(tenantDomainFromContext);
        }
        int tenantIdFromContext = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        if (StringUtils.isBlank(user.getRealm())) {
            String[] userList = getUserList(tenantIdFromContext, user.getUsername());

            if (ArrayUtils.isEmpty(userList)) {
                String msg = "Unable to find an user with username: " + user.getUsername() + " in the system.";
                LOG.error(msg);
                RecoveryUtil.handleBadRequest(msg, "404");
            } else if (userList.length == 1) {
                recoveryInitiatingRequest.getUser().setRealm(IdentityUtil.extractDomainFromName(userList[0]));
            } else {
                String msg = "There are multiple users with username: " + user.getUsername() + " in the system, " +
                        "please send the correct user-store domain along with the username.";
                LOG.error(msg);
                RecoveryUtil.handleBadRequest(msg, "400");
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
            return Response.accepted().build();
        }
        return Response.accepted(notificationResponseBean.getKey()).build();
    }

    private static String[] getUserList(int tenantId, String username) {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        String[] userList = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();

        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm
                        (tenantId).getUserStoreManager();
                userList = userStoreManager.getUserList(USERNAME_CLAIM, username, null);
            }
        } catch (Exception e) {
            String msg = "Error retrieving the user-list for the tenant : " + tenantId;
            RecoveryUtil.handleInternalServerError(msg, "500", LOG, e);
        }
        return userList;
    }
}
