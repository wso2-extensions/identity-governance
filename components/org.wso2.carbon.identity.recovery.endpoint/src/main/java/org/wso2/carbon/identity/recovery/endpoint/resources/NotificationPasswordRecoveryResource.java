
package org.wso2.carbon.identity.recovery.endpoint.resources;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.*;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.bean.UserPassword;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/notification")
@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
@Produces(MediaType.APPLICATION_JSON)
public class NotificationPasswordRecoveryResource extends AbstractResource {
    private static final Log LOG = LogFactory.getLog(NotificationPasswordRecoveryResource.class);

    public NotificationPasswordRecoveryResource() {

    }

    @POST
    @Path("/notify")
    public Response sendRecoveryNotification(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                             User user) {

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil
                .getNotificationBasedPwdRecoveryManager();
        NotificationResponseBean notificationResponseBean;
        try {
            notificationResponseBean = notificationPasswordRecoveryManager.sendRecoveryNotification(user);
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while sending recovery notification ", e);
            }
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while sending recovery notification ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while sending recovery notification ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        if (StringUtils.isBlank(notificationResponseBean.getKey())) {
            return Response.ok().build();
        }
        return Response.ok(notificationResponseBean).build();
    }


    @PUT
    @Path("/reset-password")
    public Response updatePassword(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                   UserPassword userPassword) {

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil.getNotificationBasedPwdRecoveryManager();
        try {
            notificationPasswordRecoveryManager.updatePassword(userPassword.getUser(), userPassword.getCode(),
                    userPassword.getPassword());
        } catch (IdentityRecoveryClientException e) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Client Error while resetting password ", e);
            }
            return handleErrorResponse(ResponseStatus.INVALID, e.getErrorDescription(), e.getErrorCode());
        } catch (IdentityRecoveryException e) {
            LOG.error("Error while resetting password ", e);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, e.getErrorCode());
        } catch (Throwable throwable) {
            LOG.error("Unexpected Error while resetting password ", throwable);
            return handleErrorResponse(ResponseStatus.FAILED, Constants.SERVER_ERROR, IdentityRecoveryConstants
                    .ErrorMessages.ERROR_CODE_UNEXPECTED.getCode());
        }
        return Response.ok().build();
    }
}
