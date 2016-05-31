
package org.wso2.carbon.identity.recovery.endpoint.resources;

import com.google.gson.Gson;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.bean.ResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.bean.UserPassword;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/notification")
@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
@Produces(MediaType.APPLICATION_JSON)
public class NotificationPasswordRecoveryResource extends AbstractResource {
    private static final Log LOG = LogFactory.getLog(NotificationPasswordRecoveryResource.class);
    Gson gson;

    public NotificationPasswordRecoveryResource() {
        gson = new Gson();
    }

    @PUT
    @Path("/notify")
    public Response sendRecoveryNotification(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                             User user) {

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil.getNotificationBasedPwdRecoveryManager();
        ResponseBean responseBean;
        try {
            responseBean = notificationPasswordRecoveryManager.sendRecoveryNotification(user);
        } catch (IdentityRecoveryException e) {
            return handleResponse(ResponseStatus.FAILED, e.getError());
        }
        return Response.ok(responseBean).build();
    }


    @PUT
    @Path("/reset_password")
    public Response updatePassword(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                   UserPassword userPassword) {

        NotificationPasswordRecoveryManager notificationPasswordRecoveryManager = RecoveryUtil.getNotificationBasedPwdRecoveryManager();
        try {
            notificationPasswordRecoveryManager.updatePassword(userPassword.getUser(), userPassword.getCode(),
                    userPassword.getPassword());
        } catch (IdentityRecoveryException e) {
            return handleResponse(ResponseStatus.FAILED, e.getError());
        }
        return Response.ok().build();
    }
}
