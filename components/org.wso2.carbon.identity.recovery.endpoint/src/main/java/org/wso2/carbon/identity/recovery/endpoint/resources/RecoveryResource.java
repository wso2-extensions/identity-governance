
package org.wso2.carbon.identity.recovery.endpoint.resources;

import com.google.gson.Gson;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.NotificationBasedPwdRecoveryManager;
import org.wso2.carbon.identity.recovery.endpoint.Constants;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.bean.StandardResponse;
import org.wso2.carbon.identity.recovery.bean.ResponseBean;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/test")
@Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
@Produces(MediaType.APPLICATION_JSON)
public class RecoveryResource {
    private static final Log LOG = LogFactory.getLog(RecoveryResource.class);
    Gson gson;

    public RecoveryResource() {
        gson = new Gson();
    }

    @PUT
    @Path("/notify")
    public Response sendRecoveryNotification(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization,
                                             User user) {

        NotificationBasedPwdRecoveryManager notificationBasedPwdRecoveryManager = RecoveryUtil.getNotificationBasedPwdRecoveryManager();
        ResponseBean responseBean;
        try {
            responseBean = notificationBasedPwdRecoveryManager.sendRecoveryNotification(user);
        } catch (IdentityRecoveryException e) {
            String errorCode = e.getCode();
            StandardResponse error = gson.fromJson(errorCode, StandardResponse.class);
            return Response.serverError().entity(error).build();
        }
        return Response.ok(responseBean).build();
    }

    @GET
    @Path("/notify")
    public Response getNotification(@HeaderParam(Constants.AUTHORIZATION_HEADER) String authorization) {
        User user = new User();
        user.setTenantDomain("carbon.super");
        user.setUserName("isura");
        user.setUserStoreDomain("PRIMARY");
        ResponseBean responseBean = new ResponseBean(user);
        return Response.ok(responseBean).build();
    }
}
