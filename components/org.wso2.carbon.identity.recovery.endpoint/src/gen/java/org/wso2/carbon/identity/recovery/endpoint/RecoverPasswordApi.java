package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.RecoverPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.RecoverPasswordApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/recover-password")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/recover-password", description = "the recover-password API")
public class RecoverPasswordApi  {

   private final RecoverPasswordApiService delegate = RecoverPasswordApiServiceFactory.getRecoverPasswordApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to send password recovery confirmation over defined channels like email/sms\n", response = String.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response recoverPasswordPost(@ApiParam(value = "It can be sent optional property parameters over email based on email template." ,required=true ) RecoveryInitiatingRequestDTO recoveryInitiatingRequest,
    @ApiParam(value = "Notification Type") @QueryParam("type") String type,
    @ApiParam(value = "If notify=true then, notifications will be internally managed.") @QueryParam("notify") Boolean notify)
    {
    return delegate.recoverPasswordPost(recoveryInitiatingRequest,type,notify);
    }
}

