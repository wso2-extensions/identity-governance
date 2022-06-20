package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.SetPasswordApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.SetPasswordApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.RetryErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ResetPasswordRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/set-password")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/set-password", description = "the set-password API")
public class SetPasswordApi  {

   private final SetPasswordApiService delegate = SetPasswordApiServiceFactory.getSetPasswordApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Update Password\n", notes = "This API is used to reset user password using the confirmation key recieved through the recovery process. Need to input `key` and the new `password`.\n", response = UserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 412, message = "Precondition Failed"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response setPasswordPost(@ApiParam(value = "Key, password, and optional metadata properties." ,required=true ) ResetPasswordRequestDTO resetPasswordRequest)
    {
    return delegate.setPasswordPost(resetPasswordRequest);
    }
}

