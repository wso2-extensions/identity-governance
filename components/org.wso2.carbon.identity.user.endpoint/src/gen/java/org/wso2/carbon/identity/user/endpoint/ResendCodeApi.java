package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.ResendCodeApiService;
import org.wso2.carbon.identity.user.endpoint.factories.ResendCodeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/resend-code")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/resend-code", description = "the resend-code API")
public class ResendCodeApi  {

   private final ResendCodeApiService delegate = ResendCodeApiServiceFactory.getResendCodeApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Resend code\n", notes = "This API is used to resend the confirmation code if it is missing.\n", response = String.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response resendCodePost(@ApiParam(value = "Sends optional property parameters over email based on an email template." ,required=true ) ResendCodeRequestDTO user)
    {
    return delegate.resendCodePost(user);
    }
}

