package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.MeApiService;
import org.wso2.carbon.identity.user.endpoint.factories.MeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SuccessfulUserCreationDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeCodeValidationRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/me")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/me", description = "the me API")
public class MeApi  {

   private final MeApiService delegate = MeApiServiceFactory.getMeApi();

    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Export self user information", notes = "This API is used to retrieve the personal information of the authenticated user.", response = ExportedUserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request") })

    public Response getMe()
    {
    return delegate.getMe();
    }
    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Register user\n", notes = "This API is used for user self registration.\n", response = SuccessfulUserCreationDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 201, message = "Successfully created"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 409, message = "Conflict"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response mePost(@ApiParam(value = "Sends optional property parameters over email based on an email template." ,required=true ) SelfUserRegistrationRequestDTO user)
    {
    return delegate.mePost(user);
    }
    @POST
    @Path("/resend-code")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Resend code\n", notes = "This API is used to resend the confirmation code to the authenticated user if it is missing.\n", response = String.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 201, message = "Created"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response meResendCodePost(@ApiParam(value = "Sends property parameters such as the recovery scenario." ,required=true ) MeResendCodeRequestDTO meResendCodeRequest)
    {
    return delegate.meResendCodePost(meResendCodeRequest);
    }
    @POST
    @Path("/validate-code")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Validate code\n", notes = "This API is used to validate the code submitted by the authenticated user in a verification scenario.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Accepted"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response meValidateCodePost(@ApiParam(value = "The validation code retrieved for the verification scenario, and optional property parameters." ,required=true ) MeCodeValidationRequestDTO code)
    {
    return delegate.meValidateCodePost(code);
    }
}

