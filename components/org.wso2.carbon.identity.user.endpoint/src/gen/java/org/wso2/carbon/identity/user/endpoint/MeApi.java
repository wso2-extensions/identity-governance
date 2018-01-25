package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.MeApiService;
import org.wso2.carbon.identity.user.endpoint.factories.MeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;

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
    @io.swagger.annotations.ApiOperation(value = "Export self user information", notes = "Returns the personal information of the authenticated user", response = ExportedUserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request") })

    public Response getMe()
    {
    return delegate.getMe();
    }
    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Register User\n", notes = "This API is used to user self registration.\n", response = String.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 201, message = "Successful created"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 409, message = "Conflict"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response mePost(@ApiParam(value = "It can be sent optional property parameters over email based on email template." ,required=true ) SelfUserRegistrationRequestDTO user)
    {
    return delegate.mePost(user);
    }
}

