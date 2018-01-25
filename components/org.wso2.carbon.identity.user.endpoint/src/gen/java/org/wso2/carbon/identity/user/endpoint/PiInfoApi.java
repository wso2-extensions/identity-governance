package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.PiInfoApiService;
import org.wso2.carbon.identity.user.endpoint.factories.PiInfoApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.ExportedUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserSearchResponseDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/pi-info")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/pi-info", description = "the pi-info API")
public class PiInfoApi  {

   private final PiInfoApiService delegate = PiInfoApiServiceFactory.getPiInfoApi();

    @GET
    @Path("/{userId}")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Export user information by ID", notes = "Returns the personal information of a single user", response = ExportedUserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found") })

    public Response getUserById(@ApiParam(value = "ID of user to return",required=true ) @PathParam("userId")  String userId)
    {
    return delegate.getUserById(userId);
    }
    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Search userId by username", notes = "Returns userId of the username to use in /pi-info endpoint", response = UserSearchResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found") })

    public Response searchUserByName(@ApiParam(value = "ID of user to return",required=true) @QueryParam("username")  String username)
    {
    return delegate.searchUserByName(username);
    }
}

