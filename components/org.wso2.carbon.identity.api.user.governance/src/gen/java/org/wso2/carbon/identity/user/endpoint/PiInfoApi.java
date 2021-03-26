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
    @io.swagger.annotations.ApiOperation(value = "Export user information by ID", notes = "This API is used to retrieve the personal information of a single user using the given user ID.", response = ExportedUserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found") })

    public Response getUserById(@ApiParam(value = "Unique Identifier of user. First encode the user's username using the Base64 Encoder. Then encode the Base64 Encoded username using the URL Encoder. Use the result you get as the user ID.",required=true ) @PathParam("userId")  String userId)
    {
    return delegate.getUserById(userId);
    }
    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Search user ID by username", notes = "Returns the user ID of the given username. This ID can be used with the /pi-info endpoint.", response = UserSearchResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized request"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found") })

    public Response searchUserByName(@ApiParam(value = "Username pattern used to search for the user ID.",required=true) @QueryParam("username")  String username)
    {
    return delegate.searchUserByName(username);
    }
}

