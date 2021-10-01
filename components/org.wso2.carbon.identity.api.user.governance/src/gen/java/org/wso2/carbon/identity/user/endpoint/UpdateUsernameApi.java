package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.UpdateUsernameApiService;
import org.wso2.carbon.identity.user.endpoint.factories.UpdateUsernameApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.UsernameUpdateRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/update-username")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/update-username", description = "the update-username API")
public class UpdateUsernameApi  {

   private final UpdateUsernameApiService delegate = UpdateUsernameApiServiceFactory.getUpdateUsernameApi();

    @PUT
    @Deprecated
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Update username\n", notes = "This API is used to update the username of a User.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized"),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error"),

        @io.swagger.annotations.ApiResponse(code = 410, message = "Gone") })

    public Response updateUsernamePut(@ApiParam(value = "User to be updated." ,required=true ) UsernameUpdateRequestDTO user)
    {
    return Response.status(Response.Status.GONE).build();
    }
}

