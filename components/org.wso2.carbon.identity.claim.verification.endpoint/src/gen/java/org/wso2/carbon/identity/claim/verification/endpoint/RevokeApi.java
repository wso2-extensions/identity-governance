package org.wso2.carbon.identity.claim.verification.endpoint;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.*;
import org.wso2.carbon.identity.claim.verification.endpoint.RevokeApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.factories.RevokeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.RevocationRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/revoke")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/revoke", description = "the revoke API")
public class RevokeApi  {

   private final RevokeApiService delegate = RevokeApiServiceFactory.getRevokeApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to terminate the verification process.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Ok"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response revokePost(@ApiParam(value = "Request object to terminate the verification process." ,required=true ) RevocationRequestDTO revocationRequest)
    {
    return delegate.revokePost(revocationRequest);
    }
}

