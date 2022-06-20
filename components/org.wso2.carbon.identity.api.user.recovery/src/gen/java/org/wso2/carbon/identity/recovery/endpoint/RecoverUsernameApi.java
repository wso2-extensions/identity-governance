package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.RecoverUsernameApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.RecoverUsernameApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserClaimDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/recover-username")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/recover-username", description = "the recover-username API")
public class RecoverUsernameApi  {

   private final RecoverUsernameApiService delegate = RecoverUsernameApiServiceFactory.getRecoverUsernameApi();

    @POST
    @Path("/")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API can be used to recover forgot username. \n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 204, message = "No content"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response recoverUsernamePost(@ApiParam(value = "User answers for recovery claims." ,required=true ) List<UserClaimDTO> claim,
    @ApiParam(value = "Tenant Domain which user belongs. Default `carbon.super`") @QueryParam("tenant-domain") String tenantDomain,
    @ApiParam(value = "If notify=true then, notifications will be internally managed.") @QueryParam("notify") Boolean notify)
    {
    return delegate.recoverUsernamePost(claim,tenantDomain,notify);
    }
}

