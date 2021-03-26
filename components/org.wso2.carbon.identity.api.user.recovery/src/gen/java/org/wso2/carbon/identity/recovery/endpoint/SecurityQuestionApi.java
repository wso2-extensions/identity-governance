package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.SecurityQuestionApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.SecurityQuestionApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/security-question")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/security-question", description = "the security-question API")
public class SecurityQuestionApi  {

   private final SecurityQuestionApiService delegate = SecurityQuestionApiServiceFactory.getSecurityQuestionApi();

    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to initiate password recovery using user challenge questions. Response will be a random challenge question with a confirmation key.\n", response = InitiateQuestionResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 204, message = "No content"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response securityQuestionGet(@ApiParam(value = "username of the user",required=true) @QueryParam("username") String username,
    @ApiParam(value = "`User Store Domain` which user belongs. If not specified, it will be `PRIMARY` domain.\n") @QueryParam("realm") String realm,
    @ApiParam(value = "`Tenant Domain` which user belongs. If not specified, it will be `carbon.super` domain.\n") @QueryParam("tenant-domain") String tenantDomain)
    {
    return delegate.securityQuestionGet(username,realm,tenantDomain);
    }
}

