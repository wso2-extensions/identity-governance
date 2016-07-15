package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.SecurityQuestionsApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.SecurityQuestionsApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateAllQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/security-questions")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/security-questions", description = "the security-questions API")
public class SecurityQuestionsApi  {

   private final SecurityQuestionsApiService delegate = SecurityQuestionsApiServiceFactory.getSecurityQuestionsApi();

    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to initiate password recovery using user challenge questions at once. Response will be a random challenge questions with a confirmation key.\n", response = InitiateAllQuestionResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 204, message = "No content"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response securityQuestionsGet(@ApiParam(value = "username of the user",required=true) @QueryParam("username") String username,
    @ApiParam(value = "`User Store Domain` which user belongs. If not specified, it will be `PRIMARY` domain.\n") @QueryParam("realm") String realm,
    @ApiParam(value = "`Tenant Domain` which user belongs. If not specified, it will be `carbon.super` domain.\n") @QueryParam("tenant-domain") String tenantDomain)
    {
    return delegate.securityQuestionsGet(username,realm,tenantDomain);
    }
}

