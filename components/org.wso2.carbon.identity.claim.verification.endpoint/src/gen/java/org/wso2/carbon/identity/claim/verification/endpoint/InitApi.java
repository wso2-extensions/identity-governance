package org.wso2.carbon.identity.claim.verification.endpoint;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.*;
import org.wso2.carbon.identity.claim.verification.endpoint.InitApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.factories.InitApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.VerificationInitiatingRequestDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.VerificationInitiatingResponseDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/init")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/init", description = "the init API")
public class InitApi  {

   private final InitApiService delegate = InitApiServiceFactory.getInitApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to initiate the claim verification process. It will notify the relevant parties that a claim verification has been requested.\n", response = VerificationInitiatingResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Success response along with the confirmation code to invoke next request."),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response initPost(@ApiParam(value = "Request object for initiating the claim verification process." ,required=true ) VerificationInitiatingRequestDTO verificationInitiatingRequest)
    {
    return delegate.initPost(verificationInitiatingRequest);
    }
}

