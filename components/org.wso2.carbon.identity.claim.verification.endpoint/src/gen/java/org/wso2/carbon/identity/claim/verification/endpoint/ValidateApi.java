package org.wso2.carbon.identity.claim.verification.endpoint;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.*;
import org.wso2.carbon.identity.claim.verification.endpoint.ValidateApiService;
import org.wso2.carbon.identity.claim.verification.endpoint.factories.ValidateApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.claim.verification.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ValidationResponseDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ValidationRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/validate")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/validate", description = "the validate API")
public class ValidateApi  {

   private final ValidateApiService delegate = ValidateApiServiceFactory.getValidateApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to invoke the available validation mechanism to validate given claim value.\n", response = ValidationResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Ok"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response validatePost(@ApiParam(value = "Request object containing data for the validation process." ,required=true ) ValidationRequestDTO validationRequest)
    {
    return delegate.validatePost(validationRequest);
    }
}

