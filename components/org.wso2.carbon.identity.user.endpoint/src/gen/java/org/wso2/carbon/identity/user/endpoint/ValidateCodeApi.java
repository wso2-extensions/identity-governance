package org.wso2.carbon.identity.user.endpoint;

import org.wso2.carbon.identity.user.endpoint.dto.*;
import org.wso2.carbon.identity.user.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.user.endpoint.factories.ValidateCodeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.user.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.user.endpoint.dto.CodeValidationRequestDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/validate-code")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/validate-code", description = "the validate-code API")
public class ValidateCodeApi  {

   private final ValidateCodeApiService delegate = ValidateCodeApiServiceFactory.getValidateCodeApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Validate Code\n", notes = "This API is used to validate code of self registered users\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Accepted"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response validateCodePost(@ApiParam(value = "Code retried after user self registration and optional property parameters" ,required=true ) CodeValidationRequestDTO code)
    {
    return delegate.validateCodePost(code);
    }
}

