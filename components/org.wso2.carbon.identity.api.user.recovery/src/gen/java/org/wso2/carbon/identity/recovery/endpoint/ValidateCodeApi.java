package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.ValidateCodeApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.ValidateCodeApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.CodeValidationRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;

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
    @io.swagger.annotations.ApiOperation(value = "Validate Confirmation Code\n", notes = "This API is used to validate confirmation codes sent in account recovery scenarios and self signup. You need to input the confirmation \"code\" and recovery \"step\" as parameters.\n", response = UserDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Accepted"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response validateCodePost(@ApiParam(value = "Code, recovery step, and optional parameters. For recovery steps, you can use values from \"UPDATE_PASSWORD\",\"CONFIRM_SIGN_UP\",\"VALIDATE_CHALLENGE_QUESTION\", and \"VALIDATE_ALL_CHALLENGE_QUESTION\" four categories according to the recovery scenario you want to validate the code." ,required=true ) CodeValidationRequestDTO codeValidationRequest)
    {
    return delegate.validateCodePost(codeValidationRequest);
    }
}

