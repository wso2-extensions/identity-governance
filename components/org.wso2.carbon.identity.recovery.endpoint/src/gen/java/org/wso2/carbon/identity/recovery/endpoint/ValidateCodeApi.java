package org.wso2.carbon.identity.recovery.endpoint;

import io.swagger.annotations.ApiParam;
import org.wso2.carbon.identity.recovery.endpoint.dto.CodeValidationRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.ValidateCodeApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Path("/validate-code")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/validate-code", description = "the validate-code API")
public class ValidateCodeApi {

   private final ValidateCodeApiService delegate = ValidateCodeApiServiceFactory.getValidateCodeApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Validate Confirmation Code\n", notes = "This API is used to validate confirmation codes sent in account recovery scenarios and self signup.Need to put confirmation \"code\" and recovery \"step\" as parameters.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Accepted"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response validateCodePost(@ApiParam(value = "code, recovery step and optianal parameters. For recovery steps you can use values from \"UPDATE_PASSWORD\",\"CONFIRM_SIGN_UP\",\"VALIDATE_CHALLENGE_QUESTION\", \"VALIDATE_ALL_CHALLENGE_QUESTION\" four categories according to the recovery scenario you want to validate the code." ,required=true ) CodeValidationRequestDTO codeValidationRequest)
    {
    return delegate.validateCodePost(codeValidationRequest);
    }
}

