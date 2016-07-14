package org.wso2.carbon.identity.recovery.endpoint;

import org.wso2.carbon.identity.recovery.endpoint.dto.*;
import org.wso2.carbon.identity.recovery.endpoint.ValidateAnswerApiService;
import org.wso2.carbon.identity.recovery.endpoint.factories.ValidateAnswerApiServiceFactory;

import io.swagger.annotations.ApiParam;

import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.ErrorDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.AnswerVerificationRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RetryErrorDTO;

import java.util.List;

import java.io.InputStream;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/validate-answer")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/validate-answer", description = "the validate-answer API")
public class ValidateAnswerApi  {

   private final ValidateAnswerApiService delegate = ValidateAnswerApiServiceFactory.getValidateAnswerApi();

    @POST
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This is used to validate user challenge answer. If user challenge answer is valid, it will send another challenge question to answer until the status become `COMPLETE`. If the answer is wrong, user can retry the answer.\n", response = InitiateQuestionResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })

    public Response validateAnswerPost(@ApiParam(value = "User answers verification with key returned in privious step." ,required=true ) AnswerVerificationRequestDTO answerVerificationRequest)
    {
    return delegate.validateAnswerPost(answerVerificationRequest);
    }
}

