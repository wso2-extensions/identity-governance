/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.recovery.endpoint;

import io.swagger.annotations.ApiParam;
import org.wso2.carbon.identity.recovery.endpoint.dto.AnswerVerificationRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.ValidateAnswerApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

/**
 * Validatation answer api
 */
@Path("/validate-answer")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/validate-answer", description = "the validate-answer API")
public class ValidateAnswerApi  {

   private final ValidateAnswerApiService delegate = ValidateAnswerApiServiceFactory.getValidateAnswerApi();

    @POST
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "",
            notes = "This is used to validate user challenge answer. If user challenge answer is valid, " +
                    "it will send another challenge question to answer until the status become `COMPLETE`. If the " +
                    " answer is wrong, user can retry the answer.\n", response = InitiateQuestionResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
    public Response validateAnswerPost(
            @ApiParam(value = "User answers verification with key returned in privious step.",
                    required = true) AnswerVerificationRequestDTO answerVerificationRequest) {
        return delegate.validateAnswerPost(answerVerificationRequest);
    }
}

