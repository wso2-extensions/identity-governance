/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.endpoint;

import io.swagger.annotations.ApiParam;
import org.wso2.carbon.identity.claim.verification.endpoint.dto.ConfirmationRequestDTO;
import org.wso2.carbon.identity.claim.verification.endpoint.factories.ConfirmApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Path("/confirm")
@Consumes({"application/json"})
@Produces({"application/json"})
@io.swagger.annotations.Api(value = "/confirm", description = "the confirm API")
public class ConfirmApi {

    private final ConfirmApiService delegate = ConfirmApiServiceFactory.getConfirmApi();

    @POST

    @Consumes({"application/json"})
    @Produces({"application/json"})
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API is used to confirm the new claim value after " +
            "validating it.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = {
            @io.swagger.annotations.ApiResponse(code = 200, message = "Ok"),
            @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
            @io.swagger.annotations.ApiResponse(code = 401, message = "Unauthorized"),
            @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error")})
    public Response confirmPost(@ApiParam(value = "Request object to finalize the process which states whether the " +
            "verification was successful or not.", required = true) ConfirmationRequestDTO confirmationRequest) {

        return delegate.confirmPost(confirmationRequest);
    }
}

