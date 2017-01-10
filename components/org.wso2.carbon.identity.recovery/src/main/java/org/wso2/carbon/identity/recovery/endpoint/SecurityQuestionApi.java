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
import org.wso2.carbon.identity.recovery.endpoint.dto.InitiateQuestionResponseDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.SecurityQuestionApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

/**
 * Security question api
 */
@Path("/security-question")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/security-question", description = "the security-question API")
public class SecurityQuestionApi  {

   private final SecurityQuestionApiService delegate = SecurityQuestionApiServiceFactory.getSecurityQuestionApi();

    @GET
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "",
            notes = "This API is used to initiate password recovery using user challenge questions. " +
                    "Response will be a random challenge question with a confirmation key.\n",
            response = InitiateQuestionResponseDTO.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful response"),
        @io.swagger.annotations.ApiResponse(code = 204, message = "No content"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
    public Response securityQuestionGet(@ApiParam(value = "username of the user", required = true)
                                            @QueryParam("username") String username,
    @ApiParam(value = "`User Store Domain` which user belongs. If not specified, it will be `PRIMARY` domain.\n")
                                            @QueryParam("realm") String realm,
    @ApiParam(value = "`Tenant Domain` which user belongs. If not specified, it will be `carbon.super` domain.\n")
                                            @QueryParam("tenant-domain") String tenantDomain) {
         return delegate.securityQuestionGet(username, realm, tenantDomain);
    }
}

