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
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.RecoverPasswordApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

/**
 * Recover Password API
 */
@Path("/recover-password")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/recover-password", description = "the recover-password API")
public class RecoverPasswordApi  {

   private final RecoverPasswordApiService delegate = RecoverPasswordApiServiceFactory.getRecoverPasswordApi();

    @POST
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "",
            notes = "This API is used to send password recovery confirmation over defined channels like email/sms\n",
            response = String.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
    public Response recoverPasswordPost(
            @ApiParam(value = "It can be sent optional property parameters over email based on email template." ,
                    required = true) RecoveryInitiatingRequestDTO recoveryInitiatingRequest,
            @ApiParam(value = "Notification Type") @QueryParam("type") String type,
            @ApiParam(value = "If notify = true then, notifications will be internally managed.")
            @QueryParam("notify") Boolean notify) {
        return delegate.recoverPasswordPost(recoveryInitiatingRequest, type, notify);
    }
}

