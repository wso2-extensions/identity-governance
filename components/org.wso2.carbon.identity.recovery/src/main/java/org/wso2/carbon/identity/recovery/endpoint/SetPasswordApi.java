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
import org.wso2.carbon.identity.recovery.endpoint.dto.ResetPasswordRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.SetPasswordApiServiceFactory;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

/**
 * Set password api
 */
@Path("/set-password")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/set-password", description = "the set-password API")
public class SetPasswordApi  {

   private final SetPasswordApiService delegate = SetPasswordApiServiceFactory.getSetPasswordApi();

    @POST
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "",
            notes = "This API will be used to reset user password using the confirmatin key recieved through " +
                    "recovery process. Need to input `key`  and the new `password`.\n", response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        @io.swagger.annotations.ApiResponse(code = 412, message = "Precondition Falied"),
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
    public Response setPasswordPost(
            @ApiParam(value = "key, password and optional metadata properties" ,
                    required = true) ResetPasswordRequestDTO resetPasswordRequest) {
        return delegate.setPasswordPost(resetPasswordRequest);
    }
}

