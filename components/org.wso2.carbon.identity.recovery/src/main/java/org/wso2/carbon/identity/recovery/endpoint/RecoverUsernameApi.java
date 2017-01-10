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
import org.wso2.carbon.identity.recovery.endpoint.dto.UserClaimDTO;
import org.wso2.carbon.identity.recovery.endpoint.factories.RecoverUsernameApiServiceFactory;

import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

/**
 * Api class for recover username
 */
@Path("/recover-username")
@Consumes({ "application/json" })
@Produces({ "application/json" })
@io.swagger.annotations.Api(value = "/recover-username", description = "the recover-username API")
public class RecoverUsernameApi  {

   private final RecoverUsernameApiService delegate = RecoverUsernameApiServiceFactory.getRecoverUsernameApi();

    @POST
    @Path("/")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "This API can be used to recover forgot username. \n",
            response = void.class)
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 202, message = "Successful response"),
        @io.swagger.annotations.ApiResponse(code = 204, message = "No content"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Bad Request"),
        @io.swagger.annotations.ApiResponse(code = 500, message = "Server Error") })
    public Response recoverUsernamePost(
            @ApiParam(value = "User answers for recovery claims.", required = true) List<UserClaimDTO> claim,
            @ApiParam(value = "Tenant Domain which user belongs. Default `carbon.super`")
            @QueryParam("tenant-domain") String tenantDomain,
            @ApiParam(value = "If notify = true then, notifications will be internally managed.")
            @QueryParam("notify") Boolean notify) {
        return delegate.recoverUsernamePost(claim, tenantDomain, notify);
    }
}

