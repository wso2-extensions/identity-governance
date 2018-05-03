/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.user.endpoint.impl;

import org.wso2.carbon.identity.user.endpoint.PiInfoApiService;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import javax.ws.rs.core.Response;

/**
 * Personal information service implementation.
 */
public class PiInfoApiServiceImpl extends PiInfoApiService {

    @Override
    public Response getUserById(String userId) {

        String usernameFromRequest = new String(Base64.getUrlDecoder().decode(userId.getBytes(StandardCharsets.UTF_8)),
                StandardCharsets.UTF_8);

        String userStoreDomain = UserCoreUtil.extractDomainFromName(usernameFromRequest);
        String username = UserCoreUtil.removeDomainFromName(usernameFromRequest);
        String tenantDomain = MultitenantUtils.getTenantDomain(username);
        username = MultitenantUtils.getTenantAwareUsername(usernameFromRequest);
        int tenantId;
        try {
            tenantId = Utils.getRealmService().getTenantManager().getTenantId(tenantDomain);
        } catch (UserStoreException e) {
            return Response
                    .status(Response.Status.BAD_REQUEST)
                    .entity("Invalid tenant domain provided in username.")
                    .build();
        } catch (UserExportException e) {
            return Response
                    .status(Response.Status.INTERNAL_SERVER_ERROR)
                    .entity(e.getMessage())
                    .build();
        }
        Map userAttributes;
        try {
            userAttributes = Utils.getUserInformationService().getRetainedUserInformation(username, userStoreDomain,
                    tenantId);
        } catch (UserExportException e) {
            return Response.serverError().entity(e.getMessage()).build();
        }
        return Response.ok().status(Response.Status.OK).entity(userAttributes).build();
    }

    @Override
    public Response searchUserByName(String username) {

        return Response
                .status(Response.Status.NOT_IMPLEMENTED)
                .build();
    }
}
