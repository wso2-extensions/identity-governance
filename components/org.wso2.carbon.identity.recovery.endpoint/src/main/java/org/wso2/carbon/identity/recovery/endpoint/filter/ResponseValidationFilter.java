/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.endpoint.filter;

import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.captcha.util.EnabledSecurityMechanism;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.util.Map;

/**
 * ResponseValidationFilter.
 */
public class ResponseValidationFilter implements ContainerResponseFilter {

    @Context
    private HttpServletRequest httpRequest;

    @Override
    public void filter(ContainerRequestContext containerRequestContext,
                       ContainerResponseContext containerResponseContext) throws IOException {

        try {
            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(MultitenantConstants.SUPER_TENANT_ID);
            if (containerResponseContext.getStatusInfo().getFamily() == Response.Status.Family.CLIENT_ERROR && httpRequest
                    .getSession().getAttribute("enabled-security-mechanism") != null) {

                EnabledSecurityMechanism enabledSecurityMechanism = (EnabledSecurityMechanism) httpRequest
                        .getSession().getAttribute("enabled-security-mechanism");
                containerResponseContext.getHeaders().add(enabledSecurityMechanism.getMechanism(), "true");
                if (!enabledSecurityMechanism.getProperties().isEmpty()) {
                    for (Map.Entry<String, String> entry : enabledSecurityMechanism.getProperties().entrySet()) {
                        containerResponseContext.getHeaders().add(entry.getKey(), entry.getValue());
                    }
                }
            }
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }

    }
}
