/*
 * Copyright (c) 2021, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.multi.attribute.login.resolver.regex;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.login.resolver.regex.RegexLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginResolver;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.user.core.common.AuthenticationResult;

import java.util.List;

/**
 * This class is used to implement MultiAttributeLoginResolver. In this class, users will be resolved using a regex
 * pattern.
 *
 * @deprecated To generalize the resolver concept and make it extensible.
 * Use the {@link org.wso2.carbon.identity.login.resolver.regex.RegexLoginResolver} class instead.
 */
@Deprecated
public class RegexResolver implements MultiAttributeLoginResolver {

    private static final RegexLoginResolver regexLoginResolver = new RegexLoginResolver();
    private static final Log log = LogFactory.getLog(RegexResolver.class);

    @Override
    public ResolvedUserResult resolveUser(String loginAttribute, List<String> allowedAttributes, String tenantDomain) {

        org.wso2.carbon.identity.login.resolver.mgt.ResolvedUserResult newResolvedUserResult = regexLoginResolver.
                resolveUser(loginAttribute, allowedAttributes, tenantDomain);

        ResolvedUserResult.UserResolvedStatus resolvedStatus = ResolvedUserResult.UserResolvedStatus.FAIL;
        if (newResolvedUserResult.getResolvedStatus().equals(
                org.wso2.carbon.identity.login.resolver.mgt.ResolvedUserResult.UserResolvedStatus.SUCCESS)) {
            resolvedStatus = ResolvedUserResult.UserResolvedStatus.SUCCESS;
        }

        ResolvedUserResult oldResolvedUserResult = new ResolvedUserResult(resolvedStatus);
        oldResolvedUserResult.setUser(newResolvedUserResult.getUser());
        oldResolvedUserResult.setResolvedClaim(newResolvedUserResult.getResolvedClaim());
        oldResolvedUserResult.setResolvedValue(newResolvedUserResult.getResolvedValue());
        oldResolvedUserResult.setErrorMessage(newResolvedUserResult.getErrorMessage());

        return oldResolvedUserResult;
    }

    @Override
    public ResolvedUserResult resolveUser(String loginAttribute, List<String> allowedAttributes, String tenantDomain,
                                          String hint) {

        log.warn("User resolver with hint is not yet implemented. Proceeding without the hint.");
        return resolveUser(loginAttribute, allowedAttributes, tenantDomain);
    }

    @Override
    public AuthenticationResult authenticateWithIdentifier(String loginAttributeValue, List<String> allowedAttributes,
                                                           Object credential, String tenantDomain) {

        return regexLoginResolver.authenticateWithIdentifier(loginAttributeValue, allowedAttributes, credential,
                tenantDomain);
    }
}
