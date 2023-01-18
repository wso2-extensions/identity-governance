/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.login.resolver.service.constants;

/**
 * Constants that are used within the login resolver service module.
 */
public class LoginResolverServiceConstants {

    public static final String USERNAME_CLAIM_URI = "http://wso2.org/claims/username";
    public static final String DEFAULT_LOGIN_RESOLVER_CLASS =
            "org.wso2.carbon.identity.login.resolver.regex.RegexLoginResolver";

    public static final String LOGIN_RESOLVER_PROPERTY = "account.login.resolver.handler.enable";
    public static final String LOGIN_RESOLVER_CLASS = "account.login.resolver.handler.class";
    public static final String ALLOWED_LOGIN_ATTRIBUTES = "account.login.resolver.handler.allowed.attributes";

    public static final String HANDLER_NAME = "login.resolver.handler";
    public static final String HANDLER_FRIENDLY_NAME = "Login Resolver";
    public static final String HANDLER_CATEGORY = "Account Management";
    public static final String HANDLER_SUB_CATEGORY = "DEFAULT";

    public static final class LoginResolverServiceNameMapping {

        public static final String LOGIN_RESOLVER_PROPERTY_NAME_MAPPING = "Enable Login Resolver";
        public static final String LOGIN_RESOLVER_CLASS_NAME_MAPPING = "Resolver Class";
        public static final String ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING = "Allowed Attribute Claim List";
    }

    public static final class LoginResolverServiceDescriptionMapping {
        public static final String LOGIN_RESOLVER_PROPERTY_DESCRIPTION_MAPPING = "Enable using the login resolver to " +
                "extract the user using a specific methodology.";
        public static final String LOGIN_RESOLVER_CLASS_DESCRIPTION_MAPPING = "The class which defines the login " +
                "resolver methodology. Uses the multi attribute login resolver class by default.";
        public static final String ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING = "Allowed claim list for the login " +
                "resolver separated by commas.";

    }
}
