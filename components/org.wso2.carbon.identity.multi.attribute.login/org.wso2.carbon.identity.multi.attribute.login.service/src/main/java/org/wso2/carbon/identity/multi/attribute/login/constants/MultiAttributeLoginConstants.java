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

package org.wso2.carbon.identity.multi.attribute.login.constants;

/**
 * Multi attribute login related constants.
 */
public class MultiAttributeLoginConstants {

    public static final String USERNAME_CLAIM_URI = "http://wso2.org/claims/username";

    public static final String MULTI_ATTRIBUTE_LOGIN_PROPERTY = "account.multiattributelogin.handler.enable";
    public static final String ALLOWED_LOGIN_ATTRIBUTES = "account.multiattributelogin.handler.allowedattributes";

    public static final String HANDLER_NAME = "multiattribute.login.handler";
    public static final String HANDLER_FRIENDLY_NAME = "Multi Attribute Login";
    public static final String HANDLER_CATEGORY = "Account Management";
    public static final String HANDLER_SUB_CATEGORY = "DEFAULT";

    public static final class MultiAttributeLoginNameMapping {

        public static final String MULTI_ATTRIBUTE_LOGIN_PROPERTY_NAME_MAPPING = "Enable Multi Attribute Login";
        public static final String ALLOWED_LOGIN_ATTRIBUTES_NAME_MAPPING = "Allowed Attribute Claim List";
    }

    public static final class MultiAttributeLoginDescriptionMapping {
        public static final String MULTI_ATTRIBUTE_LOGIN_PROPERTY_DESCRIPTION_MAPPING = "Enable using multiple " +
                "attributes as login identifier";
        public static final String ALLOWED_LOGIN_ATTRIBUTES_DESCRIPTION_MAPPING = "Allowed claim list separated by commas";

    }

}
