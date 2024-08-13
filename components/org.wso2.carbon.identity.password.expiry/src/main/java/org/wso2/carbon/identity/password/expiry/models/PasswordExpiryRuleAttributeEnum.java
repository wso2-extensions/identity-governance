/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.expiry.models;

/**
 * Enum for the password expiry attribute types.
 */
public enum PasswordExpiryRuleAttributeEnum {

    ROLES("roles"),
    GROUPS("groups");

    private final String value;

    PasswordExpiryRuleAttributeEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static PasswordExpiryRuleAttributeEnum fromString(String text) {
        for (PasswordExpiryRuleAttributeEnum attribute : PasswordExpiryRuleAttributeEnum.values()) {
            if (attribute.value.equalsIgnoreCase(text)) {
                return attribute;
            }
        }
        throw new IllegalArgumentException("No enum constant with text " + text + " found");
    }
}
