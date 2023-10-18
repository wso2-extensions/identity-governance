/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.auth.attribute.handler.model;

import java.util.HashMap;
import java.util.Map;

/**
 * Model class that represent an auth attribute.
 */
public class AuthAttribute {

    private String attribute;
    private boolean isClaim;
    // Used to indicate if the attribute should be handled as a sensitive attribute.
    private boolean isConfidential;
    // Used to indicate the type of the application which can be in processing or UI representation.
    private AuthAttributeType type;
    private Map<String, String> properties = new HashMap<>();

    public AuthAttribute() {

    }

    public AuthAttribute(String attribute, boolean isClaim, boolean isConfidential,
                         AuthAttributeType type) {

        this.attribute = attribute;
        this.isClaim = isClaim;
        this.isConfidential = isConfidential;
        this.type = type;
    }

    public String getAttribute() {

        return attribute;
    }

    public void setAttribute(String attribute) {

        this.attribute = attribute;
    }

    public boolean isClaim() {

        return isClaim;
    }

    public void setIsClaim(boolean claim) {

        isClaim = claim;
    }

    public boolean isConfidential() {

        return isConfidential;
    }

    public void setIsConfidential(boolean credential) {

        isConfidential = credential;
    }

    public AuthAttributeType getType() {

        return type;
    }

    public void setType(AuthAttributeType type) {

        this.type = type;
    }

    public Map<String, String> getProperties() {

        return properties;
    }

    public void setProperties(Map<String, String> properties) {

        this.properties = properties;
    }
}
