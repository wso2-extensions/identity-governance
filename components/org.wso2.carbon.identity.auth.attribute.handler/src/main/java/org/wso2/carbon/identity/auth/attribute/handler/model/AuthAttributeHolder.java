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

import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerBindingType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Model class used to represent an auth attribute handler
 * and contained the auth attributes.
 */
public class AuthAttributeHolder {

    private String handlerName;
    private AuthAttributeHandlerBindingType handlerBinding;
    private String handlerBoundIdentifier;
    private List<AuthAttribute> authAttributes = new ArrayList<>();
    private Map<String, String> properties = new HashMap<>();

    public AuthAttributeHolder() {

    }

    public AuthAttributeHolder(String handlerName, AuthAttributeHandlerBindingType handlerBinding,
                               String handlerBoundIdentifier, List<AuthAttribute> authAttributes) {

        this.handlerName = handlerName;
        this.handlerBinding = handlerBinding;
        this.handlerBoundIdentifier = handlerBoundIdentifier;
        this.authAttributes = authAttributes;
    }

    public String getHandlerName() {

        return handlerName;
    }

    public void setHandlerName(String handlerName) {

        this.handlerName = handlerName;
    }

    public AuthAttributeHandlerBindingType getHandlerBinding() {

        return handlerBinding;
    }

    public void setHandlerBinding(AuthAttributeHandlerBindingType handlerBinding) {

        this.handlerBinding = handlerBinding;
    }

    public String getHandlerBoundIdentifier() {

        return handlerBoundIdentifier;
    }

    public void setHandlerBoundIdentifier(String handlerBoundIdentifier) {

        this.handlerBoundIdentifier = handlerBoundIdentifier;
    }

    public List<AuthAttribute> getAuthAttributes() {

        return authAttributes;
    }

    public void setAuthAttributes(List<AuthAttribute> authAttributes) {

        this.authAttributes = authAttributes;
    }

    public Map<String, String> getProperties() {

        return properties;
    }

    public void setProperties(Map<String, String> properties) {

        this.properties = properties;
    }
}
