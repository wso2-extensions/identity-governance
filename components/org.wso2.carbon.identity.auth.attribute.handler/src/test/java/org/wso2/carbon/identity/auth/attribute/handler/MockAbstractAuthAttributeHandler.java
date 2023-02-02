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

package org.wso2.carbon.identity.auth.attribute.handler;

import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.model.AuthAttributeHolder;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;

import java.util.Map;

/**
 * A mock class added as an abstract class that can be used in the unit test.
 */
public abstract class MockAbstractAuthAttributeHandler implements AuthAttributeHandler {

    @Override
    public String getName() throws AuthAttributeHandlerException {

        return null;
    }

    @Override
    public AuthAttributeHandlerBindingType getBindingType() throws AuthAttributeHandlerException {

        return AuthAttributeHandlerBindingType.AUTHENTICATOR;
    }

    @Override
    public String getBoundIdentifier() throws AuthAttributeHandlerException {

        return null;
    }

    @Override
    public AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException {

        return null;
    }

    @Override
    public ValidationResult validateAttributes(Map<String, String> attributeMap) throws AuthAttributeHandlerException {

        return null;
    }
}
