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
 * Interface for an Auth Attribute Handler.
 * This interface can be implemented by any
 * component that wants to describe authentication
 * attributes that should be provided by
 * users when onboarding.
 */
public interface AuthAttributeHandler {

    /**
     * Provides the identifier of the Auth
     * Attribute Handler.
     *
     * @return Name of the auth attribute handler.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    String getName() throws AuthAttributeHandlerException;

    /**
     * Auth Attribute Handlers can be written independently
     * or be bound to other components such as Authenticators.
     * This method returns the binding type of the Auth
     * Attribute Handler.
     *
     * @return Returns an enum of AuthAttributeHandlerBindingType
     * indicating the binding type.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    AuthAttributeHandlerBindingType getBindingType() throws AuthAttributeHandlerException;

    /**
     * Returns the bound identifier if a binding exists
     * or returns null in case of no binding.
     *
     * @return Bound identifier or null.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    String getBoundIdentifier() throws AuthAttributeHandlerException;

    /**
     * Provides the data of the auth attributes including
     * the auth attributes that needs to be onboarded
     * and metadata about the Auth Attribute Handler
     *
     * @return Returns a AuthAttributeHolder object
     * containing auth attribute data.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException;

    /**
     * Used to perform validations on the received attributes.
     *
     * @param attributeMap A map containing the attributes and values.
     * @return Returns a ValidationResult object.
     * If the validation is successful isValid will be true, along with an
     * empty validationFailureReasons list. If the validation failed isValid
     * will be false and will contain a list of validationFailureReasons
     * pointing to the validation failing auth attributes.
     * @throws AuthAttributeHandlerException authAttributeHandlerException.
     */
    ValidationResult validateAttributes(Map<String, String> attributeMap) throws AuthAttributeHandlerException;
}
