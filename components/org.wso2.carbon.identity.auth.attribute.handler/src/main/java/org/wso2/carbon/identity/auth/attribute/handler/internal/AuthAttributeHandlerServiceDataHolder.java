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

package org.wso2.carbon.identity.auth.attribute.handler.internal;

import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandler;

import java.util.ArrayList;
import java.util.List;

/**
 * This class holds OSGI services required by the Auth Attribute Handler component.
 */
public class AuthAttributeHandlerServiceDataHolder {

    private static final AuthAttributeHandlerServiceDataHolder instance = new AuthAttributeHandlerServiceDataHolder();

    private final List<AuthAttributeHandler> authAttributeHandlers = new ArrayList<>();
    private ApplicationManagementService applicationManagementService;

    public static AuthAttributeHandlerServiceDataHolder getInstance() {

        return instance;
    }

    public List<AuthAttributeHandler> getAuthAttributeHandlers() {

        return authAttributeHandlers;
    }

    public ApplicationManagementService getApplicationManagementService() {

        return this.applicationManagementService;
    }

    public void setApplicationManagementService(ApplicationManagementService applicationManagementService) {

        this.applicationManagementService = applicationManagementService;
    }
}
