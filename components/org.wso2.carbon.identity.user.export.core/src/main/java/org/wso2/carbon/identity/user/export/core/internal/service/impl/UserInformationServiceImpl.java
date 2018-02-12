/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * {@inheritDoc}
 */
@Component(
        name = "org.wso2.carbon.user.export.service",
        immediate = true
)
public class UserInformationServiceImpl implements UserInformationService {

    private List<UserInformationProvider> userInformationProviders = new ArrayList<>();

    @Override
    public Map<String, Object> getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        Map<String, Object> userInformation = new HashMap<>();
        for (UserInformationProvider userInformationProvider : userInformationProviders) {
            if (userInformationProvider.isEnabled()) {
                UserInformationDTO retainedUserInformation = userInformationProvider.getRetainedUserInformation
                        (username, userStoreDomain, tenantId);
                if (retainedUserInformation != null && retainedUserInformation.isInformationAvailable()) {
                    String type = userInformationProvider.getType();
                    userInformation.put(type, retainedUserInformation.getData());
                }
            }
        }
        return userInformation;
    }

    @Reference(
            name = "user.export.attribute.provider",
            service = UserInformationProvider.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetUserAttributeProvider"
    )
    public void setUserAttributeProvider(UserInformationProvider userInformationProvider) {
        userInformationProviders.add(userInformationProvider);
    }

    public void unsetUserAttributeProvider(UserInformationProvider userInformationProvider) {
        userInformationProviders.remove(userInformationProvider);
    }
}
