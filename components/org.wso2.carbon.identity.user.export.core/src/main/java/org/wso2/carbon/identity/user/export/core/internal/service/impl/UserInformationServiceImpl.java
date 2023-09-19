/*
 * Copyright (c) 2018, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.export.core.service.UserInformationProvider;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;

import java.util.HashMap;
import java.util.Map;

/**
 * {@inheritDoc}
 */
public class UserInformationServiceImpl implements UserInformationService {


    @Override
    public Map<String, Object> getRetainedUserInformation(String username, String userStoreDomain, int tenantId)
            throws UserExportException {

        Map<String, Object> userInformation = new HashMap<>();
        for (UserInformationProvider userInformationProvider :
                UserProfileExportDataHolder.getUserInformationProviders()) {
            if (userInformationProvider.isEnabled()) {
                UserInformationDTO retainedUserInformation =
                        userInformationProvider.getRetainedUserInformation(username, userStoreDomain, tenantId);
                if (retainedUserInformation != null && retainedUserInformation.isInformationAvailable()) {
                    String type = userInformationProvider.getType();
                    userInformation.put(type, retainedUserInformation.getData());
                }
            }
        }
        return userInformation;
    }
}
