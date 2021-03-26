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

package org.wso2.carbon.identity.user.endpoint.impl;

import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.service.UserInformationService;

import java.util.HashMap;
import java.util.Map;

public class MockUserInformationService implements UserInformationService {

    @Override
    public Map<String, Object> getRetainedUserInformation(String username, String userStoreDomain, int tenantId) throws UserExportException {

        Map<String, String> attributes = new HashMap<>();
        attributes.put("http://wso2.org/claims/username", "username1");
        attributes.put("http://wso2.org/claims/givenname", "lastname1");
        attributes.put("http://wso2.org/claims/lastname", "lastname1");

        UserInformationDTO basicUserInformationDTO = new UserInformationDTO(attributes);

        Map<String, Object> info = new HashMap<>();
        info.put("basic", basicUserInformationDTO.getData());
        return info;
    }
}
