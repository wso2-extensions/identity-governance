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

import org.testng.Assert;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.identity.user.export.core.UserExportException;

import java.util.Map;

import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;

public class UserInformationServiceImplTest {

    @Test
    public void testGetRetainedUserInformation() throws Exception {

        UserInformationServiceImpl userInformationService = new UserInformationServiceImpl();
        userInformationService.setUserAttributeProvider(new MockUserInformationProvider());
        Map<String, Object> retainedUserInformation = userInformationService.getRetainedUserInformation
                ("admin", "PRIMARY", -1234);

        Object basicUserInformation = retainedUserInformation.get("basic");
        if (basicUserInformation instanceof Map) {
            Map basicUserInformationMap = (Map) basicUserInformation;
            if (basicUserInformationMap.size() != 3) {
                Assert.fail();
            }
        }
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }
}
