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

import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;

import java.util.Map;

public class UserInformationServiceImplTest {

    @Test
    public void testGetRetainedUserInformation() throws Exception {

        UserProfileExportDataHolder.getUserInformationProviders().add(new MockUserInformationProvider());
        UserInformationServiceImpl userInformationService = new UserInformationServiceImpl();
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
}
