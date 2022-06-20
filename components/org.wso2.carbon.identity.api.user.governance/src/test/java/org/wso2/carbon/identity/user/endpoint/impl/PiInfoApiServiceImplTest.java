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

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import static org.mockito.ArgumentMatchers.anyString;

public class PiInfoApiServiceImplTest {

    private MockedStatic<Utils> mockedUtils;

    @BeforeMethod
    public void setUp() {

        mockedUtils = Mockito.mockStatic(Utils.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedUtils.close();
    }

    @Test
    public void testGetUserById() throws Exception {

        RealmService realmService = Mockito.mock(RealmService.class);
        TenantManager tenantManager = Mockito.mock(TenantManager.class);
        Mockito.when(tenantManager.getTenantId(anyString())).thenReturn(-1234);
        Mockito.when(realmService.getTenantManager()).thenReturn(tenantManager);

        PiInfoApiServiceImpl piInfoApiService = new PiInfoApiServiceImpl();
        mockedUtils.when(Utils::getRealmService).thenReturn(realmService);
        mockedUtils.when(Utils::getUserInformationService).thenReturn(new MockUserInformationService());

        Assert.assertEquals(piInfoApiService.getUserById("ZHVtbXlVc2Vy").getStatus(), 200);
    }
}