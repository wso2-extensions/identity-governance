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

import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.Assert;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;

@PrepareForTest({Utils.class})
public class PiInfoApiServiceImplTest extends PowerMockTestCase {

    @Test
    public void testGetUserById() throws Exception {

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(tenantManager.getTenantId(anyString())).thenReturn(-1234);
        when(realmService.getTenantManager()).thenReturn(tenantManager);

        PiInfoApiServiceImpl piInfoApiService = new PiInfoApiServiceImpl();
        mockStatic(Utils.class);
        when(Utils.getRealmService()).thenReturn(realmService);
        when(Utils.getUserInformationService()).thenReturn(new MockUserInformationService());

        Assert.assertEquals(piInfoApiService.getUserById("ZHVtbXlVc2Vy").getStatus(), 200);
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }
}