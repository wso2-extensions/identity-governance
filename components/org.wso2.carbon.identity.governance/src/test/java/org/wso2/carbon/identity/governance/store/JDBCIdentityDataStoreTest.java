/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance.store;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.CarbonContext;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreServiceImpl;
import org.wso2.carbon.identity.governance.store.Utils.TestUtils;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;

import java.sql.Connection;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.testng.Assert.assertEquals;

public class JDBCIdentityDataStoreTest {

    private static final int TENANT_ID = 3;
    private static final String IDENTITY_DATA_STORE_TYPE = "org.wso2.carbon.identity." +
            "governance.store.JDBCIdentityDataStore";
    private static final String CLAIM_URI = "http://wso2.org/claims/identity/lastLogonTime";
    private static final String CLAIM_VALUE_1 = "1680000000000";
    private static final String CLAIM_VALUE_2 = "1673000000000";
    private static final String NESTED_CLAIM_URI = "http://wso2.org/claims/identity/accountState";
    private static final String NESTED_CLAIM_VALUE = "DISABLED";

    private MockedStatic<IdentityDatabaseUtil> mockedIdentityDatabaseUtils;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<CarbonContext> mockedCarbonContext;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;

    private UserStoreManager userStoreManager;
    IdentityDataStoreService identityDataStoreService;

    @BeforeMethod
    public void setUp() throws Exception {

        TestUtils.initiateH2Base();
        TestUtils.mockDataSource();

        Connection connection = TestUtils.getConnection();
        mockedIdentityDatabaseUtils = Mockito.mockStatic(IdentityDatabaseUtil.class);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.getDBConnection(anyBoolean()))
                .thenReturn(connection);

        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString()))
                .thenReturn(TENANT_ID);

        UserRealm userRealm = mock(UserRealm.class);
        userStoreManager = mock(UserStoreManager.class);

        mockedCarbonContext = Mockito.mockStatic(CarbonContext.class);
        CarbonContext carbonContext = mock(CarbonContext.class);
        mockedCarbonContext.when(CarbonContext::getThreadLocalCarbonContext).thenReturn(carbonContext);
        mockedCarbonContext.when(carbonContext::getUserRealm).thenReturn(userRealm);
        mockedCarbonContext.when(userRealm::getUserStoreManager).thenReturn(userStoreManager);
        mockedCarbonContext.when(() ->
                userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(anyString())).thenReturn
                (IDENTITY_DATA_STORE_TYPE);
        identityDataStoreService = spy(new IdentityDataStoreServiceImpl());
    }

    @AfterMethod
    public void tearDown() throws Exception {

        mockedIdentityDatabaseUtils.close();
        mockedIdentityTenantUtil.close();
        mockedCarbonContext.close();
        mockedIdentityUtil.close();
        TestUtils.closeH2Base();
    }

    @DataProvider
    Object[][] testDataForNestedLessThan() {
        return new Object[][] {
            { true, 3 },
            { false, 2 }
        };
    }

    @DataProvider
    Object[][] testDataForNestedBetween() {
        return new Object[][] {
                { true, 2 },
                { false, 2 }
        };
    }

    @Test(dataProvider = "testDataForNestedLessThan")
    public void testGetUserNamesLessThanClaimWithNestedClaim(boolean isIncluded, int expected) throws Exception {

        List<String> userNames =
                identityDataStoreService.getUserNamesLessThanClaimWithNestedClaim(CLAIM_URI, CLAIM_VALUE_1,
                        NESTED_CLAIM_URI, NESTED_CLAIM_VALUE, TENANT_ID, isIncluded);

        assertEquals(userNames.size(), expected);
    }

    @Test(dataProvider = "testDataForNestedBetween")
    public void testGetUserNamesBetweenGivenClaimsWithNestedClaim(boolean isIncluded, int expected) throws Exception {

        List<String> userNames =
                identityDataStoreService.getUserNamesBetweenGivenClaimsWithNestedClaim(CLAIM_URI, CLAIM_VALUE_2,
                        CLAIM_VALUE_1, NESTED_CLAIM_URI, NESTED_CLAIM_VALUE, TENANT_ID, isIncluded);

        assertEquals(userNames.size(), expected);
    }
}
