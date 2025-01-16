/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.idle.account.identification;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.CarbonContext;

import org.wso2.carbon.identity.idle.account.identification.internal.IdleAccountIdentificationDataHolder;
import org.wso2.carbon.identity.idle.account.identification.models.InactiveUserModel;
import org.wso2.carbon.identity.idle.account.identification.services.impl.IdleAccountIdentificationServiceImpl;
import org.wso2.carbon.identity.idle.account.identification.util.TestUtils;

import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreServiceImpl;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;

import java.sql.Connection;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.testng.Assert.assertEquals;

public class IdleAccountIdentificationServiceImplTest {

    private static final String TENANT_DOMAIN = "DEFAULT";
    private static final int TENANT_ID = 3;
    private static final String SAMPLE_USER_ID = "sampleUserId";
    private static final String IDENTITY_DATA_STORE_TYPE = "org.wso2.carbon.identity." +
            "governance.store.JDBCIdentityDataStore";

    private Connection connection;

    private MockedStatic<IdentityDatabaseUtil> mockedIdentityDatabaseUtils;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<CarbonContext> mockedCarbonContext;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;

    private UserRealm userRealm;
    private UserStoreManager userStoreManager;
    IdentityDataStoreService identityDataStoreService;

    @BeforeMethod
    public void setUp() throws Exception {

        TestUtils.initiateH2Base();
        TestUtils.mockDataSource();

        connection = TestUtils.getConnection();
        mockedIdentityDatabaseUtils = Mockito.mockStatic(IdentityDatabaseUtil.class);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.getDBConnection(anyBoolean()))
                .thenReturn(connection);

        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString()))
                .thenReturn(TENANT_ID);

        userRealm = mock(org.wso2.carbon.user.core.UserRealm.class);
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
        IdleAccountIdentificationDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);
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
    public Object[][] getDates1() {

        return new Object[][]{
                {LocalDate.parse("2023-01-31").atStartOfDay(), 5},
                {LocalDate.parse("2023-01-01").atStartOfDay(), 0}
        };
    }

    @Test(dataProvider = "getDates1")
    public void testGetInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, int expected) throws Exception {

        IdleAccountIdentificationServiceImpl idleAccountIdentificationService =
                spy(IdleAccountIdentificationServiceImpl.class);
        doReturn(SAMPLE_USER_ID).when(idleAccountIdentificationService).fetchUserId(anyString());

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                getInactiveUsersFromSpecificDate(inactiveAfter, TENANT_DOMAIN);

        assertEquals(inactiveUsers.size(), expected);
    }

    @DataProvider
    public Object[][] getDates2() {

        return new Object[][]{
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-15").atStartOfDay(), 3},
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-30").atStartOfDay(), 0}
        };
    }

    @Test(dataProvider = "getDates2")
    public void testGetLimitedInactiveUsersFromSpecificDate(LocalDateTime inactiveAfter, LocalDateTime excludeBefore,
                                                        int expected) throws Exception {

        IdleAccountIdentificationServiceImpl idleAccountIdentificationService =
                spy(IdleAccountIdentificationServiceImpl.class);
        doReturn(SAMPLE_USER_ID).when(idleAccountIdentificationService).fetchUserId(anyString());

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                getLimitedInactiveUsersFromSpecificDate(inactiveAfter, excludeBefore, TENANT_DOMAIN);

        assertEquals(inactiveUsers.size(), expected);
    }

    @DataProvider
    public Object[][] getDatesAndFilter() {

        return new Object[][]{
                {LocalDate.parse("2023-01-31").atStartOfDay(), null, true, 3},
                {LocalDate.parse("2023-01-31").atStartOfDay(), null, false, 2},
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-15").atStartOfDay(), true, 2},
                {LocalDate.parse("2023-01-31").atStartOfDay(), LocalDate.parse("2023-01-15").atStartOfDay(), false, 1}
        };
    }

    @Test(dataProvider = "getDatesAndFilter")
    public void testFilterInactiveUsersIfDisabled(LocalDateTime inactiveAfter, LocalDateTime excludeBefore,
                                                  boolean isDisabled, int expected) throws Exception {

        IdleAccountIdentificationServiceImpl idleAccountIdentificationService =
                spy(IdleAccountIdentificationServiceImpl.class);
        doReturn(SAMPLE_USER_ID).when(idleAccountIdentificationService).fetchUserId(anyString());

        List<InactiveUserModel> inactiveUsers = idleAccountIdentificationService.
                filterInactiveUsersIfDisabled(inactiveAfter, excludeBefore, TENANT_DOMAIN, isDisabled);

        assertEquals(inactiveUsers.size(), expected);
    }
}
