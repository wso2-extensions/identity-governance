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
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
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
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.model.ExpressionOperation;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.dbcreator.DatabaseCreator;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

public class JDBCIdentityDataStoreTest {

    private static final int TENANT_ID = 3;
    private static final String IDENTITY_DATA_STORE_TYPE = "org.wso2.carbon.identity." +
            "governance.store.JDBCIdentityDataStore";
    private static final String CLAIM_URI = "http://wso2.org/claims/identity/lastLogonTime";
    private static final String CLAIM_VALUE_1 = "1680000000000";
    private static final String CLAIM_VALUE_2 = "1673000000000";
    private static final String ACCOUNT_STATE_CLAIM_URI = "http://wso2.org/claims/identity/accountState";
    private static final String ACCOUNT_STATE_CLAIM_VALUE = "DISABLED";
    private static final String EMAIL_VERIFIED_CLAIM = "http://wso2.org/claims/identity/emailVerified";

    private MockedStatic<IdentityDatabaseUtil> mockedIdentityDatabaseUtils;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<CarbonContext> mockedCarbonContext;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<UserCoreUtil> mockedUserCoreUtil;
    private MockedStatic<DatabaseCreator> mockedDatabaseCreator;

    private UserStoreManager userStoreManager;
    IdentityDataStoreService identityDataStoreService;
    private JDBCIdentityDataStore jdbcIdentityDataStore;
    private RealmConfiguration realmConfiguration;

    @BeforeClass
    public static void setUpClass() throws Exception {

        TestUtils.initiateH2Base();
        TestUtils.mockDataSource();
    }

    @BeforeMethod
    public void setUp() throws Exception {

        setupIdentityDatabaseUtilMocks();
        setupDatabaseCreatorMocks();
        setupIdentityTenantUtilMocks();
        setupIdentityUtilMocks();
        setupCarbonContextMocks();
        setupUserCoreUtilMocks();

        identityDataStoreService = spy(new IdentityDataStoreServiceImpl());
        jdbcIdentityDataStore = new JDBCIdentityDataStore();
    }

    private void setupIdentityDatabaseUtilMocks() {

        mockedIdentityDatabaseUtils = Mockito.mockStatic(IdentityDatabaseUtil.class);

        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.getDBConnection(anyBoolean()))
                .thenAnswer(invocation -> TestUtils.getConnection());
        mockedIdentityDatabaseUtils.when(IdentityDatabaseUtil::getDBConnection)
                .thenAnswer(invocation -> TestUtils.getConnection());
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.commitTransaction(any(Connection.class)))
                .thenAnswer(invocation -> null);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.rollbackTransaction(any(Connection.class)))
                .thenAnswer(invocation -> null);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.closeConnection(any(Connection.class)))
                .thenAnswer(invocation -> null);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.closeStatement(any()))
                .thenAnswer(invocation -> null);
        mockedIdentityDatabaseUtils.when(() -> IdentityDatabaseUtil.closeResultSet(any()))
                .thenAnswer(invocation -> null);
    }

    private void setupIdentityTenantUtilMocks() {

        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString()))
                .thenReturn(TENANT_ID);
    }

    private void setupCarbonContextMocks() {

        UserRealm userRealm = mock(UserRealm.class);
        userStoreManager = mock(UserStoreManager.class);

        mockedCarbonContext = Mockito.mockStatic(CarbonContext.class);
        CarbonContext carbonContext = mock(CarbonContext.class);
        mockedCarbonContext.when(CarbonContext::getThreadLocalCarbonContext).thenReturn(carbonContext);
        mockedCarbonContext.when(carbonContext::getUserRealm).thenReturn(userRealm);
        mockedCarbonContext.when(userRealm::getUserStoreManager).thenReturn(userStoreManager);
        mockedCarbonContext.when(() ->
                userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(userStoreManager);
    }

    private void setupIdentityUtilMocks() {

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(anyString())).thenReturn
                (IDENTITY_DATA_STORE_TYPE);
    }

    private void setupUserCoreUtilMocks() throws UserStoreException {

        realmConfiguration = mock(RealmConfiguration.class);
        mockedUserCoreUtil = Mockito.mockStatic(UserCoreUtil.class);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(userStoreManager.getTenantId()).thenReturn(3);
        mockedUserCoreUtil
                .when(() -> UserCoreUtil.getDomainName(realmConfiguration))
                .thenReturn("DEFAULT");
    }

    private void setupDatabaseCreatorMocks() {

        mockedDatabaseCreator = Mockito.mockStatic(DatabaseCreator.class);
        mockedDatabaseCreator.when(() -> DatabaseCreator.getDatabaseType(any(Connection.class)))
                .thenReturn("h2");
    }

    @AfterMethod
    public void tearDown() throws Exception {

        mockedIdentityDatabaseUtils.close();
        mockedIdentityTenantUtil.close();
        mockedCarbonContext.close();
        mockedIdentityUtil.close();
        mockedUserCoreUtil.close();
        mockedDatabaseCreator.close();
    }

    @AfterClass
    public static void tearDownClass() throws Exception {

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
                        ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE, TENANT_ID, isIncluded);

        assertEquals(userNames.size(), expected);
    }

    @Test(dataProvider = "testDataForNestedBetween")
    public void testGetUserNamesBetweenGivenClaimsWithNestedClaim(boolean isIncluded, int expected) throws Exception {

        List<String> userNames =
                identityDataStoreService.getUserNamesBetweenGivenClaimsWithNestedClaim(CLAIM_URI, CLAIM_VALUE_2,
                        CLAIM_VALUE_1, ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE, TENANT_ID, isIncluded);

        assertEquals(userNames.size(), expected);
    }

    @DataProvider
    Object[][] testDataForListUsersNamesWithNE() {
        return new Object[][]{
                {EMAIL_VERIFIED_CLAIM, "true", 4},
                {ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE, 2}
        };
    }

    @Test(description = "Test getUserNamesByClaimURINotEqualValue which retrieves usernames where the claim URI " +
            "value is not equal to the given value.", dataProvider = "testDataForListUsersNamesWithNE")
    public void testGetUserNamesByClaimURINotEqualValue(String claimUri, String claimValue, int expected)
            throws Exception {

        List<String> userNames = jdbcIdentityDataStore
                .getUserNamesByClaimURINotEqualValue(claimUri, claimValue, userStoreManager);

        assertEquals(userNames.size(), expected);
    }

    @DataProvider
    Object[][] testDataForListPaginatedUsersNamesWithSingleNE() {
        return new Object[][]{
                {ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE, "DEFAULT", 10, 1, 2},
                {EMAIL_VERIFIED_CLAIM, "true", "DEFAULT", 10, 1, 4}
        };
    }

    @Test(description = "Test listPaginatedUsersNames with NE (Not Equal) operation for filtering users.",
            dataProvider = "testDataForListPaginatedUsersNamesWithSingleNE")
    public void testListPaginatedUsersNamesWithSingleNEOperation(String claimUri, String claimValue, String domain,
                                                                 int limit, int offset, int expectedCount)
            throws Exception {

        ExpressionCondition neCondition = new ExpressionCondition(
                ExpressionOperation.NE.toString(), claimUri, claimValue);
        List<ExpressionCondition> conditions = Collections.singletonList(neCondition);

        List<String> filteredUserNames = new ArrayList<>();

        List<String> result = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, domain, userStoreManager, limit, offset);

        // Verify the results.
        assertEquals(result.size(), expectedCount,
                "Expected " + expectedCount + " users for NE operation on claim: " + claimUri);
    }

    @DataProvider
    Object[][] testDataForListPaginatedUsersNamesWithMultipleNE() {
        return new Object[][]{
                {10, 1, 2},
                {1, 1, 1} // Test with pagination.
        };
    }

    @Test(description = "Test listPaginatedUsersNames with multiple NE operations for filtering users without " +
            "DISABLED state AND without emailVerified true.",
            dataProvider = "testDataForListPaginatedUsersNamesWithMultipleNE")
    public void testListPaginatedUsersNamesWithMultipleNEOperations(int limit, int offset, int expectedCount)
            throws Exception {

        List<ExpressionCondition> conditions = new ArrayList<>();

        // Add NE condition for accountState.
        conditions.add(new ExpressionCondition(
                ExpressionOperation.NE.toString(), ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE));
        // Add NE condition for emailVerified.
        conditions.add(new ExpressionCondition(
                ExpressionOperation.NE.toString(), EMAIL_VERIFIED_CLAIM, "true"));


        List<String> filteredUserNames = new ArrayList<>();
        List<String> result = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, limit, offset);

        // Verify the results.
        assertEquals(result.size(), expectedCount,
                "Expected " + expectedCount + " users for multiple NE operations");
    }

    @Test(description = "Test listPaginatedUsersNames with mixed operations (EQ and NE) for filtering users with" +
            "DISABLED state AND without emailVerified true.")
    public void testListPaginatedUsersNamesMixedOperations() throws Exception {

        List<ExpressionCondition> conditions = new ArrayList<>();

        // Add NE condition for accountState.
        conditions.add(new ExpressionCondition(
                ExpressionOperation.EQ.toString(), ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE));
        // Add NE condition for emailVerified.
        conditions.add(new ExpressionCondition(
                ExpressionOperation.NE.toString(), EMAIL_VERIFIED_CLAIM, "true"));


        List<String> filteredUserNames = new ArrayList<>();

        List<String> result = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 10, 1);

        // Verify the results.
        assertEquals(result.size(), 2, "Expected 2 user for mixed operations with EQ and NE");
    }

    @Test(description = "Test listPaginatedUsersNames with NE operation and pagination parameters.")
    public void testListPaginatedUsersNamesWithNEAndPagination() throws Exception {

        // Create ExpressionCondition with NE operation for accountState.
        ExpressionCondition neCondition = new ExpressionCondition(
                ExpressionOperation.NE.toString(), ACCOUNT_STATE_CLAIM_URI, ACCOUNT_STATE_CLAIM_VALUE);
        List<ExpressionCondition> conditions = Collections.singletonList(neCondition);

        List<String> filteredUserNames = new ArrayList<>();

        // Test with limit 1, offset 1.
        List<String> result1 = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 1, 1);
        assertEquals(result1.size(), 1, "Should return 1 user with limit 1");

        // Test with limit 2, offset 1.
        filteredUserNames.clear();
        List<String> result2 = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 2, 1);
        assertEquals(result2.size(), 2, "Should return 2 users with limit 2");

        // Test with limit 1, offset 2.
        filteredUserNames.clear();
        List<String> result3 = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 1, 2);
        assertEquals(result3.size(), 1, "Should return 1 user with limit 1, offset 2");
    }

    @Test(description = "Test listPaginatedUsersNames with NE operation for non-existent claim values.")
    public void testListPaginatedUsersNamesWithNEForNonExistentValues() throws Exception {

        ExpressionCondition neCondition = new ExpressionCondition(
                ExpressionOperation.NE.toString(), ACCOUNT_STATE_CLAIM_URI, "NON_EXISTENT_STATE");
        List<ExpressionCondition> conditions = Collections.singletonList(neCondition);

        List<String> filteredUserNames = new ArrayList<>();

        List<String> result = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 10, 1);

        // Verify the results.
        assertEquals(result.size(), 5, "Should return all 5 users for NE operation with non-existent value");
    }

    @Test(description = "Test listPaginatedUsersNames with NE operation for non-existent claim URI.")
    public void testListPaginatedUsersNamesWithNEForNonExistentClaimURI() throws Exception {

        ExpressionCondition neCondition = new ExpressionCondition(
                ExpressionOperation.NE.toString(), "http://wso2.org/claims/identity/nonExistentClaim", "anyValue");
        List<ExpressionCondition> conditions = Collections.singletonList(neCondition);

        List<String> filteredUserNames = new ArrayList<>();

        List<String> result = jdbcIdentityDataStore.listPaginatedUsersNames(
                conditions, filteredUserNames, "DEFAULT", userStoreManager, 10, 1);

        // Verify the results. Should return all 5 users since none have the non-existent claim URI.
        assertEquals(result.size(), 5, "Should return all 5 users for NE operation with non-existent claim URI");
    }
}
