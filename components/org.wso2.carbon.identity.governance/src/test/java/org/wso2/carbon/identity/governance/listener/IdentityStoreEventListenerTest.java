/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.listener;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.model.IdentityEventListenerConfig;
import org.wso2.carbon.identity.core.model.IdentityEventListenerConfigKey;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreServiceImpl;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.identity.governance.util.IdentityDataStoreUtil;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.model.ExpressionOperation;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class IdentityStoreEventListenerTest {

    private static final String USER_OPERATION_EVENT_LISTENER_TYPE = "org.wso2.carbon.user.core.listener" +
            ".UserOperationEventListener";
    private static final String DATA_STORE_PROPERTY_NAME = "Data.Store";
    private static final String STORE_IDENTITY_CLAIMS = "StoreIdentityClaims";
    private static final String IDENTITY_DATA_STORE_TYPE = "org.wso2.carbon.identity." +
            "governance.store.JDBCIdentityDataStore";
    private static final int TENANT_ID = 1234;
    private static final String TEST_TENANT_DOMAIN = "carbon.super";
    private static final String TEST_USER_STORE_DOMAIN = "PRIMARY";
    private static final String CLAIM_ACCOUNT_LOCKED = "http://wso2.org/claims/identity/accountLocked";
    private static final String CLAIM_FAILED_LOGIN_ATTEMPTS = "http://wso2.org/claims/identity/failedLoginAttempts";
    private static final String CLAIM_CUSTOM = "http://wso2.org/claims/customClaim";
    private static final String CLAIM_EMAIL_ADDRESS = "http://wso2.org/claims/emailaddress";
    private static final String TEST_USERNAME = "testUser";
    private static final String DEFAULT_PROFILE = "default";

    @Mock
    UserIdentityDataStore identityDataStore;

    @Mock
    UserStoreManager userStoreManager;

    @Mock
    UserIdentityDataStore userIdentityDataStore;

    @Mock
    RealmConfiguration realmConfiguration;

    @Mock
    IdentityDatabaseUtil identityDatabaseUtil;

    IdentityStoreEventListener identityStoreEventListener;

    IdentityDataStoreService identityDataStoreService;

    private MockedStatic<IdentityUtil> mockedIdentityUtil;

    @BeforeTest
    public void setUp() throws IllegalAccessException, InstantiationException, ClassNotFoundException {
        MockitoAnnotations.openMocks(this);
        String carbonHome = IdentityStoreEventListenerTest.class.getResource("/").getFile();
        System.setProperty("carbon.home", carbonHome);
        Map<IdentityEventListenerConfigKey, IdentityEventListenerConfig> eventListenerConfiguration;

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class, Mockito.CALLS_REAL_METHODS);
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(anyString())).thenReturn
                (IDENTITY_DATA_STORE_TYPE);
        identityDataStoreService = spy(new IdentityDataStoreServiceImpl());
        IdentityMgtServiceDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);

        IdentityUtil.populateProperties();
        identityStoreEventListener = spy(new IdentityStoreEventListener());
    }

    @AfterMethod
    public void cleanupAfterTest() {

        // End any tenant flow that might have been started.
        try {
            PrivilegedCarbonContext.endTenantFlow();
        } catch (Exception e) {
            // Ignore if no tenant flow was started.
        }

        // Clean up RealmService if it was set.
        try {
            IdentityMgtServiceDataHolder.getInstance().setRealmService(null);
        } catch (Exception e) {
            // Ignore cleanup errors.
        }

        // Reset spy stubbings to avoid interference between tests.
        Mockito.reset(identityStoreEventListener);
    }

    @AfterClass
    public void tearDown() {
        if (mockedIdentityUtil != null) {
            mockedIdentityUtil.close();
        }
    }

    @Test
    public void testGetExecutionOrderId() {
        int orderId = identityStoreEventListener.getExecutionOrderId();
        assertEquals(orderId, 97, "OrderId is not equal to " + IdentityCoreConstants.EVENT_LISTENER_ORDER_ID);

        doReturn(0).when(identityStoreEventListener).getOrderId();
        orderId = identityStoreEventListener.getExecutionOrderId();
        assertEquals(orderId, 0, "OrderId is equal to " + IdentityCoreConstants.EVENT_LISTENER_ORDER_ID);
    }

    @DataProvider(name = "addClaimHandler")
    public Object[][] getDataAddClaimData() {
        String[] roleList = {"admin1", "admin2"};
        Map<String, String> claimSet1 = new HashMap<>();
        claimSet1.put("http://wso2.org/claims/email", "john@wso2.com");
        claimSet1.put("http://wso2.org/claims/username", "john");
        claimSet1.put("http://wso2.org/claims/identity/accountLocked", "true");

        Map<String, String> claimSet2 = new HashMap<>();
        claimSet2.put("http://wso2.org/claims/identity/oidc", "oidc");
        claimSet2.put("http://wso2.org/claims/email", "john@wso2.com");
        claimSet2.put("http://wso2.org/claims/identity/accountLocked", "false");

        return new Object[][]{
                {"admin", new String("admin"), roleList, claimSet1, "muProfile"},
                {"admin", new String("admin"), roleList, claimSet2, "muProfile"}
        };
    }

    @Test(dataProvider = "addClaimHandler")
    public void testDoPreAddUser(String userName,
                                 Object pwd,
                                 String[] roleList,
                                 Map<String, String> claims,
                                 String prof) throws Exception {
        userStoreManager = mock(UserStoreManager.class);
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        Mockito.when(userStoreManager.getTenantId()).thenReturn(1001);
        assertTrue(identityStoreEventListener.doPreAddUser(userName, pwd, roleList, claims, prof, userStoreManager));
    }

    @Test(dataProvider = "addClaimHandler")
    public void testDoPostAddUser(String userName,
                                  Object pwd,
                                  String[] roleList,
                                  Map<String, String> claims,
                                  String prof) throws Exception {
        TestUtils.startTenantFlow("carbon.super");
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        assertTrue(identityStoreEventListener.doPostAddUser(userName, pwd, roleList, claims, prof, userStoreManager));
    }

    @Test(dataProvider = "addClaimHandler")
    public void testDoPreSetUserClaimValues(final String userName,
                                            Object pwd,
                                            String[] roleList,
                                            Map<String, String> claims,
                                            String prof) throws Exception {
        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        userIdentityDataStore = mock(UserIdentityDataStore.class);
        UserIdentityClaim userIdentityClaim = null;

        Field fieldIdentityStore = IdentityStoreEventListener.class
                .getDeclaredField("identityDataStore");
        fieldIdentityStore.setAccessible(true);
        fieldIdentityStore.set(identityStoreEventListener, userIdentityDataStore);

        Field fieldIdentityDataStoreService = IdentityDataStoreServiceImpl.class
                .getDeclaredField("identityDataStore");
        fieldIdentityDataStoreService.setAccessible(true);
        fieldIdentityDataStoreService.set(identityDataStoreService, userIdentityDataStore);

        doAnswer(new Answer() {
            @Override
            public Object answer(InvocationOnMock invocationOnMock) throws Throwable {
                return new UserIdentityClaim(userName);
            }
        }).when(userIdentityDataStore).load(userName, userStoreManager);

        doAnswer(new Answer() {
            @Override
            public Object answer(InvocationOnMock invocationOnMock) throws Throwable {
                return null;
            }
        }).when(userIdentityDataStore).store(userIdentityClaim, userStoreManager);

        Assert.assertTrue(identityStoreEventListener.doPreSetUserClaimValues
                (userName, claims, prof, userStoreManager),"Do pre set claim values is invalid.");
    }

    @DataProvider(name = "getUserClaimHandler")
    public Object[][] getUserClaimData() {

        String[] claimList1 = {"http://wso2.org/claims/email", "http://wso2.org/claims/email"};
        String[] claimList2 = {"http://wso2.org/claims/identity/oidc", "http://wso2.org/claims/email"};

        Map<String, String> claimSet1 = new HashMap<>();
        claimSet1.put("http://wso2.org/claims/email", "john@wso2.com");
        claimSet1.put("http://wso2.org/claims/email", "john");
        claimSet1.put("http://wso2.org/claims/identity/accountLocked", "true");

        Map<String, String> claimSet2 = new HashMap<>();
        claimSet2.put("http://wso2.org/claims/identity/oidc", "oidc");
        claimSet2.put("http://wso2.org/claims/email", "john@wso2.com");

        return new Object[][]{
                {"admin", new String("admin"), claimList1, claimSet1, "muProfile"},
                {"admin", new String("admin"), claimList2, claimSet2, "muProfile"}
        };
    }

    @Test(dataProvider = "getUserClaimHandler")
    public void testDoPreGetUserClaimValues(String userName, Object pwd, String[] claimList, Map<String, String> claims,
                                            String prof) throws Exception {

        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(UserIdentityDataStore.class);

        Field fieldIdentityStore = IdentityStoreEventListener.class.getDeclaredField("identityDataStore");
        fieldIdentityStore.setAccessible(true);
        fieldIdentityStore.set(identityStoreEventListener, userIdentityDataStore);

        assertTrue(identityStoreEventListener.doPostGetUserClaimValues(userName, claimList, prof, claims,
                userStoreManager));
    }

    @Test(dataProvider = "getUserClaimHandler")
    public void testDoPostGetUserClaimValues(String userName,
                                             Object pwd,
                                             String[] claimList,
                                             Map<String, String> claims,
                                             String prof) throws Exception {

        realmConfiguration = mock(RealmConfiguration.class);
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        userIdentityDataStore = mock(UserIdentityDataStore.class);

        Field fieldIdentityStore = IdentityStoreEventListener.class
                .getDeclaredField("identityDataStore");
        fieldIdentityStore.setAccessible(true);
        fieldIdentityStore.set(identityStoreEventListener, userIdentityDataStore);

        Field fieldIdentityDataStoreService = IdentityDataStoreServiceImpl.class
                .getDeclaredField("identityDataStore");
        fieldIdentityDataStoreService.setAccessible(true);
        fieldIdentityDataStoreService.set(identityDataStoreService, userIdentityDataStore);

        Assert.assertTrue(identityStoreEventListener.doPostGetUserClaimValues(userName, claimList,
                prof, claims, userStoreManager));
    }

    @DataProvider(name = "preGetUserClaimHandler")
    public Object[][] preGetUserClaimData() {

        String claim1 = "http://wso2.org/claims/email";
        String claim2 = "http://wso2.org/claims/oidc";

        return new Object[][]{
                {"admin", claim1, "muProfile"},
                {"admin", claim2, "muProfile"}
        };
    }

    @Test(dataProvider = "preGetUserClaimHandler")
    public void testDoPreGetUserClaimValue(String userName,
                                           String claim,
                                           String profileName) throws Exception {
        boolean preGetUserClaims = identityStoreEventListener.doPreGetUserClaimValue(userName, claim, profileName, userStoreManager);
        Assert.assertTrue(preGetUserClaims);
    }

    @Test(expectedExceptions = UserStoreException.class, priority = 1)
    public void testDoPreGetUserClaimValueException() throws Exception {
        // This test expects an exception - run with high priority before static mocking tests
        Assert.assertTrue(identityStoreEventListener.doPreGetUserClaimValue("admin",
                "http://wso2.org/claims/identity/email", "myprofile", userStoreManager));
    }

    @DataProvider(name = "setUserClaimHandler")
    public Object[][] setUserClaimData() {

        return new Object[][]{
                {"admin", "http://wso2.org/claims/email", "john@wso2.com", "muProfile"},
                {"admin", "http://wso2.org/claims/username", "john", "muProfile"}
        };
    }

    @Test(dataProvider = "setUserClaimHandler")
    public void testDoPreSetUserClaimValue(String userName,
                                           String claimUri,
                                           String claimValue,
                                           String profileName) throws Exception {

        Assert.assertTrue(identityStoreEventListener.doPreSetUserClaimValue(userName, claimUri,
                claimValue, profileName, userStoreManager));
    }

    @Test(expectedExceptions = UserStoreException.class, priority = 1)
    public void testDoPreSetUserClaimValueException() throws Exception {
        Assert.assertTrue(identityStoreEventListener.doPreSetUserClaimValue("admin",
                "http://wso2.org/claims/identity/email", "admin@wso2.com", "foo", userStoreManager));
    }

    @DataProvider(name = "getuserlistHandler")
    public Object[][] getUserListData() {
        List<String> list1 = new ArrayList<>();
        list1.add("user1");
        list1.add("user2");

        List<String> list2 = new ArrayList<>();
        list2.add("user2");
        list2.add("user3");

        return new Object[][]{
                {"http://wso2.org/claims/email", "john@wso2.com", list1, "PRIMARY"},
                {"http://wso2.org/claims/username", "john", list2, "SECONDARY"}
        };
    }

    @Test(dataProvider = "getuserlistHandler")
    public void testDoPreGetUserList(String claimUri,
                                     String claimValue,
                                     final List<String> userList,
                                     String userStore) throws Exception {
        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(UserIdentityDataStore.class);

        Field fieldIdentityStore = IdentityStoreEventListener.class
                .getDeclaredField("identityDataStore");
        fieldIdentityStore.setAccessible(true);
        fieldIdentityStore.set(identityStoreEventListener, userIdentityDataStore);

        final List<String> userIds = new ArrayList<>();
        userIds.add("PRIMARY/user1@carbon.super");
        userIds.add("PRIMARY/user2@abc.com");
        doAnswer(new Answer() {
            @Override
            public Object answer(InvocationOnMock invocationOnMock) throws Throwable {
                return userIds;
            }
        }).when(userIdentityDataStore).list(claimUri, claimValue, userStoreManager);

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn(userStore);

        assertTrue(identityStoreEventListener.doPreGetUserList(claimUri, claimValue, userList, userStoreManager));
    }

    @Test
    public void testDoPostDeleteUser() throws Exception {

        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(UserIdentityDataStore.class);
        String username = "user1";

        Field fieldIdentityStore = IdentityStoreEventListener.class
                .getDeclaredField("identityDataStore");
        fieldIdentityStore.setAccessible(true);
        fieldIdentityStore.set(identityStoreEventListener, userIdentityDataStore);

        Field fieldIdentityDataStoreService = IdentityDataStoreServiceImpl.class
                .getDeclaredField("identityDataStore");
        fieldIdentityDataStoreService.setAccessible(true);
        fieldIdentityDataStoreService.set(identityDataStoreService, userIdentityDataStore);

        doAnswer(new Answer() {
            @Override
            public Object answer(InvocationOnMock invocationOnMock) throws Throwable {
                return null;
            }
        }).when(userIdentityDataStore).remove(username, userStoreManager);

        Assert.assertTrue(identityStoreEventListener.doPostDeleteUser(username, userStoreManager));
    }

    @Test(description = "Verify doPreGetUserList supports NE operator on identity claims.")
    public void testDoPreGetUserListWithNEClaim() throws Exception {

        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        final String claimUri = "http://wso2.org/claims/identity/accountLocked";
        final String claimValue = "true";
        String domainName = "PRIMARY";

        // Build an ExpressionCondition that uses the NE operation.
        ExpressionCondition condition = new ExpressionCondition(
                ExpressionOperation.NE.toString(), claimUri, claimValue);

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS))
                .thenReturn(String.valueOf(false));
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn(domainName);

        List<String> expectedUsernames = Arrays.asList(
                domainName + "/user1",
                domainName + "/user2");

        Mockito.doReturn(expectedUsernames).when(identityDataStoreService)
                .getUserNamesByClaimURINotEqualValue(
                        Mockito.any(Condition.class), Mockito.anyString(), Mockito.anyString(),
                        Mockito.any(UserStoreManager.class));
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            mockedUtil.when(() -> IdentityDataStoreUtil.isManagedInIdentityDataStoreByClaimConfig(
                    Mockito.anyString(), Mockito.any(), Mockito.anyString())).thenReturn(true);
            mockedUtil.when(() -> IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(
                    Mockito.any())).thenReturn(false);

            List<String> filteredUserList = new ArrayList<>();
            boolean result = identityStoreEventListener.doPreGetUserList(
                    condition, filteredUserList, userStoreManager, domainName);

            // Assert that the method returns true and the filtered user list matches the expected usernames.
            assertTrue(result, "doPreGetUserList should return true on success.");
            assertEquals(filteredUserList, expectedUsernames,
                    "Filtered user list should match the data-store results for NE filtering.");

            // Verify that the identity-data-store was actually queried with NE.
            Mockito.verify(identityDataStoreService, Mockito.times(1))
                    .getUserNamesByClaimURINotEqualValue(condition, claimUri, claimValue, userStoreManager);
        }
    }

    @DataProvider
    public Object[][] getGreaterThanAndLessThanFilters() {

        List<String> filteredUsernames = new ArrayList<>();
        filteredUsernames.add("user1");
        filteredUsernames.add("SECONDARY" + "/user2");

        return new Object[][]{
                {ExpressionOperation.GE.toString(), "PRIMARY", filteredUsernames, filteredUsernames.get(0)},
                {ExpressionOperation.LE.toString(), "PRIMARY", filteredUsernames, filteredUsernames.get(0)},
                {ExpressionOperation.GE.toString(), "SECONDARY", filteredUsernames, filteredUsernames.get(1)},
                {ExpressionOperation.LE.toString(), "SECONDARY", filteredUsernames, filteredUsernames.get(1)}
        };
    }
    @Test(dataProvider = "getGreaterThanAndLessThanFilters",
          description = "Verify doPreGetUserList supports GE and LE operators on identity claims.")
    public void testDoPreGetUserListWithGreaterThanAndLessThanFilters(String operation, String userStoreDomain,
                                                                      List<String> filteredUsernames,
                                                                      String filteredUser) throws Exception {

        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        RealmService realmService = mock(RealmService.class);
        RealmConfiguration bootstrapRealmConfig = mock(RealmConfiguration.class);
        final String claimUri = "http://wso2.org/claims/identity/lastPasswordUpdateTime";
        final String claimValue = "1759196318937";

        IdentityMgtServiceDataHolder.getInstance().setRealmService(realmService);
        Mockito.when(realmService.getBootstrapRealmConfiguration()).thenReturn(bootstrapRealmConfig);
        Mockito.when(bootstrapRealmConfig.getUserStoreProperty(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(userStoreManager.getTenantId()).thenReturn(-1234);
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn(userStoreDomain);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        if (operation.equals(ExpressionOperation.GE.toString())) {
            Mockito.doReturn(new ArrayList<>(filteredUsernames)).when(identityDataStoreService)
                    .getUserNamesMoreThanProvidedClaimValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt());
        } else if (operation.equals(ExpressionOperation.LE.toString())) {
            Mockito.doReturn(new ArrayList<>(filteredUsernames)).when(identityDataStoreService)
                    .getUserNamesLessThanProvidedClaimValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt());
        }

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            // Accept any tenant domain and return true for identity claims
            mockedUtil.when(() -> IdentityDataStoreUtil.isManagedInIdentityDataStoreByClaimConfig(
                    Mockito.anyString(), Mockito.any(), Mockito.anyString())).thenReturn(true);
            mockedUtil.when(() -> IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(
                    Mockito.any())).thenReturn(false);

            List<String> filteredUserList = new ArrayList<>();

            ExpressionCondition condition = new ExpressionCondition(operation, claimUri, claimValue);
            boolean result = identityStoreEventListener.doPreGetUserList(condition, filteredUserList, userStoreManager,
                    userStoreDomain);

            List<String> expectedUsernames = new ArrayList<>();
            expectedUsernames.add(filteredUser);

            assertTrue(result, "doPreGetUserList should return true on success.");
            assertEquals(filteredUserList, expectedUsernames, "Filtered user list should match the data-store " +
                    "results for " + operation + " filtering.");
        }
    }

    @DataProvider(name = "doPostGetUserClaimValueHandler")
    public Object[][] doPostGetUserClaimValueData() {

        return new Object[][]{
                // claim, claimValue, isManagedInIdentityDataStore, expectedValue, description.
                {CLAIM_ACCOUNT_LOCKED, "true", true, "true", "identity claim in identity store"},
                {CLAIM_FAILED_LOGIN_ATTEMPTS, "3", true, "3", "identity claim in identity store"},
                {CLAIM_CUSTOM, "customValue", true, "customValue", "non-identity claim stored in identity store"},
                {CLAIM_EMAIL_ADDRESS, "email@example.com", false, "email@example.com", "non-identity claim in user store"}
        };
    }

    @Test(dataProvider = "doPostGetUserClaimValueHandler",
            description = "Test doPostGetUserClaimValue with different claim configurations")
    public void testDoPostGetUserClaimValue(String claim, String claimValue, boolean isManagedInIdentityDataStore,
                                            String expectedValue, String testDescription) throws Exception {

        // Always setup basic mocks.
        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn(TEST_USER_STORE_DOMAIN);
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        // Only setup identity data store for claims managed in identity data store.
        // Non-managed claims return early, so identity data store is never accessed.
        if (isManagedInIdentityDataStore) {
            setupMocksForIdentityDataStore(TEST_USERNAME, claim, claimValue);
        }

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            setupIdentityDataStoreUtilMocks(mockedUtil, claim,
                    isManagedInIdentityDataStore, false);

            // For non-managed claims, the initial value from user store should be preserved.
            String initialValue = isManagedInIdentityDataStore ? "oldValue" : expectedValue;
            List<String> resultClaimValue = new ArrayList<>(Arrays.asList(initialValue));
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME, claim, resultClaimValue,
                    DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(resultClaimValue.size(), 1);
            assertEquals(resultClaimValue.get(0), expectedValue);
        }
    }


    @Test(description = "Test doPostGetUserClaimValue when listener is disabled")
    public void testDoPostGetUserClaimValueWhenDisabled() throws Exception {

        doReturn(false).when(identityStoreEventListener).isEnable();

        List<String> claimValue = new ArrayList<>(Arrays.asList("initialValue"));
        boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

        assertTrue(result);
        assertEquals(claimValue.get(0), "initialValue");
    }

    @Test(description = "Test doPostGetUserClaimValue when using user store based identity data store")
    public void testDoPostGetUserClaimValueWithUserStoreBasedIdentityDataStore() throws Exception {

        Mockito.doReturn(true).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        List<String> claimValue = new ArrayList<>(Arrays.asList("userStoreValue"));
        boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

        assertTrue(result);
        assertEquals(claimValue.get(0), "userStoreValue");
    }

    @Test(description = "Test doPostGetUserClaimValue when StoreIdentityClaims is enabled")
    public void testDoPostGetUserClaimValueWithStoreIdentityClaimsEnabled() throws Exception {

        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(true));
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            mockedUtil.when(() -> IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(userStoreManager))
                    .thenReturn(true);

            List<String> claimValue = new ArrayList<>(Arrays.asList("userStoreValue"));
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                    CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(claimValue.get(0), "userStoreValue");
        }
    }

    @Test(description = "Test doPostGetUserClaimValue when no identity data exists")
    public void testDoPostGetUserClaimValueWithNoIdentityData() throws Exception {

        TestUtils.startTenantFlow(TEST_TENANT_DOMAIN);
        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(UserIdentityDataStore.class);

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn("PRIMARY");
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        Field field = IdentityDataStoreServiceImpl.class.getDeclaredField("identityDataStore");
        field.setAccessible(true);
        field.set(identityDataStoreService, userIdentityDataStore);

        Mockito.when(userIdentityDataStore.load(TEST_USERNAME, userStoreManager)).thenReturn(null);

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            setupIdentityDataStoreUtilMocks(mockedUtil, CLAIM_ACCOUNT_LOCKED, true, false);

            List<String> claimValue = new ArrayList<>();
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                    CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(claimValue.size(), 0);
        }
    }

    @Test(description = "Test doPostGetUserClaimValue with blank claim value")
    public void testDoPostGetUserClaimValueWithBlankClaimValue() throws Exception {

        setupMocksForIdentityDataStore(TEST_USERNAME, CLAIM_ACCOUNT_LOCKED, "");

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            setupIdentityDataStoreUtilMocks(mockedUtil, CLAIM_ACCOUNT_LOCKED, true, false);

            List<String> claimValue = new ArrayList<>();
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                    CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(claimValue.size(), 0);
        }
    }

    @Test(description = "Test doPostGetUserClaimValue clears user store value when hybrid mode disabled")
    public void testDoPostGetUserClaimValueClearsUserStoreValue() throws Exception {

        setupMocksForIdentityDataStore(TEST_USERNAME, CLAIM_ACCOUNT_LOCKED, "true");

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            setupIdentityDataStoreUtilMocks(mockedUtil, CLAIM_ACCOUNT_LOCKED, true,
                    false);

            List<String> claimValue = new ArrayList<>(Arrays.asList("false"));
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                    CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(claimValue.size(), 1);
            assertEquals(claimValue.get(0), "true");
        }
    }

    @Test(description = "Test doPostGetUserClaimValue when claim not present in identity data store")
    public void testDoPostGetUserClaimValueWhenClaimNotInIdentityDataStore() throws Exception {

        setupMocksForIdentityDataStore(TEST_USERNAME, CLAIM_FAILED_LOGIN_ATTEMPTS, "5");

        try (MockedStatic<IdentityDataStoreUtil> mockedUtil = Mockito.mockStatic(IdentityDataStoreUtil.class)) {
            setupIdentityDataStoreUtilMocks(mockedUtil, CLAIM_ACCOUNT_LOCKED, true, false);

            List<String> claimValue = new ArrayList<>();
            boolean result = identityStoreEventListener.doPostGetUserClaimValue(TEST_USERNAME,
                    CLAIM_ACCOUNT_LOCKED, claimValue, DEFAULT_PROFILE, userStoreManager);

            assertTrue(result);
            assertEquals(claimValue.size(), 0);
        }
    }

    private void setupMocksForIdentityDataStore(String userName, String claim, String claimValue) throws Exception {

        TestUtils.startTenantFlow(TEST_TENANT_DOMAIN);
        userStoreManager = mock(UserStoreManager.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(UserIdentityDataStore.class);

        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(STORE_IDENTITY_CLAIMS)).thenReturn(String.valueOf(false));
        Mockito.when(UserCoreUtil.getDomainName(realmConfiguration)).thenReturn(TEST_USER_STORE_DOMAIN);
        Mockito.doReturn(false).when(identityDataStoreService).isUserStoreBasedIdentityDataStore();

        Field field = IdentityDataStoreServiceImpl.class.getDeclaredField("identityDataStore");
        field.setAccessible(true);
        field.set(identityDataStoreService, userIdentityDataStore);

        Map<String, String> userIdentityDataMap = new HashMap<>();
        if (claim != null && claimValue != null) {
            userIdentityDataMap.put(claim, claimValue);
        }
        UserIdentityClaim userIdentityClaim = new UserIdentityClaim(userName, userIdentityDataMap);
        Mockito.when(userIdentityDataStore.load(userName, userStoreManager)).thenReturn(userIdentityClaim);
    }

    private void setupIdentityDataStoreUtilMocks(MockedStatic<IdentityDataStoreUtil> mockedUtil, String claim,
                                                 boolean isManagedInIdentityDataStore,
                                                 boolean isStoreIdentityClaimsEnabled) {

        mockedUtil.when(() -> IdentityDataStoreUtil.isManagedInIdentityDataStoreByClaimConfig(
                claim, TEST_TENANT_DOMAIN, TEST_USER_STORE_DOMAIN)).thenReturn(isManagedInIdentityDataStore);
        mockedUtil.when(() -> IdentityDataStoreUtil.isStoreIdentityClaimsInUserStoreEnabled(
                Mockito.any())).thenReturn(isStoreIdentityClaimsEnabled);
        mockedUtil.when(() -> IdentityDataStoreUtil.maskIfRequired(Mockito.anyString()))
                .thenAnswer(invocation -> invocation.getArgument(0));
    }
}
