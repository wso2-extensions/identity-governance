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
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
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
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
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

    @Test(expectedExceptions = UserStoreException.class)
    public void testDoPreGetUserClaimValueException() throws Exception {
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

    @Test(expectedExceptions = UserStoreException.class)
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

        List<String> filteredUserList = new ArrayList<>();
        boolean result = identityStoreEventListener.doPreGetUserList(
                condition, filteredUserList, userStoreManager, "SECONDARY");

        // Assert that the method returns true and the filtered user list matches the expected usernames.
        assertTrue(result, "doPreGetUserList should return true on success.");
        assertEquals(filteredUserList, expectedUsernames,
                "Filtered user list should match the data-store results for NE filtering.");

        // Verify that the identity-data-store was actually queried with NE.
        Mockito.verify(identityDataStoreService, Mockito.times(1))
                .getUserNamesByClaimURINotEqualValue(condition, claimUri, claimValue, userStoreManager);
    }
}
