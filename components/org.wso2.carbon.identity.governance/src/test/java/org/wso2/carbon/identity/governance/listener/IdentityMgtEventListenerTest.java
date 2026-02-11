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

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.user.api.Permission;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.expectThrows;

/**
 * Test class for IdentityMgtEventListener.
 */
public class IdentityMgtEventListenerTest {

    private int SAMPLE_TENANT_ID = 3456;
    private String SAMPLE_TENANT_DOMAIN = "abc.com";

    @Mock
    UserStoreManager userStoreManager;

    @Mock
    RealmConfiguration realmConfiguration;

    @Mock
    TenantManager tenantManager;

    @Mock
    RealmService realmService;

    @Mock
    IdentityEventService identityEventService;

    IdentityMgtServiceDataHolder instance;

    @InjectMocks
    @Spy
    IdentityMgtEventListener identityMgtEventListener;

    @BeforeTest
    public void setUp() throws Exception {
        instance = IdentityMgtServiceDataHolder.getInstance();
        identityEventService = Mockito.mock(IdentityEventService.class);
        instance.setIdentityEventService(identityEventService);
        realmService = Mockito.mock(RealmService.class);
        instance.setRealmService(realmService);
        identityMgtEventListener = new IdentityMgtEventListener();
        mockHandleEvent();
    }

    @Test
    public void testGetExecutionOrderId() {
        int orderId = identityMgtEventListener.getExecutionOrderId();
        assertEquals(orderId, 95, "OrderId is not equal to IdentityCoreConstants.EVENT_LISTENER_ORDER_ID");

        IdentityMgtEventListener testIdentityMgtEventListener = spy(IdentityMgtEventListener.class);
        Mockito.doReturn(55).when(testIdentityMgtEventListener).getOrderId();
        orderId = testIdentityMgtEventListener.getExecutionOrderId();
        assertEquals(orderId, 55, "OrderId is not equal to IdentityCoreConstants.EVENT_LISTENER_ORDER_ID");
    }

    @DataProvider(name = "authenticatedDataHandler")
    public Object[][] getAuthenticatedData() {
        return new Object[][]{
                {"admin", new String("admin")},
                {"admin", null},
                {null, new String("admin")},
                {null, null}
        };
    }

    @Test(expectedExceptions = UserStoreException.class, dataProvider = "authenticatedDataHandler")
    public void testDoPreAuthenticate(String username,
                                      Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPreAuthenticate(username, pwd, userStoreManager),
                "Do pre authenticate flow is failed.");

        doThrow(IdentityEventException.class).when(identityEventService).handleEvent(any(Event.class));
        identityMgtEventListener.doPreAuthenticate(username, pwd, userStoreManager);
    }

    @DataProvider(name = "postAuthenticateHandler")
    public Object[][] getPostAuthenticateData() {
        return new Object[][]{
                {"admin", true},
                {"admin", false},
                {null, true},
                {null, false}
        };
    }

    @Test(dataProvider = "postAuthenticateHandler")
    public void testDoPostAuthenticate(String userName,
                                       boolean authenticated) throws Exception {
        IdentityTenantUtil identityTenantUtil = new IdentityTenantUtil();
        identityTenantUtil.setRealmService(realmService);
        when(realmService.getBootstrapRealmConfiguration()).thenReturn(realmConfiguration);
        assertTrue(identityMgtEventListener.doPostAuthenticate(userName, authenticated, userStoreManager),
                "Do post authenticate flow is failed.");
    }

    @DataProvider(name = "setUserClaimValueHandler")
    public Object[][] getUserClaimData() {
        Map<String, String> claimSet1 = new HashMap<>();
        claimSet1.put("http://wso2.org/claims/email", "example1@wso2.com");
        claimSet1.put("http://wso2.org/claims/username", "john");
        claimSet1.put("http://wso2.org/claims/identity/accountLocked", "true");
        claimSet1.put("http://wso2.org/claims/identity/preferredChannel","EMAIL");

        Map<String, String> claimSet2 = new HashMap<>();
        claimSet2.put("http://wso2.org/claims/identity/oidc", "oidc");
        claimSet2.put("http://wso2.org/claims/email", "example@wso2.com");
        claimSet2.put("http://wso2.org/claims/identity/accountLocked", "false");
        claimSet2.put("http://wso2.org/claims/identity/preferredChannel","EMAIL");

        return new Object[][]{
                {"admin", claimSet1, "myProfile"},
                {"user", claimSet2, "admin"}
        };
    }

    @Test(dataProvider = "setUserClaimValueHandler")
    public void testDoPreSetUserClaimValues(String userName,
                                            Map<String, String> claims,
                                            String profileName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreSetUserClaimValues(userName, claims, profileName,
                userStoreManager), "Do pre set claim values failed.");
    }

    @Test(dataProvider = "setUserClaimValueHandler")
    public void testDoPostSetUserClaimValues(String userName,
                                             Map<String, String> claims,
                                             String profileName) throws Exception {
        assertTrue(identityMgtEventListener.doPostSetUserClaimValues(userName, claims, profileName, userStoreManager),
                "Do post set claim values failed.");
    }

    @DataProvider(name = "addUserDataHandler")
    public Object[][] addUserGetData() {
        String[] roleList = {"admin1", "admin2"};
        Map<String, String> claimSet1 = new HashMap<>();
        claimSet1.put("http://wso2.org/claims/email", "example@wso2.com");
        claimSet1.put("http://wso2.org/claims/username", "john");
        claimSet1.put("http://wso2.org/claims/identity/accountLocked", "true");
        claimSet1.put("http://wso2.org/claims/identity/preferredChannel","EMAIL");

        Map<String, String> claimSet2 = new HashMap<>();
        claimSet2.put("http://wso2.org/claims/identity/oidc", "oidc");
        claimSet2.put("http://wso2.org/claims/email", "john@wso2.com");
        claimSet2.put("http://wso2.org/claims/identity/accountLocked", "false");
        claimSet1.put("http://wso2.org/claims/identity/preferredChannel","EMAIL");

        return new Object[][]{
                {"admin", new String("admin"), roleList, claimSet1, "muProfile"},
                {"admin", new String("admin"), roleList, claimSet2, "muProfile"}
        };
    }

    @Test(dataProvider = "addUserDataHandler")
    public void testDoPreAddUser(String userName,
                                 Object pwd,
                                 String[] roleList,
                                 Map<String, String> claims,
                                 String profileName) throws Exception {
        assertTrue(identityMgtEventListener.doPreAddUser(userName, pwd, roleList, claims, profileName, userStoreManager),
                "Do pre add user failed.");
    }

    @Test(dataProvider = "addUserDataHandler")
    public void testDoPostAddUser(String userName,
                                  Object pwd,
                                  String[] roleList,
                                  Map<String, String> claims,
                                  String profileName) throws Exception {
        assertTrue(identityMgtEventListener.doPostAddUser(userName, pwd, roleList, claims, profileName, userStoreManager),
                "Do post add user failed.");
    }

    @DataProvider(name = "credentialHandler")
    public Object[][] getCredentialData() {
        return new Object[][]{
                {"admin", new String("admin"), new String("user1")},
                {"user", null, new String("user2")}
        };
    }

    @Test(dataProvider = "credentialHandler")
    public void testDoPreUpdateCredential(String username,
                                          Object oldPwd,
                                          Object newPwd) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreUpdateCredential(username, oldPwd, newPwd, userStoreManager),
                "Do pre update credential failed.");
    }

    @Test(dataProvider = "credentialHandler")
    public void testDoPostUpdateCredential(String username,
                                           Object oldPwd,
                                           Object newPwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostUpdateCredential(username, newPwd, userStoreManager),
                "Do post update credential failed.");
    }

    @Test(dataProvider = "credentialHandler")
    public void testDoPreUpdateCredentialByAdmin(String username,
                                                 Object oldPwd,
                                                 Object newPwd) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreUpdateCredentialByAdmin(username, newPwd, userStoreManager),
                "Do pre update credential by admin failed.");
    }

    @Test(dataProvider = "credentialHandler")
    public void testDoPostUpdateCredentialByAdmin(String username,
                                                  Object oldPwd,
                                                  Object newPwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostUpdateCredentialByAdmin(username, newPwd, userStoreManager),
                "Do post update credential by admin failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPreDeleteUser(String username,
                                    Object pwd) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreDeleteUser(username, userStoreManager),
                "Do pre update delete user failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPostDeleteUser(String username,
                                     Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostDeleteUser(username, userStoreManager),
                "Do post delete user failed.");
    }

    @Test(dataProvider = "setClaimHandler")
    public void testDoPreSetUserClaimValue(String userName,
                                           String claimURI,
                                           String claimValue,
                                           String profileName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreSetUserClaimValue(userName, claimURI, claimValue, profileName,
                userStoreManager), "Do post delete user failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPostSetUserClaimValue(String username,
                                            Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostSetUserClaimValue(username, userStoreManager),
                "Do post set user claim value failed.");
    }

    @DataProvider(name = "deleteClaimHandler")
    public Object[][] getDeleteClaimData() {
        String[] claims1 = {"http://wso2.org/claims/email", "http://wso2.org/claims/username"};
        String[] claims2 = {"http://wso2.org/claims/birthday", "http://wso2.org/claims/address"};
        return new Object[][]{
                {"admin", claims1, "muProfile"},
                {"admin", claims2, "muProfile"}
        };
    }

    @Test(dataProvider = "deleteClaimHandler")
    public void testDoPreDeleteUserClaimValues(String userName,
                                               String[] claims,
                                               String profileName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreDeleteUserClaimValues(userName, claims, profileName,
                userStoreManager), "Do pre delete user claim values failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPostDeleteUserClaimValues(String userName,
                                                Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostDeleteUserClaimValues(userName, userStoreManager),
                "Do post delete user claim values failed.");
    }

    @DataProvider(name = "setClaimHandler")
    public Object[][] hetClaimData() {
        return new Object[][]{
                {"admin", "http://wso2.org/claims/email", "example1@wso2.com", "muProfile"},
                {"admin", "http://wso2.org/claims/username", "example", "muProfile"}
        };
    }

    @Test(dataProvider = "setClaimHandler")
    public void testDoPreDeleteUserClaimValue(String userName,
                                              String claimURI,
                                              String claimValue,
                                              String profileName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreDeleteUserClaimValue(userName, claimURI, profileName,
                userStoreManager), "Do pre delete user claim value failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPostDeleteUserClaimValue(String userName,
                                               Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostDeleteUserClaimValue(userName, userStoreManager),
                "Do post delete user claim value failed.");
    }

    @DataProvider(name = "roleHandler")
    public Object[][] getRoleData() {
        String[] userList1 = {"user1", "user2"};
        String[] userList2 = {"user3", "user4"};

        return new Object[][]{
                {"admin", userList1},
                {"admin", userList2}
        };
    }

    @Test(dataProvider = "roleHandler")
    public void testDoPreAddRole(String userName,
                                 String[] roleList) throws Exception {
        Permission permission1 = new Permission("resourceId1", "read");
        Permission permission2 = new Permission("resourceId2", "write");

        Permission[] permissions = {permission1, permission2, null};
        assertTrue(identityMgtEventListener.doPreAddRole(userName, roleList, permissions, userStoreManager),
                "Do pre add role failed.");
    }

    @Test(dataProvider = "roleHandler")
    public void testDoPostAddRole(String userName,
                                  String[] roleList) throws Exception {
        Permission permission1 = new Permission("resourceId1", "read");
        Permission permission2 = new Permission("resourceId2", "write");

        Permission[] permissions = {permission1, permission2, null};
        assertTrue(identityMgtEventListener.doPostAddRole(userName, roleList, permissions, userStoreManager),
                "Do pre add role failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPreDeleteRole(String roleName,
                                    Object pwd) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreDeleteRole(roleName, userStoreManager),
                "Do pre add role failed.");
    }

    @Test(dataProvider = "authenticatedDataHandler")
    public void testDoPostDeleteRole(String roleName,
                                     Object pwd) throws Exception {
        assertTrue(identityMgtEventListener.doPostDeleteRole(roleName, userStoreManager),
                "Do pre add role failed.");
    }

    @DataProvider(name = "updateRoleHandler")
    public Object[][] getUpdateRoleData() {
        return new String[][]{
                {"user1", "User1"},
                {"user2", "user3"}
        };
    }

    @Test(dataProvider = "updateRoleHandler")
    public void testDoPreUpdateRoleName(String roleName,
                                        String newRoleName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreUpdateRoleName(roleName, newRoleName, userStoreManager),
                "Do pre update role failed.");
    }

    @Test(dataProvider = "updateRoleHandler")
    public void testDoPostUpdateRoleName(String roleName,
                                         String newRoleName) throws Exception {
        assertTrue(identityMgtEventListener.doPostUpdateRoleName(roleName, newRoleName, userStoreManager),
                "Do post update role failed.");
    }

    @DataProvider(name = "updateRoleListHandler")
    public Object[][] getData10() {
        String[] deletedUsers1 = {"user1", "user2"};
        String[] deletedUsers2 = {"user3", "user4"};

        String[] newUsers1 = {"puser1", "puser2"};
        String[] newUsers2 = {"puser3", "puser4"};

        return new Object[][]{
                {"role1", deletedUsers1, newUsers1},
                {"role2", deletedUsers2, newUsers2}
        };
    }

    @Test(dataProvider = "updateRoleListHandler")
    public void testDoPreUpdateUserListOfRole(String roleName,
                                              String[] deletedUsers,
                                              String[] newUsers) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPreUpdateUserListOfRole(roleName, deletedUsers, newUsers,
                userStoreManager), "Do pre update roles failed.");
    }

    @Test(dataProvider = "updateRoleListHandler")
    public void testDoPostUpdateUserListOfRole(String roleName,
                                               String[] deletedUsers,
                                               String[] newUsers) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPostUpdateUserListOfRole(roleName, deletedUsers, newUsers,
                userStoreManager), "Do post update roles failed.");
    }

    @Test(dataProvider = "updateRoleListHandler")
    public void testDoPreUpdateRoleListOfUser(String roleName,
                                              String[] deletedUsers,
                                              String[] newUsers) throws Exception {
        assertTrue(identityMgtEventListener.doPreUpdateRoleListOfUser(roleName, deletedUsers, newUsers,
                userStoreManager), "Do pre update role list of user failed.");
    }

    @Test(dataProvider = "updateRoleListHandler")
    public void testDoPostUpdateRoleListOfUser(String roleName,
                                               String[] deletedUsers,
                                               String[] newUsers) throws Exception {
        assertTrue(identityMgtEventListener.doPostUpdateRoleListOfUser(roleName, deletedUsers, newUsers,
                userStoreManager), "Do post update role list of user failed.");
    }

    @DataProvider(name = "postuserClaimHandler")
    public Object[][] getPostClaimData() {
        List<String> listClaimValues1 = new ArrayList<>();
        listClaimValues1.add("example1@wso2.com");
        listClaimValues1.add("example2@wso2.com");

        List<String> listClaimValues2 = new ArrayList<>();
        listClaimValues2.add("role1");
        listClaimValues2.add("role2");

        return new Object[][]{
                {"user1", "http://wso2.org/claims/email", listClaimValues1, "myProfile"},
                {"user2", "http://wso2.org/claims/roles", listClaimValues2, "myProfile"}
        };
    }

    @Test(dataProvider = "postuserClaimHandler")
    public void testDoPostGetUserClaimValue(String userName,
                                            String claim,
                                            List<String> values,
                                            String profileName) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPostGetUserClaimValue(userName, claim, values, profileName,
                userStoreManager), "Do post get user claim value failed.");
    }

    @DataProvider(name = "postGetClaimvaluesHandler")
    public Object[][] getPostClaimValues() {
        String[] claimList1 = {"http://wso2.org/claims/email", "http://wso2.org/claims/username",
                "http://wso2.org/claims/identity/accountLocked"};
        String[] claimList2 = {"http://wso2.org/claims/identity/oidc", "http://wso2.org/claims/email",
                "http://wso2.org/claims/identity/accountLocked"};

        Map<String, String> claimSet1 = new HashMap<>();
        claimSet1.put("http://wso2.org/claims/email", "example1@wso2.com");
        claimSet1.put("http://wso2.org/claims/username", "john");
        claimSet1.put("http://wso2.org/claims/identity/accountLocked", "true");

        Map<String, String> claimSet2 = new HashMap<>();
        claimSet2.put("http://wso2.org/claims/identity/oidc", "oidc");
        claimSet2.put("http://wso2.org/claims/email", "example@wso2.com");
        claimSet2.put("http://wso2.org/claims/identity/accountLocked", "false");

        return new Object[][]{
                {"user1", claimList1, "myProfile", claimSet1},
                {"user2", claimList2, "myProfile", claimSet2}
        };
    }

    @Test(dataProvider = "postGetClaimvaluesHandler")
    public void testDoPostGetUserClaimValues(String userName,
                                             String[] claims,
                                             String profileName,
                                             Map<String, String> claimMap) throws Exception {
        mockHandleEvent();
        assertTrue(identityMgtEventListener.doPostGetUserClaimValues(userName, claims, profileName,
                claimMap, userStoreManager), "Do post get user claim value failed.");
    }

    private void mockHandleEvent() throws Exception {
        identityMgtEventListener = spy(IdentityMgtEventListener.class);
        userStoreManager = Mockito.mock(UserStoreManager.class);
        realmConfiguration = Mockito.mock(RealmConfiguration.class);
        tenantManager = Mockito.mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(SAMPLE_TENANT_ID)).thenReturn(SAMPLE_TENANT_DOMAIN);

        TestUtils.startTenantFlow(SAMPLE_TENANT_DOMAIN);
        when(userStoreManager.getTenantId()).thenReturn(SAMPLE_TENANT_ID);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");

        Map<String, Object> properties = new HashMap<>();
        Event identityMgtEvent = new Event("sampleEvent", properties);

        doNothing().when(identityEventService).handleEvent(any(Event.class));
        identityEventService.handleEvent(identityMgtEvent);
    }

    @Test
    void testHandleEventWithPasswordPolicyLoadError() throws Exception {

        IdentityMgtEventListener listener = new IdentityMgtEventListener();

        when(userStoreManager.getTenantId()).thenReturn(1);

        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(1);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain("testDomain");

        IdentityMgtServiceDataHolder.getInstance().setRealmService(realmService);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(1)).thenReturn("testDomain");

        Method handleEventMethod = IdentityMgtEventListener.class.getDeclaredMethod(
                "handleEvent", String.class, HashMap.class, UserStoreManager.class);
        handleEventMethod.setAccessible(true);

        doThrow(new IdentityEventException("40001", "Policy load failed"))
                .when(identityEventService).handleEvent(any());
        InvocationTargetException ite = expectThrows(
            InvocationTargetException.class,
            () -> handleEventMethod.invoke(listener, "USER_ON_ADD", new HashMap<>(), userStoreManager)
        );
        Throwable cause = ite.getTargetException();
        assertTrue(cause instanceof UserStoreException);
        UserStoreException userStoreException = (UserStoreException) cause;
        assertEquals(userStoreException.getMessage(), "Policy load failed");

        IdentityEventClientException clientEx =
                new IdentityEventClientException("C123", "client‐failure");
        doThrow(clientEx)
                .when(identityEventService).handleEvent(any(Event.class));
        ite = expectThrows(InvocationTargetException.class, () -> {
            handleEventMethod.invoke(listener, "USER_ON_ADD", new HashMap<>(), userStoreManager);
        });
        cause = ite.getTargetException();
        assertTrue(cause instanceof UserStoreClientException);
        UserStoreClientException userStoreClientException = (UserStoreClientException) cause;
        assertEquals(userStoreClientException.getMessage(), "client‐failure");
        assertEquals(userStoreClientException.getErrorCode(), "C123");

        doThrow(new IdentityEventException("40002", "policy‐violation"))
                .when(identityEventService).handleEvent(any(Event.class));

        ite = expectThrows(InvocationTargetException.class, () -> {
            handleEventMethod.invoke(listener, "USER_ON_ADD", new HashMap<>(), userStoreManager);
        });
        cause = ite.getTargetException();
        assertTrue(cause instanceof UserStoreException);
        assertEquals(cause.getMessage(), "policy‐violation");
    }
}
