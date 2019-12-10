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

package org.wso2.carbon.identity.user.rename.core.internal.service.impl;

import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.testutil.powermock.PowerMockIdentityBaseTest;
import org.wso2.carbon.identity.user.rename.core.dto.StatusDTO;
import org.wso2.carbon.identity.user.rename.core.dto.UserDTO;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateException;
import org.wso2.carbon.identity.user.rename.core.internal.service.impl.ForgetMeToolExecutor;
import org.wso2.carbon.identity.user.rename.core.internal.service.impl.UsernameUpdateServiceImpl;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.doNothing;
import static org.powermock.api.mockito.PowerMockito.doThrow;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.wso2.carbon.identity.user.rename.core.constants.UsernameUpdateServiceConstants.Status.STATUS_SUCCESS;

@PrepareForTest({ForgetMeToolExecutor.class})
@PowerMockIgnore({"org.apache.log4j.*"})
public class UsernameUpdateServiceImplTest extends PowerMockIdentityBaseTest {

    @DataProvider
    public Object[][] getValidUsers() {

        return new Object[][]{
                {"testuser1", "testuser11", null, null},
                {"testuser2", "testuser22", UserCoreConstants
                        .PRIMARY_DEFAULT_DOMAIN_NAME, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME,},
                {"testuser3", "testuser33", "test.com", MultitenantConstants.SUPER_TENANT_DOMAIN_NAME},
        };
    }

    @DataProvider
    public Object[][] getInvalidUsers() {

        return new Object[][]{
                {null, "testuser11", null, null},
                {"testuser2", null, null, null},
                {"", "", null, null},
                {"testuser1", "testuser1", null, null},
        };
    }

    @Test(dataProvider = "getValidUsers")
    public void testUpdateUsername(String existingUsername, String newUsername, String userStoreDomain, String
            tenantDomain) throws Exception {

        UserDTO userDTO = buildUserDTO(existingUsername, newUsername, userStoreDomain, tenantDomain);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.isExistingUser(UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO.getUserStoreDomain())))
                .thenReturn(true);

        mockStatic(ForgetMeToolExecutor.class);
        doNothing().when(ForgetMeToolExecutor.class, "run", userDTO.getExistingUsername(), userDTO.getNewUsername()
                , userDTO.getUserStoreDomain(), userDTO.getTenantDomain(), MultitenantConstants.SUPER_TENANT_ID);

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        StatusDTO statusDTO = usernameUpdateServiceImpl.updateUsername(userDTO);

        usernameUpdateServiceImpl.unsetRealmService(null);

        Assert.assertEquals(STATUS_SUCCESS.getCode(), statusDTO.getCode());
        Assert.assertEquals(String.format(STATUS_SUCCESS.getMessage(), userDTO.getExistingUsername(), userDTO.getNewUsername()),
                statusDTO.getMessage());
    }

    @Test(dataProvider = "getInvalidUsers", expectedExceptions = UsernameUpdateException.class)
    public void testExceptionAtInvalidParameters(String existingUsername, String newUsername, String userStoreDomain, String
            tenantDomain) throws Exception {

        UserDTO userDTO = buildUserDTO(existingUsername, newUsername, userStoreDomain, tenantDomain);
        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionAtNonExistingUser() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.isExistingUser(UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO.getUserStoreDomain())))
                .thenReturn(false);

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionWhileRetrievingTenantID() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenThrow(new
                UserStoreException("Error while retrieving tenant id"));

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionWhileRetrievingNullRealm() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenReturn(null);

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionWhileRetrievingRealm() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenThrow(new UserStoreException
                ("Error while retrieving tenant realm"));

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionWhileVerifyingExistingUser() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.isExistingUser(UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO.getUserStoreDomain())))
                .thenThrow(new UserStoreException("Error while checking for existing user"));

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    @Test(expectedExceptions = UsernameUpdateException.class)
    public void testExceptionWhileUpdatingAccountDisableClaim() throws Exception {

        UserDTO userDTO = buildUserDTO("testuser1", "testuser11", UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME,
                MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        RealmService realmService = mock(RealmService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME)).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(realmService.getTenantUserRealm(MultitenantConstants.SUPER_TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.isExistingUser(UserCoreUtil.addDomainToName(userDTO.getExistingUsername(), userDTO.getUserStoreDomain())))
                .thenReturn(true);
        doThrow(new UserStoreException("Error while setting account disable claim")).when(userStoreManager,
                "setUserClaimValues", anyString(), anyMap(), anyString());

        UsernameUpdateServiceImpl usernameUpdateServiceImpl = new UsernameUpdateServiceImpl();
        usernameUpdateServiceImpl.setRealmService(realmService);

        usernameUpdateServiceImpl.updateUsername(userDTO);
    }

    private UserDTO buildUserDTO(String existingUsername, String newUsername, String userStoreDomain, String
            tenantDomain) {

        UserDTO userDTO = new UserDTO();
        userDTO.setExistingUsername(existingUsername);
        userDTO.setNewUsername(newUsername);
        userDTO.setUserStoreDomain(userStoreDomain);
        userDTO.setTenantDomain(tenantDomain);

        return userDTO;
    }
}
