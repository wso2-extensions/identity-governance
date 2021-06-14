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

package org.wso2.carbon.identity.user.export.core.internal.service.impl;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.CarbonException;
import org.wso2.carbon.core.util.AnonymousSessionUtil;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.registry.core.service.RegistryService;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.Claim;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class BasicUserInformationProviderTest {

    public static final String USERNAME_CLAIM_URI = "http://wso2.org/claims/username";
    public static final String GIVEN_NAME_CLAIM_URI = "http://wso2.org/claims/givenname";
    public static final String LAST_NAME_CLAIM_URI = "http://wso2.org/claims/lastname";
    public static final String USERNAME_CLAIM_VALUE = "username1";
    public static final String GIVEN_NAME_CLAIM_VALUE = "givenName1";
    public static final String LAST_NAME_CLAIM_VALUE = "lastName1";
    private MockedStatic<AnonymousSessionUtil> mockedAnonymousSessionUtil;

    @BeforeMethod
    public void setUp() {

        mockedAnonymousSessionUtil = Mockito.mockStatic(AnonymousSessionUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedAnonymousSessionUtil.close();
    }

    @Test
    public void testGetUserAttributes() throws Exception {

        RealmService realmService = mock(RealmService.class);
        RegistryService registryService = mock(RegistryService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        UserStoreManager secUserStoreManager = mock(UserStoreManager.class);

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        mockedAnonymousSessionUtil.when(() -> AnonymousSessionUtil
                .getRealmByTenantDomain(any(RegistryService.class), any(RealmService.class), anyString()))
                .thenReturn(userRealm);
        Claim[] claims = getClaims();

        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(claims);

        BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
        basicUserInformationProvider.setRealmService(realmService);
        basicUserInformationProvider.setRegistryService(registryService);
        UserInformationDTO userAttributesObj = basicUserInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
        if (userAttributesObj != null && userAttributesObj.getData() instanceof Map) {
            Map userAttributes = (Map) userAttributesObj.getData();
            Assert.assertEquals(userAttributes.get(USERNAME_CLAIM_URI), USERNAME_CLAIM_VALUE);
            Assert.assertEquals(userAttributes.get(GIVEN_NAME_CLAIM_URI), GIVEN_NAME_CLAIM_VALUE);
            Assert.assertEquals(userAttributes.get(LAST_NAME_CLAIM_URI), LAST_NAME_CLAIM_VALUE);
        } else {
            Assert.fail();
        }
    }

    @Test
    public void testGetUserAttributesEmpty() throws Exception {

        RealmService realmService = mock(RealmService.class);
        RegistryService registryService = mock(RegistryService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        UserStoreManager secUserStoreManager = mock(UserStoreManager.class);

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        mockedAnonymousSessionUtil.when(() -> AnonymousSessionUtil
                .getRealmByTenantDomain(any(RegistryService.class), any(RealmService.class), anyString()))
                .thenReturn(userRealm);

        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(null);

        BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
        basicUserInformationProvider.setRealmService(realmService);
        basicUserInformationProvider.setRegistryService(registryService);
        UserInformationDTO userAttributesObj = basicUserInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
        if (userAttributesObj.isInformationAvailable()) {
            Assert.fail();
        }
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetUserAttributesExceptionOnGetRealmByTenantDomain() throws Exception {

        RealmService realmService = mock(RealmService.class);
        RegistryService registryService = mock(RegistryService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        UserStoreManager secUserStoreManager = mock(UserStoreManager.class);

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        mockedAnonymousSessionUtil.when(() -> AnonymousSessionUtil
                .getRealmByTenantDomain(any(RegistryService.class), any(RealmService.class), anyString()))
                .thenThrow(new CarbonException("Mock Exception"));

        Claim[] claims = getClaims();

        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(claims);

        BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
        basicUserInformationProvider.setRealmService(realmService);
        basicUserInformationProvider.setRegistryService(registryService);
        basicUserInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetUserAttributesExceptionOnGetUserStoreManager() throws Exception {

        RealmService realmService = mock(RealmService.class);
        RegistryService registryService = mock(RegistryService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        UserStoreManager secUserStoreManager = mock(UserStoreManager.class);

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenThrow(new UserStoreException());

        mockedAnonymousSessionUtil.when(() -> AnonymousSessionUtil
                .getRealmByTenantDomain(any(RegistryService.class), any(RealmService.class), anyString()))
                .thenReturn(userRealm);

        Claim[] claims = getClaims();

        when(secUserStoreManager.getUserClaimValues(USERNAME_CLAIM_VALUE, null)).thenReturn(claims);

        BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
        basicUserInformationProvider.setRealmService(realmService);
        basicUserInformationProvider.setRegistryService(registryService);
        basicUserInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }

    @Test(expectedExceptions = UserExportException.class)
    public void testGetUserAttributesExceptionOnGetUserClaimValues() throws Exception {

        RealmService realmService = mock(RealmService.class);
        RegistryService registryService = mock(RegistryService.class);
        TenantManager tenantManager = mock(TenantManager.class);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        UserStoreManager secUserStoreManager = mock(UserStoreManager.class);

        when(userStoreManager.getSecondaryUserStoreManager(anyString())).thenReturn(secUserStoreManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        mockedAnonymousSessionUtil.when(() -> AnonymousSessionUtil
                .getRealmByTenantDomain(any(RegistryService.class), any(RealmService.class), anyString()))
                .thenReturn(userRealm);

        when(secUserStoreManager.getUserClaimValues(anyString(), isNull())).thenThrow(new UserStoreException());

        BasicUserInformationProvider basicUserInformationProvider = new BasicUserInformationProvider();
        basicUserInformationProvider.setRealmService(realmService);
        basicUserInformationProvider.setRegistryService(registryService);
        basicUserInformationProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME, -1234);
    }

    private Claim[] getClaims() {

        Claim[] claims = new Claim[3];
        Claim claim1 = new Claim();
        claim1.setClaimUri(USERNAME_CLAIM_URI);
        claim1.setValue(USERNAME_CLAIM_VALUE);
        claims[0] = claim1;

        Claim claim2 = new Claim();
        claim2.setClaimUri(GIVEN_NAME_CLAIM_URI);
        claim2.setValue(GIVEN_NAME_CLAIM_VALUE);
        claims[1] = claim2;

        Claim claim3 = new Claim();
        claim3.setClaimUri(LAST_NAME_CLAIM_URI);
        claim3.setValue(LAST_NAME_CLAIM_VALUE);
        claims[2] = claim3;
        return claims;
    }
}
