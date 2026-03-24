/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.governance.store;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCache;
import org.wso2.carbon.identity.governance.internal.cache.IdentityDataStoreCacheKey;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.jdbc.JDBCUserStoreManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

public class InMemoryIdentityDataStoreTest {

    @Mock
    UserStoreManager userStoreManager;

    @Mock
    UserIdentityDataStore userIdentityDataStore;

    @Mock
    RealmConfiguration realmConfiguration;

    @Mock
    UserIdentityClaim userIdentityClaim;

    @Mock
    PrivilegedCarbonContext privilegedCarbonContext;

    MockedStatic<IdentityUtil> mockedIdentityUtil;
    MockedStatic<UserCoreUtil> mockedUserCoreUtil;
    MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    MockedStatic<IdentityDataStoreCache> mockedIdentityDataStoreCache;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedUserCoreUtil = Mockito.mockStatic(UserCoreUtil.class);
        mockedPrivilegedCarbonContext = Mockito.mockStatic(PrivilegedCarbonContext.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedUserCoreUtil.close();
        mockedPrivilegedCarbonContext.close();
        if (mockedIdentityDataStoreCache != null) {
            mockedIdentityDataStoreCache.close();
            mockedIdentityDataStoreCache = null;
        }
    }

    @Test(testName = "testStore", description = "Test whether the map in UserIdentityClaim object containing " +
            "identity claims gets modified after this method is executed.")
    public void testStore() throws Exception {

        Map<String, String> identityClaimsMap = new HashMap<>();
        identityClaimsMap.put("keyOne", "valueOne");
        identityClaimsMap.put("keyTwo", "valueTwo");

        initPrivilegedCarbonContext();

        Map<String, String> identityClaimsMapClone = new HashMap<>(identityClaimsMap);

        userStoreManager = mock(JDBCUserStoreManager.class);
        userIdentityClaim = mock(UserIdentityClaim.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(InMemoryIdentityDataStore.class);
        privilegedCarbonContext = mock(PrivilegedCarbonContext.class);

        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(userStoreManager)).thenReturn(true);
        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("gayashan")).thenReturn("gayashan");
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);

        Mockito.when(userStoreManager.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        Mockito.when(userIdentityClaim.getUserName()).thenReturn("gayashan");
        Mockito.when(userIdentityClaim.getUserIdentityDataMap()).thenReturn(identityClaimsMap);
        Mockito.when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        Mockito.when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn("PRIMARY");

        InMemoryIdentityDataStore inMemoryIdentityDataStore = new InMemoryIdentityDataStore();
        inMemoryIdentityDataStore.store(userIdentityClaim, userStoreManager);
        assertEquals(identityClaimsMap, identityClaimsMapClone, "UserIdentity map of the UserIdentityClaim " +
                "object has been modified.");

    }

    // ── storeOnRead tests ─────────────────────────────────────────────────────

    @Test(description = "storeOnRead should silently do nothing when the DTO is null.")
    public void testStoreOnRead_withNullDTO_doesNothing() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        InMemoryIdentityDataStore store = new InMemoryIdentityDataStore();
        store.storeOnRead(null, mock(JDBCUserStoreManager.class));

        verify(mockCache, never()).addToCacheOnRead(any(), any(), anyInt());
        verify(mockCache, never()).addToCache(any(), any(), anyInt());
    }

    @Test(description = "storeOnRead should silently do nothing when the userName inside the DTO is null.")
    public void testStoreOnRead_withNullUserName_doesNothing() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        when(dto.getUserName()).thenReturn(null);

        InMemoryIdentityDataStore store = new InMemoryIdentityDataStore();
        store.storeOnRead(dto, mock(JDBCUserStoreManager.class));

        verify(mockCache, never()).addToCacheOnRead(any(), any(), anyInt());
        verify(mockCache, never()).addToCache(any(), any(), anyInt());
    }

    @Test(description = "storeOnRead should store directly via addToCacheOnRead when no entry exists in cache.")
    public void testStoreOnRead_noCachedEntry_storesDirectly() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        JDBCUserStoreManager usm = mock(JDBCUserStoreManager.class);
        RealmConfiguration rc = mock(RealmConfiguration.class);
        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        Map<String, String> claims = new HashMap<>();
        claims.put("claimKey", "claimVal");

        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("alice")).thenReturn("alice");
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(usm)).thenReturn(true);
        mockedIdentityUtil.when(() -> IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(usm)).thenReturn(true);

        when(dto.getUserName()).thenReturn("alice");
        when(dto.getUserIdentityDataMap()).thenReturn(claims);
        when(usm.getRealmConfiguration()).thenReturn(rc);
        when(usm.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(rc.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(null);

        new InMemoryIdentityDataStore().storeOnRead(dto, usm);

        verify(mockCache).addToCacheOnRead(any(IdentityDataStoreCacheKey.class), any(UserIdentityClaim.class),
                anyInt());
        verify(mockCache, never()).addToCache(any(), any(), anyInt());
    }

    @Test(description = "storeOnRead should merge claims with the cached entry and call addToCacheOnRead.")
    public void testStoreOnRead_existingCachedEntry_mergesAndCallsAddToCacheOnRead() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        JDBCUserStoreManager usm = mock(JDBCUserStoreManager.class);
        RealmConfiguration rc = mock(RealmConfiguration.class);

        Map<String, String> newClaims = new HashMap<>();
        newClaims.put("newKey", "newVal");
        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        when(dto.getUserName()).thenReturn("bob");
        when(dto.getUserIdentityDataMap()).thenReturn(newClaims);

        Map<String, String> existingClaims = new HashMap<>();
        existingClaims.put("existingKey", "existingVal");
        UserIdentityClaim cachedDto = mock(UserIdentityClaim.class);
        when(cachedDto.getUserIdentityDataMap()).thenReturn(existingClaims);

        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("bob")).thenReturn("bob");
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(usm)).thenReturn(true);
        mockedIdentityUtil.when(() -> IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(usm)).thenReturn(true);

        when(usm.getRealmConfiguration()).thenReturn(rc);
        when(usm.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(rc.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(cachedDto);

        new InMemoryIdentityDataStore().storeOnRead(dto, usm);

        // Existing map should have had newClaims merged in.
        assertEquals(existingClaims.get("newKey"), "newVal");
        verify(mockCache).addToCacheOnRead(any(IdentityDataStoreCacheKey.class), any(UserIdentityClaim.class),
                anyInt());
        verify(mockCache, never()).addToCache(any(), any(), anyInt());
    }

    @Test(description = "storeOnRead should lowercase the username when the user store is case-insensitive.")
    public void testStoreOnRead_caseInsensitiveUserStore_lowercasesUsername() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        JDBCUserStoreManager usm = mock(JDBCUserStoreManager.class);
        RealmConfiguration rc = mock(RealmConfiguration.class);
        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        Map<String, String> claims = new HashMap<>();

        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("Charlie")).thenReturn("Charlie");
        // Case-insensitive store: isUserStoreCaseSensitive returns false.
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(usm)).thenReturn(false);

        when(dto.getUserName()).thenReturn("Charlie");
        when(dto.getUserIdentityDataMap()).thenReturn(claims);
        when(usm.getRealmConfiguration()).thenReturn(rc);
        when(usm.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(rc.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(null);

        new InMemoryIdentityDataStore().storeOnRead(dto, usm);

        // Verify cache was called with the lowercased key.
        IdentityDataStoreCacheKey expectedKey = new IdentityDataStoreCacheKey("PRIMARY", "charlie");
        verify(mockCache).addToCacheOnRead(
                Mockito.eq(expectedKey),
                any(UserIdentityClaim.class), anyInt());
    }

    @Test(description = "storeOnRead should lowercase the username when case-sensitive cache keys are disabled.")
    public void testStoreOnRead_caseInsensitiveCacheKey_lowercasesUsername() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        JDBCUserStoreManager usm = mock(JDBCUserStoreManager.class);
        RealmConfiguration rc = mock(RealmConfiguration.class);
        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        Map<String, String> claims = new HashMap<>();

        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("Dave")).thenReturn("Dave");
        // Store is case-sensitive, but cache key is not.
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(usm)).thenReturn(true);
        mockedIdentityUtil.when(() -> IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(usm)).thenReturn(false);

        when(dto.getUserName()).thenReturn("Dave");
        when(dto.getUserIdentityDataMap()).thenReturn(claims);
        when(usm.getRealmConfiguration()).thenReturn(rc);
        when(usm.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(rc.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(null);

        new InMemoryIdentityDataStore().storeOnRead(dto, usm);

        IdentityDataStoreCacheKey expectedKey = new IdentityDataStoreCacheKey("PRIMARY", "dave");
        verify(mockCache).addToCacheOnRead(
                Mockito.eq(expectedKey),
                any(UserIdentityClaim.class), anyInt());
    }

    @Test(description = "storeOnRead should NOT call addToCache — only addToCacheOnRead is allowed.")
    public void testStoreOnRead_neverCallsAddToCache() throws Exception {

        IdentityDataStoreCache mockCache = mock(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache = Mockito.mockStatic(IdentityDataStoreCache.class);
        mockedIdentityDataStoreCache.when(IdentityDataStoreCache::getInstance).thenReturn(mockCache);

        JDBCUserStoreManager usm = mock(JDBCUserStoreManager.class);
        RealmConfiguration rc = mock(RealmConfiguration.class);
        UserIdentityClaim dto = mock(UserIdentityClaim.class);
        Map<String, String> claims = new HashMap<>();
        claims.put("k", "v");

        mockedUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName("eve")).thenReturn("eve");
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(usm)).thenReturn(true);
        mockedIdentityUtil.when(() -> IdentityUtil.isUseCaseSensitiveUsernameForCacheKeys(usm)).thenReturn(true);

        when(dto.getUserName()).thenReturn("eve");
        when(dto.getUserIdentityDataMap()).thenReturn(claims);
        when(usm.getRealmConfiguration()).thenReturn(rc);
        when(usm.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(rc.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME)).thenReturn("PRIMARY");

        // Test both branches: no cached entry, and with cached entry.
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(null);
        new InMemoryIdentityDataStore().storeOnRead(dto, usm);
        verify(mockCache, never()).addToCache(any(), any(), anyInt());

        Map<String, String> cachedClaims = new HashMap<>();
        UserIdentityClaim cachedDto = mock(UserIdentityClaim.class);
        when(cachedDto.getUserIdentityDataMap()).thenReturn(cachedClaims);
        when(mockCache.getValueFromCache(any(IdentityDataStoreCacheKey.class), anyInt())).thenReturn(cachedDto);
        new InMemoryIdentityDataStore().storeOnRead(dto, usm);
        verify(mockCache, never()).addToCache(any(), any(), anyInt());
    }

    @Test(description = "The default storeOnRead in UserIdentityDataStore should be a no-op with no exception.")
    public void testDefaultStoreOnRead_inBaseClass_doesNothing() throws Exception {

        // Concrete minimal subclass to test the base default.
        UserIdentityDataStore baseStore = new UserIdentityDataStore() {
            @Override
            public void store(UserIdentityClaim dto, org.wso2.carbon.user.api.UserStoreManager usm)
                    throws org.wso2.carbon.identity.base.IdentityException { }
            @Override
            public UserIdentityClaim load(String userName, org.wso2.carbon.user.api.UserStoreManager usm) {
                return null;
            }
            @Override
            public void remove(String userName, org.wso2.carbon.user.api.UserStoreManager usm)
                    throws org.wso2.carbon.identity.base.IdentityException { }
        };

        // Should complete without throwing.
        baseStore.storeOnRead(mock(UserIdentityClaim.class), mock(UserStoreManager.class));
    }

    // ── store tests ───────────────────────────────────────────────────────────

    public static void initPrivilegedCarbonContext(String tenantDomain, int tenantID, String userName) throws Exception {
        String carbonHome = Paths.get(System.getProperty("user.dir"), "target").toString();
        System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
        PrivilegedCarbonContext.startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(tenantDomain);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(tenantID);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setUsername(userName);
    }

    public static void initPrivilegedCarbonContext(String tenantDomain, String userName) throws Exception {
        int tenantID = org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_ID;
        initPrivilegedCarbonContext(tenantDomain, tenantID, userName);
    }

    public static void initPrivilegedCarbonContext(String tenantDomain) throws Exception {
        int tenantID = org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_ID;
        String userName = "testUser";

        initPrivilegedCarbonContext(tenantDomain, tenantID, userName);
    }

    public static void initPrivilegedCarbonContext() throws Exception {
        String tenantDomain = org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        int tenantID = org.wso2.carbon.base.MultitenantConstants.SUPER_TENANT_ID;
        String userName = "testUser";

        initPrivilegedCarbonContext(tenantDomain, tenantID, userName);
    }
}

