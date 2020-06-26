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
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.model.UserIdentityClaim;
import org.wso2.carbon.identity.governance.store.InMemoryIdentityDataStore;
import org.wso2.carbon.identity.governance.store.UserIdentityDataStore;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.jdbc.JDBCUserStoreManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertEquals;

@PrepareForTest({IdentityUtil.class, UserCoreUtil.class, PrivilegedCarbonContext.class})
@PowerMockIgnore({"javax.net.*", "javax.security.*", "javax.crypto.*", "javax.xml.*", "javax.management.*"
      //  , "org.wso2.carbon.context.*"
})
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

    @ObjectFactory
    public IObjectFactory getObjectFactory() {
        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

    @Test(testName = "testStore", description = "Test whether the map in UserIdentityClaim object containing " +
            "identity claims gets modified after this method is executed.")
    public void testStore() throws Exception {

        Map<String, String> identityClaimsMap = new HashMap<>();
        identityClaimsMap.put("keyOne", "valueOne");
        identityClaimsMap.put("keyTwo", "valueTwo");

       initPrivilegedCarbonContext();


        Map<String, String> identityClaimsMapClone = new HashMap<>(identityClaimsMap);

        mockStatic(IdentityUtil.class);
        mockStatic(UserCoreUtil.class);
        mockStatic(PrivilegedCarbonContext.class);

        userStoreManager = mock(JDBCUserStoreManager.class);
        userIdentityClaim = mock(UserIdentityClaim.class);
        realmConfiguration = mock(RealmConfiguration.class);
        userIdentityDataStore = mock(InMemoryIdentityDataStore.class);
        privilegedCarbonContext = mock(PrivilegedCarbonContext.class);

        when(IdentityUtil.isUserStoreCaseSensitive(userStoreManager)).thenReturn(true);
        when(UserCoreUtil.removeDomainFromName("gayashan")).thenReturn("gayashan");
        when(PrivilegedCarbonContext.getThreadLocalCarbonContext()).thenReturn(privilegedCarbonContext);

        when(userStoreManager.getTenantId()).thenReturn(MultitenantConstants.SUPER_TENANT_ID);
        when(userIdentityClaim.getUserName()).thenReturn("gayashan");
        when(userIdentityClaim.getUserIdentityDataMap()).thenReturn(identityClaimsMap);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn("PRIMARY");

        InMemoryIdentityDataStore inMemoryIdentityDataStore = new InMemoryIdentityDataStore();
        inMemoryIdentityDataStore.store(userIdentityClaim, userStoreManager);
        assertEquals(identityClaimsMap, identityClaimsMapClone, "UserIdentity map of the UserIdentityClaim " +
                "object has been modified.");

    }

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

