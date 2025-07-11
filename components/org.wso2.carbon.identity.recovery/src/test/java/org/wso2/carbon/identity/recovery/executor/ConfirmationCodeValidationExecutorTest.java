/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.executor;

import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

@WithCarbonHome
public class ConfirmationCodeValidationExecutorTest {

    private static final String CONFIRMATION_CODE = "confirmationCode";
    private static final String USERNAME = "john";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_ID = "abc123";
    private static final int TENANT_ID = 1234;
    private ConfirmationCodeValidationExecutor executor;
    private MockedStatic<JDBCRecoveryDataStore> mockedJdbcStore;
    private MockedStatic<PrivilegedCarbonContext> mockedCarbonContext;
    private MockedStatic<IdentityTenantUtil> mockedTenantUtil;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;

    @BeforeMethod
    public void setUp() {

        executor = new ConfirmationCodeValidationExecutor();
        mockedJdbcStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedJdbcStore.close();
        mockedCarbonContext.close();
        mockedTenantUtil.close();
        mockedDataHolderStatic.close();
    }

    @Test
    public void testExecuteWithMissingConfirmationCode() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        when(context.getUserInputData()).thenReturn(userInputData);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_CLIENT_INPUT_REQUIRED);
        assertTrue(response.getRequiredData().contains(CONFIRMATION_CODE));
    }

    @Test
    public void testExecuteWithValidConfirmationCode() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.addClaims(new HashMap<>());
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(CONFIRMATION_CODE, "valid-code");

        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowUser()).thenReturn(flowUser);

        // Mock User and UserRecoveryData
        User mockUser = new User();
        mockUser.setUserName(USERNAME);
        mockUser.setTenantDomain(TENANT_DOMAIN);
        mockUser.setUserStoreDomain("PRIMARY");

        UserRecoveryData mockRecoveryData = mock(UserRecoveryData.class);
        when(mockRecoveryData.getUser()).thenReturn(mockUser);

        // Mock UserRecoveryDataStore
        UserRecoveryDataStore mockStore = mock(UserRecoveryDataStore.class);
        mockedJdbcStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockStore);
        when(mockStore.load(anyString())).thenReturn(mockRecoveryData);

        // Mock PrivilegedCarbonContext
        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext).thenReturn(carbonContext);
        when(carbonContext.getTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Mock IdentityTenantUtil
        mockedTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);

        // Mock DataHolder & UserStoreManager
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);

        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(anyString())).thenReturn(USER_ID);
        when(userStoreManager.getUserClaimValues(anyString(), any(String[].class), any())).thenReturn(Collections.emptyMap());

        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        // Mock ClaimMetadataManagementService
        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(dataHolder.getClaimMetadataManagementService()).thenReturn(claimMetadataManagementService);


        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        assertEquals(flowUser.getUsername(), "john");
        assertEquals(flowUser.getUserId(), USER_ID);
    }

    @Test
    public void testExecuteWithInvalidConfirmationCode() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(CONFIRMATION_CODE, "valid-code");
        when(context.getUserInputData()).thenReturn(userInputData);

        // Mock User and UserRecoveryData
        User mockUser = new User();
        mockUser.setUserName(USERNAME);
        mockUser.setTenantDomain("wrong-domain");
        mockUser.setUserStoreDomain("PRIMARY");

        UserRecoveryData mockRecoveryData = mock(UserRecoveryData.class);
        when(mockRecoveryData.getUser()).thenReturn(mockUser);

        UserRecoveryDataStore mockStore = mock(UserRecoveryDataStore.class);
        mockedJdbcStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockStore);
        when(mockStore.load(anyString())).thenReturn(mockRecoveryData);

        // PrivilegedCarbonContext
        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext).thenReturn(carbonContext);
        when(carbonContext.getTenantDomain()).thenReturn(TENANT_DOMAIN);

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);

        // Mock ClaimMetadataManagementService
        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(dataHolder.getClaimMetadataManagementService()).thenReturn(claimMetadataManagementService);


        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
        assertNotNull(response.getErrorMessage());
    }
}
