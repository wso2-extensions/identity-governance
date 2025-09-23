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
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.RECOVERY_SCENARIO;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.USER;

/**
 * Unit tests for {@link PasswordProvisioningExecutor}.
 */
@WithCarbonHome
public class PasswordProvisioningExecutorTest {

    private static final String PASSWORD_KEY = "password";
    private static final String USERNAME = "test@wso2.com";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_ID = "abc123";
    private static final String PASSWORD_RECOVERY = "PASSWORD_RECOVERY";
    private static final int TENANT_ID = 1234;

    private PasswordProvisioningExecutor executor;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() {

        executor = new PasswordProvisioningExecutor();
        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedDataHolderStatic.close();
        mockedIdentityTenantUtil.close();
    }

    @Test
    public void testExecuteWithMissingPasswordAndCredentials() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getUserInputData()).thenReturn(Collections.emptyMap());

        FlowUser flowUser = new FlowUser();
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED);
        assertTrue(response.getRequiredData().contains(PASSWORD_KEY));
    }

    @Test(dataProvider = "flowTypes")
    public void testExecuteWithValidData(String flowType) throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        flowUser.addClaims(new HashMap<>());
        flowUser.setUserStoreDomain("PRIMARY");


        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, "Password123");
        userInputData.put("http://wso2.org/claims/givenname", "John");
        userInputData.put(CONFIRMATION_CODE_INPUT, "valid-code");

        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowType()).thenReturn(flowType);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getFlowUser()).thenReturn(flowUser);

        if (Flow.Name.INVITED_USER_REGISTRATION.name().equals(flowType)) {
            when(context.getProperty(CONFIRMATION_CODE_INPUT)).thenReturn("valid-code");

            User user = new User();
            user.setUserName(USERNAME);
            user.setTenantDomain(TENANT_DOMAIN);
            user.setUserStoreDomain("PRIMARY");
            when(context.getProperty(USER)).thenReturn(user);
            when(context.getProperty(RECOVERY_SCENARIO)).thenReturn("SCENARIO");

            // Mock IdentityTenantUtil for INVITED_USER_REGISTRATION flow
            mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN))
                    .thenReturn(TENANT_ID);
        }

        // Mocks for data holder and user store
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager storeManager = mock(AbstractUserStoreManager.class);
        TenantManager tenantManager = mock(TenantManager.class);

        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);

        if (Flow.Name.INVITED_USER_REGISTRATION.name().equals(flowType)) {
            when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
            when(userRealm.getUserStoreManager()).thenReturn(storeManager);
        } else {
            // For PASSWORD_RECOVERY flow
            when(realmService.getTenantManager()).thenReturn(tenantManager);
            when(tenantManager.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
            when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
            when(userRealm.getUserStoreManager()).thenReturn(storeManager);
            when(storeManager.getSecondaryUserStoreManager(anyString())).thenReturn(storeManager);
        }

        when(storeManager.getUserIDFromUserName(anyString())).thenReturn(USER_ID);

        IdentityEventService eventServiceMock = mock(IdentityEventService.class);
        when(dataHolder.getIdentityEventService()).thenReturn(eventServiceMock);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        if (Flow.Name.INVITED_USER_REGISTRATION.name().equals(flowType)) {
            assertEquals(flowUser.getUserId(), USER_ID);
            assertTrue(flowUser.getClaims().containsKey("http://wso2.org/claims/givenname"));
            assertEquals(flowUser.getClaims().get("http://wso2.org/claims/givenname"), "John");
        }
        verify(storeManager).updateCredentialByAdmin(eq(USERNAME), any(char[].class));
    }

    @Test
    public void testExecuteWithExceptionInUpdate() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        flowUser.setUserStoreDomain("PRIMARY");

        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, "Password123");

        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowType()).thenReturn(PASSWORD_RECOVERY);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getFlowUser()).thenReturn(flowUser);

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager storeManager = mock(AbstractUserStoreManager.class);
        TenantManager tenantManager = mock(TenantManager.class);

        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(storeManager);
        when(storeManager.getSecondaryUserStoreManager(anyString())).thenReturn(storeManager);

        doThrow(new UserStoreException("Error while updating credential"))
                .when(storeManager).updateCredentialByAdmin(anyString(), any(char[].class));

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
    }

    @Test
    public void testHandleAskPasswordFlowWithException() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        flowUser.addClaims(new HashMap<>());
        flowUser.setUserStoreDomain("PRIMARY");

        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, "Password123");
        userInputData.put(CONFIRMATION_CODE_INPUT, "valid-code");

        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowType()).thenReturn(Flow.Name.INVITED_USER_REGISTRATION.name());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getProperty(CONFIRMATION_CODE_INPUT)).thenReturn("valid-code");

        User user = new User();
        user.setUserName(USERNAME);
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain("PRIMARY");
        when(context.getProperty(USER)).thenReturn(user);
        when(context.getProperty(RECOVERY_SCENARIO)).thenReturn("SCENARIO");

        // Mock IdentityTenantUtil
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN))
                .thenReturn(TENANT_ID);

        // Mocks for data holder and user store
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager storeManager = mock(AbstractUserStoreManager.class);

        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(storeManager);

        IdentityEventService eventServiceMock = mock(IdentityEventService.class);
        when(dataHolder.getIdentityEventService()).thenReturn(eventServiceMock);

        // Throw UserStoreException to trigger the catch block
        doThrow(new UserStoreException("Error while updating credential"))
                .when(storeManager).updateCredentialByAdmin(anyString(), any(char[].class));

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
    }

    @DataProvider(name = "flowTypes")
    public Object[][] provideFlowTypes() {

        return new Object[][]{
                {Flow.Name.INVITED_USER_REGISTRATION.name()},
                { PASSWORD_RECOVERY }
        };
    }
}
