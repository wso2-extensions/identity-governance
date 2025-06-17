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
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class PasswordProvisioningExecutorTest {

    private static final String CONFIRMATION_CODE = "confirmationCode";
    private static final String PASSWORD_KEY = "password";
    private static final String USERNAME = "test@wso2.com";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_ID = "abc123";
    private static final String ASK_PASSWORD = "ASK_PASSWORD";
    private static final String PASSWORD_RECOVERY = "PASSWORD_RECOVERY";
    private static final Map<String, String> userInputData = new HashMap<>();
    private static final int TENANT_ID = 1234;
    private PasswordProvisioningExecutor executor;
    private MockedStatic<NotificationPasswordRecoveryManager> mockedRecoveryStatic;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() {

        executor = new PasswordProvisioningExecutor();
        mockedRecoveryStatic = mockStatic(NotificationPasswordRecoveryManager.class);
        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryStatic.close();
        mockedDataHolderStatic.close();
        mockedIdentityTenantUtil.close();
    }

    @DataProvider(name = "flowTypes")
    public Object[][] provideFlowTypes() {
        return new Object[][] {
                { ASK_PASSWORD },
                { PASSWORD_RECOVERY }
        };
    }

    @Test
    public void testExecuteWithMissingConfirmationCode() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getFlowType()).thenReturn(ASK_PASSWORD);
        userInputData.put(PASSWORD_KEY, "Password123");
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getProperty(CONFIRMATION_CODE)).thenReturn(null);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
        assertTrue(response.getErrorMessage().contains("Confirmation code is not provided"));
    }

    @Test
    public void testExecuteWithMissingPasswordAndCredentials() throws Exception {

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
        when(context.getFlowType()).thenReturn(flowType);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getFlowUser()).thenReturn(flowUser);
        userInputData.put(PASSWORD_KEY, "Password123");
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getProperty(CONFIRMATION_CODE)).thenReturn("valid-code");

        // Mocks for Recovery Manager and User Store
        NotificationPasswordRecoveryManager recoveryManager = mock(NotificationPasswordRecoveryManager.class);
        mockedRecoveryStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(recoveryManager);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager storeManager = mock(AbstractUserStoreManager.class);

        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(storeManager);
        when(storeManager.getUserIDFromUserName(USERNAME)).thenReturn(USER_ID);
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);

        ExecutorResponse response = executor.execute(context);

        if (flowType.equals(ASK_PASSWORD)) {
            assertEquals(flowUser.getUserId(), USER_ID);
        }
        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
    }

    @Test
    public void testExecuteWithExceptionInPasswordUpdate() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getFlowType()).thenReturn(ASK_PASSWORD);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getFlowUser()).thenReturn(flowUser);
        userInputData.put(PASSWORD_KEY, "Password123");
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getProperty(CONFIRMATION_CODE)).thenReturn("valid-code");

        NotificationPasswordRecoveryManager recoveryManager = mock(NotificationPasswordRecoveryManager.class);
        doThrow(new IdentityRecoveryException("Simulated failure"))
                .when(recoveryManager).updatePassword(anyString(), anyString(), any());
        mockedRecoveryStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(recoveryManager);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager storeManager = mock(UserStoreManager.class);

        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(storeManager);
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
        assertTrue(response.getErrorMessage().contains("Simulated failure"));
    }
}
