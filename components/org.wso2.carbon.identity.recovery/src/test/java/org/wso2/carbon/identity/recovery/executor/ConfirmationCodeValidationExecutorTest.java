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
import org.wso2.carbon.identity.application.common.model.User;
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

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public class ConfirmationCodeValidationExecutorTest {

    private static final String CONFIRMATION_CODE = "confirmationCode";
    private static final String USERNAME = "john";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_ID = "abc123";
    private static final int TENANT_ID = 1234;
    private ConfirmationCodeValidationExecutor executor;
    private MockedStatic<NotificationPasswordRecoveryManager> mockedRecoveryManagerStatic;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() {

        executor = new ConfirmationCodeValidationExecutor();
        mockedRecoveryManagerStatic = mockStatic(NotificationPasswordRecoveryManager.class);
        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryManagerStatic.close();
        mockedDataHolderStatic.close();
        mockedIdentityTenantUtil.close();
    }

    @Test
    public void testExecuteWithMissingConfirmationCode() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(CONFIRMATION_CODE, null);
        when(context.getUserInputData()).thenReturn(userInputData);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_CLIENT_INPUT_REQUIRED);
        assertTrue(response.getRequiredData().contains(CONFIRMATION_CODE));
    }

    @Test
    public void testExecuteWithValidConfirmationCode() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(CONFIRMATION_CODE, "valid-code");
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowUser()).thenReturn(flowUser);

        User mockUser = new User();
        mockUser.setUserName(USERNAME);
        mockUser.setTenantDomain(TENANT_DOMAIN);

        NotificationPasswordRecoveryManager mockRecoveryManager = mock(NotificationPasswordRecoveryManager.class);
        when(mockRecoveryManager.getValidatedUser("valid-code", null)).thenReturn(mockUser);
        mockedRecoveryManagerStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(mockRecoveryManager);

        // Mock user store resolution
        IdentityRecoveryServiceDataHolder mockDataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService mockRealmService = mock(RealmService.class);
        UserRealm mockUserRealm = mock(UserRealm.class);
        UserStoreManager mockStoreManager = mock(AbstractUserStoreManager.class);

        when(mockDataHolder.getRealmService()).thenReturn(mockRealmService);
        when(mockRealmService.getTenantUserRealm(anyInt())).thenReturn(mockUserRealm);
        when(mockUserRealm.getUserStoreManager()).thenReturn(mockStoreManager);
        when(((AbstractUserStoreManager) mockStoreManager).getUserIDFromUserName(USERNAME)).thenReturn(USER_ID);
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(mockDataHolder);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        assertEquals(flowUser.getUsername(), USERNAME);
        assertEquals(flowUser.getUserId(), USER_ID);
    }

    @Test
    public void testExecuteWithInvalidConfirmationCode() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(CONFIRMATION_CODE, "invalid-code");
        when(context.getUserInputData()).thenReturn(userInputData);

        NotificationPasswordRecoveryManager mockRecoveryManager = mock(NotificationPasswordRecoveryManager.class);
        when(mockRecoveryManager.getValidatedUser("invalid-code", null))
                .thenThrow(new IdentityRecoveryException("Invalid code"));
        mockedRecoveryManagerStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(mockRecoveryManager);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_ERROR);
        assertNotNull(response.getErrorMessage());
    }
}
