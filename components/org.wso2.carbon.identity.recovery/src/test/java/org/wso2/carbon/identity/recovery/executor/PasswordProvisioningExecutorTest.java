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

import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for {@link PasswordProvisioningExecutor}.
 * <p>
 * The executor only captures the submitted password onto the flow user; the credential is persisted later by the
 * terminal {@code UserProvisioningExecutor}. These tests assert that capture-only behavior.
 */
@WithCarbonHome
public class PasswordProvisioningExecutorTest {

    private static final String PASSWORD_KEY = "password";
    private static final String PASSWORD_RECOVERY = "PASSWORD_RECOVERY";

    private PasswordProvisioningExecutor executor;

    @BeforeMethod
    public void setUp() {

        executor = new PasswordProvisioningExecutor();
    }

    @Test
    public void testGetName() {

        assertEquals(executor.getName(), "PasswordProvisioningExecutor");
    }

    @Test
    public void testGetAMRValue() {

        assertEquals(executor.getAMRValue(), "BasicAuthenticator");
    }

    @Test
    public void testGetInitiationData() {

        assertNotNull(executor.getInitiationData());
        assertTrue(executor.getInitiationData().contains(PASSWORD_KEY));
    }

    @Test
    public void testRollback() {

        assertNull(executor.rollback(mock(FlowExecutionContext.class)));
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
    public void testExecuteCapturesPasswordOntoFlowUser(String flowType) {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();

        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, "Password123");

        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getFlowType()).thenReturn(flowType);
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        // Regardless of the flow type, the executor only captures the credential and completes; it does not
        // update the user store.
        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        assertNotNull(flowUser.getUserCredentials().get(PASSWORD_KEY));
        assertEquals(new String(flowUser.getUserCredentials().get(PASSWORD_KEY)), "Password123");
    }

    @Test
    public void testExecuteUsesExistingCredentialsWhenNoInput() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, "Existing123".toCharArray());
        flowUser.setUserCredentials(credentials);

        when(context.getUserInputData()).thenReturn(Collections.emptyMap());
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
    }

    @DataProvider(name = "flowTypes")
    public Object[][] provideFlowTypes() {

        return new Object[][]{
                {Flow.Name.INVITED_USER_REGISTRATION.name()},
                {PASSWORD_RECOVERY},
                {"REGISTRATION"}
        };
    }
}
