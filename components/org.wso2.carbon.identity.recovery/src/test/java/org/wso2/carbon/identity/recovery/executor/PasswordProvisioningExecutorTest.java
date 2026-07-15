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
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for {@link PasswordProvisioningExecutor}.
 * <p>
 * The executor no longer updates the credential. It only captures the supplied password into the flow user so
 * that the downstream {@link UserProvisioningExecutor} can provision it. These tests cover that capture
 * behaviour and the metadata methods.
 */
@WithCarbonHome
public class PasswordProvisioningExecutorTest {

    private static final String PASSWORD_KEY = "password";
    private static final String PASSWORD_VALUE = "Password123";

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

        List<String> initiationData = executor.getInitiationData();
        assertTrue(initiationData.contains(PASSWORD_KEY));
    }

    @Test
    public void testRollback() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        assertNull(executor.rollback(context));
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

    @Test
    public void testExecuteCapturesPasswordFromUserInput() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, PASSWORD_VALUE);
        when(context.getUserInputData()).thenReturn(userInputData);

        FlowUser flowUser = new FlowUser();
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        // The password is captured into the flow user so the downstream executor can provision it.
        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        Map<String, char[]> credentials = flowUser.getUserCredentials();
        assertEquals(new String(credentials.get(PASSWORD_KEY)), PASSWORD_VALUE);
    }

    @Test
    public void testExecuteWithExistingCredentialsAndNoUserInput() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getUserInputData()).thenReturn(Collections.emptyMap());

        FlowUser flowUser = new FlowUser();
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD_VALUE.toCharArray());
        flowUser.setUserCredentials(credentials);
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        // Credentials already captured (e.g. by a previous step); no further input is required.
        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
    }

    @Test
    public void testExecuteWithCredentialsLackingPasswordRequiresInput() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getUserInputData()).thenReturn(Collections.emptyMap());

        FlowUser flowUser = new FlowUser();
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put("otherCredential", "value".toCharArray());
        flowUser.setUserCredentials(credentials);
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        // Password availability is decided by the password entry itself, not by map occupancy.
        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED);
        assertTrue(response.getRequiredData().contains(PASSWORD_KEY));
    }

    @Test
    public void testExecuteCapturePreservesOtherCredentialEntries() {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(PASSWORD_KEY, PASSWORD_VALUE);
        when(context.getUserInputData()).thenReturn(userInputData);

        FlowUser flowUser = new FlowUser();
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put("otherCredential", "value".toCharArray());
        flowUser.setUserCredentials(credentials);
        when(context.getFlowUser()).thenReturn(flowUser);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
        // Capturing the password must not clear other credential entries already on the flow user.
        assertEquals(new String(flowUser.getUserCredentials().get(PASSWORD_KEY)), PASSWORD_VALUE);
        assertEquals(new String(flowUser.getUserCredentials().get("otherCredential")), "value");
    }

    @Test
    public void testExecuteDoesNotTouchUserStore() {

        try (MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolder =
                     mockStatic(IdentityRecoveryServiceDataHolder.class)) {
            FlowExecutionContext context = mock(FlowExecutionContext.class);
            Map<String, String> userInputData = new HashMap<>();
            userInputData.put(PASSWORD_KEY, PASSWORD_VALUE);
            when(context.getUserInputData()).thenReturn(userInputData);
            when(context.getFlowType()).thenReturn("PASSWORD_RECOVERY");
            when(context.getFlowUser()).thenReturn(new FlowUser());

            ExecutorResponse response = executor.execute(context);

            // Capture-only contract: even for recovery flows this executor never reaches the user store;
            // the downstream UserProvisioningExecutor performs the credential update.
            assertEquals(response.getResult(), Constants.ExecutorStatus.STATUS_COMPLETE);
            mockedDataHolder.verifyNoInteractions();
        }
    }
}
