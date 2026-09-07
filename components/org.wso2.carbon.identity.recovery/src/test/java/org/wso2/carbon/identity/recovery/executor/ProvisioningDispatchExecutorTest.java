/*
 * Copyright (c) 2026, WSO2 LLC. (https://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.recovery.executor;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.testng.annotations.DataProvider;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_RETRY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;

/**
 * Unit tests for {@link ProvisioningDispatchExecutor}.
 */
public class ProvisioningDispatchExecutorTest {

    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";

    private ProvisioningDispatchExecutor executor;
    private Executor userProvisioningExecutor;
    private Executor organizationProvisioningExecutor;

    @BeforeMethod
    public void setUp() {

        executor = new ProvisioningDispatchExecutor();
        userProvisioningExecutor = namedExecutor(USER_PROVISIONING_EXECUTOR);
        organizationProvisioningExecutor = namedExecutor(ORGANIZATION_PROVISIONING_EXECUTOR);

        IdentityRecoveryServiceDataHolder.getInstance().addFlowExecutor(userProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().addFlowExecutor(organizationProvisioningExecutor);
    }

    @AfterMethod
    public void tearDown() {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(userProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(organizationProvisioningExecutor);
    }

    @Test(description = "The executor name is referenced by string from persisted flows and must not change.")
    public void testExecutorName() {

        Assert.assertEquals(executor.getName(), "ProvisioningDispatchExecutor");
    }

    @Test(description = "Both executors run, and the user is provisioned before the organization.")
    public void testUserIsProvisionedBeforeOrganization() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_COMPLETE);
        org.mockito.InOrder inOrder = org.mockito.Mockito.inOrder(
                userProvisioningExecutor, organizationProvisioningExecutor);
        inOrder.verify(userProvisioningExecutor).execute(context);
        inOrder.verify(organizationProvisioningExecutor).execute(context);
    }

    @Test(dataProvider = "incompleteUserStatusProvider",
            description = "If user provisioning does not complete, the organization must not be created.")
    public void testOrganizationIsSkippedWhenUserProvisioningDoesNotComplete(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(status));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), status);
        verify(organizationProvisioningExecutor, never()).execute(any());
    }

    @DataProvider(name = "incompleteUserStatusProvider")
    public Object[][] incompleteUserStatusProvider() {

        return new Object[][]{
                {STATUS_RETRY},
                {STATUS_ERROR},
                {STATUS_USER_INPUT_REQUIRED}
        };
    }

    @Test(description = "A missing organization executor is a deployment problem, not a user error, and it "
            + "must be detected before a user is provisioned and left without an organization.")
    public void testMissingOrganizationExecutorReturnsErrorWithoutProvisioningUser() throws Exception {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(organizationProvisioningExecutor);
        FlowExecutionContext context = new FlowExecutionContext();

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(userProvisioningExecutor, never()).execute(any());
    }

    @Test(description = "Organization provisioning returns RETRY on a recoverable failure, bringing the flow "
            + "back to this node. The already provisioned user must not be provisioned a second time.")
    public void testUserIsNotProvisionedAgainOnRetry() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.getFlowUser().setUserId("dcaf39b8-4c8c-4a8d-9a3a-2f8c1b7f6e21");
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(userProvisioningExecutor, never()).execute(any());
        verify(organizationProvisioningExecutor).execute(context);
    }

    @Test(description = "The Executor contract does not promise a response, and the implementation behind "
            + "a name is not known here, so a null response is reported rather than propagated.")
    public void testNullResponseFromDispatchedExecutorReturnsError() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, null);

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(organizationProvisioningExecutor, never()).execute(any());
    }

    @Test(description = "A missing user executor fails before anything is provisioned.")
    public void testMissingUserExecutorReturnsError() throws Exception {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(userProvisioningExecutor);
        FlowExecutionContext context = new FlowExecutionContext();

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(organizationProvisioningExecutor, never()).execute(any());
    }

    private Executor namedExecutor(String name) {

        Executor mockExecutor = mock(Executor.class);
        when(mockExecutor.getName()).thenReturn(name);
        return mockExecutor;
    }

    private void stub(Executor mockExecutor, ExecutorResponse response) throws Exception {

        when(mockExecutor.execute(any())).thenReturn(response);
    }

    private ExecutorResponse response(String status) {

        ExecutorResponse response = new ExecutorResponse();
        response.setResult(status);
        return response;
    }
}
