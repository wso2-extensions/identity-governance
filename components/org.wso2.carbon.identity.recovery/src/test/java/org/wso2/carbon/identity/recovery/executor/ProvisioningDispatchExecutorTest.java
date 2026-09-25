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
import org.mockito.InOrder;
import org.mockito.MockedStatic;
import org.testng.annotations.DataProvider;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineServerException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.mgt.model.ExecutorDTO;
import org.wso2.carbon.identity.flow.mgt.model.NodeConfig;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;

import java.nio.file.Paths;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_RETRY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;

/**
 * Unit tests for {@link ProvisioningDispatchExecutor}.
 */
public class ProvisioningDispatchExecutorTest {

    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";
    private static final String PROVISION_TARGET = "provisionTarget";
    private static final String NEW_ORGANIZATION = "NEW_ORGANIZATION";
    private static final String NEW_ORG_HANDLE = "acmecorporation";
    private static final String CURRENT_TENANT_DOMAIN = "carbon.super";

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
        verify(userProvisioningExecutor, never()).rollback(any());
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

    @Test(description = "When the flow returns to this node after the user was provisioned, the user must not "
            + "be provisioned a second time.")
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

    @Test(description = "Without the provision target set, the user is provisioned in the organization "
            + "the flow is executing in, which is what every flow published so far does.")
    public void testUserIsProvisionedInCurrentOrganizationByDefault() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setCurrentNode(nodeWithProvisionTarget(null));
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        executor.execute(context);

        InOrder inOrder = org.mockito.Mockito.inOrder(userProvisioningExecutor, organizationProvisioningExecutor);
        inOrder.verify(userProvisioningExecutor).execute(context);
        inOrder.verify(organizationProvisioningExecutor).execute(context);

    }

    @Test(description = "Targeting the new organization creates it first, then provisions the user inside "
            + "it, so the user never gets a record in the organization the flow is executing in.")
    public void testUserIsProvisionedInsideTheNewOrganization() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));

        try (MockedStatic<PrivilegedCarbonContext> carbonContext = mockedCarbonContext()) {
            ExecutorResponse response = executor.execute(context);

            Assert.assertEquals(response.getResult(), STATUS_COMPLETE);
            InOrder inOrder = org.mockito.Mockito.inOrder(organizationProvisioningExecutor,
                    userProvisioningExecutor);
            inOrder.verify(organizationProvisioningExecutor).execute(context);
            inOrder.verify(userProvisioningExecutor).execute(context);
            carbonContext.verify(PrivilegedCarbonContext::endTenantFlow);
            verify(organizationProvisioningExecutor, never()).rollback(any());
        }
    }

    @Test(description = "The user must be provisioned against the new organization's tenant, not the one "
            + "the flow is executing in.")
    public void testUserIsProvisionedAgainstTheNewOrganizationTenant() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        when(userProvisioningExecutor.execute(any())).thenAnswer(invocation -> {
            FlowExecutionContext passed = invocation.getArgument(0);
            Assert.assertEquals(passed.getTenantDomain(), NEW_ORG_HANDLE,
                    "User provisioning reads the tenant from the flow context.");
            return response(STATUS_COMPLETE);
        });

        try (MockedStatic<PrivilegedCarbonContext> ignored = mockedCarbonContext()) {
            executor.execute(context);
        }

        Assert.assertEquals(context.getTenantDomain(), CURRENT_TENANT_DOMAIN,
                "The flow context tenant must be restored once provisioning is done.");
    }

    @Test(description = "A failure inside the new organization must not leave the tenant switched, or the "
            + "next request on this thread runs as the wrong tenant.")
    public void testTenantIsRestoredWhenUserProvisioningFails() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        when(userProvisioningExecutor.execute(any())).thenThrow(
                new FlowEngineServerException("60000", "Provisioning failed.", "Provisioning failed."));

        try (MockedStatic<PrivilegedCarbonContext> carbonContext = mockedCarbonContext()) {
            Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));
            carbonContext.verify(PrivilegedCarbonContext::endTenantFlow);
        }

        Assert.assertEquals(context.getTenantDomain(), CURRENT_TENANT_DOMAIN);
    }

    @Test(description = "Without a handle the new organization's tenant is unknown, so provisioning the "
            + "user would silently target the wrong organization.")
    public void testMissingOrganizationHandleReturnsError() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        context.getFlowOrganization().setOrganizationHandle(null);
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(userProvisioningExecutor, never()).execute(any());
    }

    @Test(description = "A failed organization creation is reported as is, and no user is provisioned.")
    public void testUserIsNotProvisionedWhenOrganizationCreationFails() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_RETRY));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_RETRY);
        verify(userProvisioningExecutor, never()).execute(any());
        verify(organizationProvisioningExecutor, never()).rollback(any());
    }

    @Test(dataProvider = "flowEndingStatusProvider",
            description = "If organization creation ends the flow, the user is rolled back and the organization "
                    + "step's outcome is reported as is.")
    public void testUserIsRolledBackWhenOrganizationCreationEndsTheFlow(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        ExecutorResponse organizationResponse = response(status);
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, organizationResponse);

        ExecutorResponse response = executor.execute(context);

        Assert.assertSame(response, organizationResponse);
        verify(userProvisioningExecutor).rollback(context);
    }

    @Test(dataProvider = "returnToNodeStatusProvider",
            description = "A status that brings the flow back to this node keeps the user for the next pass.")
    public void testUserIsKeptWhenOrganizationCreationReturnsToThisNode(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(status));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), status);
        verify(userProvisioningExecutor, never()).rollback(any());
    }

    @Test(description = "An organization executor that throws also ends the flow, so the user is rolled back.")
    public void testUserIsRolledBackWhenOrganizationCreationThrows() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        when(organizationProvisioningExecutor.execute(any())).thenThrow(
                new FlowEngineServerException("60000", "Provisioning failed.", "Provisioning failed."));

        Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));
        verify(userProvisioningExecutor).rollback(context);
    }

    @Test(description = "A failed rollback is logged, not thrown, so the flow reports the failure that "
            + "caused it.")
    public void testFailedRollbackDoesNotReplaceTheOriginalFailure() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        ExecutorResponse organizationResponse = response(STATUS_ERROR);
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, organizationResponse);
        when(userProvisioningExecutor.rollback(any())).thenThrow(
                new FlowEngineServerException("60000", "Rollback failed.", "Rollback failed."));

        ExecutorResponse response = executor.execute(context);

        Assert.assertSame(response, organizationResponse);
    }

    @Test(dataProvider = "flowEndingStatusProvider",
            description = "If user provisioning ends the flow, the new organization is rolled back once the "
                    + "tenant is switched back, because an organization cannot delete itself.")
    public void testOrganizationIsRolledBackWhenUserProvisioningEndsTheFlow(String status) throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        ExecutorResponse userResponse = response(status);
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        stub(userProvisioningExecutor, userResponse);

        try (MockedStatic<PrivilegedCarbonContext> carbonContext = mockedCarbonContext()) {
            when(organizationProvisioningExecutor.rollback(any())).thenAnswer(invocation -> {
                carbonContext.verify(PrivilegedCarbonContext::endTenantFlow);
                FlowExecutionContext passed = invocation.getArgument(0);
                Assert.assertEquals(passed.getTenantDomain(), CURRENT_TENANT_DOMAIN);
                return null;
            });

            ExecutorResponse response = executor.execute(context);

            Assert.assertSame(response, userResponse);
            verify(organizationProvisioningExecutor).rollback(context);
        }
    }

    @Test(description = "A user executor that throws also ends the flow, so the new organization is rolled "
            + "back.")
    public void testOrganizationIsRolledBackWhenUserProvisioningThrows() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        when(userProvisioningExecutor.execute(any())).thenThrow(
                new FlowEngineServerException("60000", "Provisioning failed.", "Provisioning failed."));

        try (MockedStatic<PrivilegedCarbonContext> ignored = mockedCarbonContext()) {
            Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));
        }

        verify(organizationProvisioningExecutor).rollback(context);
    }

    @DataProvider(name = "flowEndingStatusProvider")
    public Object[][] flowEndingStatusProvider() {

        return new Object[][]{
                {STATUS_ERROR},
                {STATUS_USER_ERROR}
        };
    }

    @DataProvider(name = "returnToNodeStatusProvider")
    public Object[][] returnToNodeStatusProvider() {

        return new Object[][]{
                {STATUS_RETRY},
                {STATUS_USER_INPUT_REQUIRED}
        };
    }

    /**
     * A context targeting the new organization, with the handle the organization executor would have
     * recorded once it created it.
     */
    private FlowExecutionContext newOrganizationContext() {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setTenantDomain(CURRENT_TENANT_DOMAIN);
        context.setCurrentNode(nodeWithProvisionTarget(NEW_ORGANIZATION));
        context.getFlowOrganization().setOrganizationHandle(NEW_ORG_HANDLE);
        return context;
    }

    private NodeConfig nodeWithProvisionTarget(String provisionTarget) {

        ExecutorDTO executorConfig = new ExecutorDTO("ProvisioningDispatchExecutor");
        if (provisionTarget != null) {
            executorConfig.addMetadata(PROVISION_TARGET, provisionTarget);
        }
        NodeConfig nodeConfig = new NodeConfig.Builder().id("END").build();
        nodeConfig.setExecutorConfig(executorConfig);
        return nodeConfig;
    }

    /**
     * PrivilegedCarbonContext fails to load unless carbon home is set, and switching the tenant needs a running
     * server, so the static calls are mocked and can be verified.
     */
    private MockedStatic<PrivilegedCarbonContext> mockedCarbonContext() {

        System.setProperty(CarbonBaseConstants.CARBON_HOME,
                Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString());
        MockedStatic<PrivilegedCarbonContext> carbonContext = mockStatic(PrivilegedCarbonContext.class);
        carbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(mock(PrivilegedCarbonContext.class));
        return carbonContext;
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
