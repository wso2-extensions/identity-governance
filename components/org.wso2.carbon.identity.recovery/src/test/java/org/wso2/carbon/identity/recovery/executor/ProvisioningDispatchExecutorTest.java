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
import org.mockito.MockedConstruction;
import org.testng.annotations.DataProvider;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineServerException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.mgt.model.ExecutorDTO;
import org.wso2.carbon.identity.flow.mgt.model.NodeConfig;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.handler.FlowRegistrationCompletionHandler;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.nio.file.Paths;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_RETRY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;
import static org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral.NOTIFICATION_PROPERTIES;
import static org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral.NOTIFY_ACCOUNT_CREATION;
import static org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral.PUBLISH_SUCCESS_EVENT;
import static org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral.SEND_CONFIRMATION;

/**
 * Unit tests for {@link ProvisioningDispatchExecutor}.
 */
public class ProvisioningDispatchExecutorTest {

    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";
    private static final String ORGANIZATION_ROLE_ASSIGNMENT_EXECUTOR = "OrganizationRoleAssignmentExecutor";
    private static final String PROVISION_TARGET = "provisionTarget";
    private static final String NEW_ORGANIZATION = "NEW_ORGANIZATION";
    private static final String NEW_ORG_HANDLE = "acmecorporation";
    private static final String CURRENT_TENANT_DOMAIN = "carbon.super";

    private ProvisioningDispatchExecutor executor;
    private Executor userProvisioningExecutor;
    private Executor organizationProvisioningExecutor;
    private Executor roleAssignmentExecutor;
    private Property[] previousNotificationProperties;

    @BeforeMethod
    public void setUp() {

        previousNotificationProperties = Utils.getArbitraryProperties();
        executor = new ProvisioningDispatchExecutor();
        userProvisioningExecutor = namedExecutor(USER_PROVISIONING_EXECUTOR);
        organizationProvisioningExecutor = namedExecutor(ORGANIZATION_PROVISIONING_EXECUTOR);
        roleAssignmentExecutor = namedExecutor(ORGANIZATION_ROLE_ASSIGNMENT_EXECUTOR);

        IdentityRecoveryServiceDataHolder.getInstance().addFlowExecutor(userProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().addFlowExecutor(organizationProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().addFlowExecutor(roleAssignmentExecutor);
    }

    @AfterMethod
    public void tearDown() {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(userProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(organizationProvisioningExecutor);
        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(roleAssignmentExecutor);
        if (previousNotificationProperties == null) {
            Utils.clearArbitraryProperties();
        } else {
            Utils.setArbitraryProperties(previousNotificationProperties);
        }
    }

    @Test
    public void testExecutorName() {

        Assert.assertEquals(executor.getName(), "ProvisioningDispatchExecutor");
    }

    @Test
    public void testRegistrationCompletesOnlyAfterOrganizationProvisioning() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        Map<String, Object> completion = expectedCompletion(CURRENT_TENANT_DOMAIN);
        deferDuringUserProvisioning(CURRENT_TENANT_DOMAIN, STATUS_COMPLETE);
        try (MockedStatic<PrivilegedCarbonContext> ignored = mockedCarbonContext();
             MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            when(organizationProvisioningExecutor.execute(context)).thenAnswer(invocation -> {
                Assert.assertTrue(handlers.constructed().isEmpty());
                return response(STATUS_COMPLETE);
            });

            Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);

            Assert.assertEquals(handlers.constructed().size(), 1);
            verify(handlers.constructed().get(0)).completeRegistration(context, completion);
            Assert.assertEquals(context.getFlowUser().getUsername(), "testuser");
            Assert.assertEquals(context.getFlowUser().getUserStoreDomain(), "PRIMARY");
            Assert.assertEquals(context.getFlowUser().getClaims().get("http://wso2.org/claims/emailaddress"),
                    "test@example.com");
            Assert.assertTrue(context.getProperties().isEmpty());
            Assert.assertFalse(deferCompletion(CURRENT_TENANT_DOMAIN), "The thread-local scope must be cleared.");
        }
    }

    @Test(dataProvider = "flowEndingStatusProvider")
    public void testFailedOrganizationDiscardsRegistrationCompletion(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        deferDuringUserProvisioning(CURRENT_TENANT_DOMAIN, STATUS_COMPLETE);
        stub(organizationProvisioningExecutor, response(status));
        try (MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            Assert.assertEquals(executor.execute(context).getResult(), status);
            Assert.assertTrue(handlers.constructed().isEmpty());
            Assert.assertTrue(context.getProperties().isEmpty());
            verify(userProvisioningExecutor).rollback(context);
        }
    }

    @Test
    public void testOrganizationExceptionDiscardsRegistrationCompletion() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        deferDuringUserProvisioning(CURRENT_TENANT_DOMAIN, STATUS_COMPLETE);
        when(organizationProvisioningExecutor.execute(context)).thenThrow(
                new FlowEngineServerException("60000", "Failed", "Failed"));

        Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));

        Assert.assertTrue(context.getProperties().isEmpty());
        Assert.assertFalse(deferCompletion(CURRENT_TENANT_DOMAIN));
    }

    @Test(dataProvider = "returnToNodeStatusProvider")
    public void testRegistrationCompletionSurvivesRetryAndRunsOnce(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        Map<String, Object> completion = expectedCompletion(CURRENT_TENANT_DOMAIN);
        deferDuringUserProvisioning(CURRENT_TENANT_DOMAIN, STATUS_COMPLETE);
        when(organizationProvisioningExecutor.execute(context)).thenReturn(response(status), response(STATUS_COMPLETE));
        try (MockedStatic<PrivilegedCarbonContext> ignored = mockedCarbonContext();
             MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            Assert.assertEquals(executor.execute(context).getResult(), status);
            Assert.assertTrue(handlers.constructed().isEmpty());
            Assert.assertTrue(context.getProperties().containsValue(completion));

            Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
            RegistrationCompletionDeferral.complete(context);

            Assert.assertEquals(handlers.constructed().size(), 1);
            verify(handlers.constructed().get(0)).completeRegistration(context, completion);
            verify(userProvisioningExecutor).execute(context);
        }
    }

    @Test
    public void testNewOrganizationCompletionUsesTheUserTenant() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        Map<String, Object> completion = expectedCompletion(NEW_ORG_HANDLE);
        deferDuringUserProvisioning(NEW_ORG_HANDLE, STATUS_COMPLETE);
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        try (MockedStatic<PrivilegedCarbonContext> carbon = mockedCarbonContext();
             MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            executor.execute(context);

            verify(handlers.constructed().get(0)).completeRegistration(context, completion);
            verify(PrivilegedCarbonContext.getThreadLocalCarbonContext(), org.mockito.Mockito.times(2))
                    .setTenantDomain(NEW_ORG_HANDLE, true);
            carbon.verify(PrivilegedCarbonContext::endTenantFlow, org.mockito.Mockito.times(2));
            Assert.assertEquals(context.getTenantDomain(), CURRENT_TENANT_DOMAIN);
        }
    }

    @Test
    public void testNotificationFailureDoesNotRollBackSuccessfulProvisioning() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        deferDuringUserProvisioning(CURRENT_TENANT_DOMAIN, STATUS_COMPLETE);
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        try (MockedStatic<PrivilegedCarbonContext> carbon = mockedCarbonContext();
             MockedConstruction<FlowRegistrationCompletionHandler> handlers = mockConstruction(
                     FlowRegistrationCompletionHandler.class, (handler, construction) ->
                             doThrow(new org.wso2.carbon.identity.event.IdentityEventException("Notification failed"))
                                      .when(handler).completeRegistration(any(), any()))) {
            Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
            Assert.assertEquals(handlers.constructed().size(), 1);
            verify(userProvisioningExecutor, never()).rollback(any());
            verify(organizationProvisioningExecutor, never()).rollback(any());
            carbon.verify(PrivilegedCarbonContext::endTenantFlow);
        }
    }

    @Test
    public void testUserExceptionRestoresDeferralScope() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        when(userProvisioningExecutor.execute(context)).thenAnswer(invocation -> {
            Assert.assertTrue(deferCompletion(CURRENT_TENANT_DOMAIN));
            throw new FlowEngineServerException("60000", "Failed", "Failed");
        });

        Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));

        Assert.assertFalse(deferCompletion(CURRENT_TENANT_DOMAIN));
        Assert.assertTrue(context.getProperties().isEmpty());
    }

    private void deferDuringUserProvisioning(String tenantDomain, String status) throws Exception {

        when(userProvisioningExecutor.execute(any())).thenAnswer(invocation -> {
            FlowExecutionContext context = invocation.getArgument(0);
            context.getFlowUser().setUsername("testuser");
            context.getFlowUser().setUserStoreDomain("PRIMARY");
            context.getFlowUser().addClaim("http://wso2.org/claims/emailaddress", "test@example.com");
            Utils.setArbitraryProperties(new Property[]{new Property("callback", "https://example.com")});
            Assert.assertTrue(deferCompletion(tenantDomain));
            context.getFlowUser().setUserId("new-user-id");
            return response(status);
        });
    }

    private boolean deferCompletion(String tenantDomain) {

        return RegistrationCompletionDeferral.defer(tenantDomain, "EMAIL", false, true, true);
    }

    private Map<String, Object> expectedCompletion(String tenantDomain) {

        return Map.of(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain,
                IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, "EMAIL",
                NOTIFY_ACCOUNT_CREATION, false, PUBLISH_SUCCESS_EVENT, true, SEND_CONFIRMATION, true,
                NOTIFICATION_PROPERTIES, Map.of("callback", "https://example.com"));
    }

    @Test
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

    @Test(dataProvider = "incompleteUserStatusProvider")
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

    @Test
    public void testMissingOrganizationExecutorReturnsErrorWithoutProvisioningUser() throws Exception {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(organizationProvisioningExecutor);
        FlowExecutionContext context = new FlowExecutionContext();

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(userProvisioningExecutor, never()).execute(any());
    }

    @Test
    public void testUserIsNotProvisionedAgainOnRetry() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.getFlowUser().setUserId("dcaf39b8-4c8c-4a8d-9a3a-2f8c1b7f6e21");
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(userProvisioningExecutor, never()).execute(any());
        verify(organizationProvisioningExecutor).execute(context);
    }

    @Test
    public void testNullResponseFromDispatchedExecutorReturnsError() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, null);

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(organizationProvisioningExecutor, never()).execute(any());
    }

    @Test
    public void testMissingUserExecutorReturnsError() throws Exception {

        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(userProvisioningExecutor);
        FlowExecutionContext context = new FlowExecutionContext();

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(organizationProvisioningExecutor, never()).execute(any());
    }

    @Test
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

    @Test
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

    @Test
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

    @Test
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

    @Test
    public void testMissingOrganizationHandleReturnsError() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        context.getFlowOrganization().setOrganizationHandle(null);
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_ERROR);
        verify(userProvisioningExecutor, never()).execute(any());
    }

    @Test
    public void testUserIsNotProvisionedWhenOrganizationCreationFails() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        stub(organizationProvisioningExecutor, response(STATUS_RETRY));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), STATUS_RETRY);
        verify(userProvisioningExecutor, never()).execute(any());
        verify(organizationProvisioningExecutor, never()).rollback(any());
    }

    @Test(dataProvider = "flowEndingStatusProvider")
    public void testUserIsRolledBackWhenOrganizationCreationEndsTheFlow(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        ExecutorResponse organizationResponse = response(status);
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, organizationResponse);

        ExecutorResponse response = executor.execute(context);

        Assert.assertSame(response, organizationResponse);
        verify(userProvisioningExecutor).rollback(context);
    }

    @Test(dataProvider = "returnToNodeStatusProvider")
    public void testUserIsKeptWhenOrganizationCreationReturnsToThisNode(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(status));

        ExecutorResponse response = executor.execute(context);

        Assert.assertEquals(response.getResult(), status);
        verify(userProvisioningExecutor, never()).rollback(any());
    }

    @Test
    public void testUserIsRolledBackWhenOrganizationCreationThrows() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        when(organizationProvisioningExecutor.execute(any())).thenThrow(
                new FlowEngineServerException("60000", "Provisioning failed.", "Provisioning failed."));

        Assert.assertThrows(FlowEngineException.class, () -> executor.execute(context));
        verify(userProvisioningExecutor).rollback(context);
    }

    @Test
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

    @Test(dataProvider = "flowEndingStatusProvider")
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

    @Test
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

    @Test
    public void testRolesAreAssignedAfterProvisioningInCurrentOrganization() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setCurrentNode(nodeWithProvisionTarget(null));
        context.getCurrentNode().getExecutorConfig().addMetadata("roleIds", "[\"role-1\",\"role-2\"]");
        ExecutorResponse organizationResponse = response(STATUS_COMPLETE);
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, organizationResponse);
        stub(roleAssignmentExecutor, response(STATUS_COMPLETE));

        Assert.assertSame(executor.execute(context), organizationResponse);
        InOrder order = org.mockito.Mockito.inOrder(userProvisioningExecutor, organizationProvisioningExecutor,
                roleAssignmentExecutor);
        order.verify(userProvisioningExecutor).execute(context);
        order.verify(organizationProvisioningExecutor).execute(context);
        order.verify(roleAssignmentExecutor).execute(context);
    }

    @Test
    public void testRolesAreAssignedAfterNewOrganizationTenantIsRestored() throws Exception {

        FlowExecutionContext context = newOrganizationContext();
        context.getCurrentNode().getExecutorConfig().addMetadata("roleIds", "[\"role-1\"]");
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        ExecutorResponse userResponse = response(STATUS_COMPLETE);
        stub(userProvisioningExecutor, userResponse);

        try (MockedStatic<PrivilegedCarbonContext> carbonContext = mockedCarbonContext()) {
            when(roleAssignmentExecutor.execute(context)).thenAnswer(invocation -> {
                carbonContext.verify(PrivilegedCarbonContext::endTenantFlow);
                Assert.assertEquals(context.getTenantDomain(), CURRENT_TENANT_DOMAIN);
                return response(STATUS_COMPLETE);
            });

            Assert.assertSame(executor.execute(context), userResponse);
            InOrder order = org.mockito.Mockito.inOrder(organizationProvisioningExecutor, userProvisioningExecutor,
                    roleAssignmentExecutor);
            order.verify(organizationProvisioningExecutor).execute(context);
            order.verify(userProvisioningExecutor).execute(context);
            order.verify(roleAssignmentExecutor).execute(context);
        }
    }

    @Test
    public void testNoRoleMetadataKeepsExistingBehavior() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));

        executor.execute(context);

        verify(roleAssignmentExecutor, never()).execute(any());
    }

    @Test(dataProvider = "returnToNodeStatusProvider")
    public void testRoleAssignmentIsSkippedUntilProvisioningCompletes(String status) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setCurrentNode(nodeWithProvisionTarget(null));
        context.getCurrentNode().getExecutorConfig().addMetadata("roleIds", "[\"role-1\"]");
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(status));

        Assert.assertEquals(executor.execute(context).getResult(), status);
        verify(roleAssignmentExecutor, never()).execute(any());
    }

    @Test(dataProvider = "flowEndingStatusProvider")
    public void testRoleAssignmentErrorDoesNotFailProvisioning(String status) throws Exception {

        FlowExecutionContext context = roleAssignmentContext();
        stub(roleAssignmentExecutor, response(status));

        Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
        verify(userProvisioningExecutor, never()).rollback(any());
        verify(organizationProvisioningExecutor, never()).rollback(any());
    }

    @Test
    public void testUnavailableRoleAssignmentExecutorDoesNotFailProvisioning() throws Exception {

        FlowExecutionContext context = roleAssignmentContext();
        IdentityRecoveryServiceDataHolder.getInstance().removeFlowExecutor(roleAssignmentExecutor);

        Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
    }

    @Test
    public void testRoleAssignmentExceptionDoesNotFailProvisioning() throws Exception {

        FlowExecutionContext context = roleAssignmentContext();
        when(roleAssignmentExecutor.execute(context)).thenThrow(new FlowEngineServerException(
                "60000", "Role assignment failed.", "Role assignment failed."));

        Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
        verify(userProvisioningExecutor, never()).rollback(any());
        verify(organizationProvisioningExecutor, never()).rollback(any());
    }

    @Test
    public void testRoleAssignmentRuntimeExceptionDoesNotFailProvisioning() throws Exception {

        FlowExecutionContext context = roleAssignmentContext();
        when(roleAssignmentExecutor.execute(context)).thenThrow(new IllegalStateException("Service unavailable"));

        Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
    }

    @Test
    public void testNullRoleAssignmentResponseDoesNotFailProvisioning() throws Exception {

        FlowExecutionContext context = roleAssignmentContext();
        stub(roleAssignmentExecutor, null);

        Assert.assertEquals(executor.execute(context).getResult(), STATUS_COMPLETE);
    }

    private FlowExecutionContext roleAssignmentContext() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setCurrentNode(nodeWithProvisionTarget(null));
        context.getCurrentNode().getExecutorConfig().addMetadata("roleIds", "[\"role-1\"]");
        stub(userProvisioningExecutor, response(STATUS_COMPLETE));
        stub(organizationProvisioningExecutor, response(STATUS_COMPLETE));
        return context;
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
