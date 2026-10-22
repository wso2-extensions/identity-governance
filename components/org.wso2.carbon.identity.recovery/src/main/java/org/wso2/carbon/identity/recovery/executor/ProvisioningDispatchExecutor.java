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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.model.NodeConfig;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral;

import java.util.Collections;
import java.util.List;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_ERROR;

/**
 * Provisions a user and an organization in the order configured by the flow, then assigns configured roles.
 */
public class ProvisioningDispatchExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(ProvisioningDispatchExecutor.class);

    private static final String EXECUTOR_NAME = "ProvisioningDispatchExecutor";
    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";
    private static final String ORGANIZATION_ROLE_ASSIGNMENT_EXECUTOR = "OrganizationRoleAssignmentExecutor";
    private static final String ROLE_IDS = "roleIds";

    private static final String PROVISION_TARGET = "provisionTarget";
    private static final String NEW_ORGANIZATION = "NEW_ORGANIZATION";

    @Override
    public String getName() {

        return EXECUTOR_NAME;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) throws FlowEngineException {

        // Resolve both executors before provisioning to avoid partial creation when one is unavailable.
        Executor userProvisioningExecutor =
                IdentityRecoveryServiceDataHolder.getInstance().getFlowExecutor(USER_PROVISIONING_EXECUTOR);
        if (userProvisioningExecutor == null) {
            RegistrationCompletionDeferral.discard(context);
            return unavailableExecutorResponse(USER_PROVISIONING_EXECUTOR);
        }
        Executor organizationProvisioningExecutor =
                IdentityRecoveryServiceDataHolder.getInstance().getFlowExecutor(ORGANIZATION_PROVISIONING_EXECUTOR);
        if (organizationProvisioningExecutor == null) {
            RegistrationCompletionDeferral.discard(context);
            return unavailableExecutorResponse(ORGANIZATION_PROVISIONING_EXECUTOR);
        }

        ExecutorResponse response;
        try {
            if (NEW_ORGANIZATION.equals(getMetadataValue(context, PROVISION_TARGET))) {
                response = provisionInChildOrganization(userProvisioningExecutor, organizationProvisioningExecutor,
                        context);
            } else {
                response = provisionInCurrentOrganization(userProvisioningExecutor, organizationProvisioningExecutor,
                        context);
            }
        } catch (FlowEngineException | RuntimeException e) {
            RegistrationCompletionDeferral.discard(context);
            throw e;
        }
        if (STATUS_COMPLETE.equals(response.getResult())) {
            if (StringUtils.isNotBlank(getMetadataValue(context, ROLE_IDS))) {
                assignOrganizationRoles(context);
            }
            RegistrationCompletionDeferral.complete(context);
        } else if (endsFlow(response)) {
            RegistrationCompletionDeferral.discard(context);
        }
        return response;
    }

    private void assignOrganizationRoles(FlowExecutionContext context) {

        Executor roleAssignmentExecutor = IdentityRecoveryServiceDataHolder.getInstance()
                .getFlowExecutor(ORGANIZATION_ROLE_ASSIGNMENT_EXECUTOR);
        if (roleAssignmentExecutor == null) {
            LOG.warn("Skipping configured organization roles because the role assignment executor is unavailable. "
                    + "Flow: " + context.getContextIdentifier());
            return;
        }
        try {
            ExecutorResponse response = dispatch(roleAssignmentExecutor, context);
            if (!STATUS_COMPLETE.equals(response.getResult())) {
                LOG.warn("Organization role assignment did not complete. Provisioning remains successful. Flow: "
                        + context.getContextIdentifier());
            }
        } catch (FlowEngineException | RuntimeException e) {
            LOG.warn("Unable to assign configured organization roles. Provisioning remains successful. Flow: "
                    + context.getContextIdentifier(), e);
        }
    }

    private ExecutorResponse provisionInCurrentOrganization(Executor userProvisioningExecutor,
                                                            Executor organizationProvisioningExecutor,
                                                            FlowExecutionContext context)
            throws FlowEngineException {

        // Skip user provisioning on retries when the user ID is already recorded.
        FlowUser flowUser = context.getFlowUser();
        if (flowUser == null || StringUtils.isBlank(flowUser.getUserId())) {
            ExecutorResponse userResponse = provisionUser(userProvisioningExecutor, context);
            if (!STATUS_COMPLETE.equals(userResponse.getResult())) {
                return userResponse;
            }
        }

        ExecutorResponse organizationResponse;
        try {
            organizationResponse = dispatch(organizationProvisioningExecutor, context);
        } catch (FlowEngineException e) {
            rollbackStep(userProvisioningExecutor, context);
            throw e;
        }
        if (endsFlow(organizationResponse)) {
            rollbackStep(userProvisioningExecutor, context);
        }
        return organizationResponse;
    }

    private ExecutorResponse provisionInChildOrganization(Executor userProvisioningExecutor,
                                                          Executor organizationProvisioningExecutor,
                                                          FlowExecutionContext context)
            throws FlowEngineException {

        ExecutorResponse organizationResponse = dispatch(organizationProvisioningExecutor, context);
        if (!STATUS_COMPLETE.equals(organizationResponse.getResult())) {
            return organizationResponse;
        }

        String organizationTenantDomain = context.getFlowOrganization().getOrganizationHandle();
        if (StringUtils.isBlank(organizationTenantDomain)) {
            LOG.error("The organization was created but its handle is unknown, so the user cannot be "
                    + "provisioned inside it.");
            ExecutorResponse failure = new ExecutorResponse();
            failure.setResult(STATUS_ERROR);
            failure.setErrorMessage("Provisioning failed.");
            return failure;
        }

        // Restore the parent tenant before rollback; an organization cannot delete itself.
        ExecutorResponse userResponse;
        try {
            userResponse = provisionUserInOrganization(userProvisioningExecutor, context, organizationTenantDomain);
        } catch (FlowEngineException e) {
            rollbackStep(organizationProvisioningExecutor, context);
            throw e;
        }
        if (endsFlow(userResponse)) {
            rollbackStep(organizationProvisioningExecutor, context);
        }
        return userResponse;
    }

    private ExecutorResponse provisionUserInOrganization(Executor userProvisioningExecutor,
                                                         FlowExecutionContext context,
                                                         String organizationTenantDomain)
            throws FlowEngineException {

        // Switch both contexts: user provisioning reads the flow context, while listeners read the Carbon context.
        String currentTenantDomain = context.getTenantDomain();
        PrivilegedCarbonContext.startTenantFlow();
        try {
            PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .setTenantDomain(organizationTenantDomain, true);
            context.setTenantDomain(organizationTenantDomain);
            return provisionUser(userProvisioningExecutor, context);
        } finally {
            context.setTenantDomain(currentTenantDomain);
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    private ExecutorResponse provisionUser(Executor executor, FlowExecutionContext context) throws FlowEngineException {

        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            return dispatch(executor, context);
        }
    }

    private boolean endsFlow(ExecutorResponse response) {

        return STATUS_ERROR.equals(response.getResult()) || STATUS_USER_ERROR.equals(response.getResult());
    }

    private void rollbackStep(Executor executor, FlowExecutionContext context) {

        try {
            executor.rollback(context);
        } catch (FlowEngineException e) {
            // Preserve the original provisioning failure if rollback fails.
            LOG.error("Failed to roll back the step of executor: " + executor.getName(), e);
        }
    }

    private String getMetadataValue(FlowExecutionContext context, String key) {

        NodeConfig currentNode = context.getCurrentNode();
        if (currentNode == null || currentNode.getExecutorConfig() == null
                || currentNode.getExecutorConfig().getMetadata() == null) {
            return null;
        }
        return currentNode.getExecutorConfig().getMetadata().get(key);
    }

    private ExecutorResponse dispatch(Executor executor, FlowExecutionContext context)
            throws FlowEngineException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Dispatching to executor: " + executor.getName() + " for flow: "
                    + context.getContextIdentifier());
        }
        ExecutorResponse response = executor.execute(context);
        if (response == null) {
            LOG.error("Executor returned no response: " + executor.getName());
            ExecutorResponse failure = new ExecutorResponse();
            failure.setResult(STATUS_ERROR);
            failure.setErrorMessage("Provisioning is not available.");
            return failure;
        }
        return response;
    }

    /**
     * Returns an error when a required provisioning executor is unavailable.
     *
     * @param executorName Name of the executor that could not be resolved.
     * @return An error response.
     */
    private ExecutorResponse unavailableExecutorResponse(String executorName) {

        LOG.error("Executor not found: " + executorName + ". The provisioning dispatch executor requires "
                + "both the user and organization provisioning executors to be deployed.");
        ExecutorResponse response = new ExecutorResponse();
        response.setResult(STATUS_ERROR);
        response.setErrorMessage("Executor not available: " + executorName);
        return response;
    }

    @Override
    public List<String> getInitiationData() {

        return Collections.emptyList();
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext context) throws FlowEngineException {

        RegistrationCompletionDeferral.discard(context);
        return null;
    }
}
