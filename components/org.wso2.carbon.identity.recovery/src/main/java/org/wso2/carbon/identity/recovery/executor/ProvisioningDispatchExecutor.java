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

import java.util.Collections;
import java.util.List;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_ERROR;

/**
 * Flow executor that provisions both a user and the organization the flow collected.
 * <p>
 * The flow builder decides whether this executor is needed: it names this executor on the END step when
 * the flow collects organization details, and {@code UserProvisioningExecutor} when it does not. So by
 * the time this runs the decision is already made, and it dispatches to both unconditionally rather than
 * classifying the flow itself.
 * <p>
 * By default the user is provisioned in the organization the flow is executing in, and then creates the
 * organization. When the step targets the new organization, the organization is created first and the
 * user is provisioned inside it. If the second step ends the flow, the executor that ran the first step
 * is asked to roll it back.
 * <p>
 * The executors are resolved by name from executors contributed across all bundles, so this component
 * does not depend on the one that owns the organization executor.
 */
public class ProvisioningDispatchExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(ProvisioningDispatchExecutor.class);

    private static final String EXECUTOR_NAME = "ProvisioningDispatchExecutor";
    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";

    /**
     * Set by the flow builder on this executor's step. Absent means the user is provisioned in the
     * organization the flow is executing in, which is what every existing flow does.
     */
    private static final String PROVISION_TARGET = "provisionTarget";
    private static final String NEW_ORGANIZATION = "NEW_ORGANIZATION";

    @Override
    public String getName() {

        return EXECUTOR_NAME;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) throws FlowEngineException {

        // Resolved before either one runs, so a missing organization executor cannot be discovered
        // only after a user has been provisioned and left without an organization.
        Executor userProvisioningExecutor =
                IdentityRecoveryServiceDataHolder.getInstance().getFlowExecutor(USER_PROVISIONING_EXECUTOR);
        if (userProvisioningExecutor == null) {
            return unavailableExecutorResponse(USER_PROVISIONING_EXECUTOR);
        }
        Executor organizationProvisioningExecutor =
                IdentityRecoveryServiceDataHolder.getInstance().getFlowExecutor(ORGANIZATION_PROVISIONING_EXECUTOR);
        if (organizationProvisioningExecutor == null) {
            return unavailableExecutorResponse(ORGANIZATION_PROVISIONING_EXECUTOR);
        }

        if (NEW_ORGANIZATION.equals(getMetadataValue(context, PROVISION_TARGET))) {
            return provisionInNewOrganization(userProvisioningExecutor, organizationProvisioningExecutor, context);
        }
        return provisionInCurrentOrganization(userProvisioningExecutor, organizationProvisioningExecutor, context);
    }

    /**
     * Provisions the user in the organization the flow is executing in, then creates the organization
     * under that user, who becomes its owner. If organization creation ends the flow, the user is rolled
     * back.
     *
     * @param userProvisioningExecutor         Executor that provisions the user.
     * @param organizationProvisioningExecutor Executor that creates the organization.
     * @param context                          Flow execution context, shared by both executors.
     * @return The outcome of whichever step did not complete, otherwise the organization step's outcome.
     * @throws FlowEngineException If an executor fails.
     */
    private ExecutorResponse provisionInCurrentOrganization(Executor userProvisioningExecutor,
                                                            Executor organizationProvisioningExecutor,
                                                            FlowExecutionContext context)
            throws FlowEngineException {

        // The flow can return to this node after the user was provisioned. User provisioning is not
        // idempotent, so the user ID recorded on the first pass marks that step as already done.
        FlowUser flowUser = context.getFlowUser();
        if (flowUser == null || StringUtils.isBlank(flowUser.getUserId())) {
            ExecutorResponse userResponse = dispatch(userProvisioningExecutor, context);
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

    /**
     * Creates the organization first, then provisions the user inside it, so the user never gets a record
     * in the organization the flow is executing in. If user provisioning ends the flow, the organization
     * is rolled back.
     *
     * @param userProvisioningExecutor         Executor that provisions the user.
     * @param organizationProvisioningExecutor Executor that creates the organization.
     * @param context                          Flow execution context, shared by both executors.
     * @return The organization step's outcome if it did not complete, otherwise the user step's outcome.
     * @throws FlowEngineException If an executor fails.
     */
    private ExecutorResponse provisionInNewOrganization(Executor userProvisioningExecutor,
                                                        Executor organizationProvisioningExecutor,
                                                        FlowExecutionContext context)
            throws FlowEngineException {

        ExecutorResponse organizationResponse = dispatch(organizationProvisioningExecutor, context);
        if (!STATUS_COMPLETE.equals(organizationResponse.getResult())) {
            return organizationResponse;
        }

        // The handle is the new organization's tenant domain, recorded by the organization executor.
        String organizationTenantDomain = context.getFlowOrganization().getOrganizationHandle();
        if (StringUtils.isBlank(organizationTenantDomain)) {
            LOG.error("The organization was created but its handle is unknown, so the user cannot be "
                    + "provisioned inside it.");
            ExecutorResponse failure = new ExecutorResponse();
            failure.setResult(STATUS_ERROR);
            failure.setErrorMessage("Provisioning failed.");
            return failure;
        }

        // The rollback runs only once the tenant is switched back, because an organization cannot delete
        // itself.
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

    /**
     * Provisions the user inside the given organization.
     *
     * @param userProvisioningExecutor Executor that provisions the user.
     * @param context                  Flow execution context.
     * @param organizationTenantDomain Tenant domain of the organization to provision the user in.
     * @return The user step's outcome.
     * @throws FlowEngineException If the executor fails.
     */
    private ExecutorResponse provisionUserInOrganization(Executor userProvisioningExecutor,
                                                         FlowExecutionContext context,
                                                         String organizationTenantDomain)
            throws FlowEngineException {

        // User provisioning takes the tenant from the flow context, while the listeners it triggers read
        // the carbon context, so both move to the new organization for the duration of the call.
        String currentTenantDomain = context.getTenantDomain();
        PrivilegedCarbonContext.startTenantFlow();
        try {
            PrivilegedCarbonContext.getThreadLocalCarbonContext()
                    .setTenantDomain(organizationTenantDomain, true);
            context.setTenantDomain(organizationTenantDomain);
            return dispatch(userProvisioningExecutor, context);
        } finally {
            context.setTenantDomain(currentTenantDomain);
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    /**
     * Whether the flow ends on this response. The engine ends the flow when an executor returns ERROR or
     * USER_ERROR.
     *
     * @param response Response of a dispatched executor.
     * @return {@code true} if the flow ends on this response.
     */
    private boolean endsFlow(ExecutorResponse response) {

        return STATUS_ERROR.equals(response.getResult()) || STATUS_USER_ERROR.equals(response.getResult());
    }

    /**
     * Asks an executor to roll back its step. A failed rollback is logged rather than thrown, so the flow
     * reports the failure that caused it.
     *
     * @param executor Executor whose step is rolled back.
     * @param context  Flow execution context, shared by both executors.
     */
    private void rollbackStep(Executor executor, FlowExecutionContext context) {

        try {
            executor.rollback(context);
        } catch (FlowEngineException e) {
            LOG.error("Failed to roll back the step of executor: " + executor.getName(), e);
        }
    }

    /**
     * Reads a value the flow author configured on the step this executor is running on.
     *
     * @param context Flow execution context.
     * @param key     Metadata key.
     * @return The configured value, or {@code null} when the step carries none.
     */
    private String getMetadataValue(FlowExecutionContext context, String key) {

        NodeConfig currentNode = context.getCurrentNode();
        if (currentNode == null || currentNode.getExecutorConfig() == null
                || currentNode.getExecutorConfig().getMetadata() == null) {
            return null;
        }
        return currentNode.getExecutorConfig().getMetadata().get(key);
    }

    /**
     * Runs an executor against the same flow context.
     *
     * @param executor Executor to run.
     * @param context  Flow execution context, shared by both executors.
     * @return The executor's response, never {@code null}.
     * @throws FlowEngineException If the executor fails.
     */
    private ExecutorResponse dispatch(Executor executor, FlowExecutionContext context)
            throws FlowEngineException {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Dispatching to executor: " + executor.getName() + " for flow: "
                    + context.getContextIdentifier());
        }
        ExecutorResponse response = executor.execute(context);
        // The implementation behind a name is whatever bundle registered it, and Executor does not
        // promise a response.
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
     * Builds the response for a flow that names this executor while one of the executors it dispatches
     * to is not deployed. The end user cannot resolve this by retrying.
     *
     * @param executorName Name of the executor that could not be resolved.
     * @return An error response.
     */
    private ExecutorResponse unavailableExecutorResponse(String executorName) {

        LOG.error("Executor not found: " + executorName + ". The provisioning dispatch executor requires "
                + "both the user and organization provisioning executors to be deployed.");
        ExecutorResponse response = new ExecutorResponse();
        response.setResult(STATUS_ERROR);
        response.setErrorMessage("Provisioning is not available.");
        return response;
    }

    @Override
    public List<String> getInitiationData() {

        return Collections.emptyList();
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext context) throws FlowEngineException {

        return null;
    }
}
