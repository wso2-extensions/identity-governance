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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;

import java.util.Collections;
import java.util.List;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;

/**
 * Flow executor that provisions a user and then the organization the flow collected, in that order.
 * <p>
 * The flow builder decides whether this executor is needed: it names this executor on the END step when
 * the flow collects organization details, and {@code UserProvisioningExecutor} when it does not. So by
 * the time this runs the decision is already made, and it dispatches to both unconditionally rather than
 * classifying the flow itself.
 * <p>
 * Ordering matters. {@code OrganizationProvisioningExecutor} requires a provisioned user, because the
 * creating user becomes the organization owner. Running user provisioning first leaves the user ID on
 * the flow user, which the organization executor then reads from the same context.
 * <p>
 * The executors are resolved by name from executors contributed across all bundles, so this component
 * does not depend on the one that owns the organization executor.
 */
public class ProvisioningDispatchExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(ProvisioningDispatchExecutor.class);

    private static final String EXECUTOR_NAME = "ProvisioningDispatchExecutor";
    private static final String USER_PROVISIONING_EXECUTOR = "UserProvisioningExecutor";
    private static final String ORGANIZATION_PROVISIONING_EXECUTOR = "OrganizationProvisioningExecutor";

    @Override
    public String getName() {

        return EXECUTOR_NAME;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) throws FlowEngineException {

        ExecutorResponse userResponse = dispatch(USER_PROVISIONING_EXECUTOR, context);
        if (userResponse == null) {
            return unavailableExecutorResponse(USER_PROVISIONING_EXECUTOR);
        }
        // Anything other than completion is the user provisioning step's own outcome to report: it may
        // need more input, or it may have failed. The organization must not be created either way.
        if (!STATUS_COMPLETE.equals(userResponse.getResult())) {
            return userResponse;
        }

        ExecutorResponse organizationResponse = dispatch(ORGANIZATION_PROVISIONING_EXECUTOR, context);
        if (organizationResponse == null) {
            return unavailableExecutorResponse(ORGANIZATION_PROVISIONING_EXECUTOR);
        }
        return organizationResponse;
    }

    /**
     * Resolves an executor by name and runs it against the same flow context.
     *
     * @param executorName Name the executor is registered under.
     * @param context      Flow execution context, shared by both executors.
     * @return The executor's response, or {@code null} if no executor is registered under that name.
     * @throws FlowEngineException If the executor fails.
     */
    private ExecutorResponse dispatch(String executorName, FlowExecutionContext context)
            throws FlowEngineException {

        Executor executor = IdentityRecoveryServiceDataHolder.getInstance().getFlowExecutor(executorName);
        if (executor == null) {
            return null;
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("Dispatching to executor: " + executorName + " for flow: "
                    + context.getContextIdentifier());
        }
        return executor.execute(context);
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
