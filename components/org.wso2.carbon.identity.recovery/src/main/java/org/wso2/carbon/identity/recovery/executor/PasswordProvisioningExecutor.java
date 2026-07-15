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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.flow.execution.engine.graph.AuthenticationExecutor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;

/**
 * Executor to provision the password.
 * The actual password provisioning (password recovery and invited user / ask password flows) has been moved
 * into {@link UserProvisioningExecutor}. This executor is retained only so that existing flow definitions that
 * still reference it keep working. It no longer updates the credential, it just captures the password into the
 * flow user so that the downstream {@link UserProvisioningExecutor} can provision it.
 */
public class PasswordProvisioningExecutor extends AuthenticationExecutor {

    @Override
    public String getName() {

        return "PasswordProvisioningExecutor";
    }

    @Override
    public String getAMRValue() {

        return "BasicAuthenticator";
    }

    @Override
    public List<String> getInitiationData() {

        List<String> initiationData = new ArrayList<>();
        initiationData.add(PASSWORD_KEY);
        return initiationData;
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext flowExecutionContext) {

        return null;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) {

        // Capture the password into the flow user so the downstream UserProvisioningExecutor can provision
        // it. The credential is intentionally not updated here anymore.
        Map<String, char[]> credentials = context.getFlowUser().getUserCredentials();
        String passwordValue = context.getUserInputData() != null ? context.getUserInputData().get(PASSWORD_KEY) : null;
        if (StringUtils.isNotBlank(passwordValue)) {
            // Preserve any other credential entries already set on the flow user.
            Map<String, char[]> updatedCredentials = new HashMap<>();
            if (credentials != null) {
                updatedCredentials.putAll(credentials);
            }
            updatedCredentials.put(PASSWORD_KEY, passwordValue.toCharArray());
            context.getFlowUser().setUserCredentials(updatedCredentials);
        } else if (credentials == null || ArrayUtils.isEmpty(credentials.get(PASSWORD_KEY))) {
            // Password availability is decided by the password entry itself, not by map occupancy.
            return buildUserInputRequiredResponse();
        }

        return new ExecutorResponse(STATUS_COMPLETE);
    }

    /**
     * Builds a response indicating that user input is required.
     *
     * @return ExecutorResponse indicating user input is required.
     */
    private ExecutorResponse buildUserInputRequiredResponse() {

        ExecutorResponse response = new ExecutorResponse(STATUS_USER_INPUT_REQUIRED);
        response.setRequiredData(Collections.singletonList(PASSWORD_KEY));
        return response;
    }
}
