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

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_INVALID_USERNAME;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_USERNAME_NOT_PROVIDED;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.USERNAME_CLAIM_URI;
import static org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils.handleClientException;

/**
 * Executor to provision the password.
 */
public class PasswordProvisioningExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(PasswordProvisioningExecutor.class);
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";
    private static final String CONFIRMATION_CODE = "confirmationCode";

    @Override
    public String getName() {

        return "PasswordProvisioningExecutor";
    }

    @Override
    public List<String> getInitiationData() {

        List<String> initiationData = new ArrayList<>();
        initiationData.add(PASSWORD_KEY);
        return initiationData;
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext flowExecutionContext) throws FlowEngineException {

        return null;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) throws FlowEngineException {

        Map<String, char[]> credentials;
        String passwordValue = context.getUserInputData() != null ? context.getUserInputData().get(PASSWORD_KEY) : null;
        if (StringUtils.isNotBlank(passwordValue)) {
            credentials = Collections.singletonMap(PASSWORD_KEY, passwordValue.toCharArray());
        } else {
            credentials = context.getFlowUser().getUserCredentials();
            if (MapUtils.isEmpty(credentials)) {
                return buildUserInputRequiredResponse();
            }
        }

        char[] password =
                credentials.getOrDefault(PASSWORD_KEY, new DefaultPasswordGenerator().generatePassword());
        String confirmationCode = (String) context.getProperty(CONFIRMATION_CODE);
        if (StringUtils.isBlank(confirmationCode)) {
            return errorResponse(new ExecutorResponse(), "Confirmation code is not provided.");
        }

        try {
            int tenantId = IdentityTenantUtil.getTenantId(context.getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                    .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();

            updateUserClaims(userStoreManager, context);
            NotificationPasswordRecoveryManager.getInstance()
                    .updatePassword(confirmationCode, String.valueOf(password), null);

            String userId = ((AbstractUserStoreManager) userStoreManager)
                    .getUserIDFromUserName(context.getFlowUser().getUsername());
            context.getFlowUser().setUserId(userId);
            return new ExecutorResponse(STATUS_COMPLETE);
        } catch (IdentityEventException | IdentityRecoveryException | org.wso2.carbon.user.api.UserStoreException e) {
            LOG.error("Error while updating password for user: " + context.getFlowUser().getUsername(), e);
            return errorResponse(new ExecutorResponse(), e.getMessage());
        } finally {
            // Clear sensitive data.
            Arrays.fill(password, '\0');
        }
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

    /**
     * Updates user claims in the user store.
     *
     * @param userStoreManager UserStoreManager instance to update user claims.
     * @param context          FlowExecutionContext containing user information and claims.
     * @throws FlowEngineException if an error occurs during flow execution.
     * @throws UserStoreException  if an error occurs while accessing the user store.
     */
    private void updateUserClaims(UserStoreManager userStoreManager, FlowExecutionContext context)
            throws FlowEngineException, UserStoreException {

        FlowUser user = enrichUserProfile(context);
        userStoreManager.setUserClaimValues(user.getUsername(), user.getClaims(), null);
    }

    /**
     * Enriches the user profile with claims from the context.
     *
     * @param context FlowExecutionContext containing user information and claims.
     * @return Enriched FlowUser with claims and username.
     * @throws FlowEngineException if an error occurs during flow execution.
     */
    private FlowUser enrichUserProfile(FlowExecutionContext context) throws FlowEngineException {

        FlowUser user = context.getFlowUser();
        context.getUserInputData().forEach((key, value) -> {
            if (key.startsWith(WSO2_CLAIM_DIALECT) && !user.getClaims().containsKey(key)) {
                user.addClaim(key, value);
            }
        });
        user.setUsername(resolveUsername(user, context.getContextIdentifier()));
        return user;
    }

    /**
     * Resolves the username from the FlowUser object.
     *
     * @param user   FlowUser object containing user information.
     * @param flowId Flow ID for error handling.
     * @return Resolved username.
     * @throws FlowEngineException if the username is not provided or invalid.
     */
    private String resolveUsername(FlowUser user, String flowId) throws FlowEngineException {

        String username = Optional.ofNullable(user.getUsername())
                .orElseGet(() -> Optional.ofNullable(user.getClaims().get(USERNAME_CLAIM_URI)).orElse(""));
        if (StringUtils.isBlank(username)) {
            throw handleClientException(ERROR_CODE_USERNAME_NOT_PROVIDED, flowId);
        }
        if (IdentityUtil.isEmailUsernameEnabled() && !username.contains("@")) {
            throw handleClientException(ERROR_CODE_INVALID_USERNAME, username);
        }
        return username;
    }

    /**
     * Creates an error response with the provided message.
     *
     * @param response ExecutorResponse to be modified with error details.
     * @param message  Error message to be set in the response.
     * @return Modified ExecutorResponse with error details.
     */
    private ExecutorResponse errorResponse(ExecutorResponse response, String message) {

        response.setErrorMessage(message);
        response.setResult(Constants.ExecutorStatus.STATUS_ERROR);
        return response;
    }
}
