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

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.USER;

/**
 * Executor to provision the password.
 */
public class PasswordProvisioningExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(PasswordProvisioningExecutor.class);
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";
    private static final String PASSWORD_RECOVERY = "PASSWORD_RECOVERY";
    private static final String ASK_PASSWORD = "ASK_PASSWORD";

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
    public ExecutorResponse rollback(FlowExecutionContext flowExecutionContext) {

        return null;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) {

        Map<String, char[]> credentials;
        String passwordValue = context.getUserInputData() != null ? context.getUserInputData().get(PASSWORD_KEY) : null;
        if (StringUtils.isNotBlank(passwordValue)) {
            credentials = Collections.singletonMap(PASSWORD_KEY, passwordValue.toCharArray());
            context.getFlowUser().setUserCredentials(credentials);
        } else {
            credentials = context.getFlowUser().getUserCredentials();
            if (MapUtils.isEmpty(credentials)) {
                return buildUserInputRequiredResponse();
            }
        }

        char[] password = credentials.getOrDefault(PASSWORD_KEY, new DefaultPasswordGenerator().generatePassword());
        try {
            if (context.getFlowType().equals(PASSWORD_RECOVERY)) {
                return handlePasswordRecoveryFlow(context, password);
            } else if (context.getFlowType().equals(ASK_PASSWORD)) {
                return handleAskPasswordFlow(context, password);
            }
            return new ExecutorResponse();
        } finally {
            Arrays.fill(password, '\0');
        }
    }

    private ExecutorResponse handleAskPasswordFlow(FlowExecutionContext context, char[] password) {

        String confirmationCode = (String) context.getProperty(CONFIRMATION_CODE_INPUT);
        User user = (User) context.getProperty(USER);
        if (StringUtils.isBlank(confirmationCode) || user == null) {
            return errorResponse(new ExecutorResponse(), "Required properties are missing in the context.");
        }

        String recoveryScenario = getStringProperty(context, IdentityRecoveryConstants.RECOVERY_SCENARIO);

        try {
            int tenantId = IdentityTenantUtil.getTenantId(context.getTenantDomain());

            handlePrePasswordUpdate(user, recoveryScenario, confirmationCode);

            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                    .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
            // If there are any claims to be updated, update them before updating the password.
            updateUserClaims(userStoreManager, context);
            userStoreManager.updateCredentialByAdmin(context.getFlowUser().getUsername(), password);

            String userId = ((AbstractUserStoreManager) userStoreManager)
                    .getUserIDFromUserName(context.getFlowUser().getUsername());
            context.getFlowUser().setUserId(userId);
            return new ExecutorResponse(STATUS_COMPLETE);
        } catch (UserStoreException | IdentityEventException | IdentityRecoveryException e) {
            LOG.error("Error while updating password for user: " + context.getFlowUser().getUsername(), e);
            return errorResponse(new ExecutorResponse(), e.getMessage());
        }
    }

    private String getStringProperty(FlowExecutionContext context, String key) {

        Object value = context.getProperty(key);
        return (value != null) ? value.toString() : null;
    }

    private void handlePrePasswordUpdate(User user, String recoveryScenario, String confirmationCode)
            throws IdentityRecoveryException, IdentityEventException {

        updateIdentityContext();
        publishEvent(user, confirmationCode, IdentityEventConstants.Event.
                PRE_ADD_NEW_PASSWORD, recoveryScenario);
    }

    private void updateIdentityContext() {

        Flow flow = new Flow.Builder()
                .name(Flow.Name.INVITED_USER_REGISTRATION)
                .initiatingPersona(Flow.InitiatingPersona.ADMIN)
                .build();

        if (IdentityContext.getThreadLocalIdentityContext().getFlow() != null) {
            // If the flow is already set, no need to update it again.
            return;
        }
        IdentityContext.getThreadLocalIdentityContext().setFlow(flow);
    }

    public void publishEvent(User user, String code, String eventName, String recoveryScenario) throws
            IdentityEventException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER, user);
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);
        properties.put(CONFIRMATION_CODE, code);

        Event identityMgtEvent = new Event(eventName, properties);
        IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
    }

    private ExecutorResponse handlePasswordRecoveryFlow(FlowExecutionContext context, char[] password) {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(context.getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                    .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();

            userStoreManager.updateCredentialByAdmin(context.getFlowUser().getUsername(), password);
            return new ExecutorResponse(STATUS_COMPLETE);
        } catch (UserStoreException e) {
            LOG.error("Error while updating password for user: " + context.getFlowUser().getUsername(), e);
            return new ExecutorResponse(ERROR);
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
     * @throws UserStoreException if an error occurs while accessing the user store.
     */
    private void updateUserClaims(UserStoreManager userStoreManager, FlowExecutionContext context)
            throws UserStoreException {

        FlowUser user = enrichUserProfile(context);
        userStoreManager.setUserClaimValues(user.getUsername(), user.getClaims(), null);
    }

    /**
     * Enriches the user profile with claims from the context.
     *
     * @param context FlowExecutionContext containing user information and claims.
     * @return Enriched FlowUser with claims and username.
     */
    private FlowUser enrichUserProfile(FlowExecutionContext context) {

        FlowUser user = context.getFlowUser();
        context.getUserInputData().forEach((key, value) -> {
            if (key.startsWith(WSO2_CLAIM_DIALECT) && !user.getClaims().containsKey(key)) {
                user.addClaim(key, value);
            }
        });
        return user;
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
