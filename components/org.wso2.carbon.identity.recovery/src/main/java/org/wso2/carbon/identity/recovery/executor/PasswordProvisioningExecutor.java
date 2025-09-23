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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.graph.AuthenticationExecutor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.user.action.api.constant.UserActionError;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_INPUT_REQUIRED;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.INVITED_USER_REGISTRATION;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.PASSWORD_RECOVERY;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.REGISTRATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;

/**
 * Executor to provision the password.
 */
public class PasswordProvisioningExecutor extends AuthenticationExecutor {

    private static final Log LOG = LogFactory.getLog(PasswordProvisioningExecutor.class);
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";

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

        Map<String, char[]> credentials;
        String passwordValue = context.getUserInputData() != null ? context.getUserInputData().get(PASSWORD_KEY) : null;
        if (StringUtils.isNotBlank(passwordValue)) {
            credentials = new HashMap<>();
            credentials.put(PASSWORD_KEY, passwordValue.toCharArray());
            context.getFlowUser().setUserCredentials(credentials);
        } else {
            credentials = context.getFlowUser().getUserCredentials();
            if (MapUtils.isEmpty(credentials)) {
                return buildUserInputRequiredResponse();
            }
        }

        if (REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {
            return new ExecutorResponse(STATUS_COMPLETE);
        }

        char[] password = credentials.getOrDefault(PASSWORD_KEY, new DefaultPasswordGenerator().generatePassword());
        try {
            if (PASSWORD_RECOVERY.getType().equalsIgnoreCase(context.getFlowType())) {
                return handlePasswordRecoveryFlow(context, password);
            } else if (INVITED_USER_REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {
                return handleAskPasswordFlow(context, password);
            }
            return new ExecutorResponse();
        } finally {
            Arrays.fill(password, '\0');
        }
    }

    private ExecutorResponse handleAskPasswordFlow(FlowExecutionContext context, char[] password) {

        String confirmationCode = (String) context.getProperty(CONFIRMATION_CODE_INPUT);
        User user = Utils.resolveUserFromContext(context);
        if (StringUtils.isBlank(confirmationCode) || user == null) {
            return errorResponse(new ExecutorResponse(), "Required properties are missing in the context.");
        }

        String recoveryScenario = getStringProperty(context, IdentityRecoveryConstants.RECOVERY_SCENARIO);

        try {
            enterFlow();
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
            String maskedUsername = LoggerUtils.isLogMaskingEnable
                    ? LoggerUtils.getMaskedContent(context.getFlowUser().getUsername())
                    : context.getFlowUser().getUsername();
            LOG.error("Error while updating password for user: " + maskedUsername, e);

                ExecutorResponse errorResponse =
                        handleAndThrowClientExceptionForActionFailure(new ExecutorResponse(), e);
                if (errorResponse.getResult() != null) {
                    return errorResponse;
                }
            return errorResponse(new ExecutorResponse(), e.getMessage());
        } finally {
            IdentityContext.getThreadLocalIdentityContext().exitFlow();
        }
    }

    private String getStringProperty(FlowExecutionContext context, String key) {

        Object value = context.getProperty(key);
        return (value != null) ? value.toString() : null;
    }

    private void handlePrePasswordUpdate(User user, String recoveryScenario, String confirmationCode)
            throws IdentityRecoveryException, IdentityEventException {

        publishEvent(user, confirmationCode, IdentityEventConstants.Event.
                PRE_ADD_NEW_PASSWORD, recoveryScenario);
    }

    private void enterFlow() {

        Flow.InitiatingPersona initiatingPersona;
        Flow existingFlow = IdentityContext.getThreadLocalIdentityContext().getCurrentFlow();

        if (existingFlow != null) {
            initiatingPersona = existingFlow.getInitiatingPersona();
        } else {
            initiatingPersona = Flow.InitiatingPersona.ADMIN;
        }

        Flow flow = new Flow.Builder()
                .name(Flow.Name.INVITED_USER_REGISTRATION)
                .initiatingPersona(initiatingPersona)
                .build();

        IdentityContext.getThreadLocalIdentityContext().enterFlow(flow);
    }

    private void publishEvent(User user, String code, String eventName, String recoveryScenario) throws
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
            UserRealm userRealm = getUserRealm(context.getTenantDomain());
            if (userRealm == null) {
                ExecutorResponse executorResponse = new ExecutorResponse(STATUS_ERROR);
                executorResponse.setErrorMessage("User realm is not available for tenant");
                return executorResponse;
            }
            UserStoreManager userStoreManager = userRealm.getUserStoreManager()
                    .getSecondaryUserStoreManager(context.getFlowUser().getUserStoreDomain());
            userStoreManager.updateCredentialByAdmin(context.getFlowUser().getUsername(), password);
            return new ExecutorResponse(STATUS_COMPLETE);
        } catch (UserStoreException e) {
            String maskedUsername = LoggerUtils.isLogMaskingEnable
                    ? LoggerUtils.getMaskedContent(context.getFlowUser().getUsername())
                    : context.getFlowUser().getUsername();
            LOG.error("Error while updating password for user: " + maskedUsername, e);

            ExecutorResponse errorResponse = handleAndThrowClientExceptionForActionFailure(new ExecutorResponse(), e);
            if (errorResponse.getResult() != null) {
                return errorResponse;
            }
            return errorResponse(new ExecutorResponse(), e.getMessage());
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

    /**
     * Creates a user error response with the provided details.
     *
     * @param response    ExecutorResponse to be modified with user error details.
     * @param errorCode   Error code to be set in the response.
     * @param message     User error message to be set in the response.
     * @param description Description of the error to be set in the response.
     * @param throwable   Throwable associated with the error.
     * @return Modified ExecutorResponse with user error details.
     */
    private ExecutorResponse userErrorResponse(ExecutorResponse response, String errorCode, String message,
                                               String description, Throwable throwable) {

        response.setErrorCode(errorCode);
        response.setErrorMessage(message);
        response.setErrorDescription(description);
        response.setThrowable(throwable);
        response.setResult(Constants.ExecutorStatus.STATUS_USER_ERROR);
        return response;
    }

    /**
     * Retrieves the user realm for the given tenant domain.
     *
     * @param tenantDomain Tenant domain.
     * @return UserRealm instance for the tenant.
     * @throws UserStoreException If an error occurs while retrieving the user realm.
     */
    private UserRealm getUserRealm(String tenantDomain) throws UserStoreException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        int tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
        return (UserRealm) realmService.getTenantUserRealm(tenantId);
    }

    private ExecutorResponse handleAndThrowClientExceptionForActionFailure(ExecutorResponse response, Exception e) {

        if (e instanceof UserStoreClientException &&
                UserActionError.PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED
                        .equals(((UserStoreClientException) e).getErrorCode())) {
            Throwable cause = e.getCause();
            while (cause != null) {
                if (cause instanceof UserActionExecutionClientException) {
                    return userErrorResponse(response,
                            ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE.getCode(),
                            ((UserActionExecutionClientException) cause).getError(),
                            ((UserActionExecutionClientException) cause).getDescription(), cause);
                }
                cause = cause.getCause();
            }
        }
        return response;
    }

}
