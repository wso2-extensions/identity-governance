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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;

import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_CHANNEL;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.RECOVERY_SCENARIO;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.USER;

/**
 * Validates the confirmation code provided by the user during the ask password flow.
 */
public class ConfirmationCodeValidationExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(ConfirmationCodeValidationExecutor.class);

    @Override
    public String getName() {

        return "ConfirmationCodeValidationExecutor";
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext flowExecutionContext) {

        ExecutorResponse response = new ExecutorResponse();
        String confirmationCode = flowExecutionContext.getUserInputData().get(CONFIRMATION_CODE_INPUT);

        if (StringUtils.isBlank(confirmationCode)) {
            return clientInputRequiredResponse(response, CONFIRMATION_CODE_INPUT);
        }
        try {
            UserRecoveryData userRecoveryData = validateConfirmationCode(confirmationCode);
            setupFlowUser(flowExecutionContext, userRecoveryData.getUser());

            flowExecutionContext.setProperty(CONFIRMATION_CODE_INPUT, confirmationCode);
            flowExecutionContext.setProperty(USER, userRecoveryData.getUser());
            flowExecutionContext.setProperty(NOTIFICATION_CHANNEL, userRecoveryData.getRemainingSetIds());
            if (userRecoveryData.getRecoveryScenario() != null) {
                flowExecutionContext.setProperty(RECOVERY_SCENARIO, userRecoveryData.getRecoveryScenario().name());
            }

            response.setResult(STATUS_COMPLETE);
        } catch (IdentityRecoveryException | UserStoreException e) {
            String errorMessage = "Error while validating the confirmation code.";
            LOG.error(errorMessage, e);
            return errorResponse(response, errorMessage);
        }
        return response;
    }

    /**
     * Sets up the flow user with the necessary claims and user ID.
     * @param flowExecutionContext Flow execution context containing the flow user.
     * @param user                  User object containing user details.
     * @throws UserStoreException if there is an error while retrieving user claims or user ID.
     */
    private void setupFlowUser(FlowExecutionContext flowExecutionContext, User user) throws UserStoreException {

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance()
                .getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();

        try {
            // Get all local claims for the console profile.
            List<LocalClaim> localClaimList = IdentityRecoveryServiceDataHolder.getInstance()
                    .getClaimMetadataManagementService()
                    .getSupportedLocalClaimsForProfile(
                            user.getTenantDomain(),
                            ClaimConstants.DefaultAllowedClaimProfile.CONSOLE.getProfileName());

            // Extract claim URIs.
            String[] claimURIs = localClaimList.stream()
                    .map(LocalClaim::getClaimURI)
                    .toArray(String[]::new);

            String domainQualifiedName = IdentityUtil.addDomainToName(user.getUserName(),
                    user.getUserStoreDomain());
            // Fetch claims from user store.
            Map<String, String> claimsMap = userStoreManager.
                    getUserClaimValues(domainQualifiedName, claimURIs, null);

            // Add claims to the flow user for use by the following executors.
            FlowUser flowUser = flowExecutionContext.getFlowUser();
            flowUser.addClaims(claimsMap);

            // Set user ID and username.
            String userId = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(domainQualifiedName);
            flowUser.setUserId(userId);
            flowUser.setUsername(domainQualifiedName);
            flowExecutionContext.setTenantDomain(user.getTenantDomain());
        } catch (ClaimMetadataException | UserStoreException e) {
            throw new UserStoreException("Error while retrieving or setting user claims", e);
        }
    }

    /**
     * Validates the confirmation code and retrieves the associated user recovery data.
     * @param code  Confirmation code to validate.
     * @return  UserRecoveryData associated with the confirmation code.
     * @throws IdentityRecoveryException if there is an error during validation or retrieval.
     */
    private UserRecoveryData validateConfirmationCode(String code) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData;

        try {
            userRecoveryData = userRecoveryDataStore.load(Utils.hashCode(code));
        } catch (NoSuchAlgorithmException e) {
            throw new IdentityRecoveryServerException("Error while hashing the confirmation code", e);
        } catch (IdentityRecoveryException e) {
            // Fallback to plain code.
            userRecoveryData = userRecoveryDataStore.load(code);
        }

        // Validate tenant domain.
        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = userRecoveryData.getUser().getTenantDomain();

        if (!StringUtils.equals(contextTenantDomain, userTenantDomain)) {
            throw new IdentityRecoveryClientException("Invalid tenant domain: " + userTenantDomain);
        }
        return userRecoveryData;
    }

    @Override
    public List<String> getInitiationData() {

        return null;
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext flowExecutionContext) {

        return null;
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
     * Creates a response indicating that client input is required.
     *
     * @param response ExecutorResponse to be modified.
     * @param fields   Fields that are required from the client.
     * @return Modified ExecutorResponse indicating client input is required.
     */
    private ExecutorResponse clientInputRequiredResponse(ExecutorResponse response, String... fields) {

        response.setResult(Constants.ExecutorStatus.STATUS_CLIENT_INPUT_REQUIRED);
        response.setRequiredData(Arrays.asList(fields));
        return response;
    }
}
