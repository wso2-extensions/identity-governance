/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com).
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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.action.execution.api.exception.ActionExecutionException;
import org.wso2.carbon.identity.action.execution.api.model.ActionExecutionStatus;
import org.wso2.carbon.identity.action.execution.api.model.ActionType;
import org.wso2.carbon.identity.action.execution.api.model.Error;
import org.wso2.carbon.identity.action.execution.api.model.Failure;
import org.wso2.carbon.identity.action.execution.api.model.FlowContext;
import org.wso2.carbon.identity.action.execution.api.model.Organization;
import org.wso2.carbon.identity.action.execution.api.service.ActionExecutorService;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.ApplicationBasicInfo;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.application.mgt.ApplicationMgtUtil;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.core.util.LambdaExceptionUtils;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.execution.engine.Constants;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineClientException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineServerException;
import org.wso2.carbon.identity.flow.execution.engine.graph.Executor;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.recovery.util.ExecutorConsentUtils;
import org.wso2.carbon.identity.user.action.api.constant.UserActionError;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionServerException;
import org.wso2.carbon.identity.user.action.api.model.UserActionContext;
import org.wso2.carbon.identity.user.action.api.model.UserActionRequestDTO;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.exception.FederatedAssociationManagerException;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.Permission;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.user.mgt.common.DefaultPasswordGenerator;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import static java.util.Locale.ENGLISH;
import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.EMAIL_ADDRESS_CLAIM;
import static org.wso2.carbon.identity.application.mgt.ApplicationConstants.MY_ACCOUNT_APPLICATION_NAME;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.USERNAME_CLAIM_URI;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.INVITED_USER_REGISTRATION;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.PASSWORD_RECOVERY;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.PROFILE_UPDATE;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.REGISTRATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DISPLAY_CLAIM_AVAILABILITY_CONFIG;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DUPLICATE_CLAIMS_ERROR_CODE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DUPLICATE_CLAIM_ERROR_CODE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_INVALID_USERNAME;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_PRE_UPDATE_PROFILE_ACTION_VALIDATION_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_RESOLVE_NOTIFICATION_PROPERTY_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USERNAME_ALREADY_EXISTS;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USERSTORE_MANAGER_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_ONBOARD_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_PROFILE_UPDATE_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_EXISTENCE_CHECK_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_PROVISIONING_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.REGISTRATION_DEFAULT_USER_STORE_CONFIG;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.USER_ALREADY_EXISTING_USERNAME;
import static org.wso2.carbon.user.core.UserCoreConstants.APPLICATION_DOMAIN;
import static org.wso2.carbon.user.core.UserCoreConstants.INTERNAL_DOMAIN;
import static org.wso2.carbon.user.core.UserCoreConstants.WORKFLOW_DOMAIN;

/**
 * Implementation of the executor which handles onboarding the user to the system.
 */
public class UserProvisioningExecutor implements Executor {

    private static final Log LOG = LogFactory.getLog(UserProvisioningExecutor.class);
    private static final String COMPONENT_ID = "UserProvisioningExecutor";
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";
    private static final String USERNAME_PATTERN_VALIDATION_SKIPPED = "isUsernamePatternValidationSkipped";
    private static final String PROVISIONED_USER_ID = "provisionedUserId";

    @Override
    public String getName() {

        return COMPONENT_ID;
    }

    @Override
    public List<String> getInitiationData() {

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initiation data is not required for the executor: " + getName());
        }
        return Collections.emptyList();
    }

    /**
     * Deletes the user this executor created in the flow. A failed delete is logged rather than thrown, so the
     * flow reports the failure that caused the rollback.
     */
    @Override
    public ExecutorResponse rollback(FlowExecutionContext context) {

        RegistrationCompletionDeferral.discard(context);
        String userId = (String) context.getProperty(PROVISIONED_USER_ID);
        if (StringUtils.isBlank(userId)) {
            return null;
        }

        try {
            FlowUser flowUser = context.getFlowUser();
            UserStoreManager userStoreManager = getUserStoreManager(context.getTenantDomain(),
                    flowUser.getUserStoreDomain(), context.getContextIdentifier(),
                    context.getFlowType());
            ((AbstractUserStoreManager) userStoreManager).deleteUserWithID(userId);
            // Clearing the record keeps a second rollback from attempting the delete again.
            context.getProperties().remove(PROVISIONED_USER_ID);
            if (StringUtils.equals(flowUser.getUserId(), userId)) {
                flowUser.setUserId(null);
            }
            if (LOG.isDebugEnabled()) {
                LOG.debug("Rolled back the user: " + userId + " provisioned in the flow: "
                        + context.getContextIdentifier());
            }
        } catch (FlowEngineException | UserStoreException e) {
            LOG.error("Failed to roll back the user: " + userId + " provisioned in the flow: "
                    + context.getContextIdentifier(), e);
        }
        return null;
    }

    @Override
    public ExecutorResponse execute(FlowExecutionContext context) {

        ExecutorResponse response = new ExecutorResponse();
        if (REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {
            return handleRegistrationFlow(response, context);
        }

        try {
            // If the flow type is not registration, update the user profile.
            FlowUser user = updateUserProfile(context);
            // Only persist claims the user actually changed during this flow. The preloaded CONSOLE-profile
            // snapshot (which includes non-attribute claims like role/roles) must not be written back.
            Map<String, String> userClaims = filterUpdatedClaims(user);
            Map<String, char[]> userCredentials = user.getUserCredentials();

            String userStoreDomainName = resolveUserStoreDomain(user, context.getFlowType());
            UserStoreManager userStoreManager = getUserStoreManager(context.getTenantDomain(), userStoreDomainName,
                    context.getContextIdentifier(), context.getFlowType());

            if (PROFILE_UPDATE.getType().equalsIgnoreCase(context.getFlowType())) {
                resolveVerifiedClaims(userClaims);
            }
            handlePreUpdateProfileAction(userClaims, user, context);
            handleUserPasswordUpdate(userCredentials, user, userStoreManager, context);
            handleUserClaimUpdate(userClaims, user, userStoreManager, userStoreDomainName, context.getFlowType());

            ExecutorConsentUtils.processUserConsent(COMPONENT_ID, context, user, userStoreDomainName);

            if (INVITED_USER_REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {
                createFederatedAssociations(user, context.getTenantDomain(), context.getContextIdentifier());
            }

            response.setResult(STATUS_COMPLETE);
            return response;
        } catch (UserStoreException e) {
            response = buildClientErrorResponseForActionFailure(response, e, context.getFlowType());
            if (response.getResult() != null) {
                return response;
            }
            if (e instanceof UserStoreClientException) {
                UserStoreClientException exception = (UserStoreClientException) e;
                boolean displayClaimAvailability = Boolean.parseBoolean(
                        IdentityUtil.getProperty(DISPLAY_CLAIM_AVAILABILITY_CONFIG));
                if (displayClaimAvailability && (DUPLICATE_CLAIM_ERROR_CODE.equals(exception.getErrorCode()) ||
                        DUPLICATE_CLAIMS_ERROR_CODE.equals(exception.getErrorCode()))) {
                    return userErrorResponse(response, ExecutorErrorMessages.ERROR_CODE_USER_CLAIM_ALREADY_EXISTS,
                            e.getMessage());
                }
                return userErrorResponse(response, ERROR_CODE_USER_PROVISIONING_FAILURE,
                        context.getContextIdentifier());
            }
            if (PROFILE_UPDATE.getType().equalsIgnoreCase(context.getFlowType())) {
                return errorResponse(response, ERROR_CODE_USER_PROFILE_UPDATE_FAILURE, e,
                        Utils.maskIfRequired(context.getFlowUser().getUsername()), context.getContextIdentifier());
            }
            return errorResponse(response, ERROR_CODE_USER_ONBOARD_FAILURE, e, context.getFlowUser().getUsername(),
                    context.getContextIdentifier());
        } catch (FlowEngineClientException e) {
            return userErrorResponse(response, e);
        } catch (FlowEngineException e) {
            return errorResponse(response, e);
        }
    }

    private void handleUserPasswordUpdate(Map<String, char[]> userCredentials, FlowUser flowUser,
                                          UserStoreManager userStoreManager, FlowExecutionContext context)
            throws FlowEngineException, UserStoreException {

        if (INVITED_USER_REGISTRATION.getType().equalsIgnoreCase(context.getFlowType())) {
            String confirmationCode = (String) context.getProperty(CONFIRMATION_CODE_INPUT);
            User user = Utils.resolveUserFromContext(context);
            if (StringUtils.isBlank(confirmationCode) || user == null) {
                throw new FlowEngineException("Required properties are missing in the context.");
            }
            String recoveryScenario = getRecoveryScenario(context);
            try {
                enterFlow();
                handlePrePasswordUpdate(user, recoveryScenario, confirmationCode);
                updateUserPassword(userCredentials, flowUser, userStoreManager);
                String userId = ((AbstractUserStoreManager) userStoreManager)
                        .getUserIDFromUserName(flowUser.getUsername());
                flowUser.setUserId(userId);
                flowUser.setUserStoreDomain(user.getUserStoreDomain());
            } finally {
                IdentityContext.getThreadLocalIdentityContext().exitFlow();
            }
        } else if (PASSWORD_RECOVERY.getType().equalsIgnoreCase(context.getFlowType())) {
            updateUserPassword(userCredentials, flowUser, userStoreManager);
        }
    }

    private void updateUserPassword(Map<String, char[]> userCredentials, FlowUser user,
                                    UserStoreManager userStoreManager)
            throws UserStoreException, FlowEngineException {

        char[] password = userCredentials.getOrDefault(PASSWORD_KEY, new DefaultPasswordGenerator().generatePassword());
        try {
            userStoreManager.updateCredentialByAdmin(user.getUsername(), password);
        } catch (UserStoreException e) {
            if (e instanceof UserStoreClientException && UserActionError.PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED
                    .equals(((UserStoreClientException) e).getErrorCode())) {
                throw e;
            }
            throw new FlowEngineException(null, e.getMessage(), null, e);
        } finally {
            Arrays.fill(password, '\0');
        }
    }

    private void handlePreUpdateProfileAction(Map<String, String> userClaims, FlowUser user,
                                              FlowExecutionContext context) throws UserStoreException {

        ActionExecutorService actionExecutorService =
                IdentityRecoveryServiceDataHolder.getInstance().getActionExecutorService();
        if (!PROFILE_UPDATE.getType().equalsIgnoreCase(context.getFlowType()) || userClaims.isEmpty() ||
                !actionExecutorService.isExecutionEnabled(ActionType.PRE_UPDATE_PROFILE)) {
            return;
        }

        if (LOG.isDebugEnabled()) {
            LOG.debug("Executing pre update profile action for user: " + user.getUserId() + " in the flow: "
                    + context.getContextIdentifier());
        }
        try {
            UserActionRequestDTO userActionRequestDTO = buildUserActionRequestDTO(userClaims, user,
                    context.getTenantDomain());
            UserActionContext userActionContext = new UserActionContext(userActionRequestDTO);

            FlowContext flowContext = FlowContext.create();
            flowContext.add(UserActionContext.USER_ACTION_CONTEXT_REFERENCE_KEY, userActionContext);

            ActionExecutionStatus<?> actionExecutionStatus = actionExecutorService.execute(
                    ActionType.PRE_UPDATE_PROFILE, flowContext, context.getTenantDomain());
            handleActionExecutionStatus(actionExecutionStatus);
        } catch (ActionExecutionException e) {
            throw new UserActionExecutionServerException(UserActionError.PRE_UPDATE_PROFILE_ACTION_SERVER_ERROR,
                    "Error while executing pre update profile action.", e);
        }
    }

    private UserActionRequestDTO buildUserActionRequestDTO(Map<String, String> userClaims, FlowUser user,
                                                           String tenantDomain) throws UserStoreException {

        UserActionRequestDTO.Builder userActionRequestDTOBuilder = new UserActionRequestDTO.Builder()
                .userId(user.getUserId())
                .residentOrganization(getUserResidentOrganization());

        populateUpdatingClaimsInUserActionRequestDTO(userClaims, userActionRequestDTOBuilder, tenantDomain);

        return userActionRequestDTOBuilder.build();
    }

    private void handleActionExecutionStatus(ActionExecutionStatus<?> actionExecutionStatus)
            throws UserStoreException {

        switch (actionExecutionStatus.getStatus()) {
            case SUCCESS:
                return;
            case FAILED:
                Failure failure = (Failure) actionExecutionStatus.getResponse();
                throw new UserActionExecutionClientException(UserActionError.PRE_UPDATE_PROFILE_ACTION_EXECUTION_FAILED,
                        failure.getFailureReason(), failure.getFailureDescription());
            case ERROR:
                Error error = (Error) actionExecutionStatus.getResponse();
                throw new UserActionExecutionServerException(UserActionError.PRE_UPDATE_PROFILE_ACTION_EXECUTION_ERROR,
                        error.getErrorMessage(), error.getErrorDescription());
            default:
                throw new UserActionExecutionServerException(UserActionError.PRE_UPDATE_PROFILE_ACTION_SERVER_ERROR,
                        "Unknown execution status received when executing pre update profile action.");
        }
    }

    private void populateUpdatingClaimsInUserActionRequestDTO(Map<String, String> userClaims,
                                                              UserActionRequestDTO.Builder userActionRequestDTOBuilder,
                                                              String tenantDomain) throws UserStoreException {

        String multiAttributeSeparator = FrameworkUtils.getMultiAttributeSeparator();
        for (Map.Entry<String, String> entry : userClaims.entrySet()) {
            String claimKey = entry.getKey();
            String claimValue = entry.getValue();
            if (isMultiValuedClaim(claimKey, tenantDomain)) {
                userActionRequestDTOBuilder.addClaim(claimKey,
                        StringUtils.isBlank(claimValue) ? new String[]{} :
                                claimValue.split(Pattern.quote(multiAttributeSeparator)));
            } else {
                userActionRequestDTOBuilder.addClaim(claimKey, claimValue);
            }
        }
    }

    private boolean isMultiValuedClaim(String claimUri, String tenantDomain) throws UserStoreException {

        try {
            Optional<LocalClaim> localClaim = IdentityRecoveryServiceDataHolder.getInstance()
                    .getClaimMetadataManagementService().getLocalClaim(claimUri, tenantDomain);
            if (!localClaim.isPresent()) {
                throw new UserActionExecutionServerException(UserActionError.PRE_UPDATE_PROFILE_ACTION_SERVER_ERROR,
                        "Claim not found for claim URI: " + claimUri);
            }
            return Boolean.parseBoolean(localClaim.get().getClaimProperty(ClaimConstants.MULTI_VALUED_PROPERTY));
        } catch (ClaimMetadataException e) {
            throw new UserActionExecutionServerException(UserActionError.PRE_UPDATE_PROFILE_ACTION_SERVER_ERROR,
                    "Error while retrieving claim metadata for claim URI: " + claimUri, e);
        }
    }

    private Organization getUserResidentOrganization() {

        // User resident organization is the accessing organization as shared users are not supported. Shared users
        // should invoke the flow from their resident organization.
        org.wso2.carbon.identity.core.context.model.Organization accessingOrganization =
                IdentityContext.getThreadLocalIdentityContext().getOrganization();
        if (accessingOrganization == null) {
            LOG.warn("Accessing organization is not present in the identity context. " +
                    "Hence cannot populate the user resident organization.");
            return null;
        }

        return new Organization.Builder()
                .id(accessingOrganization.getId())
                .name(accessingOrganization.getName())
                .orgHandle(accessingOrganization.getOrganizationHandle())
                .depth(accessingOrganization.getDepth())
                .build();
    }

    private void handleUserClaimUpdate(Map<String, String> userClaims, FlowUser user, UserStoreManager userStoreManager,
                                       String userStoreDomainName, String flowType)
            throws FlowEngineException, UserStoreException {

        String domainQualifiedName = IdentityUtil.addDomainToName(user.getUsername(), userStoreDomainName);
        if (userClaims.isEmpty()) {
            return;
        }
        boolean isProfileUpdate = PROFILE_UPDATE.getType().equalsIgnoreCase(flowType);
        try {
            if (isProfileUpdate) {
                skipVerificationOnUpdate(userClaims);
            }
            userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
        } finally {
            if (isProfileUpdate) {
                Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
                Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
            }
        }
    }

    /**
     * When the email or mobile is updated, keeps its verified claim only for a value verified by an OTP in the flow.
     *
     * @param userClaims Claims updated in the flow.
     */
    private void resolveVerifiedClaims(Map<String, String> userClaims) {

        resolveVerifiedClaim(userClaims, IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM,
                IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM,
                IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM);
        resolveVerifiedClaim(userClaims, IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM,
                IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM,
                IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM);
    }

    private void resolveVerifiedClaim(Map<String, String> userClaims, String claimUri, String verifiedValuesClaimUri,
                                      String verifiedClaimUri) {

        if (!userClaims.containsKey(claimUri)) {
            return;
        }
        String verifiedValue = userClaims.get(verifiedValuesClaimUri);
        String value = userClaims.get(claimUri);
        userClaims.remove(verifiedClaimUri);
        if (StringUtils.isNotBlank(value) && value.equals(verifiedValue)) {
            userClaims.put(verifiedClaimUri, Boolean.TRUE.toString());
        }
    }

    /**
     * Skips the email and mobile verification on update for an updated value verified in the flow.
     *
     * @param userClaims Claims to be saved.
     */
    private void skipVerificationOnUpdate(Map<String, String> userClaims) {

        if (userClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM) &&
                Boolean.parseBoolean(userClaims.get(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM))) {
            Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipEmailVerificationOnUpdateStates.SKIP_ON_EMAIL_OTP_FLOW.toString());
        }
        if (userClaims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM) &&
                Boolean.parseBoolean(userClaims.get(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM))) {
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_SMS_OTP_FLOW.toString());
        }
    }

    private ExecutorResponse handleRegistrationFlow(ExecutorResponse response, FlowExecutionContext context) {

        char[] password = null;
        try {
            FlowUser user = updateUserProfile(context);
            Map<String, String> userClaims = user.getClaims();
            Map<String, char[]> credentials = user.getUserCredentials();
            password =
                    credentials.getOrDefault(PASSWORD_KEY, new DefaultPasswordGenerator().generatePassword());

            String userStoreDomainName = resolveUserStoreDomain(user, context.getFlowType());
            String usernameWithoutDomain = UserCoreUtil.removeDomainFromName(user.getUsername());
            userClaims.put(USERNAME_CLAIM_URI, usernameWithoutDomain);
            UserStoreManager userStoreManager = getUserStoreManager(context.getTenantDomain(), userStoreDomainName,
                    context.getContextIdentifier(), context.getFlowType());

            if (!userStoreManager.isExistingRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                userStoreManager.addRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE, null,
                        new Permission[]{});
            }

            // Set notification properties to be used in the UserSelfRegistrationHandler.
            Property[] notificationProperties = resolveNotificationProperties(context);
            Utils.setArbitraryProperties(notificationProperties);

            String[] userRoles = new String[]{IdentityRecoveryConstants.SELF_SIGNUP_ROLE};
            userStoreManager.addUser(IdentityUtil.addDomainToName(usernameWithoutDomain, userStoreDomainName),
                    String.valueOf(password), userRoles, userClaims, null);
            String userid = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(user.getUsername());
            user.setUserStoreDomain(userStoreDomainName);
            user.setUserId(userid);
            // Only a user created here may be rolled back; the flow user's ID is also set for existing users.
            context.setProperty(PROVISIONED_USER_ID, userid);

            ExecutorConsentUtils.processUserConsent(COMPONENT_ID, context, user, userStoreDomainName);

            createFederatedAssociations(user, context.getTenantDomain(), context.getContextIdentifier());
            if (LOG.isDebugEnabled()) {
                LOG.debug("User: " + Utils.maskIfRequired(user.getUsername()) +
                        " successfully onboarded in user store: " + userStoreDomainName + " of tenant: " +
                        context.getTenantDomain());
            }
            response.setResult(STATUS_COMPLETE);
            return response;
        } catch (UserStoreException e) {
            response = buildClientErrorResponseForActionFailure(response, e, context.getFlowType());
            if (response.getResult() != null) {
                return response;
            }
            boolean displayClaimAvailability = Boolean.parseBoolean(
                    IdentityUtil.getProperty(DISPLAY_CLAIM_AVAILABILITY_CONFIG));
            if (e.getMessage().contains(USER_ALREADY_EXISTING_USERNAME)) {
                if (displayClaimAvailability) {
                    return userErrorResponse(response, ERROR_CODE_USERNAME_ALREADY_EXISTS, context.getTenantDomain());
                } else {
                    return userErrorResponse(response, ERROR_CODE_USER_PROVISIONING_FAILURE,
                            context.getContextIdentifier());
                }
            }
            if (e instanceof UserStoreClientException) {
                UserStoreClientException exception = (UserStoreClientException) e;
                if (displayClaimAvailability && (DUPLICATE_CLAIM_ERROR_CODE.equals(exception.getErrorCode()) ||
                        DUPLICATE_CLAIMS_ERROR_CODE.equals(exception.getErrorCode()))) {
                    return userErrorResponse(response, ExecutorErrorMessages.ERROR_CODE_USER_CLAIM_ALREADY_EXISTS,
                            e.getMessage());
                }
                return userErrorResponse(response, ERROR_CODE_USER_PROVISIONING_FAILURE,
                        context.getContextIdentifier());
            }
            return errorResponse(response, ERROR_CODE_USER_ONBOARD_FAILURE, e, context.getFlowUser().getUsername(),
                    context.getContextIdentifier());
        } catch (FlowEngineClientException e) {
            return userErrorResponse(response, e);
        } catch (FlowEngineException e) {
            return errorResponse(response, e);
        } finally {
            if (password != null) {
                Arrays.fill(password, '\0');
            }
        }
    }

    private ExecutorResponse buildClientErrorResponseForActionFailure(ExecutorResponse response, UserStoreException e,
                                                                      String flowType) {

        if (e instanceof UserActionExecutionClientException) {
            UserActionExecutionClientException exception = (UserActionExecutionClientException) e;
            if (UserActionError.PRE_UPDATE_PROFILE_ACTION_EXECUTION_FAILED.equals(exception.getErrorCode())) {
                return userErrorResponse(response, ERROR_CODE_PRE_UPDATE_PROFILE_ACTION_VALIDATION_FAILURE.getCode(),
                        exception.getError(), exception.getDescription(), e);
            }
        }
        if (e instanceof UserStoreClientException &&
                UserActionError.PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED
                        .equals(((UserStoreClientException) e).getErrorCode())) {
            String errorCode = REGISTRATION.getType().equalsIgnoreCase(flowType)
                    ? ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE.getCode()
                    : Constants.ErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE.getCode();
            Throwable cause = e.getCause();
            while (cause != null) {
                if (cause instanceof UserActionExecutionClientException) {
                    return userErrorResponse(response,
                            errorCode,
                            ((UserActionExecutionClientException) cause).getError(),
                            ((UserActionExecutionClientException) cause).getDescription(), cause);
                }
                cause = cause.getCause();
            }
        }
        return response;
    }

    private FlowUser updateUserProfile(FlowExecutionContext context) throws FlowEngineException {

        FlowUser user = context.getFlowUser();
        context.getUserInputData().forEach((key, value) -> {
            if (key.startsWith(WSO2_CLAIM_DIALECT)) {
                if (!user.getClaims().containsKey(key)) {
                    user.addClaim(key, value);
                }
            }
        });

        Object anonymousProfileTracker = context.getProperty(Constants.ANONYMOUS_PROFILE_TRACKER);
        if (anonymousProfileTracker != null) {
            String cdsProfileClaimUri = WSO2_CLAIM_DIALECT + IdentityRecoveryConstants.CDS_PROFILE_ATTRIBUTE;
            if (!user.getClaims().containsKey(cdsProfileClaimUri)) {
                user.addUpdatedClaim(cdsProfileClaimUri, String.valueOf(anonymousProfileTracker));
            }
        }
        user.setUsername(resolveUsername(user, context));
        setUsernamePatternValidation(context);
        return user;
    }

    private static void setUsernamePatternValidation(FlowExecutionContext context) {

        Boolean isUsernamePatternValidationSkipped = (Boolean) context.getProperty(USERNAME_PATTERN_VALIDATION_SKIPPED);
        if (isUsernamePatternValidationSkipped == null || !isUsernamePatternValidationSkipped) {
            return;
        }
        UserCoreUtil.setSkipUsernamePatternValidationThreadLocal(true);
    }

    private UserStoreManager getUserStoreManager(String tenantDomain, String userDomain, String flowId, String flowType)
            throws FlowEngineException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        UserStoreManager userStoreManager;
        try {
            UserRealm tenantUserRealm = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(tenantDomain));
            if (IdentityUtil.getPrimaryDomainName().equals(userDomain)) {
                userStoreManager = (UserStoreManager) tenantUserRealm.getUserStoreManager();
            } else {
                userStoreManager =
                        ((UserStoreManager) tenantUserRealm.getUserStoreManager())
                                .getSecondaryUserStoreManager(userDomain);
            }
            if (userStoreManager == null) {
                throw handleServerException(ERROR_CODE_USERSTORE_MANAGER_FAILURE, tenantDomain, flowType, flowId);
            }
            return userStoreManager;
        } catch (UserStoreException e) {
            throw handleServerException(ERROR_CODE_USERSTORE_MANAGER_FAILURE, e, tenantDomain, flowType, flowId);
        }
    }

    private String resolveUserStoreDomain(FlowUser user, String flowType) {

        if (PROFILE_UPDATE.getType().equalsIgnoreCase(flowType) && StringUtils.isNotBlank(user.getUserStoreDomain())) {
            return user.getUserStoreDomain();
        }

        String username = user.getUsername();
        int separatorIndex = username.indexOf(UserCoreConstants.DOMAIN_SEPARATOR);
        if (separatorIndex >= 0) {
            String domain = username.substring(0, separatorIndex);
            if (INTERNAL_DOMAIN.equalsIgnoreCase(domain) || WORKFLOW_DOMAIN.equalsIgnoreCase(domain)
                    || APPLICATION_DOMAIN.equalsIgnoreCase(domain)) {
                return domain.substring(0, 1).toUpperCase(ENGLISH) + domain.substring(1).toLowerCase(ENGLISH);
            }
            return domain.toUpperCase(ENGLISH);
        }

        String domainName = IdentityUtil.getProperty(REGISTRATION_DEFAULT_USER_STORE_CONFIG);
        return domainName != null ? domainName.toUpperCase(ENGLISH) :
                IdentityUtil.getPrimaryDomainName().toUpperCase(ENGLISH);
    }

    private String resolveUsername(FlowUser user, FlowExecutionContext context) throws FlowEngineException {

        String tenantDomain = context.getTenantDomain();
        String username = Optional.ofNullable(user.getClaims().get(USERNAME_CLAIM_URI)).orElse("");
        if (StringUtils.isBlank(username)) {
            if ((FlowExecutionEngineUtils.isEmailUsernameValidator(tenantDomain) ||
                    IdentityUtil.isEmailUsernameEnabled())
                    && StringUtils.isNotBlank((String) user.getClaim(EMAIL_ADDRESS_CLAIM))) {
                // If email format validation is enabled and username is not provided, use email as username.
                return (String) user.getClaim(EMAIL_ADDRESS_CLAIM);
            }
            // Else generate a random UUID as the username and set the skip validation flag.
            username = UUID.randomUUID().toString();
            UserCoreUtil.setSkipUsernamePatternValidationThreadLocal(true);
            return username;
        } else if (IdentityUtil.isEmailUsernameEnabled() && !username.contains("@")
                && !isExistingUser(user, context)) {
            throw handleClientException(ERROR_CODE_INVALID_USERNAME, Utils.maskIfRequired(username));
        }
        return username;
    }

    private boolean isExistingUser(FlowUser user, FlowExecutionContext context) throws FlowEngineException {

        try {
            String userStoreDomain = StringUtils.isNotBlank(user.getUserStoreDomain())
                    ? user.getUserStoreDomain()
                    : resolveUserStoreDomain(user, context.getFlowType());
            UserStoreManager userStoreManager = getUserStoreManager(context.getTenantDomain(), userStoreDomain,
                    context.getContextIdentifier(), context.getFlowType());
            return userStoreManager.isExistingUser(user.getUsername());
        } catch (UserStoreException e) {
            String maskedUsername = LoggerUtils.isLogMaskingEnable
                    ? LoggerUtils.getMaskedContent(user.getUsername()) : user.getUsername();
            throw handleServerException(ERROR_CODE_USER_EXISTENCE_CHECK_FAILURE, e,
                    maskedUsername, context.getContextIdentifier());
        }
    }

    private void createFederatedAssociations(FlowUser user, String tenantDomain, String flowId) {

        if (user.getFederatedAssociations().isEmpty()) {
            return;
        }
        FederatedAssociationManager fedAssociationManager =
                IdentityRecoveryServiceDataHolder.getInstance().getFederatedAssociationManager();
        user.getFederatedAssociations()
                .forEach(LambdaExceptionUtils.rethrowBiConsumer((idpName, idpSubjectId) -> {
                    if (StringUtils.isNotBlank(idpName) && StringUtils.isNotBlank(idpSubjectId)) {
                        try {
                            User localUser = new User();
                            localUser.setUserName(UserCoreUtil.removeDomainFromName(user.getUsername()));
                            localUser.setTenantDomain(tenantDomain);
                            localUser.setUserStoreDomain(user.getUserStoreDomain());
                            fedAssociationManager.createFederatedAssociation(localUser, idpName, idpSubjectId);
                        } catch (FederatedAssociationManagerException e) {
                            throw handleServerException(ERROR_CODE_USER_ONBOARD_FAILURE, e,
                                    Utils.maskIfRequired(user.getUsername()), flowId);
                        }
                    }
                }));
    }

    /**
     * Handle the flow engine server exceptions.
     *
     * @param error Error message.
     * @param e     Throwable.
     * @param data  The error message data.
     * @return FlowEngineServerException.
     */
    private FlowEngineServerException handleServerException(ExecutorErrorMessages error, Throwable e, Object... data) {

        String description = error.getDescription();
        if (ArrayUtils.isNotEmpty(data)) {
            description = String.format(description, data);
        }
        return new FlowEngineServerException(error.getCode(), error.getMessage(), description, e);
    }

    /**
     * Handle the flow engine server exceptions.
     *
     * @param error Error message.
     * @param data  The error message data.
     * @return FlowEngineServerException.
     */
    private FlowEngineServerException handleServerException(ExecutorErrorMessages error, Object... data) {

        String description = error.getDescription();
        if (ArrayUtils.isNotEmpty(data)) {
            description = String.format(description, data);
        }
        return new FlowEngineServerException(error.getCode(), error.getMessage(), description);
    }

    /**
     * Handle the flow engine client exceptions.
     *
     * @param error Error message.
     * @param data  The error message data.
     * @return FlowEngineClientException.
     */
    private static FlowEngineClientException handleClientException(ExecutorErrorMessages error, Object... data) {

        String description = error.getDescription();
        if (ArrayUtils.isNotEmpty(data)) {
            description = String.format(description, data);
        }
        return new FlowEngineClientException(error.getCode(), error.getMessage(), description);
    }


    /**
     * Creates an error response with the provided details.
     *
     * @param response ExecutorResponse to be modified with error details.
     * @param error    Error message enum to be set in the response.
     * @param data     Optional data to format the error description.
     * @return Modified ExecutorResponse with error details.
     */
    private ExecutorResponse errorResponse(ExecutorResponse response, ExecutorErrorMessages error,
                                           Throwable e, Object... data) {

        String description = error.getDescription();
        if (ArrayUtils.isNotEmpty(data)) {
            description = String.format(description, data);
        }
        response.setErrorCode(error.getCode());
        response.setErrorMessage(error.getMessage());
        response.setErrorDescription(description);
        response.setThrowable(e);
        response.setResult(Constants.ExecutorStatus.STATUS_ERROR);
        return response;
    }

    /**
     * Creates an error response with the provided FlowEngineException details.
     *
     * @param response            ExecutorResponse to be modified with error details.
     * @param flowEngineException FlowEngineException containing error details.
     * @return Modified ExecutorResponse with error details.
     */
    private ExecutorResponse errorResponse(ExecutorResponse response, FlowEngineException flowEngineException) {

        response.setErrorMessage(flowEngineException.getMessage());
        response.setErrorCode(flowEngineException.getErrorCode());
        response.setErrorDescription(flowEngineException.getDescription());
        response.setThrowable(flowEngineException);
        response.setResult(Constants.ExecutorStatus.STATUS_ERROR);
        return response;
    }

    /**
     * Creates an error response with the provided details.
     *
     * @param response ExecutorResponse to be modified with error details.
     * @param error    Error message enum to be set in the response.
     * @param data     Optional data to format the error description.
     * @return Modified ExecutorResponse with error details.
     */
    private ExecutorResponse userErrorResponse(ExecutorResponse response, ExecutorErrorMessages error, Object... data) {

        String description = error.getDescription();
        if (ArrayUtils.isNotEmpty(data)) {
            description = String.format(description, data);
        }
        response.setErrorCode(error.getCode());
        response.setErrorMessage(error.getMessage());
        response.setErrorDescription(description);
        response.setResult(Constants.ExecutorStatus.STATUS_USER_ERROR);
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
     * Creates a user error response with the provided FlowEngineException details.
     *
     * @param response            ExecutorResponse to be modified with user error details.
     * @param flowEngineException FlowEngineException containing error details.
     * @return Modified ExecutorResponse with user error details.
     */
    private ExecutorResponse userErrorResponse(ExecutorResponse response, FlowEngineException flowEngineException) {

        response.setErrorCode(flowEngineException.getErrorCode());
        response.setErrorMessage(flowEngineException.getMessage());
        response.setErrorDescription(flowEngineException.getDescription());
        response.setThrowable(flowEngineException);
        response.setResult(Constants.ExecutorStatus.STATUS_USER_ERROR);
        return response;
    }

    private Property[] resolveNotificationProperties(FlowExecutionContext context) throws FlowEngineServerException {

        String tenantDomain = context.getTenantDomain();
        ApplicationManagementService applicationManagementService =
                IdentityRecoveryServiceDataHolder.getInstance().getApplicationManagementService();

        String applicationId = context.getApplicationId();
        String applicationName;
        String callBackUrl;
        ApplicationBasicInfo appInfo;
        try {
            if (StringUtils.isEmpty(applicationId)) {
                applicationName = MY_ACCOUNT_APPLICATION_NAME;
                appInfo = applicationManagementService.getApplicationBasicInfoByName(applicationName, tenantDomain);
                applicationId = (appInfo != null) ? appInfo.getUuid() : null;
                callBackUrl = (appInfo != null) ? appInfo.getAccessUrl() :
                        ApplicationMgtUtil.getMyAccountAccessUrlFromServerConfig(tenantDomain);
            } else {
                appInfo = applicationManagementService.getApplicationBasicInfoByResourceId(applicationId, tenantDomain);
                applicationName = (appInfo != null) ? appInfo.getApplicationName() : null;
                callBackUrl = (appInfo != null) ? appInfo.getAccessUrl() : null;
            }
        } catch (IdentityApplicationManagementException e) {
            throw handleServerException(ERROR_CODE_RESOLVE_NOTIFICATION_PROPERTY_FAILURE, e,
                    Utils.maskIfRequired(context.getFlowUser().getUsername()), context.getContextIdentifier());
        }

        Property[] properties = new Property[3];

        properties[0] = new Property("spId", applicationId);
        properties[1] = new Property("sp", applicationName);
        properties[2] = new Property("callback", callBackUrl);
        return properties;
    }

    /**
     * Returns claims the user actually changed during this flow, based on
     * {@link FlowUser#getUpdatedClaimUris()}.
     *
     * @param user flow user whose claims are being filtered.
     * @return map of user-updated claims; empty when nothing was changed in this flow.
     */
    private Map<String, String> filterUpdatedClaims(FlowUser user) {

        Set<String> updated = user.getUpdatedClaimUris();
        if (updated == null || updated.isEmpty()) {
            return new HashMap<>();
        }
        Map<String, String> result = new HashMap<>();
        for (String uri : updated) {
            // Skip updating username in non-registration flows.
            if (USERNAME_CLAIM_URI.equals(uri)) {
                continue;
            }
            String value = user.getClaims().get(uri);
            if (value != null) {
                result.put(uri, value);
            }
        }
        return result;
    }

    private String getRecoveryScenario(FlowExecutionContext context) {

        Object value = context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO);
        return (value != null) ? value.toString() : null;
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

    private void handlePrePasswordUpdate(User user, String recoveryScenario, String confirmationCode)
            throws FlowEngineException {

        try {
            HashMap<String, Object> properties = new HashMap<>();
            properties.put(IdentityEventConstants.EventProperty.USER, user);
            properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
            properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
            properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
            properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);
            properties.put(CONFIRMATION_CODE, confirmationCode);

            Event identityMgtEvent = new Event(IdentityEventConstants.Event.
                    PRE_ADD_NEW_PASSWORD, properties);

            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw new FlowEngineException(null, e.getMessage(), null, e);
        }
    }
}
