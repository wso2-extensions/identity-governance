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
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.ApplicationBasicInfo;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.application.mgt.ApplicationMgtUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.core.util.LambdaExceptionUtils;
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
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.user.action.api.constant.UserActionError;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
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
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static java.util.Locale.ENGLISH;
import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.EMAIL_ADDRESS_CLAIM;
import static org.wso2.carbon.identity.application.mgt.ApplicationConstants.MY_ACCOUNT_APPLICATION_NAME;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.USERNAME_CLAIM_URI;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.REGISTRATION;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DISPLAY_CLAIM_AVAILABILITY_CONFIG;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DUPLICATE_CLAIMS_ERROR_CODE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DUPLICATE_CLAIM_ERROR_CODE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_INVALID_USERNAME;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_VALIDATION_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_RESOLVE_NOTIFICATION_PROPERTY_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USERNAME_ALREADY_EXISTS;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USERSTORE_MANAGER_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_ONBOARD_FAILURE;
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
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";
    private static final String USERNAME_PATTERN_VALIDATION_SKIPPED = "isUsernamePatternValidationSkipped";

    @Override
    public String getName() {

        return "UserProvisioningExecutor";
    }

    @Override
    public List<String> getInitiationData() {

        LOG.debug("Initiation data is not required for the executor: " + getName());
        return null;
    }

    @Override
    public ExecutorResponse rollback(FlowExecutionContext context) {

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
            Map<String, String> userClaims = user.getClaims();

            String userStoreDomainName = resolveUserStoreDomain(user.getUsername());
            UserStoreManager userStoreManager = getUserStoreManager(context.getTenantDomain(), userStoreDomainName,
                    context.getContextIdentifier(), context.getFlowType());
            String domainQualifiedName = IdentityUtil.addDomainToName(user.getUsername(), userStoreDomainName);
            userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
            response.setResult(STATUS_COMPLETE);
            return response;
        } catch (UserStoreException e) {
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
            return errorResponse(response, ERROR_CODE_USER_ONBOARD_FAILURE, e, context.getFlowUser().getUsername(),
                    context.getContextIdentifier());
        } catch (FlowEngineClientException e) {
            return userErrorResponse(response, e);
        } catch (FlowEngineException e) {
            return errorResponse(response, e);
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

            String userStoreDomainName = resolveUserStoreDomain(user.getUsername());
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
            userStoreManager.addUser(IdentityUtil.addDomainToName(user.getUsername(), userStoreDomainName),
                    String.valueOf(password), userRoles, userClaims, null);
            String userid = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(user.getUsername());
            user.setUserStoreDomain(userStoreDomainName);
            user.setUserId(userid);
            createFederatedAssociations(user, context.getTenantDomain(), context.getContextIdentifier());
            if (LOG.isDebugEnabled()) {
                LOG.debug("User: " + user.getUsername() + " successfully onboarded in user store: " +
                        userStoreDomainName + " of tenant: " + context.getTenantDomain());
            }
            response.setResult(STATUS_COMPLETE);
            return response;
        } catch (UserStoreException e) {
            response = handleAndThrowClientExceptionForActionFailure(response, e);
            if (response.getResult() != null) {
                return response;
            }
            boolean displayClaimAvailability = Boolean.parseBoolean(
                    IdentityUtil.getProperty(DISPLAY_CLAIM_AVAILABILITY_CONFIG));
            if (displayClaimAvailability && e.getMessage().contains(USER_ALREADY_EXISTING_USERNAME)) {
                return userErrorResponse(response, ERROR_CODE_USERNAME_ALREADY_EXISTS, context.getTenantDomain());
            } else if (e.getMessage().contains(USER_ALREADY_EXISTING_USERNAME)) {
                return userErrorResponse(response, ERROR_CODE_USER_PROVISIONING_FAILURE, context.getTenantDomain());
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

    private ExecutorResponse handleAndThrowClientExceptionForActionFailure(ExecutorResponse response, UserStoreException e) {

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

    private FlowUser updateUserProfile(FlowExecutionContext context) throws FlowEngineException {

        FlowUser user = context.getFlowUser();
        context.getUserInputData().forEach((key, value) -> {
            if (key.startsWith(WSO2_CLAIM_DIALECT)) {
                if (!user.getClaims().containsKey(key)) {
                    user.addClaim(key, value);
                }
            }
        });
        user.setUsername(resolveUsername(user, context.getTenantDomain()));
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

    private String resolveUserStoreDomain(String username) {

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

    private String resolveUsername(FlowUser user, String tenantDomain) throws FlowEngineException {

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
        } else if (IdentityUtil.isEmailUsernameEnabled() && !username.contains("@")) {
            throw handleClientException(ERROR_CODE_INVALID_USERNAME, username);
        }
        return username;
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
                            localUser.setUserName(user.getUsername());
                            localUser.setTenantDomain(tenantDomain);
                            localUser.setUserStoreDomain(user.getUserStoreDomain());
                            fedAssociationManager.createFederatedAssociation(localUser, idpName, idpSubjectId);
                        } catch (FederatedAssociationManagerException e) {
                            LOG.error("Error while creating federated association for user: " + user.getUsername()
                                    + " with IdP: " + idpName + " and subject ID: " + idpSubjectId, e);
                            throw handleServerException(ERROR_CODE_USER_ONBOARD_FAILURE, e, user.getUsername(), flowId);
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
                    context.getFlowUser().getUsername(), context.getContextIdentifier());
        }

        Property[] properties = new Property[3];

        properties[0] = new Property("spId", applicationId);
        properties[1] = new Property("sp", applicationName);
        properties[2] = new Property("callback", callBackUrl);
        return properties;
    }
}
