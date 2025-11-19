/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.axiom.om.util.Base64;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationFailureReason;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.mgt.Constants;
import org.wso2.carbon.identity.flow.mgt.exception.FlowMgtServerException;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.governance.exceptions.otp.OTPGeneratorException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.governance.service.otp.OTPGenerator;
import org.wso2.carbon.identity.handler.event.account.lock.exception.AccountLockServiceException;
import org.wso2.carbon.identity.recovery.AuditConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationClientException;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.user.functionality.mgt.UserFunctionalityMgtConstants;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.constants.UserCoreErrorConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND;
import static org.wso2.carbon.identity.core.util.IdentityUtil.getPrimaryDomainName;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_EMAIL_OTP_ENABLE;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_REGISTRATION_OPTION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.USER;
import static org.wso2.carbon.user.core.UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME;
import static org.wso2.carbon.utils.CarbonUtils.isLegacyAuditLogsDisabled;

/**
 * Class which contains the Utils for user recovery.
 */
public class Utils {

    private static final Log AUDIT_LOG = CarbonConstants.AUDIT_LOG;
    private static final Log log = LogFactory.getLog(Utils.class);

    //This is used to pass the arbitrary properties from self user manager to self user handler
    private static ThreadLocal<org.wso2.carbon.identity.recovery.model.Property[]> arbitraryProperties = new
            ThreadLocal<>();

    //This is used to pass the verifyEmail claim from preAddUser to postAddUser
    private static ThreadLocal<Claim> emailVerifyTemporaryClaim = new ThreadLocal<>();

    //This is used to pass the askPassword claim from preAddUser to postAddUser
    private static ThreadLocal<Claim> askPasswordTemporaryClaim = new ThreadLocal<>();

    /**
     * This thread local variable is used to prevent sending of a verification email when SetUserClaimsListener is
     * triggered in the UserEmailVerificationHandler in other update scenarios where the purpose is not to update the
     * email address claim with a new email address.
     */
    private static ThreadLocal<String> skipSendingEmailVerificationOnUpdateState = new ThreadLocal<>();

    /**
     * This thread local variable is used to pass the state to prevent sending a verification SMS OTP when
     * SetUserClaimsListener is triggered in the MobileNumberVerificationHandler in other update scenarios where the
     * purpose is not to update the mobile number with a new value.
     */
    private static ThreadLocal<String> skipSendingSmsOtpVerificationOnUpdate = new ThreadLocal<>();

    /**
     * This thread local variable is used to stop verification pending mobile number from being set as the primary
     * mobile number when only updating the verifiedMobileNumbers claim.
     */
    private static ThreadLocal<Boolean> isOnlyVerifiedMobileNumbersUpdated = new ThreadLocal<>();

    /**
     * This thread local variable is used to stop verification pending email address from being set as the primary
     * email address when only updating the verifiedEmailAddress claim.
     */
    private static ThreadLocal<Boolean> isOnlyVerifiedEmailAddressesUpdated = new ThreadLocal<>();

    //Error messages that are caused by password pattern violations
    private static final String[] pwdPatternViolations = new String[]{UserCoreErrorConstants.ErrorMessages
            .ERROR_CODE_ERROR_DURING_PRE_UPDATE_CREDENTIAL_BY_ADMIN.getCode(), UserCoreErrorConstants.ErrorMessages
            .ERROR_CODE_ERROR_DURING_PRE_UPDATE_CREDENTIAL.getCode()};

    private static final String PROPERTY_PASSWORD_ERROR_MSG = "PasswordJavaRegExViolationErrorMsg";

    private static final String EMAIL_USERNAME_IDENTIFIER = "@";

    /**
     * Get an instance of the NotificationChannelManager.
     *
     * @return Instance of the NotificationChannelManager
     */
    public static NotificationChannelManager getNotificationChannelManager() {

        return (NotificationChannelManager) PrivilegedCarbonContext.getThreadLocalCarbonContext()
                .getOSGiService(NotificationChannelManager.class, null);
    }

    /**
     * @return
     */
    public static org.wso2.carbon.identity.recovery.model.Property[] getArbitraryProperties() {

        if (arbitraryProperties.get() == null) {
            return null;
        }
        return arbitraryProperties.get();
    }

    /**
     * @param properties
     */
    public static void setArbitraryProperties(org.wso2.carbon.identity.recovery.model.Property[] properties) {

        arbitraryProperties.set(properties);
    }

    public static void clearArbitraryProperties() {

        arbitraryProperties.remove();
    }

    /**
     * @return
     */
    public static Claim getEmailVerifyTemporaryClaim() {

        if (emailVerifyTemporaryClaim.get() == null) {
            return null;
        }
        return emailVerifyTemporaryClaim.get();
    }

    /**
     * @param claim
     */
    public static void setEmailVerifyTemporaryClaim(Claim claim) {

        emailVerifyTemporaryClaim.set(claim);
    }

    public static void clearEmailVerifyTemporaryClaim() {

        emailVerifyTemporaryClaim.remove();
    }

    /**
     * Retrieves the temporary claim associated with the "Ask Password" functionality.
     *
     * @return The temporary {@link Claim} object if set, or {@code null} if not set.
     */
    public static Claim getAskPasswordTemporaryClaim() {

        if (askPasswordTemporaryClaim.get() == null) {
            return null;
        }
        return askPasswordTemporaryClaim.get();
    }

    /**
     * Sets a temporary claim for the ask password recovery process.
     *
     * @param claim The claim to be temporarily stored for the ask password recovery process.
     */
    public static void setAskPasswordTemporaryClaim(Claim claim) {

        askPasswordTemporaryClaim.set(claim);
    }

    public static void clearAskPasswordTemporaryClaim() {

        askPasswordTemporaryClaim.remove();
    }

    /**
     * Clears the thread local used to maintain the email verification skipping state.
     */
    public static void unsetThreadLocalToSkipSendingEmailVerificationOnUpdate() {

        skipSendingEmailVerificationOnUpdateState.remove();
    }

    /**
     * Retrieve the state to skip email verification.
     *
     * @return The state  should be skipped.
     */
    public static String getThreadLocalToSkipSendingEmailVerificationOnUpdate() {

        return skipSendingEmailVerificationOnUpdateState.get();
    }

    /**
     * Sets the thread local value to represent the state whether email verification is to be skipped.
     *
     * @param value The email verification state to be skipped.
     */
    public static void setThreadLocalToSkipSendingEmailVerificationOnUpdate(String value) {

        skipSendingEmailVerificationOnUpdateState.set(value);
    }

    /**
     * Clear the thread local used to maintain the SMS OTP verification skipping state.
     */
    public static void unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate() {

        skipSendingSmsOtpVerificationOnUpdate.remove();
    }

    /**
     * Retrieve the state to skip mobile verification.
     *
     * @return The state to be skipped.
     */
    public static String getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate() {

        return skipSendingSmsOtpVerificationOnUpdate.get();
    }

    /**
     * Set the thread local value to represent the state whether mobile verification is to be skipped.
     *
     * @param value The mobile verification state to be skipped.
     */
    public static void setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(String value) {

        skipSendingSmsOtpVerificationOnUpdate.set(value);
    }

    public static void unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated() {

        isOnlyVerifiedMobileNumbersUpdated.remove();
    }

    public static void setThreadLocalIsOnlyVerifiedMobileNumbersUpdated(Boolean value) {

        isOnlyVerifiedMobileNumbersUpdated.set(value);
    }

    public static boolean getThreadLocalIsOnlyVerifiedMobileNumbersUpdated() {

        Boolean value = isOnlyVerifiedMobileNumbersUpdated.get();
        return value != null && value;
    }

    public static void unsetThreadLocalIsOnlyVerifiedEmailAddressesUpdated() {

        isOnlyVerifiedEmailAddressesUpdated.remove();
    }

    public static void setThreadLocalIsOnlyVerifiedEmailAddressesUpdated(Boolean value) {

        isOnlyVerifiedEmailAddressesUpdated.set(value);
    }

    public static boolean getThreadLocalIsOnlyVerifiedEmailAddressesUpdated() {

        Boolean value = isOnlyVerifiedEmailAddressesUpdated.get();
        return value != null && value;
    }

    public static String getClaimFromUserStoreManager(User user, String claim)
            throws UserStoreException {

        String userStoreQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        String claimValue = "";

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        if (realmService.getTenantUserRealm(tenantId) != null) {
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        }

        if (userStoreManager != null) {
            Map<String, String> claimsMap = userStoreManager
                    .getUserClaimValues(userStoreQualifiedUsername, new String[]{claim},
                            UserCoreConstants.DEFAULT_PROFILE);
            if (claimsMap != null && !claimsMap.isEmpty()) {
                claimValue = claimsMap.get(claim);
            }
        }
        return claimValue;

    }

    public static void removeClaimFromUserStoreManager(User user, String[] claims)
            throws UserStoreException {

        String userStoreQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        if (realmService.getTenantUserRealm(tenantId) != null) {
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        }

        if (userStoreManager != null) {
            userStoreManager
                    .deleteUserClaimValues(userStoreQualifiedUsername, claims, UserCoreConstants.DEFAULT_PROFILE);
        }
    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(
                IdentityRecoveryServerException.class, error.getCode(), errorDescription);
    }

    public static IdentityRecoveryServerException handleFunctionalityLockMgtServerException(
            IdentityRecoveryConstants.ErrorMessages error, String userId, int tenantId, String functionalityIdentifier,
            boolean isDetailedErrorMessagesEnabled) throws IdentityRecoveryServerException {

        String mappedErrorCode = Utils.prependOperationScenarioToErrorCode(error.getCode(),
                IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO);
        StringBuilder message = new StringBuilder(error.getMessage());
        if (isDetailedErrorMessagesEnabled) {
            message.append(
                    String.format("functionality: %s \nuserId: %s \ntenantId: %d.", functionalityIdentifier, userId,
                            tenantId));
        }
        throw handleServerException(mappedErrorCode, message.toString(), null);
    }

    public static IdentityRecoveryServerException handleServerException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data, Throwable e)
            throws IdentityRecoveryServerException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(
                IdentityRecoveryServerException.class, error.getCode(), errorDescription, e);
    }

    /**
     * Handle Server Exceptions.
     *
     * @param errorCode    Error code of the exception
     * @param errorMessage Error message of the exception
     * @param data         Meta data associated with the exception
     * @return IdentityRecoveryServerException as the server error
     */
    public static IdentityRecoveryServerException handleServerException(String errorCode, String errorMessage,
                                                                        String data) {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(errorMessage, data);
        } else {
            errorDescription = errorMessage;
        }
        return IdentityException.error(IdentityRecoveryServerException.class, errorCode, errorDescription);
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages
                                                                                error, String data)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(IdentityRecoveryClientException.class, error.getCode(), errorDescription);
    }

    public static IdentityRecoveryClientException handleClientException(IdentityRecoveryConstants.ErrorMessages error,
                                                                        String data,
                                                                        Throwable e)
            throws IdentityRecoveryClientException {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(error.getMessage(), data);
        } else {
            errorDescription = error.getMessage();
        }

        return IdentityException.error(IdentityRecoveryClientException.class, error.getCode(), errorDescription, e);
    }

    /**
     * Handles client exceptions by creating an instance of {@link IdentityRecoveryClientException}
     * with the specified error details.
     * If the provided data is not blank, it is formatted into the error message; otherwise,
     * the default error message is used.
     *
     * @param code        The predefined error code.
     * @param message     A brief message describing the error.
     * @param description A detailed description of the error.
     * @param e           The underlying cause of the exception.
     * @return An instance of {@link IdentityRecoveryClientException} with the provided details.
     */
    public static IdentityRecoveryClientException handleClientException(String code, String message, String description,
                                                                        Throwable e) {

        return IdentityException.error(IdentityRecoveryClientException.class, code, message, description, e);
    }

    /**
     * Handle Client Exceptions.
     *
     * @param errorCode    Error code of the exception
     * @param errorMessage Error message of the exception
     * @param data         Meta data associated with the exception
     * @return IdentityRecoveryClientException
     */
    public static IdentityRecoveryClientException handleClientException(String errorCode, String errorMessage,
                                                                        String data) {

        String errorDescription;
        if (StringUtils.isNotBlank(data)) {
            errorDescription = String.format(errorMessage, data);
        } else {
            errorDescription = errorMessage;
        }
        return IdentityException.error(IdentityRecoveryClientException.class, errorCode, errorDescription);
    }

    /**
     * @param value
     * @return
     * @throws UserStoreException
     */
    @Deprecated
    public static String doHash(String value) throws UserStoreException {

        try {
            return hashCode(value);
        } catch (NoSuchAlgorithmException e) {
            log.error(e.getMessage(), e);
            throw new UserStoreException(e.getMessage(), e);
        }
    }

    /**
     * @param value     Value to be hashed
     * @return          Hashed value
     * @throws NoSuchAlgorithmException If the algorithm is not found.
     */
    public static String hashCode(String value) throws NoSuchAlgorithmException {

        String digsestFunction = "SHA-256";
        MessageDigest dgst = MessageDigest.getInstance(digsestFunction);
        byte[] byteValue = dgst.digest(value.getBytes(StandardCharsets.UTF_8));
        return Base64.encode(byteValue);
    }

    /**
     * Set claim to user store manager
     *
     * @param user  user
     * @param claim claim uri
     * @param value claim value
     * @throws IdentityException if fails
     */
    public static void setClaimInUserStoreManager(User user, String claim, String value) throws UserStoreException {

        String fullUserName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        if (realmService.getTenantUserRealm(tenantId) != null) {
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        }

        if (userStoreManager != null) {
            Map<String, String> values = userStoreManager.getUserClaimValues(fullUserName, new String[]{
                    claim}, UserCoreConstants.DEFAULT_PROFILE);
            String oldValue = values.get(claim);
            if (oldValue == null || !oldValue.equals(value)) {
                Map<String, String> claimMap = new HashMap<String, String>();
                claimMap.put(claim, value);
                userStoreManager.setUserClaimValues(fullUserName, claimMap, UserCoreConstants.DEFAULT_PROFILE);
            }
        }

    }

    /**
     * Get the claim values for given claim list of user.
     *
     * @param user       User.
     * @param claimsList Claims list to retrieve.
     * @return Map of claims list with corresponding values.
     * @throws IdentityRecoveryClientException If an invalid input is detected.
     * @throws IdentityRecoveryServerException If an error occurred while retrieving claims.
     */
    public static Map<String, String> getClaimListOfUser(User user, String[] claimsList)
            throws IdentityRecoveryClientException, IdentityRecoveryServerException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = getUserStoreManager(user);
        String userStoreQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        if (ArrayUtils.isEmpty(claimsList)) {
            throw handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS,
                    null);
        }
        try {
            return userStoreManager
                    .getUserClaimValues(userStoreQualifiedUsername, claimsList, UserCoreConstants.DEFAULT_PROFILE);
        } catch (UserStoreException e) {
            throw handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS,
                    user.getUserName(), e);
        }
    }

    /**
     * Set user claims list.
     *
     * @param user      User.
     * @param claimsMap Map of claims list to update.
     * @throws IdentityRecoveryClientException If an invalid input is detected.
     * @throws IdentityRecoveryServerException If an error occurred while updating user claims.
     */
    public static void setClaimsListOfUser(User user, Map<String, String> claimsMap)
            throws IdentityRecoveryClientException, IdentityRecoveryServerException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = getUserStoreManager(user);
        String userStoreQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(),
                user.getUserStoreDomain());
        try {
            if (MapUtils.isNotEmpty(claimsMap)) {
                userStoreManager.setUserClaimValues(userStoreQualifiedUsername, claimsMap,
                        UserCoreConstants.DEFAULT_PROFILE);
            }
        } catch (UserStoreException e) {
            throw handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS,
                    null, e);
        }
    }

    private static org.wso2.carbon.user.core.UserStoreManager getUserStoreManager(User user)
            throws IdentityRecoveryClientException, IdentityRecoveryServerException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager;

        // Validate method inputs.
        if (user == null) {
            throw handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                    "Invalid User Data provided.");
        }

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            if (realmService == null || realmService.getTenantUserRealm(tenantId) == null) {
                throw handleServerException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, user.getTenantDomain());
            }
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        } catch (UserStoreException e) {
            throw handleServerException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, user.getTenantDomain(), e);
        }

        if (userStoreManager == null) {
            throw handleServerException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null);
        }
        return userStoreManager;
    }

    public static String getRecoveryConfigs(String key, String tenantDomain) throws IdentityRecoveryServerException {

        try {
            Property[] connectorConfigs;
            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, tenantDomain);
            for (Property connectorConfig : connectorConfigs) {
                if (key.equals(connectorConfig.getName())) {
                    return connectorConfig.getValue();
                }
            }
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null);
        } catch (IdentityGovernanceException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null, e);
        }
    }

    public static String getSignUpConfigs(String key, String tenantDomain) throws IdentityRecoveryServerException {

        try {
            Property[] connectorConfigs;
            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, tenantDomain);
            if (connectorConfigs == null || connectorConfigs.length == 0 || connectorConfigs[0] == null) {
                return null;
            }
            return connectorConfigs[0].getValue();
        } catch (IdentityGovernanceException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_SIGNUP_CONFIGS, null, e);
        }
    }

    public static String getConnectorConfig(String key, String tenantDomain) throws IdentityEventException {

        try {
            Property[] connectorConfigs;
            IdentityGovernanceService identityGovernanceService = IdentityRecoveryServiceDataHolder.getInstance()
                    .getIdentityGovernanceService();
            connectorConfigs = identityGovernanceService.getConfiguration(new String[]{key}, tenantDomain);
            if (connectorConfigs == null || connectorConfigs.length == 0 || connectorConfigs[0] == null) {
                return null;
            }
            return connectorConfigs[0].getValue();
        } catch (IdentityGovernanceException e) {
            throw new IdentityEventException("Error while getting connector configurations", e);
        }
    }

    // challenge question related Util
    public static String getChallengeSetDirFromUri(String challengeSetUri) {

        if (StringUtils.isBlank(challengeSetUri)) {
            return challengeSetUri;
        }

        String[] components = challengeSetUri.split(IdentityRecoveryConstants.WSO2CARBON_CLAIM_DIALECT + "/");
        return components.length > 1 ? components[1] : components[0];
    }

    public static boolean isAccountLocked(User user) throws IdentityRecoveryException {

        try {
            return IdentityRecoveryServiceDataHolder.getInstance().getAccountLockService().isAccountLocked(user
                    .getUserName(), user.getTenantDomain(), user.getUserStoreDomain());
        } catch (AccountLockServiceException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_CHECK_ACCOUNT_LOCK_STATUS, user.getUserName(), e);
        }
    }

    public static boolean isAccountDisabled(User user) throws IdentityRecoveryException {

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        UserRealm userRealm;
        try {
            userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, user.getTenantDomain(), e);
        }

        org.wso2.carbon.user.core.UserStoreManager userStoreManager;
        try {
            userStoreManager = userRealm.getUserStoreManager();
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER, null, e);
        }

        try {
            Map<String, String> values = userStoreManager.getUserClaimValues(IdentityUtil.addDomainToName(user
                    .getUserName(), user.getUserStoreDomain()), new String[]{
                    IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM}, UserCoreConstants.DEFAULT_PROFILE);
            boolean accountDisable = Boolean.parseBoolean(values.get(IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM));
            return accountDisable;
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS, null, e);
        }
    }

    public static User createUser(String username, String tenantDomain) {

        User user = new User();
        user.setUserName(MultitenantUtils.getTenantAwareUsername(username));
        user.setTenantDomain(tenantDomain);

        return user;
    }

    public static boolean validateCallbackURL(String callbackURL, String tenantDomain, String callbackRegexType)
            throws IdentityEventException {

        String callbackRegex = getConnectorConfig(callbackRegexType, tenantDomain);
        if (callbackRegex != null && callbackURL.matches(callbackRegex)) {
            return true;
        }
        return false;
    }

    public static String getCallbackURLFromRegistration(org.wso2.carbon.identity.recovery.model.Property[] properties)
            throws UnsupportedEncodingException, MalformedURLException {

        if (properties == null) {
            return null;
        }
        String callbackURL = null;
        for (org.wso2.carbon.identity.recovery.model.Property property : properties) {
            if (IdentityRecoveryConstants.CALLBACK.equals(property.getKey())) {
                callbackURL = URLDecoder.decode(property.getValue(), IdentityRecoveryConstants.UTF_8);
                break;
            }
        }

        if (StringUtils.isNotBlank(callbackURL)) {
            URL url = new URL(callbackURL);
            callbackURL = new URL(url.getProtocol(), url.getHost(), url.getPort(), url.getPath(), null)
                    .toString();
        }
        return callbackURL;
    }

    public static String getCallbackURL(org.wso2.carbon.identity.recovery.model.Property[] properties)
            throws UnsupportedEncodingException, URISyntaxException {

        if (properties == null) {
            return null;
        }
        String callbackURL = null;
        for (org.wso2.carbon.identity.recovery.model.Property property : properties) {
            if (IdentityRecoveryConstants.CALLBACK.equals(property.getKey())) {
                callbackURL = property.getValue();
                break;
            }
        }

        if (StringUtils.isNotBlank(callbackURL)) {
            URI uri = new URI(callbackURL);
            callbackURL = new URI(uri.getScheme(), uri.getAuthority(), uri.getPath(), null, null)
                    .toString();
        }
        return callbackURL;
    }

    /**
     * Get isAccessUrlAvailable property value.
     *
     * @param properties Properties array.
     * @return boolean value of the isAccessUrlAvailable property.
     */
    public static boolean isAccessUrlAvailable(org.wso2.carbon.identity.recovery.model.Property[] properties) {

        if (properties != null) {
            for (org.wso2.carbon.identity.recovery.model.Property property : properties) {
                if (IdentityRecoveryConstants.IS_ACCESS_URL_AVAILABLE.equals(property.getKey())) {
                    return Boolean.parseBoolean(property.getValue());
                }
            }
        }
        return false;
    }

    /**
     * Get whether this is tenant flow
     *
     * @param properties
     * @return
     * @throws UnsupportedEncodingException
     * @throws URISyntaxException
     */
    public static boolean isLiteSignUp(org.wso2.carbon.identity.recovery.model.Property[] properties) {

        if (properties == null) {
            return false;
        }
        boolean isLiteSignUp = false;
        for (org.wso2.carbon.identity.recovery.model.Property property : properties) {
            if (IdentityRecoveryConstants.IS_LITE_SIGN_UP.equals(property.getKey())) {
                isLiteSignUp = Boolean.parseBoolean(property.getValue());
            }
        }
        return isLiteSignUp;
    }

    /**
     * Extracts the boolean value of 'isUserPortalURL' from the properties.
     *
     * @param properties from the request
     * @return the boolean value of 'isUserPortalURL'
     */
    public static boolean isUserPortalURL(org.wso2.carbon.identity.recovery.model.Property[] properties) {

        if (properties == null) {
            return false;
        }

        for (org.wso2.carbon.identity.recovery.model.Property property : properties) {
            if (IdentityRecoveryConstants.IS_USER_PORTAL_URL.equals(property.getKey())) {
                return Boolean.parseBoolean(property.getValue());
            }
        }
        return false;
    }

    /**
     * Check if the exception contains a password pattern violation message and act accordingly
     *
     * @param exception An UserStoreException
     * @throws IdentityRecoveryClientException If exception's message contains a password pattern violation message
     */
    public static void checkPasswordPatternViolation(UserStoreException exception, User user)
            throws IdentityRecoveryClientException {

        if (StringUtils.isBlank(exception.getMessage())) {
            return;
        }
        RealmConfiguration realmConfig = getRealmConfiguration(user);
        String passwordErrorMessage = realmConfig.getUserStoreProperty(PROPERTY_PASSWORD_ERROR_MSG);
        String exceptionMessage = exception.getMessage();
        if (((StringUtils.indexOfAny(exceptionMessage, pwdPatternViolations) >= 0) &&
                StringUtils.containsIgnoreCase(exceptionMessage, passwordErrorMessage)) ||
                exceptionMessage.contains(UserCoreErrorConstants.ErrorMessages.ERROR_CODE_INVALID_PASSWORD.getCode())) {

            if (StringUtils.isNotEmpty(passwordErrorMessage)) {
                throw IdentityException.error(IdentityRecoveryClientException.class,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
                        passwordErrorMessage, exception);
            } else {
                String errorMessage = String.format(
                        UserCoreErrorConstants.ErrorMessages.ERROR_CODE_INVALID_PASSWORD.getMessage(),
                        realmConfig.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_JAVA_REG_EX));
                throw IdentityException.error(IdentityRecoveryClientException.class,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
                        errorMessage, exception);
            }
        }
    }

    /**
     * Get RealmConfiguration by tenantId
     *
     * @param user User
     * @return realmConfiguration RealmConfiguration of the given tenant
     * @throws IdentityRecoveryClientException If fails
     */
    private static RealmConfiguration getRealmConfiguration(User user) throws IdentityRecoveryClientException {

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        try {
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            if (StringUtils.equals(user.getUserStoreDomain(), getPrimaryDomainName())) {
                return ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).getRealmConfiguration();
            }
            UserStoreManager secondaryUserStoreManager =
                    ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).getSecondaryUserStoreManager(
                            user.getUserStoreDomain());
            if (secondaryUserStoreManager != null) {
                return ((org.wso2.carbon.user.core.UserStoreManager) userStoreManager).getRealmConfiguration();
            }
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DOMAIN_VIOLATED,
                    user.getUserStoreDomain(),
                    new UserStoreClientException("Invalid domain " + user.getUserStoreDomain() + " provided."));
        } catch (UserStoreException userStoreException) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                    null, userStoreException);
        }
    }

    /**
     * Checks whether the accountState claim exists and returns true if the claim exists, else returns false.
     *
     * @param tenantDomain tenantDomain
     * @return true if accountState claim exists else return false
     * @throws IdentityEventException
     */
    public static boolean isAccountStateClaimExisting(String tenantDomain) throws IdentityEventException {

        org.wso2.carbon.user.api.UserRealm userRealm = null;
        ClaimManager claimManager = null;
        boolean isExist = false;

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        if (realmService != null) {
            try {
                int tenantId = realmService.getTenantManager().getTenantId(tenantDomain);
                // Get tenant's user realm.
                userRealm = realmService.getTenantUserRealm(tenantId);
                if (userRealm != null) {
                    // Get claim manager for manipulating attributes.
                    claimManager = (ClaimManager) userRealm.getClaimManager();
                    if (claimManager != null) {
                        Claim claim = claimManager.getClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
                        if (claim != null) {
                            isExist = true;
                        }
                    }
                }
            } catch (UserStoreException e) {
                throw new IdentityEventException("Error while retrieving accountState claim from ClaimManager.", e);
            }
        }
        return isExist;
    }

    /**
     * Prepend the operation scenario to the existing exception error code.
     * (Eg: USR-20045)
     *
     * @param exceptionErrorCode Existing error code.
     * @param scenario           Operation scenario
     * @return New error code with the scenario prepended (NOTE: Return an empty String if the provided error code is
     * empty)
     */
    public static String prependOperationScenarioToErrorCode(String exceptionErrorCode, String scenario) {

        if (StringUtils.isNotEmpty(exceptionErrorCode)) {
            // Check whether the scenario is already in the errorCode.
            if (exceptionErrorCode.contains(IdentityRecoveryConstants.EXCEPTION_SCENARIO_SEPARATOR)) {
                return exceptionErrorCode;
            }
            if (StringUtils.isNotEmpty(scenario)) {
                exceptionErrorCode =
                        scenario + IdentityRecoveryConstants.EXCEPTION_SCENARIO_SEPARATOR + exceptionErrorCode;
            }
        }
        return exceptionErrorCode;
    }

    /**
     * Check whether the internally notification management property is send with the meta properties. If management
     * mechanism is specified, return the specified boolean value. If the management mechanism is not specified in
     * meta properties return server configurations.
     *
     * @param tenantDomain Tenant domain
     * @param properties   Meta properties
     * @return True when notifications are managed internally
     */
    public static boolean isNotificationsInternallyManaged(String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException {

        if (MapUtils.isNotEmpty(properties)) {
            try {
                String manageNotificationsInternally = properties
                        .get(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY);
                // Notification managed mechanism is not specified in the request.
                if (StringUtils.isEmpty(manageNotificationsInternally)) {
                    return Boolean.parseBoolean(Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, tenantDomain));
                } else {
                    return Boolean.parseBoolean(manageNotificationsInternally);
                }
            } catch (NumberFormatException e) {
                String manageNotificationsInternally = Utils
                        .getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                                tenantDomain);
                if (log.isDebugEnabled()) {
                    String error = String
                            .format("Invalid boolean value : %s to enable enable internal notification management. "
                                            + "Server default value : %s will be used.", properties.get
                                            (IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY),
                                    manageNotificationsInternally);
                    log.debug(error);
                }
                return Boolean.parseBoolean(manageNotificationsInternally);
            }
        }
        // The request has no meta properties, then server configurations will be returned.
        return Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                        tenantDomain));
    }

    /**
     * Resolve event name according to the notification channel.
     *
     * @param notificationChannel Notification channel
     * @return Resolved event name
     */
    public static String resolveEventName(String notificationChannel) {

        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannel)) {
            return IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + notificationChannel
                    + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX
                    + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX_LOCAL;
        } else {
            return IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        }
    }

    /**
     * Validate email username.
     *
     * @param username Tenant aware username of the user.
     * @throws IdentityRecoveryClientException If username is not an email when email username is enabled.
     * @deprecated This method is deprecated. Use {@link #validateEmailUsername(User user)} instead.
     */
    @Deprecated
    public static void validateEmailUsername(String username) throws IdentityRecoveryClientException {

        if (IdentityUtil.isEmailUsernameEnabled()
                && StringUtils.countMatches(username, EMAIL_USERNAME_IDENTIFIER) == 0) {
            throw handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USERNAME, username);
        }
    }

    /**
     * Validate email username.
     *
     * @param user User .
     * @throws IdentityRecoveryClientException If username is not an email for tenanted users when email username
     * is enabled.
     */
    public static void validateEmailUsername(User user) throws IdentityRecoveryClientException {

        if (!IdentityUtil.isEmailUsernameEnabled()) {
            // Doesn't need to check for the username if "email address as the username" option is disabled.
            return;
        }
        if (MultitenantConstants.SUPER_TENANT_DOMAIN_NAME.equals(user.getTenantDomain())) {
            // Super tenant user should be able to log in with both email username or non-email username.
            return;
        }
        if (StringUtils.countMatches(user.getUserName(), EMAIL_USERNAME_IDENTIFIER) == 0) {
            // For other tenants, user should have an email address as username.
            throw handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USERNAME,
                    user.getUserName());
        }
    }

    /**
     * Build a User object.
     *
     * @param username     Username of the user with userstore domain (Ex: PRIMARY/user1).
     * @param tenantDomain Tenant domain of the user.
     * @return User object.
     */
    public static User buildUser(String username, String tenantDomain) {

        /* The User Account Recovery process can identify multiple users that match the specified conditions.
            In such cases, the usernames are represented as a comma-separated list. */
        String[] usernameSegments = username.split(",");
        User user = new User();
        user.setTenantDomain(tenantDomain);

        for (String part : usernameSegments) {
            String domainFreeName = UserCoreUtil.removeDomainFromName(part);
            if (user.getUserName() != null) {
                user.setUserName(user.getUserName() + "," + domainFreeName);
            } else {
                user.setUserName(domainFreeName);
            }

            String userStoreDomain = IdentityUtil.extractDomainFromName(part);
            if (user.getUserStoreDomain() != null) {
                user.setUserStoreDomain(user.getUserStoreDomain() + "," + userStoreDomain);
            } else {
                user.setUserStoreDomain(userStoreDomain);
            }
        }
        return user;
    }

    /**
     * Checks whether the per-user functionality locking is enabled.
     *
     * @return true if the config is set to true, false otherwise.
     */
    public static boolean isPerUserFunctionalityLockingEnabled() {

        return Boolean.parseBoolean(
                IdentityUtil.getProperty(UserFunctionalityMgtConstants.ENABLE_PER_USER_FUNCTIONALITY_LOCKING));
    }

    /**
     * Checks whether the password recovery email OTP is enabled.
     *
     * @param tenantDomain tenant Domain
     * @return true if the config is set to true for given tenant domain, false otherwise.
     * @throws IdentityRecoveryServerException
     */
    public static boolean isPasswordRecoveryEmailOtpEnabled(String tenantDomain)
            throws IdentityRecoveryServerException {

        return Boolean.parseBoolean(Utils.getRecoveryConfigs(
                IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL,
                tenantDomain));
    }

    /**
     * Skip concatenation of recovery flow id with the secret key for OTP based email recovery.
     *
     * @param tenantDomain Tenant domain.
     * @return True if the concatenation should be skipped.
     */
    public static boolean skipConcatForOTPBasedEmailRecovery(String tenantDomain) {

        boolean isSendOtpAsEmailConfirmationCodeEnabled = Boolean.parseBoolean(IdentityUtil.getProperty
                (IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE));
        if (isSendOtpAsEmailConfirmationCodeEnabled) {
            try {
                return Boolean.parseBoolean(Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL, tenantDomain));
            } catch (IdentityRecoveryException e) {
                return false;
            }
        }
        return false;
    }

    /**
     * Checks whether detailed error messages are enabled.
     *
     * @return true if the config is set to true, false otherwise.
     */
    public static boolean isDetailedErrorResponseEnabled() {

        return Boolean
                .parseBoolean(IdentityUtil.getProperty(IdentityRecoveryConstants.ENABLE_DETAILED_ERROR_RESPONSE));
    }

    /**
     * Get the unique user ID of a user given the tenant domain and the user name.
     *
     * @param tenantId Tenant Id of the user.
     * @param userName Username of the user.
     * @return Unique identifier of the user.
     */
    public static String getUserId(String userName, int tenantId)
            throws IdentityRecoveryServerException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager;
        String userId;

        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            if (realmService == null || realmService.getTenantUserRealm(tenantId) == null) {
                throw handleServerException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, String.valueOf(tenantId));
            }
            userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm(tenantId).
                    getUserStoreManager();
        } catch (UserStoreException | IdentityRecoveryServerException e) {
            throw handleServerException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE, String.valueOf(tenantId), e);
        }
        try {
            userId = ((AbstractUserStoreManager) userStoreManager).getUserIDFromUserName(userName);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS, null, e);
        }
        return userId;
    }

    /**
     * Check for the configuration to skip challenge question-based password recovery if the user has not set answers
     * for a sufficient number of questions.
     *
     * @return true if the config is set to true, false otherwise.
     */
    public static boolean isSkipRecoveryWithChallengeQuestionsForInsufficientAnswersEnabled() {

        return Boolean.parseBoolean(IdentityUtil.getProperty(IdentityRecoveryConstants
                .RECOVERY_QUESTION_PASSWORD_SKIP_ON_INSUFFICIENT_ANSWERS));
    }

    /**
     * To create an audit message based on provided parameters.
     *
     * @param action     Activity
     * @param target     Target affected by this activity.
     * @param dataObject Information passed along with the request.
     * @param result     Result value.
     */
    public static void createAuditMessage(String action, String target, JSONObject dataObject, String result) {

        if (!isLegacyAuditLogsDisabled()) {
            AUDIT_LOG.info(String
                    .format(AuditConstants.AUDIT_MESSAGE, getInitiator(), action, maskIfRequired(target),
                    dataObject, result));
        }
    }

    /**
     * Get the initiator for audit logs.
     *
     * @return initiator based on whether log masking is enabled or not.
     */
    private static String getInitiator() {

        String initiator = null;
        String loggedInUser = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        if (StringUtils.isBlank(loggedInUser)) {
            loggedInUser = CarbonConstants.REGISTRY_SYSTEM_USERNAME;
        }
        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        if (LoggerUtils.isLogMaskingEnable) {
            if (StringUtils.isNotBlank(loggedInUser) && StringUtils.isNotBlank(tenantDomain)) {
                initiator = IdentityUtil.getInitiatorId(loggedInUser, tenantDomain);
            }
            if (StringUtils.isBlank(initiator)) {
                initiator = LoggerUtils.getMaskedContent(UserCoreUtil.addTenantDomainToEntry(loggedInUser,
                        tenantDomain));
            }
        } else {
            initiator = UserCoreUtil.addTenantDomainToEntry(loggedInUser, tenantDomain);
        }
        return initiator;
    }

    /**
     * This method is deprecated.
     * Generate a secret key according to the given channel. Method will generate an OTP for mobile channel and a
     * UUID for other channels. OTP is generated based on the defined regex.
     *
     * @param channel          Recovery notification channel.
     * @param tenantDomain     Tenant domain.
     * @param recoveryScenario Recovery scenario.
     * @return Secret key.
     * @throws IdentityRecoveryServerException while getting sms otp regex.
     */
    @Deprecated
    public static String generateSecretKey(String channel, String tenantDomain, String recoveryScenario)
            throws IdentityRecoveryServerException {

        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel)) {
            int otpLength = IdentityRecoveryConstants.SMS_OTP_CODE_LENGTH;
            String otpRegex = null;
            boolean useNumeric = true;
            boolean useUppercaseLetters = true;
            boolean useLowercaseLetters = true;
            if (StringUtils.equals(RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name(), recoveryScenario)) {
                otpRegex = Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.
                        PASSWORD_RECOVERY_SMS_OTP_REGEX, tenantDomain);
            } else if (StringUtils.equals(RecoveryScenarios.SELF_SIGN_UP.name(), recoveryScenario)) {
                otpRegex = Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.
                        SELF_REGISTRATION_SMS_OTP_REGEX, tenantDomain);
            } else if (StringUtils.equals(RecoveryScenarios.LITE_SIGN_UP.name(), recoveryScenario)) {
                otpRegex = Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.
                        LITE_REGISTRATION_SMS_OTP_REGEX, tenantDomain);
            }
            // If the OTP regex is not specified we need to ensure that the default behavior will be executed.
            if (StringUtils.isNotBlank(otpRegex)) {
                if (StringUtils.equals(RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name(), recoveryScenario) ||
                        StringUtils.equals(RecoveryScenarios.SELF_SIGN_UP.name(), recoveryScenario) ||
                        StringUtils.equals(RecoveryScenarios.LITE_SIGN_UP.name(), recoveryScenario)) {
                    if (!Pattern.matches(IdentityRecoveryConstants.VALID_SMS_OTP_REGEX_PATTERN, otpRegex)) {
                        throw new IdentityRecoveryServerException(
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_SMS_OTP_REGEX.getCode(),
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_SMS_OTP_REGEX.getMessage());
                    }
                    String charsRegex = otpRegex.replaceAll("[{].*", "");
                    otpLength = Integer.parseInt(otpRegex.replaceAll(".*[{]", "").replaceAll("}", ""));
                    if (!charsRegex.contains("A-Z")) {
                        useUppercaseLetters = false;
                    }
                    if (!charsRegex.contains("a-z")) {
                        useLowercaseLetters = false;
                    }
                    if (!charsRegex.contains("0-9")) {
                        useNumeric = false;
                    }
                }
            }
            try {
                OTPGenerator otpGenerator = IdentityRecoveryServiceDataHolder.getInstance().getOtpGenerator();
                return otpGenerator.generateOTP(useNumeric, useUppercaseLetters, useLowercaseLetters, otpLength,
                        recoveryScenario);
            } catch (OTPGeneratorException otpGeneratorException) {
                throw new IdentityRecoveryServerException(otpGeneratorException.getErrorCode(),
                        otpGeneratorException.getMessage());
            }
        } else {
            return UUID.randomUUID().toString();
        }
    }

    /**
     * Generate a secret key according to the given channel. Method will generate an OTP for mobile channel, and an OTP
     * for e-mail channel when 'sendOTPInEmail' property enabled. For other scenarios method will generate UUID.
     *
     * @param channel Recovery notification channel.
     * @param recoveryScenario       Recovery scenario.
     * @param tenantDomain           Tenant domain.
     * @param connectorName          Connector name.
     * @return Secret key.
     * @throws IdentityRecoveryServerException while getting OTP generated.
     */
    public static String generateSecretKey(String channel, String recoveryScenario, String tenantDomain, String connectorName)
            throws IdentityRecoveryServerException {

        // Set default OTP configuration values, for scenarios that don't have specific OTP configurations.
        boolean useUppercase = true;
        boolean useLowercase = true;
        boolean useNumeric = true;
        boolean sendOTPInEmail = false;
        boolean isSelfRegistrationOTPEnabled = false;
        int otpLength = IdentityRecoveryConstants.OTP_CODE_DEFAULT_LENGTH;
        // Set connector specific OTP configuration values, for connectors that have separate OTP configurations.
        if (StringUtils.isNotBlank(connectorName)) {
            sendOTPInEmail = Boolean.parseBoolean(getRecoveryConfigs(
                    connectorName + ".OTP.SendOTPInEmail", tenantDomain));
            isSelfRegistrationOTPEnabled = "SelfRegistration".equals(connectorName) &&
                    Boolean.parseBoolean(getRecoveryConfigs(SELF_REGISTRATION_EMAIL_OTP_ENABLE, tenantDomain));
            useUppercase = Boolean.parseBoolean(getRecoveryConfigs(
                    connectorName + ".OTP.UseUppercaseCharactersInOTP", tenantDomain));
            useLowercase = Boolean.parseBoolean(getRecoveryConfigs(
                    connectorName + ".OTP.UseLowercaseCharactersInOTP", tenantDomain));
            useNumeric = Boolean.parseBoolean(getRecoveryConfigs(
                    connectorName + ".OTP.UseNumbersInOTP", tenantDomain));
            try {
                otpLength = Integer.parseInt(Utils.getRecoveryConfigs(connectorName + ".OTP.OTPLength", tenantDomain));
            } catch (NumberFormatException ex) {
                log.warn("Configured OTP length is not a number. Hence using default length of "
                        + IdentityRecoveryConstants.OTP_CODE_DEFAULT_LENGTH + " for OTP.");
            }
        }
        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel) ||
                RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.name().equals(recoveryScenario) ||
                RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.name().equals(recoveryScenario) ||
                RecoveryScenarios.EMAIL_VERIFICATION_OTP.name().equals(recoveryScenario) ||
                sendOTPInEmail || isSelfRegistrationOTPEnabled) {
            try {
                OTPGenerator otpGenerator = IdentityRecoveryServiceDataHolder.getInstance().getOtpGenerator();
                return otpGenerator.generateOTP(useNumeric, useUppercase, useLowercase, otpLength,
                        recoveryScenario);
            } catch (OTPGeneratorException otpGeneratorException) {
                throw new IdentityRecoveryServerException(otpGeneratorException.getErrorCode(),
                        otpGeneratorException.getMessage());
            }
        }
        return UUID.randomUUID().toString();
    }

    /**
     * Concatenate recovery flow id with the generated secret key if the notification channel is email.
     *
     * @param recoveryFlowId      Recovery flow id.
     * @param notificationChannel Recovery notification channel.
     * @param secretKey           Secret Key.
     * @return Secret key.
     */
    public static String concatRecoveryFlowIdWithSecretKey(String recoveryFlowId, String notificationChannel,
                                                           String secretKey) {
        if (recoveryFlowId != null && (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(notificationChannel)
                || NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel))) {
            secretKey = recoveryFlowId + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + secretKey;
        }
        return secretKey;
    }

    /**
     * Return user account state.
     *
     * @param user  User.
     * @return account state.
     */
    public static String getAccountState(User user) {

        String accountState = StringUtils.EMPTY;
        try {
            org.wso2.carbon.user.core.UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.
                    getInstance().getRealmService().getBootstrapRealm().getUserStoreManager();
            while (!userStoreManager.isExistingUser(user.getUserName())) {
                userStoreManager = userStoreManager.getSecondaryUserStoreManager();
                if (userStoreManager == null) {
                    return accountState;
                }
            }
            Map<String, String> claimMap =
                    ((AbstractUserStoreManager) userStoreManager).getUserClaimValues(user.getUserName(),
                            new String[]{IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI}, "default");
            if (!claimMap.isEmpty()) {
                if (claimMap.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI)) {
                    accountState = claimMap.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
                }
            }
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            log.error("Error occurred while retrieving UserStoreManager.", e);
        }
        return accountState;
    }


    /**
     * Return user account state for user with username which is not domain qualified.
     *
     * @param user  User without domain qualified username.
     * @return account state.
     */
    public static String getAccountStateForUserNameWithoutUserDomain(User user) {

        String accountState = StringUtils.EMPTY;
        try {
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            String username = UserCoreUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            Map<String, String> claimMap =
                    ((AbstractUserStoreManager) userStoreManager).getUserClaimValues(username,
                            new String[]{IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI}, "default");
            if (!claimMap.isEmpty() && claimMap.containsKey(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI)) {
                    accountState = claimMap.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
            }
        } catch (UserStoreException e) {
            log.error("Error occurred while retrieving UserStoreManager.", e);
        }
        return accountState;
    }

    /**
     * When updating email/mobile claim value, sending the verification notification can be controlled by sending
     * an additional temporary claim ('verifyEmail'/'verifyMobile') along with the update request.
     * This option can be enabled form identity.xml by setting 'UseVerifyClaim' to true. When this option is enabled,
     * email/mobile verification notification on a claim update will be triggered based on the
     * 'verifyEmail'/'verifyMobile' temporary claim sent along with the update request.
     *
     * @return True if 'UseVerifyClaim' config is set to true, false otherwise.
     */
    public static boolean isUseVerifyClaimEnabled() {

        return Boolean.parseBoolean(IdentityUtil.getProperty
                (IdentityRecoveryConstants.ConnectorConfig.USE_VERIFY_CLAIM_ON_UPDATE));
    }

    /**
     * Check whether the supporting multiple email addresses and mobile numbers per user feature is enabled.
     *
     * @param tenantDomain   Tenant domain.
     * @param userStoreDomain User store domain.
     * @return True if the config is set to true, false otherwise.
     */
    public static boolean isMultiEmailsAndMobileNumbersPerUserEnabled(String tenantDomain, String userStoreDomain) {

        return isMultiEmailAddressesPerUserEnabled(tenantDomain, userStoreDomain)
                && isMultiMobileNumbersPerUserEnabled(tenantDomain, userStoreDomain);
    }

    /**
     * Check if multiple email addresses are supported for the given tenant and user store.
     *
     * @param tenantDomain    Tenant domain.
     * @param userStoreDomain User store domain.
     * @return True if multiple email addresses are supported.
     */
    public static boolean isMultiEmailAddressesPerUserEnabled(String tenantDomain, String userStoreDomain) {

        if (!isMultiEmailOrMobileConfigEnabled()) {
            return false;
        }

        if (StringUtils.isBlank(tenantDomain) || StringUtils.isBlank(userStoreDomain)) {
            return false;
        }

        try {
            List<LocalClaim> localClaims =
                    IdentityRecoveryServiceDataHolder.getInstance().getClaimMetadataManagementService()
                            .getLocalClaims(tenantDomain);

            List<String> requiredEmailClaims = new ArrayList<>();
            requiredEmailClaims.add(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);

            boolean isEmailVerificationOnUpdateEnabled;
            try {
                isEmailVerificationOnUpdateEnabled = isEmailVerificationOnUpdateEnabled(tenantDomain);
            } catch (IdentityEventException e) {
                log.error("Error while retrieving email verification on update config.", e);
                return false;
            }

            if (isEmailVerificationOnUpdateEnabled) {
                requiredEmailClaims.add(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
            }

            return areClaimsSupportedForUserStore(localClaims, requiredEmailClaims, userStoreDomain);
        } catch (ClaimMetadataException e) {
            log.error("Error while retrieving multiple emails config.", e);
            return false;
        }
    }

    /**
     * Check if multiple mobile numbers are supported for the given tenant and user store.
     *
     * @param tenantDomain    Tenant domain.
     * @param userStoreDomain User store domain.
     * @return True if multiple mobile numbers are supported.
     */
    public static boolean isMultiMobileNumbersPerUserEnabled(String tenantDomain, String userStoreDomain) {

        if (!isMultiEmailOrMobileConfigEnabled()) {
            return false;
        }

        if (StringUtils.isBlank(tenantDomain) || StringUtils.isBlank(userStoreDomain)) {
            return false;
        }

        try {
            List<LocalClaim> localClaims =
                    IdentityRecoveryServiceDataHolder.getInstance().getClaimMetadataManagementService()
                            .getLocalClaims(tenantDomain);

            List<String> requiredMobileClaims = new ArrayList<>();
            requiredMobileClaims.add(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);

            boolean isMobileVerificationOnUpdateEnabled;
            try {
                isMobileVerificationOnUpdateEnabled = isMobileVerificationOnUpdateEnabled(tenantDomain);
            } catch (IdentityEventException e) {
                log.error("Error while retrieving mobile verification on update config.", e);
                return false;
            }

            if (isMobileVerificationOnUpdateEnabled) {
                requiredMobileClaims.add(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
            }

            return areClaimsSupportedForUserStore(localClaims, requiredMobileClaims, userStoreDomain);
        } catch (ClaimMetadataException e) {
            log.error("Error while retrieving multiple mobile numbers config.", e);
            return false;
        }
    }

    private static boolean isMultiEmailOrMobileConfigEnabled() {

        return Boolean.parseBoolean(IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER));
    }

    private static boolean isEmailVerificationOnUpdateEnabled(String tenantDomain) throws IdentityEventException {

        String configValue = Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE, tenantDomain);
        return Boolean.parseBoolean(configValue);
    }

    private static boolean isMobileVerificationOnUpdateEnabled(String tenantDomain) throws IdentityEventException {

        String configValue = Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, tenantDomain);
        return Boolean.parseBoolean(configValue);
    }

    private static boolean areClaimsSupportedForUserStore(List<LocalClaim> localClaims, List<String> claimUris,
                                                          String userStoreDomain) {

        return claimUris.stream().allMatch(claimUri ->
                isClaimSupportedForUserStore(localClaims, claimUri, userStoreDomain));
    }

    /**
     * Check if a claim is supported and not excluded for a specific user store.
     *
     * @param localClaims     List of local claims.
     * @param claimUri        URI of the claim to check.
     * @param userStoreDomain User store domain to validate against.
     * @return True if claim is supported and not excluded.
     */
    private static boolean isClaimSupportedForUserStore(List<LocalClaim> localClaims, String claimUri,
                                                        String userStoreDomain) {

        return localClaims.stream()
            .filter(claim -> claimUri.equals(claim.getClaimURI()))
            .anyMatch(claim -> {
                Map<String, String> properties = claim.getClaimProperties();

                // Check if claim is supported by default.
                boolean isSupported = Boolean.parseBoolean(
                        properties.getOrDefault(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY,
                                Boolean.FALSE.toString()));

                // Check if user store is not in excluded list.
                String excludedUserStoreDomains = properties.get(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY);
                boolean isNotExcluded = StringUtils.isBlank(excludedUserStoreDomains) ||
                        !Arrays.asList(excludedUserStoreDomains.toUpperCase().split(","))
                                .contains(userStoreDomain.toUpperCase());

                return isSupported && isNotExcluded;
            });
    }

    /**
     * Trigger recovery event.
     *
     * @param map              The map containing the event properties.
     * @param eventName        The event name.
     * @param confirmationCode The confirmation code.
     * @throws IdentityEventException If error occurred when handleEvent.
     */
    public static void publishRecoveryEvent(Map<String, Object> map, String eventName, String confirmationCode)
            throws IdentityEventException {

        Map<String, Object> eventProperties = cloneMap(map);
        if (MapUtils.isNotEmpty(eventProperties) && StringUtils.isNotEmpty(confirmationCode)) {
            eventProperties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, confirmationCode);
        }

        Event identityMgtEvent = new Event(eventName, eventProperties);
        IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
    }

    /**
     * Clones the given map.
     *
     * @param map          Map.
     * @return             Cloned Map.
     */
    private static Map<String, Object> cloneMap(Map<String, Object> map) {

        if (MapUtils.isEmpty(map)) {
            return null;
        }
        Map<String, Object> clonedMap = new HashMap<String, Object>();
        clonedMap.putAll(map);
        return clonedMap;
    }

    /**
     * Checks whether the existing confirmation code can be reused based on the configured email confirmation code
     * tolerance period.
     *
     * @param recoveryDataDO      Recovery data of the corresponding user.
     * @param notificationChannel Method which is used to send the recovery code. eg:- EMAIL, SMS.
     * @return True if the existing confirmation code can be used. Otherwise false.
     */
    public static boolean reIssueExistingConfirmationCode(UserRecoveryData recoveryDataDO, String notificationChannel) {

        if (recoveryDataDO == null) {
            return false;
        }

        int codeToleranceInMinutes = 0;
        long codeToleranceTimeInMillis = 0;
        String recoveryScenario = recoveryDataDO.getRecoveryScenario().toString();
        if (RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.toString().equals(recoveryScenario)) {
            codeToleranceInMinutes = getEmailCodeToleranceInMinutes();
            codeToleranceTimeInMillis = recoveryDataDO.getTimeCreated().getTime() +
                    TimeUnit.MINUTES.toMillis(codeToleranceInMinutes);
            return System.currentTimeMillis() < codeToleranceTimeInMillis;
        }

        User user = recoveryDataDO.getUser();
        if (user == null) {
            return false;
        }
        String tenantDomain = user.getTenantDomain();
        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }
        if (RecoveryScenarios.SELF_SIGN_UP.toString().equals(recoveryScenario)) {
            codeToleranceInMinutes = getSelfRegistrationCodeToleranceInMinutes(tenantDomain, notificationChannel);
            codeToleranceTimeInMillis = recoveryDataDO.getTimeCreated().getTime() +
                    TimeUnit.MINUTES.toMillis(codeToleranceInMinutes);
            return System.currentTimeMillis() < codeToleranceTimeInMillis;
        } else if (RecoveryScenarios.ASK_PASSWORD.toString().equals(recoveryDataDO.getRecoveryScenario().toString())) {
            codeToleranceInMinutes = getAskPasswordCodeExpiryTime(tenantDomain);
            codeToleranceTimeInMillis = recoveryDataDO.getTimeCreated().getTime() +
                    TimeUnit.MINUTES.toMillis(codeToleranceInMinutes);
            return System.currentTimeMillis() < codeToleranceTimeInMillis;
        }

        return false;
    }

    /**
     * Generate a random password in the given length.
     *
     * @param passwordLength Required length of the password.
     * @return password.
     */
    public static char[] generateRandomPassword(int passwordLength) {

        String letters = "abcdefghjkmnpqrstuvwxyzABCDEFGHJKMNPQRSTUVWXYZ23456789+@";
        StringBuilder pw = new StringBuilder();
        SecureRandom random = new SecureRandom();
        for (int i = 0; i < (passwordLength - 4); i++) {
            int index = (int) (random.nextDouble() * letters.length());
            pw.append(letters.substring(index, index + 1));
        }
        pw.append("A$g0");
        char[] password = new char[pw.length()];
        pw.getChars(0, pw.length(), password, 0);
        return password;
    }

    /**
     * Retrieves the email confirmation code tolerance period in minutes.
     *
     * @return The email confirmation code tolerance in minutes.
     */
    private static int getEmailCodeToleranceInMinutes() {

        String emailCodeTolerance = IdentityUtil.
                getProperty(IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_TOLERANCE_PERIOD);
        if (StringUtils.isEmpty(emailCodeTolerance)) {
            return IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE;
        }
        try {
            int codeTolerance = Integer.parseInt(emailCodeTolerance);
            int recoveryCodeExpiryTime = getRecoveryCodeExpiryTime();
            if (recoveryCodeExpiryTime < 0 || recoveryCodeExpiryTime < codeTolerance) {
                String message = String.format("Recovery code expiry is less than zero or code tolerance is less " +
                                "than recovery code expiry. Therefore setting the DEFAULT time : %s minutes",
                        IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE);
                log.warn(message);
                return IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE;
            }
            return codeTolerance;
        } catch (NumberFormatException e) {
            String message = String.format("Recovery confirmation code tolerance parsing is failed. Therefore" +
                            "setting the DEFAULT time : %s minutes",
                    IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE);
            log.error(message);

            return IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE;
        }
    }

    /**
     * Get the expiry time of the recovery code given at username recovery and password recovery init.
     *
     * @return Expiry time of the recovery code (In minutes)
     */
    private static int getRecoveryCodeExpiryTime() {

        String expiryTime = IdentityUtil
                .getProperty(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CODE_EXPIRY_TIME);
        if (StringUtils.isEmpty(expiryTime)) {
            return IdentityRecoveryConstants.RECOVERY_CODE_DEFAULT_EXPIRY_TIME;
        }
        try {
            return Integer.parseInt(expiryTime);
        } catch (NumberFormatException e) {
            String message = String
                    .format("User recovery code expiry time parsing is failed. Therefore setting DEFAULT expiry time " +
                            ": %s minutes", IdentityRecoveryConstants.RECOVERY_CODE_DEFAULT_EXPIRY_TIME);
            log.error(message);
            return IdentityRecoveryConstants.RECOVERY_CODE_DEFAULT_EXPIRY_TIME;
        }
    }

    /**
     * Retrieves the self registration confirmation code tolerance period in minutes.
     *
     * @param tenantDomain        Tenant domain of the user.
     * @param notificationChannel Method which is used to send the recovery code. eg:- EMAIL, SMS, EXTERNAL.
     * @return The self registration confirmation code tolerance in minutes.
     */
    private static int getSelfRegistrationCodeToleranceInMinutes(String tenantDomain, String notificationChannel) {

        String selfRegistrationCodeTolerance = null;
        String selfRegistrationCodeExpiryTime = null;
        try {
            // Getting the self registration confirmation code expiry time and tolerance period for the given channel.
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equalsIgnoreCase(notificationChannel)) {
                selfRegistrationCodeTolerance = IdentityUtil.
                        getProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD);
                selfRegistrationCodeExpiryTime = getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                        tenantDomain);
            } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equalsIgnoreCase(notificationChannel)) {
                selfRegistrationCodeTolerance = IdentityUtil.
                        getProperty(IdentityRecoveryConstants.SELF_SIGN_UP_SMS_CONFIRMATION_CODE_TOLERANCE_PERIOD);
                selfRegistrationCodeExpiryTime = getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig
                                .SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME, tenantDomain);
            } else {
                selfRegistrationCodeTolerance = IdentityUtil.
                        getProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD);
                selfRegistrationCodeExpiryTime = getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME,
                        tenantDomain);
            }
        } catch (IdentityRecoveryServerException e) {
            log.error("Error while retrieving self registration code recovery time.", e);
            return IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE;
        }

        if (StringUtils.isEmpty(selfRegistrationCodeTolerance) || StringUtils.isEmpty(selfRegistrationCodeExpiryTime)) {
            return IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE;
        }

        try {
            int codeTolerance = Integer.parseInt(selfRegistrationCodeTolerance);
            int codeExpiryTime = Integer.parseInt(selfRegistrationCodeExpiryTime);
            // If the code expiry time is less than zero, code has infinite validity. Hence, we only need this
            // condition to check whether the code expiry time is less than code tolerance when code expiry time is
            // greater than or equal zero.
            if (codeExpiryTime >= 0 && codeExpiryTime < codeTolerance) {
                String message = String.format("Self registration code expiry time is less than code tolerance. " +
                                "Therefore setting the DEFAULT time : %s minutes",
                        IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE);
                log.warn(message);
                return IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE;
            }
            return codeTolerance;
        } catch (NumberFormatException e) {
            String message = String.format("Self registration confirmation code tolerance parsing is failed. " +
                            "Therefore setting the DEFAULT time : %s minutes",
                    IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE);
            log.error(message);
            return IdentityRecoveryConstants.SELF_SIGN_UP_CODE_DEFAULT_TOLERANCE;
        }
    }

    /**
     * Retrieves the ask password confirmation code tolerance period in minutes.
     *
     * @param tenantDomain Tenant domain of the user.
     * @return The ask password confirmation code tolerance in minutes.
     */
    private static int getAskPasswordCodeExpiryTime(String tenantDomain) {

        String askPasswordCodeTolerance = null;
        String askPasswordCodeExpiryTime = null;
        try {
            askPasswordCodeTolerance = IdentityUtil.
                    getProperty(IdentityRecoveryConstants.ASK_PASSWORD_CONFIRMATION_CODE_TOLERANCE_PERIOD);
            askPasswordCodeExpiryTime = getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME,
                    tenantDomain);
        } catch (IdentityRecoveryServerException e) {
            log.error("Error while retrieving ask password code recovery time.", e);
            return IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE;
        }

        if (StringUtils.isEmpty(askPasswordCodeTolerance) || StringUtils.isEmpty(askPasswordCodeExpiryTime)) {
            return IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE;
        }

        try {
            int codeTolerance = Integer.parseInt(askPasswordCodeTolerance);
            int codeExpiryTime = Integer.parseInt(askPasswordCodeExpiryTime);
            // If the code expiry time is less than zero, code has infinite validity. Hence, we only need this
            // condition to check whether the code expiry time is less than code tolerance when code expiry time is
            // greater than or equal zero.
            if (codeExpiryTime >= 0 && codeExpiryTime < codeTolerance) {
                String message = String.format("Ask password code expiry time is less than code tolerance. " +
                                "Therefore setting the DEFAULT time : %s minutes",
                        IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE);
                log.warn(message);
                return IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE;
            }
            return codeTolerance;
        } catch (NumberFormatException e) {
            String message = String.format("Ask password confirmation code tolerance parsing is failed. " +
                            "Therefore setting the DEFAULT time : %s minutes",
                    IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE);
            log.error(message);
            return IdentityRecoveryConstants.ASK_PASSWORD_CODE_DEFAULT_TOLERANCE;
        }
    }

    /**
     * Handle the auth attribution validation failure.
     *
     * @param validationResult Validation result.
     * @throws SelfRegistrationException Exception specific to the failing reason
     */
    public static void handleAttributeValidationFailure(ValidationResult validationResult)
            throws SelfRegistrationException {

        if (validationResult == null) {
            throw new SelfRegistrationClientException(
                    ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getCode(),
                    ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getMessage(),
                    "The attribute verification result is undefined.");
        }
        String errorDescription = "The mandatory attributes of the selected registration option are missing or empty " +
                "in the request.";
        if (validationResult.getValidationFailureReasons() != null) {
            errorDescription = convertFailureReasonsToString(validationResult.getValidationFailureReasons());
        }
        throw new SelfRegistrationClientException(
                ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION.getCode(),
                ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION.getMessage(),
                errorDescription);
    }

    /**
     * Handle the exceptions thrown during attribute validation.
     *
     * @param e AuthAttributeHandlerException.
     * @throws SelfRegistrationException Exception specific to the failing reason.
     */
    public static void handleAttributeValidationFailure(AuthAttributeHandlerException e)
            throws SelfRegistrationException {

        if (e instanceof AuthAttributeHandlerClientException
                && ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getCode().equals(e.getErrorCode())) {
            throw new SelfRegistrationClientException(
                    ERROR_CODE_INVALID_REGISTRATION_OPTION.getCode(),
                    ERROR_CODE_INVALID_REGISTRATION_OPTION.getMessage(),
                    e.getMessage());
        }
        throw new SelfRegistrationException(
                ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getCode(),
                ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES.getMessage(),
                e.getMessage(),
                e);
    }

    /**
     * Mask the given value if it is required.
     *
     * @param value Value to be masked.
     * @return Masked/unmasked value.
     */
    public static String maskIfRequired(String value) {

        return LoggerUtils.isLogMaskingEnable ? LoggerUtils.getMaskedContent(value) : value;
    }

    /**
     * Convert the validation failure reasons to a string.
     *
     * @param validationFailureReasons List of validation failure reasons.
     * @return String of validation failure reasons.
     */
    private static String convertFailureReasonsToString(List<ValidationFailureReason> validationFailureReasons) {

        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("The following validation failures occurred against each attribute. ");
        for (ValidationFailureReason validationFailureReason : validationFailureReasons) {
            stringBuilder
                    .append(validationFailureReason.getAuthAttribute())
                    .append(" : ")
                    .append(validationFailureReason.getErrorCode())
                    .append(" ")
                    .append(validationFailureReason.getReason())
                    .append(",");
        }
        stringBuilder.deleteCharAt(stringBuilder.length() - 1);
        return stringBuilder.toString();
    }

    /**
     * Retrieve user claim of the user.
     * Note : This method cannot be used to retrieve identity claim values of the user.
     *
     * @param user      User from whom the claim needs to be retrieved.
     * @param userClaim Claim URI of the user.
     * @return Claim value of the user.
     * @throws IdentityRecoveryServerException Error while retrieving the claim.
     */
    public static String getUserClaim(User user, String userClaim) throws IdentityRecoveryServerException {

        String userStoreDomain = user.getUserStoreDomain();
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            org.wso2.carbon.user.api.UserRealm userRealm =
                    realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            UserStoreManager userStoreManager = userRealm.getUserStoreManager();

            if (userStoreManager == null) {
                throw new IdentityRecoveryServerException(String.format("userStoreManager is null for user: " +
                        "%s in tenant domain : %s", user.getUserName(), user.getTenantDomain()));
            }
            if (StringUtils.isNotBlank(userStoreDomain) && !PRIMARY_DEFAULT_DOMAIN_NAME.equals(userStoreDomain)) {
                userStoreManager =
                        ((AbstractUserStoreManager) userStoreManager).getSecondaryUserStoreManager(userStoreDomain);
            }

            return userStoreManager.getUserClaimValue(user.getUserName(), userClaim, null);

        } catch (UserStoreException e) {
            String error = String.format("Error occurred while retrieving claim for user: " +
                    "%s in tenant domain : %s", user.getUserName(), user.getTenantDomain());
            throw new IdentityRecoveryServerException(error, e);
        }
    }

    /**
     * Retrieves the existing multi-valued claims for a given claim URI.
     *
     * @param userStoreManager User store manager.
     * @param user User object.
     * @param claimURI Claim URI to retrieve.
     * @return List of existing claim values.
     * @throws IdentityEventException If an error occurs while retrieving the claim value.
     */
    public static List<String> getMultiValuedClaim(UserStoreManager userStoreManager, User user, String claimURI)
            throws IdentityEventException {

        try {
            String claimValue = userStoreManager.getUserClaimValue(user.getUserName(), claimURI, null);
            if (StringUtils.isBlank(claimValue)) {
                return new ArrayList<>();
            }

            String separator = FrameworkUtils.getMultiAttributeSeparator();
            return new ArrayList<>(Arrays.asList(claimValue.split(separator)));
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error retrieving claim " + claimURI +
                    " for user: " + user.toFullQualifiedUsername(), e);
        }
    }

    /**
     * Retrieves the existing claim value for a given claim URI.
     *
     * @param userStoreManager User store manager.
     * @param user             User object.
     * @param claimURI         Claim URI to retrieve.
     * @return List of existing claim values.
     * @throws IdentityEventException If an error occurs while retrieving the claim value.
     */
    public static String getSingleValuedClaim(UserStoreManager userStoreManager, User user, String claimURI)
            throws IdentityEventException {

        try {
            return userStoreManager.getUserClaimValue(user.getUserName(), claimURI, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error retrieving claim " + claimURI +
                    " for user: " + maskIfRequired(user.toFullQualifiedUsername()), e);
        }
    }

    /**
     * Retrieve user claim of the user from the user store manager.
     * Note : This method can be used to retrieve identity claim values of the user.
     *
     * @param userStoreManager The user store manager instance.
     * @param user             The user object containing user details.
     * @param claimURI         The URI of the claim to be retrieved.
     * @return The claim value for the given claim URI. Returns null if no claim value is found.
     * @throws IdentityEventException If an error occurs while retrieving the user claim value.
     */
    public static String getUserClaim(org.wso2.carbon.user.core.UserStoreManager userStoreManager, User user,
                                      String claimURI) throws IdentityEventException {

        Map<String, String> userClaimsMap;

        try {
            userClaimsMap = userStoreManager.getUserClaimValues(user.getUserName(), new String[]{claimURI}, null);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityEventException(String.format("Error while getting user claim: '%s' for user: %s",
                    claimURI, maskIfRequired(user.getUserName())), e);
        }

        if (MapUtils.isEmpty(userClaimsMap)) {
            return null;
        }

        for (Map.Entry<String, String> entry : userClaimsMap.entrySet()) {
            String userClaimURI = entry.getKey();
            if (userClaimURI.equals(claimURI)) {
                return entry.getValue();
            }
        }
        return null;
    }

    /**
     * Load user recovery data from the database using the provided code.
     * @param code The recovery code to be used for loading the user recovery data.
     * @return  UserRecoveryData object containing the recovery data for the user.
     * @throws IdentityRecoveryException if an error occurs while loading the user recovery data.
     */
    public static UserRecoveryData loadUserRecoveryData(String code) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData;
        try {
            String hashedCode = Utils.hashCode(code);
            userRecoveryData = userRecoveryDataStore.load(hashedCode);
        } catch (NoSuchAlgorithmException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO_FOR_CODE, null);
        } catch (IdentityRecoveryException e) {
            userRecoveryData = userRecoveryDataStore.load(code);
        }
        return userRecoveryData;
    }

    /**
     * This method attempts to retrieve the user object from the context
     * and deserialize it if necessary.
     * @param context The FlowExecutionContext from which to resolve the user.
     * @return The User object if found and successfully deserialized, null otherwise.
     */
    public static User resolveUserFromContext(FlowExecutionContext context) {

        Object raw = context.getProperty(USER);

        if (raw instanceof User) {
            return (User) raw;
        }

        ObjectMapper mapper = new ObjectMapper()
                .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        try {
            if (raw instanceof String) {
                return mapper.readValue((String) raw, User.class);
            }
            return mapper.convertValue(raw, User.class);
        } catch (JsonProcessingException e) {
            log.warn("Failed to resolve User from context.", e);
            return null;
        }
    }

    /**
     * Get the domain qualified username.
     *
     * @param user Flow user.
     * @return Domain qualified username.
     */
    public static String getDomainQualifiedUsername(FlowUser user) {

        String username = user.getUsername();
        if (!StringUtils.isBlank(user.getUserStoreDomain()) &&
                !IdentityUtil.getPrimaryDomainName().equals(user.getUserStoreDomain())) {
            username = UserCoreUtil.addDomainToName(username, user.getUserStoreDomain());
        }
        return username;
    }

    /**
     * Get the flow completion configuration for the given flow type and tenant domain.
     *
     * @param flowType     Flow type.
     * @param tenantDomain Tenant domain.
     * @param config       Flow completion configuration.
     * @return Flow completion configuration value.
     * @throws FlowMgtServerException If an error occurs while retrieving the configuration.
     */
    public static String getFlowCompletionConfig(Constants.FlowTypes flowType, String tenantDomain,
                                                  Constants.FlowCompletionConfig config) throws FlowMgtServerException {

        return FlowMgtConfigUtils.getFlowConfig(flowType.getType(), tenantDomain)
                .getFlowCompletionConfig(config);
    }

    /**
     * Get the flow configuration for the given flow type and tenant domain.
     *
     * @param flowType     Flow type.
     * @param tenantDomain Tenant domain.
     * @return Flow configuration.
     * @throws FlowMgtServerException If an error occurs while retrieving the configuration.
     */
    public static FlowConfigDTO getFlowConfig(String flowType, String tenantDomain) throws FlowMgtServerException {

        return FlowMgtConfigUtils.getFlowConfig(flowType, tenantDomain);
    }
}
