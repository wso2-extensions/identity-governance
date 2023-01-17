/*
 *
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.signup;

import com.google.gson.Gson;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.slf4j.MDC;
import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.CarbonException;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.Purpose;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.consent.mgt.core.model.ReceiptServiceInput;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.core.util.AnonymousSessionUtil;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.consent.mgt.exceptions.ConsentUtilityServiceException;
import org.wso2.carbon.identity.consent.mgt.services.ConsentUtilityService;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.IdentityEventServerException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerClientException;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.recovery.AuditConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.Permission;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreConfigConstants;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.AUDIT_FAILED;
import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.AUDIT_SUCCESS;

/**
 * Manager class which can be used to recover passwords using a notification.
 */
public class UserSelfRegistrationManager {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationManager.class);

    private static UserSelfRegistrationManager instance = new UserSelfRegistrationManager();
    private static final String PURPOSE_GROUP_SELF_REGISTER = "SELF-SIGNUP";
    private static final String PURPOSE_GROUP_TYPE_SYSTEM = "SYSTEM";

    private UserSelfRegistrationManager() {

    }

    public static UserSelfRegistrationManager getInstance() {

        return instance;
    }

    public NotificationResponseBean registerUser(User user, String password, Claim[] claims, Property[] properties)
            throws IdentityRecoveryException {

        publishEvent(user, claims, properties, IdentityEventConstants.Event.PRE_SELF_SIGNUP_REGISTER);

        String consent = getPropertyValue(properties, IdentityRecoveryConstants.Consent.CONSENT);
        String tenantDomain = user.getTenantDomain();

        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        // Callback URL validation
        String callbackURL = null;
        try {
            callbackURL = Utils.getCallbackURLFromRegistration(properties);
            if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, tenantDomain,
                    IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX)) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
            }
        } catch (MalformedURLException | UnsupportedEncodingException | IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("registerUser :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("registerUser :User store domain is not in the request. set to default for user : " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }
        NotificationResponseBean notificationResponseBean;
        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());

            Map<String, String> claimsMap = new HashMap<>();
            for (Claim claim : claims) {
                claimsMap.put(claim.getClaimUri(), claim.getValue());
            }

            //Set arbitrary properties to use in UserSelfRegistrationHandler
            Utils.setArbitraryProperties(properties);
            validateAndFilterFromReceipt(consent, claimsMap);

            // User preferred notification channel.
            String preferredChannel;
            try {

                //TODO It is required to add this role before tenant creation. And also, this role should not not be able remove.
                if (!userStoreManager.isExistingRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                    Permission permission = new Permission("/permission/admin/login", IdentityRecoveryConstants.EXECUTE_ACTION);
                    userStoreManager.addRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE, null, new Permission[]{permission});
                }
                String[] userRoles = new String[]{IdentityRecoveryConstants.SELF_SIGNUP_ROLE};
                try {
                    NotificationChannelManager notificationChannelManager = Utils.getNotificationChannelManager();
                    preferredChannel = notificationChannelManager
                            .resolveCommunicationChannel(user.getUserName(), user.getTenantDomain(),
                                    user.getUserStoreDomain(), claimsMap);
                } catch (NotificationChannelManagerException e) {
                    throw mapNotificationChannelManagerException(e, user);
                }
                // If the preferred channel value is not in the claims map, add the value to  claims map if the
                // resolved channel is not empty.
                if (StringUtils.isEmpty(claimsMap.get(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM)) && StringUtils
                        .isNotEmpty(preferredChannel)) {
                    claimsMap.put(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM, preferredChannel);
                }
                userStoreManager
                        .addUser(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()), password,
                                userRoles, claimsMap, null);
            } catch (UserStoreException e) {
                Throwable cause = e;
                while (cause != null) {
                    if (cause instanceof PolicyViolationException) {
                        throw IdentityException.error(IdentityRecoveryClientException.class,
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(), cause.getMessage(), e);
                    }
                    cause = cause.getCause();
                }
                Utils.checkPasswordPatternViolation(e, user);

                return handleClientException(user, e);
            }
            addUserConsent(consent, tenantDomain);

            // Build the notification response.
            notificationResponseBean = buildNotificationResponseBean(user, preferredChannel, claimsMap);
        } finally {
            Utils.clearArbitraryProperties();
            PrivilegedCarbonContext.endTenantFlow();
        }
        publishEvent(user, claims, properties, IdentityEventConstants.Event.POST_SELF_SIGNUP_REGISTER);
        return notificationResponseBean;
    }

    /**
     * Build the notification response bean.
     *
     * @param user             User
     * @param preferredChannel User preferred channel
     * @param claimsMap        Claim map of the user
     * @return NotificationResponseBean object
     * @throws IdentityRecoveryException Error while building the response.
     */
    private NotificationResponseBean buildNotificationResponseBean(User user, String preferredChannel,
            Map<String, String> claimsMap) throws IdentityRecoveryException {

        boolean isAccountLockOnCreation = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                        user.getTenantDomain()));
        boolean isNotificationInternallyManage = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                        user.getTenantDomain()));

        // Check whether the preferred channel is already verified. In this case no need to send confirmation
        // mails.
        boolean preferredChannelVerified = isPreferredChannelVerified(user.getUserName(), preferredChannel, claimsMap);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        // If the channel is already verified, no need to lock the account or ask to verify the account
        // since, the notification channel is already verified.
        if (preferredChannelVerified) {
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getMessage());
        } else if (isNotificationInternallyManage && isAccountLockOnCreation) {

            // When the channel is not verified, notifications are internally managed and account is locked
            // on creating, API should ask the user to verify the user account and and notification channel.
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION.getMessage());
            notificationResponseBean.setNotificationChannel(preferredChannel);
        } else if (!isAccountLockOnCreation) {

            // When the preferred channel is not verified and account is not locked on user creation, response needs to
            // convey that no verification is needed.
            // In this scenario notification managed mechanism will not effect.
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION.getMessage());
        } else {
            // When the notification is externally managed and the account is locked on user creation.
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.invalidate(user);

            String secretKey = UUIDGenerator.generateUUID();
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios.SELF_SIGN_UP,
                    RecoverySteps.CONFIRM_SIGN_UP);
            recoveryDataDO.setRemainingSetIds(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());

            userRecoveryDataStore.store(recoveryDataDO);
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getMessage());
            notificationResponseBean.setRecoveryId(secretKey);
            notificationResponseBean.setNotificationChannel(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());

            // Populate the key variable in response bean to maintain backward compatibility.
            notificationResponseBean.setKey(secretKey);
        }
        return notificationResponseBean;
    }

    private NotificationResponseBean handleClientException(User user, UserStoreException e) throws
            IdentityRecoveryException {

        if (StringUtils.isEmpty(e.getMessage())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_ADD_SELF_USER, user.getUserName(), e);
        }

        if (e.getMessage().contains("31301")) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_USERNAME_POLICY_VIOLATED, user.getUserName(), e);
        }

        if (e.getMessage().contains("PasswordInvalidAsk")) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_PASSWORD_POLICY_VIOLATED, StringUtils.EMPTY, e);
        }

        if (e.getMessage().contains("UserAlreadyExisting")) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_USER_ALREADY_EXISTS, user.getUserName(), e);
        }

        if (e.getMessage().contains("Invalid Domain")) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_DOMAIN_VIOLATED, user.getUserStoreDomain(), e);
        }

        throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.
                ERROR_CODE_ADD_SELF_USER, user.getUserName(), e);
    }

    /**
     * Maps the errors thrown during channel resolving with the scenarios in the user self registration process.
     *
     * @param e    Exception
     * @param user User
     * @throws IdentityRecoveryException Exception
     */
    private IdentityRecoveryException mapNotificationChannelManagerException(NotificationChannelManagerException e,
            User user) throws IdentityRecoveryException {

        // Check error is due to not providing preferred channel values.
        if (StringUtils.isNotEmpty(e.getErrorCode()) && e.getErrorCode()
                .equals(IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL
                        .getCode())) {
            return Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PREFERRED_CHANNEL_VALUE_EMPTY,
                    user.getUserName(), e);
        }
        // Check whether the error is due to unsupported preferred channel.
        if (StringUtils.isNotEmpty(e.getErrorCode()) && e.getErrorCode()
                .equals(IdentityMgtConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNEL.getCode())) {
            return Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS,
                    user.getUserName(), e);
        }
        return Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_BAD_SELF_REGISTER_REQUEST,
                user.getUserName(), e);
    }

    /**
     * Checks whether the notification channel is already verified for the user.
     *
     * @param username            Username
     * @param notificationChannel Notification channel
     * @param claimsMap           Properties related to the event
     * @return True if the channel is already verified
     * @throws IdentityRecoveryClientException Error while getting the notification channel
     */
    private boolean isPreferredChannelVerified(String username, String notificationChannel,
            Map<String, String> claimsMap) throws IdentityRecoveryClientException {

        boolean isEnableAccountLockForVerifiedPreferredChannelEnabled = Boolean.parseBoolean(IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_ACCOUNT_LOCK_FOR_VERIFIED_PREFERRED_CHANNEL));
        if (!isEnableAccountLockForVerifiedPreferredChannelEnabled) {
            NotificationChannels channel = getNotificationChannel(username, notificationChannel);

            // Get the matching claim uri for the channel.
            String verifiedClaimUri = channel.getVerifiedClaimUrl();

            // Get the verified status for given channel.
            String verifiedStatus = claimsMap.get(verifiedClaimUri);
            return StringUtils.isNotEmpty(verifiedStatus) && Boolean.parseBoolean(verifiedStatus);
        }
        return false;
    }

    /**
     * Get the NotificationChannels object which matches the given channel type.
     *
     * @param username            Username
     * @param notificationChannel Notification channel
     * @return NotificationChannels object
     * @throws IdentityRecoveryClientException Unsupported channel type
     */
    private NotificationChannels getNotificationChannel(String username, String notificationChannel)
            throws IdentityRecoveryClientException {

        NotificationChannels channel;
        try {
            channel = NotificationChannels.getNotificationChannel(notificationChannel);
        } catch (NotificationChannelManagerClientException e) {
            if (log.isDebugEnabled()) {
                log.debug("Unsupported channel type : " + notificationChannel, e);
            }
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS, username, e);
        }
        return channel;
    }

    /**
     * Adds user consent.
     *
     * @param consent      Consent String.
     * @param tenantDomain Tenant Domain.
     * @throws IdentityRecoveryServerException IdentityRecoveryServerException.
     */
    public void addUserConsent(String consent, String tenantDomain) throws IdentityRecoveryServerException {

        if (StringUtils.isNotEmpty(consent)) {
            if (log.isDebugEnabled()) {
                log.debug(String.format("Adding consent to tenant domain : %s : %s", tenantDomain, consent));
            }
            try {
                addConsent(consent, tenantDomain);
            } catch (ConsentManagementException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ADD_USER_CONSENT,
                        "", e);
            }
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Consent string is empty. Hence not adding consent");
            }
        }
    }

    private void validateAndFilterFromReceipt(String consent, Map<String, String> claimsMap) throws
            IdentityRecoveryServerException {

        if (StringUtils.isEmpty(consent)) {
            return;
        }
        ConsentManager consentManager = IdentityRecoveryServiceDataHolder.getInstance().getConsentManager();
        try {
            List<Purpose> purposes = consentManager.listPurposes(PURPOSE_GROUP_SELF_REGISTER,
                    PURPOSE_GROUP_TYPE_SYSTEM, 0, 0);
            Gson gson = new Gson();
            ReceiptInput receiptInput = gson.fromJson(consent, ReceiptInput.class);
            validateUserConsent(receiptInput, purposes);
            filterClaimsFromReceipt(receiptInput, claimsMap);
        } catch (ConsentManagementException e) {
            throw new IdentityRecoveryServerException("Error while retrieving System purposes for self registration",
                    e);

        }
    }

    private void validateUserConsent(ReceiptInput receiptInput, List<Purpose> purposes) throws
            IdentityRecoveryServerException {

        ConsentUtilityService consentUtilityService =
                IdentityRecoveryServiceDataHolder.getInstance().getConsentUtilityService();
        try {
            consentUtilityService.validateReceiptPIIs(receiptInput, purposes);
        } catch (ConsentUtilityServiceException e) {
            throw new IdentityRecoveryServerException("Receipt validation failed against purposes", e);
        }

    }

    private void filterClaimsFromReceipt(ReceiptInput receiptInput, Map<String, String> claims) throws
            IdentityRecoveryServerException {

        ConsentUtilityService consentUtilityService =
                IdentityRecoveryServiceDataHolder.getInstance().getConsentUtilityService();
        try {
            Set<String> filteredKeys = consentUtilityService.filterPIIsFromReceipt(claims.keySet(), receiptInput);
            claims.keySet().retainAll(filteredKeys);
        } catch (ConsentUtilityServiceException e) {
            throw new IdentityRecoveryServerException("Receipt validation failed against purposes", e);
        }
    }

    /**
     * Check whether user is already confirmed or not.
     *
     * @param user
     * @return
     * @throws IdentityRecoveryException
     */
    public boolean isUserConfirmed(User user) throws IdentityRecoveryException {

        boolean isUserConfirmed = false;
        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default for user : " + user.getUserName());
        }
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData load =
                userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);

        if (load == null || !RecoveryScenarios.SELF_SIGN_UP.equals(load.getRecoveryScenario())) {
            isUserConfirmed = true;
        }
        return isUserConfirmed;

    }

    /**
     * This method is deprecated.
     *
     * @since 1.1.37
     * @deprecated Additional properties cannot be passed in to the method.
     * Use
     * {@link
     * org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager#confirmUserSelfRegistration
     * (String, Map, String, String)}
     */
    public void confirmUserSelfRegistration(String code) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        UserRecoveryData recoveryData = userRecoveryDataStore.load(code);
        User user = recoveryData.getUser();

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = user.getTenantDomain();
        if (!StringUtils.equalsIgnoreCase(contextTenantDomain, userTenantDomain)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_TENANT, contextTenantDomain);
        }

        if (!RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep())
                && !RecoverySteps.CONFIRM_LITE_SIGN_UP.equals(recoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());

            HashMap<String, String> userClaims = new HashMap<>();
            //Need to lock user account
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
            userClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
            try {
                userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                        user.getUserStoreDomain()), userClaims, null);
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNLOCK_USER_USER,
                        user.getUserName(), e);
            }

            //Invalidate code
            userRecoveryDataStore.invalidate(code);

        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    /**
     * Confirms the user self registration by validating the confirmation code and sets externally verified claims.
     *
     * @param code                 Confirmation code
     * @param verifiedChannelType  Type of the verified channel (SMS or EMAIL)
     * @param verifiedChannelClaim Claim associated with verified channel
     * @param properties           Properties sent with the validate code request
     * @throws IdentityRecoveryException Error validating the confirmation code
     */
    @Deprecated
    public void confirmUserSelfRegistration(String code, String verifiedChannelType,
            String verifiedChannelClaim, Map<String, String> properties) throws IdentityRecoveryException {

        getConfirmedSelfRegisteredUser(code, verifiedChannelType, verifiedChannelClaim, properties);
    }

    /**
     * Confirms the user self registration by validating the confirmation code and sets externally verified claims.
     *
     * @param code                 Confirmation code
     * @param verifiedChannelType  Type of the verified channel (SMS or EMAIL)
     * @param verifiedChannelClaim Claim associated with verified channel
     * @param properties           Properties sent with the validate code request
     * @throws IdentityRecoveryException Error validating the confirmation code
     */
    public User getConfirmedSelfRegisteredUser(String code, String verifiedChannelType,
                                               String verifiedChannelClaim, Map<String, String> properties) throws
            IdentityRecoveryException {

        User user = null;
        publishEvent(code, verifiedChannelType, verifiedChannelClaim, properties,
                IdentityEventConstants.Event.PRE_SELF_SIGNUP_CONFIRM);
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = validateSelfRegistrationCode(code, verifiedChannelType,
                verifiedChannelClaim, properties, false);
        user = userRecoveryData.getUser();
        // Invalidate code.
        userRecoveryDataStore.invalidate(code);

        boolean isSelfRegistrationConfirmationNotify = false;
        isSelfRegistrationConfirmationNotify = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                        user.getTenantDomain()));
        if (isSelfRegistrationConfirmationNotify) {
            triggerNotification(user);
        }
        publishEvent(user, code, verifiedChannelType, verifiedChannelClaim, properties,
                IdentityEventConstants.Event.POST_SELF_SIGNUP_CONFIRM);

        return user;
    }

    /**
     * Introspect the user self registration by validating the confirmation code, sets externally verified claims and
     * return the details. Does not invalidate the code.
     *
     * @param code                 Confirmation code
     * @param verifiedChannelType  Type of the verified channel (SMS or EMAIL)
     * @param verifiedChannelClaim Claim associated with verified channel
     * @param properties           Properties sent with the validate code request
     * @throws IdentityRecoveryException Error validating the confirmation code
     * @return
     */
    public UserRecoveryData introspectUserSelfRegistration(String code, String verifiedChannelType,
                                                           String verifiedChannelClaim, Map<String, String> properties)
            throws IdentityRecoveryException {

        return introspectUserSelfRegistration(false, code,verifiedChannelType,verifiedChannelClaim,properties);
    }

    /**
     * Introspect the user self registration by validating the confirmation code, sets externally verified claims and
     * return the details. Does not invalidate the code.
     *
     * @param skipExpiredCodeValidation   Skip confirmation code validation against expiration.
     * @param code                        Confirmation code.
     * @param verifiedChannelType         Type of the verified channel (SMS or EMAIL).
     * @param verifiedChannelClaim        Claim associated with verified channel.
     * @param properties                  Properties sent with the validate code request.
     * @return UserRecoveryData           Data associated with the provided code, including related user and scenarios.
     * @throws IdentityRecoveryException  Error validating the confirmation code
     */
    public UserRecoveryData introspectUserSelfRegistration(boolean skipExpiredCodeValidation, String code,
                                                           String verifiedChannelType,
                                                           String verifiedChannelClaim, Map<String, String> properties)
            throws IdentityRecoveryException {

        return introspectSelfRegistrationCode(code, skipExpiredCodeValidation);
    }

    private UserRecoveryData validateSelfRegistrationCode(String code, String verifiedChannelType,
                                                          String verifiedChannelClaim, Map<String, String> properties, boolean skipExpiredCodeValidation)
            throws IdentityRecoveryException {

        Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        // If the code is validated, the load method will return data. Otherwise method will throw exceptions.
        UserRecoveryData recoveryData;
        if (!skipExpiredCodeValidation) {
             recoveryData = userRecoveryDataStore.load(code);
        } else {
            recoveryData = userRecoveryDataStore.load(code,skipExpiredCodeValidation);
        }
        User user = recoveryData.getUser();

        // Validate context tenant domain name with user tenant domain.
        validateContextTenantDomainWithUserTenantDomain(user);

        // Validate the recovery step to confirm self sign up, to verify email account or to verify mobile number.
        validateRecoverySteps(recoveryData, user);

        // Get the userstore manager for the user.
        UserStoreManager userStoreManager = getUserStoreManager(user);
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER, user);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        if (RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep())) {
            triggerEvent(eventProperties, IdentityEventConstants.Event.PRE_USER_ACCOUNT_CONFIRMATION);
        } else if (RecoverySteps.VERIFY_EMAIL.equals(recoveryData.getRecoveryStep())) {
            triggerEvent(eventProperties, IdentityEventConstants.Event.PRE_EMAIL_CHANGE_VERIFICATION);
        }

        String externallyVerifiedClaim = null;

        // Get externally verified claim from the validation request which is bound to the verified channel.
        // If the channel type is EXTERNAL, no verified claims are associated to it.
        if (!NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(verifiedChannelType)) {
            externallyVerifiedClaim = getChannelVerifiedClaim(recoveryData.getUser().getUserName(), verifiedChannelType,
                    verifiedChannelClaim);
        }

        // Get the claims that needs to be updated.
        // NOTE: Verification channel is stored in Remaining_Sets in user recovery data.
        HashMap<String, String> userClaims = getClaimsListToUpdate(user, verifiedChannelType,
                externallyVerifiedClaim, recoveryData.getRecoveryScenario().toString());

        if (RecoverySteps.VERIFY_EMAIL.equals(recoveryData.getRecoveryStep())) {
            String pendingEmailClaimValue = recoveryData.getRemainingSetIds();
            if (StringUtils.isNotBlank(pendingEmailClaimValue)) {
                eventProperties.put(IdentityEventConstants.EventProperty.VERIFIED_EMAIL, pendingEmailClaimValue);
                userClaims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
                userClaims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, pendingEmailClaimValue); //todo??
                // Todo passes when email address is properly set here.
                Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipEmailVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString());
            }
        }
        if (RecoverySteps.VERIFY_MOBILE_NUMBER.equals(recoveryData.getRecoveryStep())) {
            String pendingMobileClaimValue = recoveryData.getRemainingSetIds();
            if (StringUtils.isNotBlank(pendingMobileClaimValue)) {
                userClaims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
                userClaims.put(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, pendingMobileClaimValue);
                // Todo passes when mobile number is properly set here.
                Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString());
            }
        }
        // Update the user claims.
        updateUserClaims(userStoreManager, user, userClaims);

        if (RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep())) {
            String verifiedChannelURI = extractVerifiedChannelURI(userClaims, verifiedChannelClaim);
            eventProperties.put(IdentityEventConstants.EventProperty.VERIFIED_CHANNEL, verifiedChannelURI);
            triggerEvent(eventProperties, IdentityEventConstants.Event.POST_USER_ACCOUNT_CONFIRMATION);
        } else if (RecoverySteps.VERIFY_EMAIL.equals(recoveryData.getRecoveryStep())) {
            triggerEvent(eventProperties, IdentityEventConstants.Event.POST_EMAIL_CHANGE_VERIFICATION);
        }
        auditRecoveryConfirm(recoveryData, null, AUDIT_SUCCESS);
        return recoveryData;
    }

    private void validateRecoverySteps(UserRecoveryData recoveryData, User user)
            throws IdentityRecoveryServerException, IdentityRecoveryClientException {

        if (!RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep()) &&
                !RecoverySteps.VERIFY_EMAIL.equals(recoveryData.getRecoveryStep()) &&
                !RecoverySteps.CONFIRM_LITE_SIGN_UP.equals(recoveryData.getRecoveryStep()) &&
                !RecoverySteps.VERIFY_MOBILE_NUMBER.equals(recoveryData.getRecoveryStep())) {
            auditRecoveryConfirm(recoveryData,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getMessage(), AUDIT_FAILED);
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNELS, null);
        }
        if (RecoverySteps.VERIFY_MOBILE_NUMBER.equals(recoveryData.getRecoveryStep()) &&
                !isMobileVerificationEnabledForPrivilegedUsers(user.getTenantDomain())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MOBILE_VERIFICATION_NOT_ENABLE_PRIVILEGED_USERS,
                    null);
        }
    }

    private boolean isMobileVerificationEnabledForPrivilegedUsers(String tenantDomain)
            throws IdentityRecoveryServerException {

        try {
            return Boolean.parseBoolean(
                    Utils.getConnectorConfig(
                            IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                            tenantDomain));
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_CONNECTOR_CONFIG,
                    tenantDomain, e);
        }
    }


    /**
     * Introspects self registration confirmation code details without invalidating it.
     * Does not triggering notification events or update user claims.
     *
     * @param skipExpiredCodeValidation   Skip confirmation code validation against expiration.
     * @param code                      Confirmation code.
     * @return UserRecoveryData           Data associated with the provided code, including related user and scenarios.
     * @throws IdentityRecoveryException  Error validating the confirmation code
     */
    private UserRecoveryData introspectSelfRegistrationCode(String code, boolean skipExpiredCodeValidation)
            throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        // If the code is validated, the load method will return data. Otherwise method will throw exceptions.
        UserRecoveryData recoveryData;
        if (!skipExpiredCodeValidation) {
            recoveryData = userRecoveryDataStore.load(code);
        } else {
            recoveryData = userRecoveryDataStore.load(code,skipExpiredCodeValidation);
        }
        User user = recoveryData.getUser();

        // Validate context tenant domain name with user tenant domain.
        validateContextTenantDomainWithUserTenantDomain(user);

        // Validate the recovery step to confirm self sign up, to verify email account or to verify mobile number.
        validateRecoverySteps(recoveryData, user);

        return recoveryData;
    }

    private String extractVerifiedChannelURI(HashMap<String, String> userClaims, String externallyVerifiedClaim) {

        String verifiedChannelURI = null;
        for (Map.Entry<String, String> entry : userClaims.entrySet()) {
            String key = entry.getKey();
            if (key.equals(externallyVerifiedClaim) || key.equals(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM) ||
                    key.equals(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl())) {
                verifiedChannelURI = key;
                break;
            }
        }
        return verifiedChannelURI;
    }

    private void triggerEvent(Map<String, Object> properties, String eventName)
            throws IdentityRecoveryServerException, IdentityRecoveryClientException {

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventClientException e) {
            throw new IdentityRecoveryClientException(e.getErrorCode(), e.getMessage(), e);
        } catch (IdentityEventServerException e) {
            throw new IdentityRecoveryServerException(e.getErrorCode(), e.getMessage(), e);
        } catch (IdentityEventException e) {
            throw Utils
                    .handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PUBLISH_EVENT,
                            eventName, e);
        }
    }

    /**
     * Validates the verification code and update verified claims of the authenticated user.
     *
     * @param code                 Confirmation code.
     * @param properties           Properties sent with the validate code request.
     * @throws IdentityRecoveryException Error validating the confirmation code.
     */
    public void confirmVerificationCodeMe(String code, Map<String, String> properties) throws
            IdentityRecoveryException {

        Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        // If the code is validated, the load method will return data. Otherwise method will throw exceptions.
        UserRecoveryData recoveryData = userRecoveryDataStore.load(code);

        // Validate the recovery step to verify mobile claim scenario.
        if (!RecoverySteps.VERIFY_MOBILE_NUMBER.equals(recoveryData.getRecoveryStep())) {
            auditRecoveryConfirm(recoveryData,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getMessage(), AUDIT_FAILED);
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        User user = recoveryData.getUser();
        // Validate context username and tenant domain name with user from recovery data.
        validateUser(user);

        // Get the userstore manager for the user.
        UserStoreManager userStoreManager = getUserStoreManager(user);
        HashMap<String, String> userClaims = new HashMap<>();

        if (RecoverySteps.VERIFY_MOBILE_NUMBER.equals(recoveryData.getRecoveryStep())) {
            String pendingMobileNumberClaimValue = recoveryData.getRemainingSetIds();
            if (StringUtils.isNotBlank(pendingMobileNumberClaimValue)) {
                userClaims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
                userClaims.put(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, pendingMobileNumberClaimValue);
                userClaims.put(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
                Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString());
            }
        }
        // Update the user claims.
        updateUserClaims(userStoreManager, user, userClaims);
        // Invalidate code.
        userRecoveryDataStore.invalidate(code);
        auditRecoveryConfirm(recoveryData, null, AUDIT_SUCCESS);
    }

    /**
     * Update the user claims.
     *
     * @param userStoreManager Userstore manager
     * @param user             User
     * @param userClaims       Claims that needs to be updated
     * @throws IdentityRecoveryException Error while unlocking user
     */
    private void updateUserClaims(UserStoreManager userStoreManager, User user, HashMap<String, String> userClaims)
            throws IdentityRecoveryException {

        try {
            userStoreManager
                    .setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()),
                            userClaims, null);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNLOCK_USER_USER,
                    user.getUserName(), e);
        }
    }

    /**
     * Get the verified channel claim associated with the externally verified channel.
     *
     * @param username             Username of the user
     * @param verifiedChannelType  Verified channel type
     * @param verifiedChannelClaim Verified channel claim
     * @return Verified claim associated with the externally verified channel
     */
    private String getChannelVerifiedClaim(String username, String verifiedChannelType, String verifiedChannelClaim)
            throws IdentityRecoveryException {

        if (StringUtils.isNotEmpty(verifiedChannelType) && StringUtils.isNotEmpty(verifiedChannelClaim)) {

            // Get the notification channel which matches the given channel type
            NotificationChannels channel = getNotificationChannel(username, verifiedChannelType);
            String channelClaim = channel.getClaimUri();

            // Check whether the channels claims are matching.
            if (channelClaim.equals(verifiedChannelClaim)) {
                return channel.getVerifiedClaimUrl();
            } else {
                if (log.isDebugEnabled()) {
                    String error = String.format("Channel claim: %s in the request does not match the channel claim "
                            + "bound to channelType : %s", verifiedChannelType, verifiedChannelType);
                    log.debug(error);
                }
                throw new IdentityRecoveryException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_VERIFICATION_CHANNEL
                                .getMessage(),
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_VERIFICATION_CHANNEL.getCode());
            }
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Externally verified channels are not specified");
            }
            return null;
        }
    }

    /**
     * Get the claims that needs to be updated during the registration confirmation.
     *
     * @param user                           User
     * @param verificationChannel            Verification channel associated with the confirmation code
     * @param externallyVerifiedChannelClaim Externally verified channel claim
     * @param recoveryScenario               Recovery scenario
     * @return Claims that needs to be updated
     */
    private HashMap<String, String> getClaimsListToUpdate(User user, String verificationChannel,
            String externallyVerifiedChannelClaim, String recoveryScenario) {

        HashMap<String, String> userClaims = new HashMap<>();

        // Need to unlock user account
        userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
        userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM, StringUtils.EMPTY);

        // Set the verified claims to TRUE.
        setVerificationClaims(user, verificationChannel, externallyVerifiedChannelClaim, recoveryScenario, userClaims);

        //Set account verified time claim.
        userClaims.put(IdentityRecoveryConstants.ACCOUNT_CONFIRMED_TIME_CLAIM, Instant.now().toString());

        return userClaims;
    }

    /**
     * Get the userstore manager for the user.
     *
     * @param user User
     * @return Userstore manager
     * @throws IdentityRecoveryException Error getting the userstore manager.
     */
    private UserStoreManager getUserStoreManager(User user) throws IdentityRecoveryException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            org.wso2.carbon.user.api.UserRealm tenantUserRealm = realmService.getTenantUserRealm(IdentityTenantUtil.
                    getTenantId(user.getTenantDomain()));
            if (IdentityUtil.getPrimaryDomainName().equals(user.getUserStoreDomain())) {
                return tenantUserRealm.getUserStoreManager();
            }
            return ((org.wso2.carbon.user.core.UserStoreManager) tenantUserRealm.getUserStoreManager())
                    .getSecondaryUserStoreManager(user.getUserStoreDomain());
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("Error getting the user store manager for the user : %s with in domain : %s.",
                                user.getUserStoreDomain() + CarbonConstants.DOMAIN_SEPARATOR + user.getUserName(),
                                user.getTenantDomain());
                log.debug(message);
            }
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                    user.getUserName(), e);
        }
    }

    /**
     * Validate context username and tenant with the stored user's username and tenant domain..
     *
     * @param user User
     * @throws IdentityRecoveryException Invalid Username/Tenant.
     */
    private void validateUser(User user) throws IdentityRecoveryException {

        validateContextTenantDomainWithUserTenantDomain(user);
        String contextUsername = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        String username;
        if (!UserStoreConfigConstants.PRIMARY.equals(user.getUserStoreDomain())) {
            username = user.getUserStoreDomain() + CarbonConstants.DOMAIN_SEPARATOR + user.getUserName();
        } else {
            username = user.getUserName();
        }
        if (!StringUtils.equalsIgnoreCase(contextUsername, username)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                    contextUsername);
        }
    }

    /**
     * Validate context tenant domain name with user tenant domain.
     *
     * @param user User
     * @throws IdentityRecoveryException Invalid Tenant
     */
    private void validateContextTenantDomainWithUserTenantDomain(User user) throws IdentityRecoveryException {

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = user.getTenantDomain();
        if (!StringUtils.equalsIgnoreCase(contextTenantDomain, userTenantDomain)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_TENANT,
                    contextTenantDomain);
        }
    }

    /**
     * Set the email verified or mobile verified claim to TRUE according to the verified channel in the request.
     *
     * @param user                           User
     * @param verificationChannel            Verification channel (SMS, EMAIL, EXTERNAL)
     * @param externallyVerifiedChannelClaim Claims to be set at confirmation
     * @param recoveryScenario               Recovery scenario
     * @param userClaims                     User claims for the user
     */
    private void setVerificationClaims(User user, String verificationChannel, String externallyVerifiedChannelClaim,
                                       String recoveryScenario, HashMap<String, String> userClaims) {

        // Externally verified channel claims are sent with the code validation request.
        if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(verificationChannel)) {
            if (StringUtils.isNotEmpty(externallyVerifiedChannelClaim)) {
                if (log.isDebugEnabled()) {
                    String message = String
                            .format("Externally verified claim is available for user :%s in tenant domain : %s ",
                                    user.getUserName(), user.getTenantDomain());
                    log.debug(message);
                }
                userClaims.put(externallyVerifiedChannelClaim, Boolean.TRUE.toString());
            } else {
                // Externally verified channel claims are not in the request, set the email claim as the verified
                // channel.
                if (log.isDebugEnabled()) {
                    String message = String
                            .format("Externally verified channel claims are not available for user : %s in tenant "
                                            + "domain : %s. Therefore, setting %s claim as the default " +
                                            "verified channel.", user.getUserName(), user.getTenantDomain(),
                                    NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl());
                    log.debug(message);
                }
                // If no verification claims are sent, set the email verified claim to true.
                // This is to support backward compatibility.
                userClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
            }
        } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equalsIgnoreCase(verificationChannel)) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("Self sign-up via SMS channel detected. Updating %s value for user : %s in tenant "
                                        + "domain : %s ", NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(),
                                user.getUserName(), user.getTenantDomain());
                log.debug(message);
            }
            userClaims.put(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
        } else if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equalsIgnoreCase(verificationChannel)) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("Self sign-up via EMAIL channel detected. Updating %s value for user : %s in tenant "
                                        + "domain : %s ", NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(),
                                user.getUserName(), user.getTenantDomain());
                log.debug(message);
            }
            userClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
        } else {
            if (log.isDebugEnabled()) {
                String message = String.format("No notification channel detected for user : %s in tenant domain : %s "
                                + "for recovery scenario : %s. Therefore setting email as the verified channel.",
                        user.getUserName(), user.getTenantDomain(), recoveryScenario);
                log.debug(message);
            }
            userClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
        }
    }

    /**
     * This method is deprecated.
     *
     * @since 1.3.51
     * @deprecated New APIs have been provided.
     * Use
     * {@link org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager#resendConfirmationCode(User, String, String, String, Property[])}
     * method.
     */
    @Deprecated
    public NotificationResponseBean resendConfirmationCode(User user, Property[] properties)
            throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
            if (StringUtils.isBlank(tenantDomain)) {
                tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
            }
            user.setTenantDomain(tenantDomain);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for " +
                    "user : " + user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default " +
                    "for user : " + user.getUserName());
        }

        boolean selfRegistrationEnabled = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!selfRegistrationEnabled) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP,
                    user.getUserName());
        }

        ResendConfirmationManager resendConfirmationManager = ResendConfirmationManager.getInstance();
        NotificationResponseBean notificationResponseBean =
                resendConfirmationManager.resendConfirmationCode(user, RecoveryScenarios.SELF_SIGN_UP.toString()
                        , RecoverySteps.CONFIRM_SIGN_UP.toString(),
                        IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM, properties);
        notificationResponseBean.setCode(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE.getCode());
        notificationResponseBean.setMessage(
                IdentityRecoveryConstants.SuccessEvents.SUCCESS_STATUS_CODE_RESEND_CONFIRMATION_CODE.getMessage());
        return notificationResponseBean;
    }

    /**
     * Checks whether the given tenant domain of a username is valid / exists or not.
     *
     * @param tenantDomain Tenant domain.
     * @return True if the tenant domain of the user is valid / available, else false.
     */
    public boolean isValidTenantDomain(String tenantDomain) throws IdentityRecoveryException {

        boolean isValidTenant = false;
        try {
            UserRealm userRealm = getUserRealm(tenantDomain);
            isValidTenant = userRealm != null;

        } catch (CarbonException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while getting user realm for user " + tenantDomain);
            }
            // In a case of a non existing tenant.
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        }
        return isValidTenant;
    }

    /**
     * Checks whether the given userstore domain of a username is valid / exists or not.
     *
     * @param userStoreDomain Userstore domain of the user.
     * @return True if the userstore domain of the user is valid / available, else false.
     */
    public boolean isValidUserStoreDomain(String userStoreDomain, String tenantDomain) throws IdentityRecoveryException {

        boolean isValidUserStore;
        try {
            UserStoreManager userStoreManager = getUserRealm(tenantDomain).getUserStoreManager().
                    getSecondaryUserStoreManager(userStoreDomain);
            isValidUserStore = userStoreManager != null;

        } catch (CarbonException | UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while getting secondary userstore manager for domain " + userStoreDomain);
            }
            // In a case of a non existing tenant.
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        }
        return isValidUserStore;
    }

    /**
     * Returns whether a given username is already taken by a user or not.
     *
     * @param username Username.
     * @return True if the username is already taken, else false.
     * @deprecated  After v1.4.5 due to inability to support tenant based username check.
     *              Use isUsernameAlreadyTaken(String username, String tenantDomain)
     */
    @Deprecated
    public boolean isUsernameAlreadyTaken(String username) throws IdentityRecoveryException {

        return isUsernameAlreadyTaken(username, null);
    }

    /**
     * Returns whether a given username is already taken.
     *
     * @param username Username
     * @param tenantDomain Tenant domain in the request.
     * @return True if username is already taken, else false.
     * @throws IdentityRecoveryException Error occurred while retrieving user realm.
     */
    public boolean isUsernameAlreadyTaken(String username, String tenantDomain) throws IdentityRecoveryException {

        boolean isUsernameAlreadyTaken = true;
        if (StringUtils.isBlank(tenantDomain)) {
            tenantDomain = MultitenantUtils.getTenantDomain(username);
        }
        try {
            String tenantAwareUsername = MultitenantUtils.getTenantAwareUsername(username);

            UserRealm userRealm = getUserRealm(tenantDomain);
            if (userRealm != null) {
                isUsernameAlreadyTaken = userRealm.getUserStoreManager().isExistingUser(tenantAwareUsername);
            }
        } catch (CarbonException | org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        }
        return isUsernameAlreadyTaken;
    }

    /**
     * Checks whether self registration is enabled or not for a given tenant domain
     *
     * @param tenantDomain Tenant domain.
     * @return True if self registration is enabled for a tenant domain. If not returns false.
     */
    public boolean isSelfRegistrationEnabled(String tenantDomain) throws IdentityRecoveryException {

        String selfSignUpEnabled = getIDPProperty(tenantDomain,
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        return Boolean.parseBoolean(selfSignUpEnabled);
    }

    private String getIDPProperty(String tenantDomain, String propertyName) throws
            IdentityRecoveryException {

        String propertyValue = "";
        try {
            org.wso2.carbon.identity.application.common.model.Property[] configuration =
                    IdentityRecoveryServiceDataHolder.getInstance().getIdentityGovernanceService().
                            getConfiguration(new String[]{IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP},
                                    tenantDomain);
            for (org.wso2.carbon.identity.application.common.model.Property configProperty : configuration) {
                if (configProperty != null && propertyName.equalsIgnoreCase(configProperty.getName())) {
                    propertyValue = configProperty.getValue();
                }
            }
        } catch (IdentityGovernanceException e) {
            throw new IdentityRecoveryException("Error while retrieving resident identity provider for tenant : "
                    + tenantDomain, e);
        }
        return propertyValue;
    }

    private void addConsent(String consent, String tenantDomain) throws ConsentManagementException, IdentityRecoveryServerException {

        Gson gson = new Gson();
        ReceiptInput receiptInput = gson.fromJson(consent, ReceiptInput.class);
        ConsentManager consentManager = IdentityRecoveryServiceDataHolder.getInstance().getConsentManager();

        if (receiptInput.getServices().size() < 0) {
            throw new IdentityRecoveryServerException("A service should be available in a receipt");
        }
        // There should be a one receipt
        ReceiptServiceInput receiptServiceInput = receiptInput.getServices().get(0);

        // Handle the scenario, where all the purposes are having optional PII attributes and then the user register
        // without giving consent to any of the purposes.
        if (receiptServiceInput.getPurposes().isEmpty()) {
            if (log.isDebugEnabled()) {
                log.debug("Consent does not contain any purposes. Hence not adding consent");
            }
            return;
        }

        receiptServiceInput.setTenantDomain(tenantDomain);
        try {
            setIDPData(tenantDomain, receiptServiceInput);
        } catch (IdentityProviderManagementException e) {
            throw new ConsentManagementException("Error while retrieving identity provider data", "Error while " +
                    "setting IDP data", e);
        }
        receiptInput.setTenantDomain(tenantDomain);
        consentManager.addConsent(receiptInput);
    }

    private void setIDPData(String tenantDomain, ReceiptServiceInput receiptServiceInput)
            throws IdentityProviderManagementException {

        IdentityProviderManager idpManager = IdentityProviderManager.getInstance();
        IdentityProvider residentIdP = idpManager.getResidentIdP(tenantDomain);

        if (StringUtils.isEmpty(receiptServiceInput.getService())) {
            if (log.isDebugEnabled()) {
                log.debug("No service name found. Hence adding resident IDP home realm ID");
            }
            receiptServiceInput.setService(residentIdP.getHomeRealmId());
        }
        if (StringUtils.isEmpty(receiptServiceInput.getTenantDomain())) {
            receiptServiceInput.setTenantDomain(tenantDomain);
        }
        if (StringUtils.isEmpty(receiptServiceInput.getSpDescription())) {
            if (StringUtils.isNotEmpty(residentIdP.getIdentityProviderDescription())) {
                receiptServiceInput.setSpDescription(residentIdP.getIdentityProviderDescription());
            } else {
                receiptServiceInput.setSpDescription(IdentityRecoveryConstants.Consent.RESIDENT_IDP);
            }
        }
        if (StringUtils.isEmpty(receiptServiceInput.getSpDisplayName())) {
            if (StringUtils.isNotEmpty(residentIdP.getDisplayName())) {
                receiptServiceInput.setSpDisplayName(residentIdP.getDisplayName());
            } else {
                receiptServiceInput.setSpDisplayName(IdentityRecoveryConstants.Consent.RESIDENT_IDP);
            }
        }
    }

    private String getPropertyValue(Property[] properties, String key) {

        String propertyValue = "";
        if (properties != null && StringUtils.isNotEmpty(key)) {
            for (int index = 0; index < properties.length; index++) {
                Property property = properties[index];
                if (key.equalsIgnoreCase(property.getKey())) {
                    propertyValue = property.getValue();
                    ArrayUtils.removeElement(properties, property);
                    break;
                }
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("Returning value for key : " + key + " - " + propertyValue);
        }
        return propertyValue;
    }

    private UserRealm getUserRealm(String tenantDomain) throws CarbonException {

        return AnonymousSessionUtil.getRealmByTenantDomain(IdentityRecoveryServiceDataHolder.getInstance()
                        .getRegistryService(), IdentityRecoveryServiceDataHolder.getInstance().getRealmService(),
                tenantDomain);
    }

    /**
     * Checks whether the given tenant domain of a username is valid / exists or not.
     *
     * @param tenantDomain Tenant domain.
     * @return True if the tenant domain of the user is valid / available, else false.
     */
    public boolean isMatchUserNameRegex(String tenantDomain, String username) throws IdentityRecoveryException {

        boolean isValidUsername;
        String userDomain = IdentityUtil.extractDomainFromName(username);
        try {
            UserRealm userRealm = getUserRealm(tenantDomain);
            RealmConfiguration realmConfiguration = userRealm.getUserStoreManager().getSecondaryUserStoreManager
                    (userDomain).getRealmConfiguration();
            String tenantAwareUsername = MultitenantUtils.getTenantAwareUsername(username);
            String userStoreDomainAwareUsername = UserCoreUtil.removeDomainFromName(tenantAwareUsername);
            isValidUsername = checkUserNameValid(userStoreDomainAwareUsername, realmConfiguration);

        } catch (CarbonException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while getting user realm for user " + tenantDomain);
            }
            // In a case of a non existing tenant.
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while getting user store configuration for tenant: " + tenantDomain + ", domain: " +
                        userDomain);
            }
            // In a case of a non existing tenant.
            throw new IdentityRecoveryException("Error while retrieving user store configuration for: " + userDomain, e);
        }
        return isValidUsername;
    }

    /** This is similar to username validation in UserstoreManager
     * @param userName
     * @return
     * @throws
     */
    private boolean checkUserNameValid(String userName, RealmConfiguration realmConfig) {

        if (userName == null || CarbonConstants.REGISTRY_SYSTEM_USERNAME.equals(userName)) {
            return false;
        }

        userName = userName.trim();

        if (userName.length() < 1) {
            return false;
        }

        String regularExpression = realmConfig
                .getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_JAVA_REG_EX);

        if (MultitenantUtils.isEmailUserName()) {
            regularExpression = realmConfig
                    .getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_USER_NAME_WITH_EMAIL_JS_REG_EX);
            if (regularExpression == null) {
                regularExpression = UserCoreConstants.RealmConfig.EMAIL_VALIDATION_REGEX;
            }
        }

        if (regularExpression != null) {
            regularExpression = regularExpression.trim();
        }

        return StringUtils.isEmpty(regularExpression) || isFormatCorrect(regularExpression, userName);

    }

    /**
     * Validate with regex
     * @param regularExpression
     * @param attribute
     * @return
     */
    private boolean isFormatCorrect(String regularExpression, String attribute) {
        Pattern p2 = Pattern.compile(regularExpression);
        Matcher m2 = p2.matcher(attribute);
        return m2.matches();
    }

    /**
     * Pre validate password against the policies defined in the Identity Server during password recovery instance.
     *
     * @param confirmationKey Confirmation code for password recovery.
     * @param password        Password to be pre-validated against the policies defined in IS.
     * @throws IdentityEventException    Error handling the event.
     * @throws IdentityRecoveryException Error getting the userstore manager.
     */
    public void preValidatePasswordWithConfirmationKey(String confirmationKey, String password) throws
            IdentityEventException, IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData recoveryData = userRecoveryDataStore.load(confirmationKey);
        User user = recoveryData.getUser();
        String userStoreDomain = user.getUserStoreDomain();
        String username = user.getUserName();

        preValidatePassword(username, password, userStoreDomain);
    }

    /**
     * Pre validate the password against the policies defined in the Identity Server.
     *
     * @param username Username.
     * @param password Password to be pre-validated against the policies defined in IS.
     * @throws IdentityRecoveryServerException Error getting the userstore manager.
     * @throws IdentityEventException          Error handling the event.
     */
    public void preValidatePasswordWithUsername(String username, String password) throws IdentityEventException,
            IdentityRecoveryServerException {

        String userStoreDomain = IdentityUtil.extractDomainFromName(username);
        username = UserCoreUtil.removeDomainFromName(username);

        preValidatePassword(username, password, userStoreDomain);
    }

    private void preValidatePassword(String username, String password, String userStoreDomain) throws
            IdentityRecoveryServerException, IdentityEventException {

        String tenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);

        String eventName = IdentityEventConstants.Event.PRE_UPDATE_CREDENTIAL_BY_ADMIN;
        HashMap<String, Object> properties = new HashMap<>();

        properties.put(IdentityEventConstants.EventProperty.USER_NAME, username);
        properties.put(IdentityEventConstants.EventProperty.CREDENTIAL, password);
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        properties.put(IdentityEventConstants.EventProperty.TENANT_ID, tenantId);

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            AbstractUserStoreManager userStoreManager = (AbstractUserStoreManager)
                    realmService.getTenantUserRealm(tenantId).getUserStoreManager();
            UserStoreManager secondaryUserStoreManager = userStoreManager.getSecondaryUserStoreManager
                    (userStoreDomain);
            properties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, secondaryUserStoreManager);
        } catch (UserStoreException e) {
            String message = String.format("Error getting the user store manager for the user : %s in domain :" +
                    " %s.", userStoreDomain + CarbonConstants.DOMAIN_SEPARATOR + username, tenantDomain);
            if(log.isDebugEnabled()){
                log.debug(message, e);
            }
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }

        if (log.isDebugEnabled()) {
            log.debug(String.format("Validating password against policies for user: %s in tenant: %s and in user " +
                    "store: %s", username, tenantDomain, userStoreDomain));
        }

        Event identityMgtEvent = new Event(eventName, properties);
        IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
    }

    public NotificationResponseBean registerLiteUser(User user, Claim[] claims, Property[] properties) throws IdentityRecoveryException {

        String consent = getPropertyValue(properties, IdentityRecoveryConstants.Consent.CONSENT);
        String tenantDomain = user.getTenantDomain();

        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        // Callback URL validation
        String callbackURL = null;
        try {
            callbackURL = Utils.getCallbackURLFromRegistration(properties);
            if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, tenantDomain,
                    IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_CALLBACK_REGEX)) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
            }
        } catch (MalformedURLException | UnsupportedEncodingException | IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("registerUser :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("registerUser :User store domain is not in the request. set to default for user : " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_LITE_SIGN_UP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_LITE_SIGN_UP, user
                    .getUserName());
        }
        NotificationResponseBean notificationResponseBean;
        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());

            Map<String, String> claimsMap = new HashMap<>();
            for (Claim claim : claims) {
                claimsMap.put(claim.getClaimUri(), claim.getValue());
            }

            //Set lite user sign up claim to indicate the profile
            claimsMap.put(IdentityRecoveryConstants.LITE_USER_CLAIM,Boolean.TRUE.toString());

            //Set arbitrary properties to use in UserSelfRegistrationHandler
            Utils.setArbitraryProperties(properties);
            validateAndFilterFromReceipt(consent, claimsMap);

            // User preferred notification channel.
            String preferredChannel;
            try {
                String[] userRoles = new String[]{};
                try {
                    NotificationChannelManager notificationChannelManager = Utils.getNotificationChannelManager();
                    preferredChannel = notificationChannelManager
                            .resolveCommunicationChannel(user.getUserName(), user.getTenantDomain(),
                                    user.getUserStoreDomain(), claimsMap);
                } catch (NotificationChannelManagerException e) {
                    throw mapNotificationChannelManagerException(e, user);
                }
                // If the preferred channel value is not in the claims map, add the value to  claims map if the
                // resolved channel is not empty.
                if (StringUtils.isEmpty(claimsMap.get(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM)) && StringUtils
                        .isNotEmpty(preferredChannel)) {
                    claimsMap.put(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM, preferredChannel);
                }
                userStoreManager
                        .addUser(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()), Utils.generateRandomPassword(12),
                                userRoles, claimsMap, null);
            } catch (UserStoreException e) {
                Throwable cause = e;
                while (cause != null) {
                    if (cause instanceof PolicyViolationException) {
                        throw IdentityException.error(IdentityRecoveryClientException.class,
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(), cause.getMessage(), e);
                    }
                    cause = cause.getCause();
                }

                return handleClientException(user, e);
            }
            addUserConsent(consent, tenantDomain);

            // Build the notification response for lite user.
            notificationResponseBean = buildLiteNotificationResponseBean(user, preferredChannel, claimsMap);
        } finally {
            Utils.clearArbitraryProperties();
            PrivilegedCarbonContext.endTenantFlow();
        }
        return notificationResponseBean;
    }

    /**
     * Build the notification response bean.
     *
     * @param user             User
     * @param preferredChannel User preferred channel
     * @param claimsMap        Claim map of the user
     * @return NotificationResponseBean object
     * @throws IdentityRecoveryException Error while building the response.
     */
    private NotificationResponseBean buildLiteNotificationResponseBean(User user, String preferredChannel,
                                                                   Map<String, String> claimsMap) throws IdentityRecoveryException {

        boolean isAccountLockOnCreation = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.LITE_ACCOUNT_LOCK_ON_CREATION,
                        user.getTenantDomain()));
        boolean isNotificationInternallyManage = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.LITE_SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                        user.getTenantDomain()));

        // Check whether the preferred channel is already verified. In this case no need to send confirmation
        // mails.
        boolean preferredChannelVerified = isPreferredChannelVerified(user.getUserName(), preferredChannel, claimsMap);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        // If the channel is already verified, no need to lock the account or ask to verify the account
        // since, the notification channel is already verified.
        if (preferredChannelVerified) {
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_WITH_VERIFIED_CHANNEL.getMessage());
        } else if (isNotificationInternallyManage && isAccountLockOnCreation) {

            // When the channel is not verified, notifications are internally managed and account is locked
            // on creating, API should ask the user to verify the user account and and notification channel.
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_INTERNAL_VERIFICATION.getMessage());
            notificationResponseBean.setNotificationChannel(preferredChannel);
        } else if (!isAccountLockOnCreation) {

            // When the preferred channel is not verified and account is not locked on user creation, response needs to
            // convey that no verification is needed.
            // In this scenario notification managed mechanism will not effect.
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_UNLOCKED_WITH_NO_VERIFICATION.getMessage());
        } else {
            // When the notification is externally managed and the account is locked on user creation.
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.invalidate(user);

            String secretKey = UUIDGenerator.generateUUID();
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios.LITE_SIGN_UP,
                    RecoverySteps.CONFIRM_LITE_SIGN_UP);
            recoveryDataDO.setRemainingSetIds(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());

            userRecoveryDataStore.store(recoveryDataDO);
            notificationResponseBean.setCode(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getCode());
            notificationResponseBean.setMessage(IdentityRecoveryConstants.SuccessEvents.
                    SUCCESS_STATUS_CODE_SUCCESSFUL_USER_CREATION_EXTERNAL_VERIFICATION.getMessage());
            notificationResponseBean.setRecoveryId(secretKey);
            notificationResponseBean.setNotificationChannel(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());

            // Populate the key variable in response bean to maintain backward compatibility.
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;
    }

    private void auditRecoveryConfirm(UserRecoveryData recoveryData, String errorMsg, String result) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.EMAIL_TO_BE_CHANGED, recoveryData.getRemainingSetIds());
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(recoveryData.getRecoveryScenario().toString(), recoveryData.getUser().getUserName(),
                dataObject, result);
    }

    private void triggerNotification(User user) throws IdentityRecoveryServerException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE,
                IdentityRecoveryConstants.NOTIFICATION_TYPE_SELF_SIGNUP_SUCCESS);

        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd/MM/yy hh:mm:ss");
        String selfSignUpConfirmationTime = simpleDateFormat.format(new Date(System.currentTimeMillis()));
        properties.put(IdentityEventConstants.EventProperty.SELF_SIGNUP_CONFIRM_TIME, selfSignUpConfirmationTime);

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    /**
     * Method to publish pre and post self sign up register event.
     *
     * @param user           self sign up user
     * @param claims         claims of the user
     * @param metaProperties other properties of the request
     * @param eventName      event name (PRE_SELF_SIGNUP_REGISTER,POST_SELF_SIGNUP_REGISTER)
     * @throws IdentityRecoveryException
     */
    private void publishEvent(User user, Claim[] claims, Property[] metaProperties,
                              String eventName) throws
            IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);

        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        handleEvent(eventName,properties,user);
    }

    /**
     * Method to publish post self sign up confirm event.
     *
     * @param user                 self sign up user
     * @param code                 self signup confirmation code
     * @param verifiedChannelType  verified channel type
     * @param verifiedChannelClaim verified channel claim.
     * @param metaProperties       metaproperties of the request
     * @param eventName            event name (POST_SELF_SIGNUP_CONFIRM)
     * @throws IdentityRecoveryException
     */
    private void publishEvent(User user, String code, String verifiedChannelType,String verifiedChannelClaim,
                              Map<String, String> metaProperties,
                              String eventName) throws
            IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_CODE, code);
        }
        if (StringUtils.isNotBlank(verifiedChannelType)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_VERIFIED_CHANNEL, verifiedChannelType);
        }
        if (StringUtils.isNotBlank(verifiedChannelClaim)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_VERIFIED_CHANNEL_CLAIM, verifiedChannelClaim);
        }
        if (metaProperties != null) {
            for (Map.Entry<String, String> entry : metaProperties.entrySet()) {
                if (StringUtils.isNotBlank(entry.getValue()) && StringUtils.isNotBlank(entry.getKey())) {
                    properties.put(entry.getKey(), entry.getValue());
                }
            }
        }
        handleEvent(eventName,properties,user);
    }

    /**
     * Method to publish pre self sign up confirm event.
     *
     * @param code self signup confirmation code
     * @param verifiedChannelType verified channel type
     * @param verifiedChannelClaim verified channel claim.
     * @param metaProperties metaproperties of the request
     * @param eventName event name (PRE_SELF_SIGNUP_CONFIRM)
     * @throws IdentityRecoveryException
     */
    private void publishEvent(String code, String verifiedChannelType,String verifiedChannelClaim,
                              Map<String, String> metaProperties,
                              String eventName) throws
            IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_CODE, code);
        }
        if (StringUtils.isNotBlank(verifiedChannelType)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_VERIFIED_CHANNEL, verifiedChannelType);
        }
        if (StringUtils.isNotBlank(verifiedChannelClaim)) {
            properties.put(IdentityEventConstants.EventProperty.SELF_REGISTRATION_VERIFIED_CHANNEL_CLAIM, verifiedChannelClaim);
        }
        if (metaProperties != null) {
            for (Map.Entry<String, String> entry : metaProperties.entrySet()) {
                if (StringUtils.isNotBlank(entry.getValue()) && StringUtils.isNotBlank(entry.getKey())) {
                    properties.put(entry.getKey(), entry.getValue());
                }
            }
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            log.error("Error occurred while publishing event " + eventName);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PUBLISH_EVENT,
                    eventName, e);
        }

    }

    private void handleEvent(String eventName, HashMap<String, Object> properties, User user)
            throws IdentityRecoveryServerException {

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PUBLISH_EVENT,
                    eventName, e);
        }

    }
}
