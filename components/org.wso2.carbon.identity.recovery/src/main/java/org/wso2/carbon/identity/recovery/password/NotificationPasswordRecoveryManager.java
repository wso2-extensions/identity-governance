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

package org.wso2.carbon.identity.recovery.password;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONObject;
import org.slf4j.MDC;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.context.IdentityContext;
import org.wso2.carbon.identity.core.persistence.registry.RegistryResourceMgtService;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.handler.event.account.lock.constants.AccountConstants;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.organization.management.service.exception.OrganizationManagementException;
import org.wso2.carbon.identity.organization.management.service.util.OrganizationManagementUtil;
import org.wso2.carbon.identity.recovery.AuditConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.identity.user.action.api.constant.UserActionError;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
import org.wso2.carbon.registry.core.Resource;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.AUDIT_FAILED;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.LOGIN_IDENTIFIER;
import static org.wso2.carbon.registry.core.RegistryConstants.PATH_SEPARATOR;

/**
 * Manager class which can be used to recover passwords using a notification.
 */
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);
    private static NotificationPasswordRecoveryManager instance = new NotificationPasswordRecoveryManager();

    private NotificationPasswordRecoveryManager() {

    }

    public static NotificationPasswordRecoveryManager getInstance() {

        return instance;
    }

    /**
     * Send password recovery information to the user.
     *
     * @param user       User
     * @param type       Notification Type
     * @param notify     Manage notifications internally
     * @param properties Meta properties
     * @return NotificationResponseBean
     * @throws IdentityRecoveryException Error while sending recovery information.
     */
    public NotificationResponseBean sendRecoveryNotification(User user, String type, Boolean notify,
                                                             Property[] properties)
            throws IdentityRecoveryException {

        publishEvent(user, String.valueOf(notify), null, null, properties,
                IdentityEventConstants.Event.PRE_SEND_RECOVERY_NOTIFICATION, new UserRecoveryData(user, null,
                        RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD));

        validateUserStoreDomain(user);
        Utils.validateEmailUsername(user);

        // Resolve user attributes.
        resolveUserAttributes(user);
        validatePasswordRecoveryConfiguration(user.getTenantDomain());
        validateCallback(properties, user.getTenantDomain());

        // Build a property map from the properties in the request.
        HashMap<String, String> propertyMap = buildPropertyMap(properties);
        String notificationChannel = getNotificationChannelFromProperties(propertyMap);

        // Check whether to manage notifications internally.
        boolean isNotificationInternallyManage = isNotificationsInternallyManaged(user.getTenantDomain(), notify);
        if (!isNotificationInternallyManage) {
            notificationChannel = NotificationChannels.EXTERNAL_CHANNEL.getChannelType();
        }
        // Check whether the user is already verified ( NOTE: This property is set by the new recovery API).
        if (!isUserVerified(propertyMap)) {
            if (!isExistingUser(user)) {

                /* If the user does not exist, Check for NOTIFY_USER_EXISTENCE property. If the property is not
                enabled, notify with an empty NotificationResponseBean.*/
                boolean notifyUserExistence = Boolean.parseBoolean(
                        IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_EXISTENCE));
                if (notifyUserExistence) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                            user.getUserName());
                }
                return new NotificationResponseBean(user);
            } else if (isExistingUser(user) && StringUtils.isEmpty(Utils.getUserClaim(user,
                    IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM))) {

            /* If the email is not found for the user, Check for NOTIFY_RECOVERY_EMAIL_EXISTENCE property.
            If the property is not enabled, notify with an empty NotificationResponseBean.*/
                boolean notifyRecoveryEmailExistence = Boolean.parseBoolean(
                        IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_RECOVERY_EMAIL_EXISTENCE));
                if (notifyRecoveryEmailExistence) {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EMAIL_NOT_FOUND, user.getUserName());
                }
                return new NotificationResponseBean(user);
            }
        }
        // Check if the user has a local credential to recover. If not skip sending the recovery mail.
        if (!isLocalCredentialAvailable(user)) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FEDERATED_USER,
                    user.getUserName());
        }
        String eventName = Utils.resolveEventName(notificationChannel);
        if (Utils.isAccountDisabled(user)) {
            // If the NotifyUserAccountStatus is disabled, notify with an empty NotificationResponseBean.
            if (getNotifyUserAccountStatus()) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT,
                        user.getUserName());
            }
            triggerAccountStatusNotification(user, notificationChannel,
                    IdentityRecoveryConstants.ACCOUNT_STATUS_DISABLED, eventName, properties);
            return new NotificationResponseBean(user);
        } else if (Utils.isAccountLocked(user)) {
            if (!Utils.isUserExistenceHidden()) {
                // Check user in PENDING_SR or PENDING_AP status.
                checkAccountPendingStatus(user);
                // If the NotifyUserAccountStatus is disabled, notify with an empty NotificationResponseBean.
                if (getNotifyUserAccountStatus()) {
                    String loginIdentifier = getLoginIdentifierFromProperties(properties);
                    loginIdentifier = StringUtils.isNotBlank(loginIdentifier) ? loginIdentifier : user.getUserName();
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT,
                            loginIdentifier);
                }
                triggerAccountStatusNotification(user, notificationChannel,
                        IdentityRecoveryConstants.ACCOUNT_STATUS_LOCKED, eventName, properties);
            }
            return new NotificationResponseBean(user);
        }
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        String secretKey;
        UserRecoveryData recoveryDataDO;
        // Loading the existing user recovery details with the code created timestamp.
        recoveryDataDO = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);
        /* Checking whether the existing confirmation code can be used based on the email confirmation code tolerance
           and the existing recovery details. */
        if (!Utils.reIssueExistingConfirmationCode(recoveryDataDO, notificationChannel)) {
            recoveryDataDO = generateNewConfirmationCode(user, notificationChannel);
        }
        secretKey = recoveryDataDO.getSecret();
        if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannel)) {
            String sendTo = Utils.getUserClaim(user, IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);
            if (StringUtils.isEmpty(sendTo)) {
                /* If the mobile number is not found for the user, notify with an empty
                NotificationResponseBean.*/
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MOBILE_NOT_FOUND, user.getUserName());
            }
            properties = addMobileNumberToProperties(properties, sendTo);
        }
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        if (isNotificationInternallyManage) {
            // Manage notifications by the identity server.
            String templateName = IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET;
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(notificationChannel) &&
                    Utils.isPasswordRecoveryEmailOtpEnabled(user.getTenantDomain())) {
                templateName = IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_EMAIL_OTP;
            }
            triggerNotification(user, notificationChannel, templateName, secretKey, eventName, properties,
                    recoveryDataDO);
        } else {
            // Set password recovery key since the notifications are managed by an external mechanism.
            notificationResponseBean.setKey(secretKey);
        }
        publishEvent(user, String.valueOf(notify), secretKey, null, properties,
                IdentityEventConstants.Event.POST_SEND_RECOVERY_NOTIFICATION, recoveryDataDO);
        return notificationResponseBean;
    }

    /**
     * Retrieves the login identifier from the provided list of properties.
     *
     * @param properties Array of properties sent in the recovery request.
     * @return The value of the property with key 'loginIdentifier', or null if not found or if the array is empty.
     */
    private String getLoginIdentifierFromProperties(Property[] properties) {

        if (ArrayUtils.isEmpty(properties)) {
            return null;
        }
        for (Property property : properties) {
            if (LOGIN_IDENTIFIER.equals(property.getKey())) {
                return property.getValue();
            }
        }
        return null;
    }

    /**
     * Whether to inform the user if the user's account is locked.
     *
     * @return True, if notify user account status is enabled in the identity.xml. Also, true will be returned if the
     * property is not configured.
     */
    private boolean getNotifyUserAccountStatus() {

        String notifyStatus =
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFY_USER_ACCOUNT_STATUS);
        if (StringUtils.isBlank(notifyStatus)) {
            /*
            This indicates config not in the identity.xml. In that case the we need to maintain backward compatibility.
             */
            return true;
        }
        return Boolean.parseBoolean(notifyStatus);
    }

    /**
     * Check whether the account is pending self signup or pending ask password.
     *
     * @param user User.
     * @throws IdentityRecoveryException If account is in locked or disabled status.
     */
    private void checkAccountPendingStatus(User user) throws IdentityRecoveryException {

        String accountState = Utils.getAccountState(user);
        if (StringUtils.isNotBlank(accountState)) {
            if (IdentityRecoveryConstants.PENDING_SELF_REGISTRATION.equals(accountState)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.
                        ErrorMessages.ERROR_CODE_PENDING_SELF_REGISTERED_ACCOUNT, user.getUserName());
            }
            if (IdentityRecoveryConstants.PENDING_ASK_PASSWORD.equals(accountState)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.
                        ErrorMessages.ERROR_CODE_PENDING_PASSWORD_RESET_ACCOUNT, user.getUserName());
            }
        }
    }

    /**
     * Generates the new confirmation code details for a corresponding user.
     *
     * @param user Details of the user that needs the confirmation code.
     * @param notificationChannel Method to send the recovery information. eg : EMAIL, SMS.
     * @return Created recovery data object.
     * @throws IdentityRecoveryException Error while generating the recovery information.
     */
    private UserRecoveryData generateNewConfirmationCode(User user, String notificationChannel)
            throws IdentityRecoveryException {

        String recoveryFlowId = null;
        String hashedSecretKey;
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);
        if (userRecoveryData != null) {
            recoveryFlowId = userRecoveryData.getRecoveryFlowId();
        }
        userRecoveryDataStore.invalidate(user);
        String secretKey = Utils.generateSecretKey(notificationChannel, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name(),
                user.getTenantDomain(), "Recovery.Notification.Password");
        if (!skipConcatForOTPBasedEmailRecovery(user.getTenantDomain())) {
            secretKey = Utils.concatRecoveryFlowIdWithSecretKey(recoveryFlowId, notificationChannel, secretKey);
        }
        try {
            hashedSecretKey = Utils.hashCode(secretKey);
        } catch (NoSuchAlgorithmException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO_FOR_CODE, null);
        }
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, recoveryFlowId, secretKey,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);
        UserRecoveryData hashedRecoveryDataDO = new UserRecoveryData(user, recoveryFlowId, hashedSecretKey,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);


        // Store the notified channel in the recovery object for future reference.
        recoveryDataDO.setRemainingSetIds(notificationChannel);
        hashedRecoveryDataDO.setRemainingSetIds(notificationChannel);
        userRecoveryDataStore.storeConfirmationCode(hashedRecoveryDataDO);
        return recoveryDataDO;
    }

    /**
     * Skip concatenation of recovery flow id with the secret key for OTP based email recovery.
     *
     * @param tenantDomain Tenant domain.
     * @return True if the concatenation should be skipped.
     */
    private static boolean skipConcatForOTPBasedEmailRecovery(String tenantDomain) {

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
     * Check for user existence.
     *
     * @param user User
     * @return True if the user exists
     * @throws IdentityRecoveryException Error while checking user existence
     */
    private boolean isExistingUser(User user) throws IdentityRecoveryException {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager;
            userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil
                    .addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                if (log.isDebugEnabled()) {
                    log.debug("No user found for recovery with username: " + user.toFullQualifiedUsername());
                }
                return false;
            }
            return true;
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }
    }

    private boolean isLocalCredentialAvailable(User user) throws IdentityRecoveryServerException {

        try {
            String[] requiredClaims = getClaimsToBeQueriedForUser(user);
            int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil
                    .addDomainToName(user.getUserName(), user.getUserStoreDomain());
            Map<String, String> claimValues =
                    userStoreManager.getUserClaimValues(domainQualifiedUsername, requiredClaims, null);
            if (MapUtils.isEmpty(claimValues)) {
                return true;
            }
            String managedOrgId = claimValues.get(IdentityRecoveryConstants.MANAGED_ORG_CLAIM_URI);
            String userSourceId = claimValues.get(IdentityRecoveryConstants.USER_SOURCE_ID_CLAIM_URI);
            String localCredentialExists = claimValues.get(
                    IdentityRecoveryConstants.LOCAL_CREDENTIAL_EXISTS_CLAIM_URI);
            if (StringUtils.isNotBlank(managedOrgId)) {
                // If the user is managed by a different organization, the user is considered as a shared user.
                return false;
            }
            if (StringUtils.isNotEmpty(userSourceId)) {
                if (localCredentialExists != null && !Boolean.parseBoolean(localCredentialExists)) {
                    return false;
                }
            }
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while checking user's local credential availability. Error message: "
                        + e.getMessage());
            }
        }
        return true;
    }

    private String[] getClaimsToBeQueriedForUser(User user) throws IdentityRecoveryServerException {

        try {
            boolean isOrganization = OrganizationManagementUtil.isOrganization(user.getTenantDomain());
            if (isOrganization) {
                return new String[]{
                        IdentityRecoveryConstants.USER_SOURCE_ID_CLAIM_URI,
                        IdentityRecoveryConstants.LOCAL_CREDENTIAL_EXISTS_CLAIM_URI,
                        IdentityRecoveryConstants.MANAGED_ORG_CLAIM_URI
                };
            }
            return new String[]{
                    IdentityRecoveryConstants.USER_SOURCE_ID_CLAIM_URI,
                    IdentityRecoveryConstants.LOCAL_CREDENTIAL_EXISTS_CLAIM_URI
            };
        } catch (OrganizationManagementException e) {
            throw new IdentityRecoveryServerException("Error occurred while resolving organization.", e);
        }
    }

    /**
     * Check whether notifications are internally managed.
     *
     * @param tenantDomain Tenant Domain
     * @param notify       Manage notifications internally
     * @return True of the notifications are internally managed
     * @throws IdentityRecoveryException Error while checking for configurations
     */
    private boolean isNotificationsInternallyManaged(String tenantDomain, Boolean notify)
            throws IdentityRecoveryException {

        if (notify == null) {
            boolean manageNotificationsInternally = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            tenantDomain));
            if (log.isDebugEnabled()) {
                log.debug("Notify parameter not in the request. ManageNotificationsInternally set to " +
                        "server default value: " + manageNotificationsInternally);
            }
            return manageNotificationsInternally;
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Notify parameter in the request. ManageNotificationsInternally set to : " + notify);
            }
            return notify;
        }
    }

    /**
     * Check whether the VERIFIED_USER_PROPERTY_KEY set to TRUE or FALSE in the property map. If the property is not
     * set, return FALSE.
     *
     * @param propertyMap Properties
     * @return TRUE if the user is already verified.
     */
    private boolean isUserVerified(HashMap<String, String> propertyMap) {

        if (MapUtils.isEmpty(propertyMap)) {
            if (log.isDebugEnabled()) {
                log.debug("Empty property map received in the recovery request");
            }
            return false;
        }
        String verified = propertyMap.get(IdentityRecoveryConstants.VERIFIED_USER_PROPERTY_KEY);
        if (StringUtils.isBlank(verified)) {
            if (log.isDebugEnabled()) {
                log.debug("Property : verifiedUser is not in the property map. Hence returning FALSE");
            }
            return false;
        }
        try {
            return Boolean.parseBoolean(verified);
        } catch (NumberFormatException e) {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("Value : %s given for property : %s is not a boolean. Therefore, "
                                        + "returning false for the property.", verified,
                                IdentityRecoveryConstants.VERIFIED_USER_PROPERTY_KEY);
                log.debug(message);
            }
        }
        return false;
    }

    /**
     * Get notification channel from a given map properties.
     *
     * @param propertyMap Properties
     * @return Notification channel
     */
    private String getNotificationChannelFromProperties(HashMap<String, String> propertyMap) {

        if (MapUtils.isEmpty(propertyMap)) {
            return NotificationChannels.EMAIL_CHANNEL.getChannelType();
        }
        String notificationChannel = propertyMap.get(IdentityRecoveryConstants.NOTIFICATION_CHANNEL_PROPERTY_KEY);
        return getServerSupportedNotificationChannel(notificationChannel);
    }

    /**
     * Get server supported notification channel.
     *
     * @param channel Notification channel
     * @return Server supported notification channel
     */
    public String getServerSupportedNotificationChannel(String channel) {

        if (StringUtils.isEmpty(channel)) {
            if (log.isDebugEnabled()) {
                String message =
                        "No notification channel in the request properties. Configuring the notification channel" +
                                " to: " + NotificationChannels.EMAIL_CHANNEL.getChannelType();
                log.debug(message);
            }
            return NotificationChannels.EMAIL_CHANNEL.getChannelType();
        }
        // Validate notification channels.
        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else if (NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(channel)) {
            return channel;
        } else {
            String defaultNotificationChannel = IdentityGovernanceUtil.getDefaultNotificationChannel();
            if (log.isDebugEnabled()) {
                String message = String.format("Not a server supported notification channel : %s. Therefore "
                                + "default notification channel : %s will be used.", channel,
                        defaultNotificationChannel);
                log.debug(message);
            }
            return defaultNotificationChannel;
        }
    }

    /**
     * Build a map of properties.
     *
     * @param properties Property list
     * @return Properties
     */
    private HashMap<String, String> buildPropertyMap(Property[] properties) {

        HashMap<String, String> propertyMap = new HashMap<>();
        if (ArrayUtils.isNotEmpty(properties)) {
            for (Property property : properties) {
                if (StringUtils.isNotEmpty(property.getKey())) {
                    propertyMap.put(property.getKey(), property.getValue());
                }
            }
        }
        return propertyMap;
    }

    /**
     * Validate the callback Url.
     *
     * @param properties   Properties
     * @param tenantDomain Tenant Domain
     * @throws IdentityRecoveryServerException Error validating the callback
     */
    private void validateCallback(Property[] properties, String tenantDomain) throws IdentityRecoveryServerException {

        String callbackURL = null;
        try {
            if (!Utils.isAccessUrlAvailable(properties)) {
                callbackURL = Utils.getCallbackURL(properties);
                if (StringUtils.isNotBlank(callbackURL) && !Utils.validateCallbackURL(callbackURL, tenantDomain,
                        IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CALLBACK_REGEX)) {
                    throw Utils.handleServerException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL);
                }
            }
        } catch (URISyntaxException | UnsupportedEncodingException | IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID,
                    callbackURL);
        }
    }

    /**
     * Verify whether password recovery is enabled for the tenant domain.
     *
     * @param tenantDomain String tenant domain
     * @throws IdentityRecoveryException Error while validating configurations
     */
    private void validatePasswordRecoveryConfiguration(String tenantDomain) throws IdentityRecoveryException {

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                        tenantDomain));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_BASED_PASSWORD_RECOVERY_NOT_ENABLE,
                    null);
        }
    }

    /**
     * Resolve tenant domain and user store domain of the user.
     *
     * @param user User
     */
    private void resolveUserAttributes(User user) {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            if (log.isDebugEnabled()) {
                log.debug("SendRecoveryNotification :Tenant domain is not in the request. set to default for " +
                        "user : " + user.getUserName());
            }
        }
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            if (log.isDebugEnabled()) {
                log.debug("SendRecoveryNotification : User store domain is not in the request. set to " +
                        "default for user : " + user.getUserName());
            }
        }
    }

    /**
     * Update the password of the user.
     *
     * @param code       Password Reset code.
     * @param password   New password.
     * @param properties Properties.
     * @throws IdentityRecoveryException Error while updating the password.
     * @throws IdentityEventException    Error while updating the password.
     */
    public void updatePassword(String code, String password, Property[] properties)
            throws IdentityRecoveryException, IdentityEventException {
        updateUserPassword(code, password, properties);
    }

    /**
     * Update the password of the user.
     *
     * @param code              Password Reset code.
     * @param confirmationCode  Confirmation code.
     * @param password          New password.
     * @param properties        Properties.
     * @throws IdentityRecoveryException Error while updating the password.
     * @throws IdentityEventException    Error while updating the password.
     */
    public void updatePassword(String code, String confirmationCode, String password, Property[] properties)
            throws IdentityRecoveryException, IdentityEventException {

        updateUserPassword(code, confirmationCode, password, properties);
    }

    /**
     * Update the password of the user.
     *
     * @param code       Password Reset code.
     * @param password   New password.
     * @param properties Properties.
     * @throws IdentityRecoveryException Error while updating the password.
     * @throws IdentityEventException    Error while updating the password.
     */
    public User updateUserPassword(String code, String password, Property[] properties)
            throws IdentityRecoveryException, IdentityEventException {

        UserRecoveryData userRecoveryData;
        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            try {
                String hashedCode = Utils.hashCode(code);
                userRecoveryData = userRecoveryDataStore.load(hashedCode);
            } catch (NoSuchAlgorithmException e) {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO_FOR_CODE, null);
            } catch (IdentityRecoveryException e) {
                userRecoveryData = userRecoveryDataStore.load(code);
            }

            enterFlow(userRecoveryData);

            validateCallback(properties, userRecoveryData.getUser().getTenantDomain());
            publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                    IdentityEventConstants.Event.PRE_ADD_NEW_PASSWORD, userRecoveryData);
            validateTenantDomain(userRecoveryData.getUser());
            String recoveryFlowId = userRecoveryDataStore.loadWithoutCodeExpiryValidation(userRecoveryData.getUser())
                    .getRecoveryFlowId();

            // Validate recovery step.
            if (!RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep()) &&
                    !RecoverySteps.SET_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);
            }
            // Get the notification channel.
            String notificationChannel = getServerSupportedNotificationChannel(userRecoveryData.getRemainingSetIds());
            boolean notificationsInternallyManaged = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            userRecoveryData.getUser().getTenantDomain()));
            boolean isNotificationSendWhenSuccess = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                    userRecoveryData.getUser().getTenantDomain()));
            boolean isNotificationSendOnAccountActivation = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_ACCOUNT_ACTIVATION,
                    userRecoveryData.getUser().getTenantDomain()));
            String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(),
                    userRecoveryData.getUser().getUserStoreDomain());

            // Update the password.
            updateNewPassword(userRecoveryData.getUser(), password, domainQualifiedName, userRecoveryData,
                    notificationsInternallyManaged);
            if (recoveryFlowId != null) {
                userRecoveryDataStore.invalidateWithRecoveryFlowId(recoveryFlowId);
            } else {
                userRecoveryDataStore.invalidate(userRecoveryData.getUser());
            }
            if (notificationsInternallyManaged &&
                    !NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
                String template = null;
            if (isAskPasswordFlow(userRecoveryData)) {
                if (isNotificationSendOnAccountActivation) {
                    if (RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(userRecoveryData.getRecoveryScenario())) {
                        template = IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP_SET_SUCCESS;
                        notificationChannel = NotificationChannels.SMS_CHANNEL.getChannelType();
                        String sendTo = Utils.getUserClaim(userRecoveryData.getUser(),
                                IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);
                        if (StringUtils.isEmpty(sendTo)) {
                            /* If the mobile number is not found for the user, notify with an empty
                            NotificationResponseBean.*/
                            throw Utils.handleClientException(
                                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MOBILE_NOT_FOUND,
                                    userRecoveryData.getUser().getUserName());
                        }
                        properties = addMobileNumberToProperties(properties, sendTo);
                    } else {
                        template = IdentityRecoveryConstants.ACCOUNT_ACTIVATION_SUCCESS;
                    }
                }
            } else if (isNotificationSendWhenSuccess) {
                    template = IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS;
                }
                try {
                    String eventName = Utils.resolveEventName(notificationChannel);
                    if (StringUtils.isNotBlank(template)) {
                        triggerNotification(userRecoveryData.getUser(), notificationChannel, template,
                                StringUtils.EMPTY, eventName, properties, userRecoveryData);
                    }
                } catch (IdentityRecoveryException e) {
                    String errorMsg =
                            String.format("Error while sending password reset success notification to user : %s",
                                    userRecoveryData.getUser().getUserName());
                    log.error(errorMsg);
                    String recoveryScenario = userRecoveryData.getRecoveryScenario().name();
                    String recoveryStep = userRecoveryData.getRecoveryStep().name();
                    auditPasswordReset(userRecoveryData.getUser(), AuditConstants.ACTION_PASSWORD_RESET, errorMsg,
                            FrameworkConstants.AUDIT_SUCCESS, recoveryScenario, recoveryStep);
                }
            }
            publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                    IdentityEventConstants.Event.POST_ADD_NEW_PASSWORD, userRecoveryData);
            if (log.isDebugEnabled()) {
                String msg = "Password is updated for  user: " + domainQualifiedName;
                log.debug(msg);
            }
            String recoveryScenario = userRecoveryData.getRecoveryScenario().name();
            String recoveryStep = userRecoveryData.getRecoveryStep().name();
            auditPasswordReset(userRecoveryData.getUser(), AuditConstants.ACTION_PASSWORD_RESET, null,
                    FrameworkConstants.AUDIT_SUCCESS, recoveryScenario, recoveryStep);

        } finally {
            IdentityContext.getThreadLocalIdentityContext().exitFlow();
        }
        return userRecoveryData.getUser();
    }

    /**
     * Update the password of the user.
     *
     * @param code              Password Reset code.
     * @param confirmationCode  Confirmation code.
     * @param password          New password.
     * @param properties        Properties.
     * @throws IdentityRecoveryException Error while updating the password.
     * @throws IdentityEventException    Error while updating the password.
     */
    public User updateUserPassword(String code, String confirmationCode, String password, Property[] properties)
            throws IdentityRecoveryException, IdentityEventException {

        UserRecoveryData userRecoveryData;
        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            try {
                userRecoveryData = userRecoveryDataStore.loadFromRecoveryFlowId(confirmationCode,
                        RecoverySteps.UPDATE_PASSWORD);

                enterFlow(userRecoveryData);

                validateCallback(properties, userRecoveryData.getUser().getTenantDomain());
                publishEvent(userRecoveryData.getUser(), null, null, password, properties,
                        IdentityEventConstants.Event.PRE_ADD_NEW_PASSWORD, userRecoveryData);
                validateTenantDomain(userRecoveryData.getUser());
                int failedAttempts = userRecoveryData.getFailedAttempts();

                String hashedCode;
                try {
                    hashedCode = Utils.hashCode(code);
                } catch (NoSuchAlgorithmException e) {
                    throw Utils.handleServerException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO_FOR_CODE, null);
                }
                if (!(StringUtils.equals(hashedCode, userRecoveryData.getSecret()) || StringUtils.equals(code,
                        userRecoveryData.getSecret()))) {
                    if ((failedAttempts + 1) >= Integer.parseInt(Utils.getRecoveryConfigs(IdentityRecoveryConstants.
                                    ConnectorConfig.RECOVERY_NOTIFICATION_PASSWORD_MAX_FAILED_ATTEMPTS,
                            userRecoveryData.getUser().
                                    getTenantDomain()))) {
                        userRecoveryDataStore.invalidateWithRecoveryFlowId(confirmationCode);
                        throw Utils.handleClientException(
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_FLOW_ID.getCode(),
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_FLOW_ID.getMessage(),
                                confirmationCode);
                    }
                    userRecoveryDataStore.updateFailedAttempts(confirmationCode, failedAttempts + 1);
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getCode(),
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getMessage(), code);
                }
            } catch (IdentityRecoveryException e) {
            /* This is a fallback logic to support already initiated email link based recovery flows and EXTERNAL
            channel based recovery flows using the recovery V1 API, which do not have recovery flow ids. */
                userRecoveryData = validateUserRecoveryDataFromCode(code, confirmationCode, password, properties);
            }

            // Get the notification channel.
            String notificationChannel = getServerSupportedNotificationChannel(userRecoveryData.getRemainingSetIds());
            boolean notificationsInternallyManaged = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                            userRecoveryData.getUser().getTenantDomain()));
            boolean isNotificationSendWhenSuccess = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS,
                    userRecoveryData.getUser().getTenantDomain()));
            boolean isNotificationSendOnAccountActivation = Boolean.parseBoolean(Utils.getRecoveryConfigs(
                    IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_ACCOUNT_ACTIVATION,
                    userRecoveryData.getUser().getTenantDomain()));
            String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(),
                    userRecoveryData.getUser().getUserStoreDomain());

            // Update the password.
            updateNewPassword(userRecoveryData.getUser(), password, domainQualifiedName, userRecoveryData,
                    notificationsInternallyManaged);
            if (userRecoveryData.getRecoveryFlowId() != null) {
                userRecoveryDataStore.invalidateWithRecoveryFlowId(userRecoveryData.getRecoveryFlowId());
            } else {
                userRecoveryDataStore.invalidate(userRecoveryData.getUser());
            }
            if (notificationsInternallyManaged &&
                    !NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(notificationChannel)) {
                String template = null;
            if (isAskPasswordFlow(userRecoveryData)) {
                if (isNotificationSendOnAccountActivation) {
                    if (RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(userRecoveryData.getRecoveryScenario())) {
                        template = IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD_SMS_OTP_SET_SUCCESS;
                        notificationChannel = NotificationChannels.SMS_CHANNEL.getChannelType();
                        String sendTo = Utils.getUserClaim(userRecoveryData.getUser(),
                                IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);
                        if (StringUtils.isEmpty(sendTo)) {
                            /* If the mobile number is not found for the user, notify with an empty
                            NotificationResponseBean.*/
                            throw Utils.handleClientException(
                                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MOBILE_NOT_FOUND,
                                    userRecoveryData.getUser().getUserName());
                        }
                        properties = addMobileNumberToProperties(properties, sendTo);
                    } else {
                        template = IdentityRecoveryConstants.ACCOUNT_ACTIVATION_SUCCESS;
                    }
                }
            } else if (isNotificationSendWhenSuccess) {
                    template = IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS;
                }
                try {
                    String eventName = Utils.resolveEventName(notificationChannel);
                    if (StringUtils.isNotBlank(template)) {
                        triggerNotification(userRecoveryData.getUser(), notificationChannel, template,
                                StringUtils.EMPTY, eventName, properties, userRecoveryData);
                    }
                } catch (IdentityRecoveryException e) {
                    String errorMsg =
                            String.format("Error while sending password reset success notification to user : %s",
                                    userRecoveryData.getUser().getUserName());
                    log.error(errorMsg);
                    String recoveryScenario = userRecoveryData.getRecoveryScenario().name();
                    String recoveryStep = userRecoveryData.getRecoveryStep().name();
                    auditPasswordReset(userRecoveryData.getUser(), AuditConstants.ACTION_PASSWORD_RESET, errorMsg,
                            FrameworkConstants.AUDIT_SUCCESS, recoveryScenario, recoveryStep);
                }
            }
            publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                    IdentityEventConstants.Event.POST_ADD_NEW_PASSWORD, userRecoveryData);
            if (log.isDebugEnabled()) {
                String msg = "Password is updated for  user: " + domainQualifiedName;
                log.debug(msg);
            }
            String recoveryScenario = userRecoveryData.getRecoveryScenario().name();
            String recoveryStep = userRecoveryData.getRecoveryStep().name();
            auditPasswordReset(userRecoveryData.getUser(), AuditConstants.ACTION_PASSWORD_RESET, null,
                    FrameworkConstants.AUDIT_SUCCESS, recoveryScenario, recoveryStep);

        } finally {
            IdentityContext.getThreadLocalIdentityContext().exitFlow();
        }
        return userRecoveryData.getUser();
    }

    /**
     * This method is to validate user recovery data using the reset code when there's no recovery flow id.
     * This is added as a fallback logic to handle the already initiated email link based recovery flows and EXTERNAL
     * channel based recovery flows which do not have recovery flow ids, which were initiated before moving to the
     * Recovery V2 API. This shouldn't be used for any other purpose and should be kept for sometime.
     *
     * @param code                       Password Reset code.
     * @param confirmationCode           Confirmation code.
     * @param password                   New password.
     * @param properties                 Properties.
     * @return UserRecoveryData.
     * @throws IdentityRecoveryException Error while updating the password.
     */
    @Deprecated
    private UserRecoveryData validateUserRecoveryDataFromCode(String code, String confirmationCode, String password,
                                                              Property[] properties) throws IdentityRecoveryException {

        UserRecoveryData userRecoveryData;
        UserAccountRecoveryManager userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        try {
            String hashedCode = Utils.hashCode(code);
            userRecoveryData = userAccountRecoveryManager.getUserRecoveryData(hashedCode,
                    RecoverySteps.UPDATE_PASSWORD);
        } catch (NoSuchAlgorithmException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_HASHING_ALGO_FOR_CODE, null);
        } catch (IdentityRecoveryException e) {
            try {
                userRecoveryData = userAccountRecoveryManager.getUserRecoveryData(code,
                        RecoverySteps.UPDATE_PASSWORD);
            } catch (IdentityRecoveryException ex) {
                if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_CODE.getCode().equals(
                        ex.getErrorCode())) {
                    ex.setErrorCode(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_FLOW_ID.getCode());
                }
                throw ex;
            }
        }
        if (!(NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds()) ||
                NotificationChannels.EXTERNAL_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds()))) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_FLOW_ID, confirmationCode);
        }
        validateCallback(properties, userRecoveryData.getUser().getTenantDomain());
        publishEvent(userRecoveryData.getUser(), null, code, password, properties,
                IdentityEventConstants.Event.PRE_ADD_NEW_PASSWORD, userRecoveryData);
        validateTenantDomain(userRecoveryData.getUser());

        // Validate recovery step.
        if (!RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, code);
        }
        return userRecoveryData;
    }

    /**
     * Update the new password of the user.
     *
     * @param user                            User
     * @param password                        New password
     * @param domainQualifiedName             Domain qualified name
     * @param userRecoveryData                User recovery data
     * @param isNotificationInternallyManaged Whether the notifications are internally managed.
     * @throws IdentityRecoveryException Error while checking for account state claim
     * @throws IdentityRecoveryException Error while updating the password
     */
    private void updateNewPassword(User user, String password, String domainQualifiedName,
                                   UserRecoveryData userRecoveryData,
                                   boolean isNotificationInternallyManaged)
            throws IdentityEventException, IdentityRecoveryException {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(userRecoveryData.getUser().getTenantDomain());
            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(domainQualifiedName, password);

            // Get the claims that related to a password reset.
            HashMap<String, String> userClaims = getAccountStateClaims(userRecoveryData,
                    isNotificationInternallyManaged);
            if (MapUtils.isNotEmpty(userClaims)) {
                // Update the retrieved claims set.
                userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
            }
        } catch (UserStoreException e) {
            checkPasswordValidity(e, user);
            if (log.isDebugEnabled()) {
                log.debug("NotificationPasswordRecoveryManager: Unexpected Error occurred while updating password "
                        + "for the user: " + domainQualifiedName, e);
            }
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }
    }

    /**
     * Get the claims that needs to be updated when a user attempts a password reset.
     *
     * @param userRecoveryData                User recovery data
     * @param isNotificationInternallyManaged Whether the notifications are internally managed
     * @return Claims that are related to account state
     * @throws IdentityEventException Error while checking for account state claim
     */
    private HashMap<String, String> getAccountStateClaims(UserRecoveryData userRecoveryData,
                                                          boolean isNotificationInternallyManaged)
            throws IdentityEventException {

        HashMap<String, String> userClaims = new HashMap<>();
        Enum<RecoveryScenarios> recoveryScenario = userRecoveryData.getRecoveryScenario();
        // If notifications are internally managed we try to set the verified claims since this is an opportunity
        // to verify a user channel.
        if (isNotificationInternallyManaged && !isNotificationLessRecoveryMethod(recoveryScenario)) {
            if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds())) {
                userClaims.put(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(userRecoveryData.getRemainingSetIds())) {
                userClaims.put(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            } else {
                if (log.isDebugEnabled()) {
                    String error = String
                            .format("No notification channels for the user : %s in tenant domain : " + "%s",
                                    userRecoveryData.getUser().getUserStoreDomain() + userRecoveryData.getUser()
                                            .getUserName(), userRecoveryData.getUser().getTenantDomain());
                    log.debug(error);
                }
                userClaims.put(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(), Boolean.TRUE.toString());
            }
        }
        // We don't need to change any states during user initiated password recovery.
        if (RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.equals(recoveryScenario)
                || RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY.equals(recoveryScenario)
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(recoveryScenario)
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(recoveryScenario)
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.equals(recoveryScenario)
                || RecoveryScenarios.ASK_PASSWORD.equals(recoveryScenario)
                || RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.equals(recoveryScenario)
                || RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(recoveryScenario)) {
            IdentityUtil.threadLocalProperties.get().put(AccountConstants.ADMIN_INITIATED, false);
        }

        if (Utils.isAccountStateClaimExisting(userRecoveryData.getUser().getTenantDomain())) {
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI,
                    IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED);
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM, StringUtils.EMPTY);
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
        }

        // If the scenario is initiated by the admin, set the account locked claim to FALSE.
        if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(recoveryScenario)
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(recoveryScenario)
                || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP.equals(recoveryScenario)) {
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
            userClaims.remove(IdentityRecoveryConstants.ACCOUNT_LOCKED_REASON_CLAIM);
        }
        return userClaims;
    }

    /**
     * Check whether the recovery scenario is notification based.
     * A set of recovery scenarios such as question based password recovery, and
     * password reset on password expiry does not require sending notifications to the user.
     *
     * @param recoveryScenario Recovery scenario
     * @return True if the recovery scenario does not require sending notifications
     */
    private boolean isNotificationLessRecoveryMethod(Enum recoveryScenario) {

        return RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY.equals(recoveryScenario) ||
                RecoveryScenarios.PASSWORD_EXPIRY.equals(recoveryScenario);
    }

    /**
     * Validate Tenant domain of the user with the domain in the context.
     *
     * @param user User
     * @throws IdentityRecoveryClientException Tenant domain miss match
     */
    private void validateTenantDomain(User user) throws IdentityRecoveryClientException {

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = user.getTenantDomain();
        if (!StringUtils.equals(contextTenantDomain, userTenantDomain)) {
            throw new IdentityRecoveryClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_TENANT_DOMAIN_MISS_MATCH_WITH_CONTEXT
                            .getCode(), "invalid tenant domain: " + userTenantDomain);
        }
    }

    private void checkPasswordValidity(UserStoreException e, User user) throws IdentityRecoveryClientException {

        Throwable cause = e.getCause();
        while (cause != null) {
            if (cause instanceof IdentityEventException) {
                String errorCode = ((IdentityEventException) cause).getErrorCode();
                if (StringUtils.equals(errorCode, "22001")) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_HISTORY_VIOLATE, null, e);
                }
            }

            if (cause instanceof PolicyViolationException) {
                throw IdentityException.error(IdentityRecoveryClientException.class,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
                        cause.getMessage(), e);
            }

            if (cause instanceof UserActionExecutionClientException &&
                    ((UserActionExecutionClientException) cause).getErrorCode()
                            .equals(UserActionError.PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED)) {

                throw Utils.handleClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_FAILURE.getCode(),
                        ((UserActionExecutionClientException) cause).getError(),
                        ((UserActionExecutionClientException) cause).getDescription(), cause);
            }
            cause = cause.getCause();
        }
        Utils.checkPasswordPatternViolation(e, user);
    }

    /**
     * Trigger notification to send userName recovery information.
     *
     * @param user                User
     * @param notificationChannel Notification channel
     * @param templateName        Notification Template name
     * @param code                Secret key
     * @param eventName           Event name
     * @param metaProperties      Meta properties to be send with the notification.
     * @throws IdentityRecoveryException Error while triggering notification.
     */
    private void triggerNotification(User user, String notificationChannel, String templateName, String code,
                                     String eventName, Property[] metaProperties, UserRecoveryData userRecoveryData)
            throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);
        if (userRecoveryData != null) {
            properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO,
                    userRecoveryData.getRecoveryScenario().name());
        }
        if (StringUtils.isNotBlank(code)) {
            if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(notificationChannel)) {
                properties.put(IdentityRecoveryConstants.OTP_TOKEN_STRING, code);
            } else {
                properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
            }
        }
        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, templateName);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
            auditPasswordRecovery(AuditConstants.ACTION_PASSWORD_RECOVERY, notificationChannel, user, null,
                    FrameworkConstants.AUDIT_SUCCESS, userRecoveryData, templateName);
        } catch (IdentityEventException e) {
            auditPasswordRecovery(AuditConstants.ACTION_PASSWORD_RECOVERY, notificationChannel, user, e.getMessage(),
                    FrameworkConstants.AUDIT_FAILED, userRecoveryData, templateName);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }

    }

    /**
     * Trigger notification to send account status information.
     *
     * @param user                User
     * @param notificationChannel Notification channel
     * @param status              Account status
     * @param eventName           Event name
     * @param metaProperties      Meta properties to be sent with the notification.
     * @throws IdentityRecoveryException Error while triggering notification.
     */
    private void triggerAccountStatusNotification(User user, String notificationChannel,
                                                  String status, String eventName, Property[] metaProperties)
            throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, notificationChannel);
        if (StringUtils.isNotBlank(status)) {
            properties.put(IdentityRecoveryConstants.ERROR_KEY,
                    Base64.getUrlEncoder().encodeToString(status.getBytes(StandardCharsets.UTF_8)));
        }
        // This property is used to ignore throwing an error if the template is not found. This allows to preserve the
        // backward compatibility for the tenants without the specific email template.
        properties.put(IdentityRecoveryConstants.IGNORE_IF_TEMPLATE_NOT_FOUND, true);

        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE,
                IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_STATUS_NOTIFY);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
            auditAccountStatusNotify(notificationChannel, user, null,
                    FrameworkConstants.AUDIT_SUCCESS);
        } catch (IdentityEventException e) {
            auditAccountStatusNotify(notificationChannel, user,
                    e.getMessage(), FrameworkConstants.AUDIT_FAILED);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }

    private void publishEvent(User user, String notify, String code, String password, Property[] metaProperties,
                              String eventName, UserRecoveryData userRecoveryData) throws
            IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER, user);
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        if (userRecoveryData != null) {
            properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO,
                    userRecoveryData.getRecoveryScenario().name());
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
            properties.put(IdentityRecoveryConstants.OTP_TOKEN_STRING, code);
        }

        if (StringUtils.isNotBlank(notify)) {
            properties.put(IdentityRecoveryConstants.NOTIFY, notify);
        }

        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventClientException e) {
            throw Utils.handleClientException(e.getErrorCode(), e.getMessage(), null);
        } catch (IdentityEventException e) {
            log.error("Error occurred while publishing event " + eventName + " for user " + user);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PUBLISH_EVENT,
                    eventName, e);
        }

    }

    /**
     * Method to validate confirmation code of password reset flow.
     *
     * @param code         confirmation code
     * @param recoveryStep recovery step
     * @throws IdentityRecoveryException
     */
    public void validateConfirmationCode(String code, String recoveryStep) throws IdentityRecoveryException {

        getValidatedUser(code, recoveryStep);
    }

    /**
     * Method to validate confirmation code of password reset flow.
     *
     * @param code         confirmation code
     * @param recoveryStep recovery step
     * @throws IdentityRecoveryException
     */
    public User getValidatedUser(String code, String recoveryStep) throws IdentityRecoveryException {

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
        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = userRecoveryData.getUser().getTenantDomain();
        if (!StringUtils.equals(contextTenantDomain, userTenantDomain)) {
            throw new IdentityRecoveryClientException("Invalid tenant domain: " + userTenantDomain);
        }
        if (StringUtils.isNotBlank(recoveryStep) && !recoveryStep.equals(userRecoveryData.getRecoveryStep().name())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }
        User user = userRecoveryData.getUser();
        String domainQualifiedName = IdentityUtil
                .addDomainToName(user.getUserName(), userRecoveryData.getUser().getUserStoreDomain());
        if (log.isDebugEnabled()) {
            log.debug("Valid confirmation code for user: " + domainQualifiedName);
        }
        return user;
    }

    private void auditPasswordRecovery(String action, String notificationChannel, User user, String errorMsg,
                                       String result, UserRecoveryData userRecoveryData,
                                       String notificationTemplateType) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.NOTIFICATION_CHANNEL, notificationChannel);
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        dataObject.put(AuditConstants.USER_STORE_DOMAIN, user.getUserStoreDomain());
        if (userRecoveryData != null) {
            dataObject.put(AuditConstants.RECOVERY_SCENARIO, userRecoveryData.getRecoveryScenario().name());
            dataObject.put(AuditConstants.RECOVERY_STEP, userRecoveryData.getRecoveryStep().name());
        }
        dataObject.put(AuditConstants.TENANT_DOMAIN, user.getTenantDomain());
        dataObject.put(AuditConstants.NOTIFICATION_TEMPLATE_TYPE, notificationTemplateType);

        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(action, user.getUserName(), dataObject, result);
    }

    private void auditAccountStatusNotify(String notificationChannel, User user, String errorMsg, String result) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.NOTIFICATION_CHANNEL, notificationChannel);
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        dataObject.put(AuditConstants.USER_STORE_DOMAIN, user.getUserStoreDomain());
        dataObject.put(AuditConstants.TENANT_DOMAIN, user.getTenantDomain());
        dataObject.put(AuditConstants.NOTIFICATION_TEMPLATE_TYPE, IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_STATUS_NOTIFY);

        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(AuditConstants.ACTION_ACCOUNT_STATUS_NOTIFY, user.getUserName(), dataObject, result);
    }

    private void auditPasswordReset(User user, String action, String errorMsg, String result, String recoveryScenario,
                                    String recoveryStep) {

        JSONObject dataObject = new JSONObject();
        dataObject.put(AuditConstants.REMOTE_ADDRESS_KEY, MDC.get(AuditConstants.REMOTE_ADDRESS_QUERY_KEY));
        dataObject.put(AuditConstants.USER_AGENT_KEY, MDC.get(AuditConstants.USER_AGENT_QUERY_KEY));
        dataObject.put(AuditConstants.SERVICE_PROVIDER_KEY, MDC.get(AuditConstants.SERVICE_PROVIDER_QUERY_KEY));
        dataObject.put(AuditConstants.USER_STORE_DOMAIN, user.getUserStoreDomain());
        dataObject.put(AuditConstants.RECOVERY_SCENARIO, recoveryScenario);
        dataObject.put(AuditConstants.RECOVERY_STEP, recoveryStep);
        dataObject.put(AuditConstants.TENANT_DOMAIN, user.getTenantDomain());

        if (AUDIT_FAILED.equals(result)) {
            dataObject.put(AuditConstants.ERROR_MESSAGE_KEY, errorMsg);
        }
        Utils.createAuditMessage(action, user.getUserName(), dataObject, result);
    }

    private void validateUserStoreDomain(User user) throws IdentityRecoveryClientException,
            IdentityRecoveryServerException {

        int tenantID = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            String[] userList = getUserList(tenantID, user.getUserName());
            if (ArrayUtils.isEmpty(userList)) {
                String msg = "Unable to find an user with username: " + user.getUserName() + " in the system.";
                if (log.isDebugEnabled()) {
                    log.debug(msg);
                }
            } else if (userList.length == 1) {
                user.setUserStoreDomain(IdentityUtil.extractDomainFromName(userList[0]));
            } else {
                String msg = "There are multiple users with username: " + user.getUserName() + " in the system, " +
                        "please send the correct user-store domain along with the username.";
                throw new IdentityRecoveryClientException(msg);
            }
        }
    }

    private static String[] getUserList(int tenantId, String username) throws IdentityRecoveryServerException {

        org.wso2.carbon.user.core.UserStoreManager userStoreManager = null;
        String[] userList = null;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();

        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (org.wso2.carbon.user.core.UserStoreManager) realmService.getTenantUserRealm
                        (tenantId).getUserStoreManager();
                userList = userStoreManager.listUsers(username , 2) ;
            }
        } catch (UserStoreException e) {
            String msg = "Error retrieving the user-list for the tenant : " + tenantId;
            throw new IdentityRecoveryServerException(msg, e);
        }
        return userList;
    }

    private boolean isAskPasswordFlow(UserRecoveryData userRecoveryData) {

        return RecoveryScenarios.ASK_PASSWORD.equals(userRecoveryData.getRecoveryScenario())
                || RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP.equals(userRecoveryData.getRecoveryScenario())
                || RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP.equals(userRecoveryData.getRecoveryScenario());
    }

    public boolean isAskPasswordEmailTemplateTypeExists(String tenantDomain) {

        String path = IdentityRecoveryConstants.EMAIL_TEMPLATE_PATH + PATH_SEPARATOR +
                IdentityRecoveryConstants.ACCOUNT_ACTIVATION_SUCCESS.toLowerCase();

        RegistryResourceMgtService resourceMgtService =
                IdentityRecoveryServiceDataHolder.getInstance().getResourceMgtService();
        Resource templateType = resourceMgtService.getIdentityResource(path, tenantDomain);
        return templateType != null;
    }

    private Property[] addMobileNumberToProperties(Property[] properties, String mobile) {

        if (ArrayUtils.isEmpty(properties)) {
            properties = new Property[0];
        }
        Property[] newProperties = Arrays.copyOf(properties, properties.length + 1);
        newProperties[properties.length] = new Property(IdentityRecoveryConstants.SEND_TO, mobile);
        return newProperties;
    }

    /**
     * Updates the identity context for the current thread based on the provided user recovery data.\
     *
     * @param userRecoveryData User and recovery scenario information.
     */
    private void enterFlow(UserRecoveryData userRecoveryData) {

        RecoveryScenarios recoveryScenario = (RecoveryScenarios) userRecoveryData.getRecoveryScenario();
        Flow flow;
        switch (recoveryScenario) {
            case NOTIFICATION_BASED_PW_RECOVERY:
                flow = new Flow.CredentialFlowBuilder()
                        .name(Flow.Name.CREDENTIAL_RESET)
                        .initiatingPersona(Flow.InitiatingPersona.USER)
                        .credentialType(Flow.CredentialType.PASSWORD)
                        .build();
                IdentityContext.getThreadLocalIdentityContext().enterFlow(flow);
                break;
            case ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK:
            case ADMIN_FORCED_PASSWORD_RESET_VIA_OTP:
            case ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP:
                flow = new Flow.CredentialFlowBuilder()
                        .name(Flow.Name.CREDENTIAL_RESET)
                        .initiatingPersona(Flow.InitiatingPersona.ADMIN)
                        .credentialType(Flow.CredentialType.PASSWORD)
                        .build();
                IdentityContext.getThreadLocalIdentityContext().enterFlow(flow);
                break;
            case ASK_PASSWORD:
            case ASK_PASSWORD_VIA_EMAIL_OTP:
            case ASK_PASSWORD_VIA_SMS_OTP:
            case ADMIN_INVITE_SET_PASSWORD_OFFLINE:
                flow = new Flow.Builder()
                        .name(Flow.Name.INVITE)
                        .initiatingPersona(Flow.InitiatingPersona.ADMIN)
                        .build();
                IdentityContext.getThreadLocalIdentityContext().enterFlow(flow);
                break;
            default:
                break;
        }
    }
}
