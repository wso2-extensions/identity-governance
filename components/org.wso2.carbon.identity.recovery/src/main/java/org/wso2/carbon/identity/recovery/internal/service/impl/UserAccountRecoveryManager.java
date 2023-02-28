/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.internal.service.impl;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.dto.NotificationChannelDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryChannelInfoDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.NotificationChannel;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import org.wso2.carbon.identity.user.functionality.mgt.UserFunctionalityManager;
import org.wso2.carbon.identity.user.functionality.mgt.exception.UserFunctionalityManagementException;
import org.wso2.carbon.identity.user.functionality.mgt.model.FunctionalityLockStatus;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.model.ExpressionCondition;
import org.wso2.carbon.user.core.model.ExpressionOperation;
import org.wso2.carbon.user.core.model.OperationalCondition;
import org.wso2.carbon.user.core.model.OperationalOperation;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.recovery.RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY;
import static org.wso2.carbon.identity.recovery.RecoveryScenarios.QUESTION_BASED_PWD_RECOVERY;
import static org.wso2.carbon.identity.recovery.RecoveryScenarios.USERNAME_RECOVERY;

/**
 * Manager class which can be used to recover user account with available verified communication channels for a user.
 */
public class UserAccountRecoveryManager {

    private static final Log log = LogFactory.getLog(UserAccountRecoveryManager.class);
    private static UserAccountRecoveryManager instance = new UserAccountRecoveryManager();
    private static final String FORWARD_SLASH = "/";
    private static final NotificationChannels[] notificationChannels = {
            NotificationChannels.EMAIL_CHANNEL, NotificationChannels.SMS_CHANNEL};
    private static final boolean PER_USER_FUNCTIONALITY_LOCKING_ENABLED = Utils.isPerUserFunctionalityLockingEnabled();
    private static final String FUNCTIONALITY_PREFIX = "FUNCTIONALITY_";

    /**
     * Constructor.
     */
    private UserAccountRecoveryManager() {

    }

    /**
     * Get an instance of UserAccountRecoveryManager.
     *
     * @return UserAccountRecoveryManager instance.
     */
    public static UserAccountRecoveryManager getInstance() {

        return instance;
    }

    /**
     * Initiate the recovery flow for the user with matching claims.
     *
     * @param claims           User claims
     * @param tenantDomain     Tenant domain
     * @param recoveryScenario Recovery scenario
     * @param properties       Meta properties
     * @return RecoveryChannelInfoDTO object.
     */
    public RecoveryChannelInfoDTO retrieveUserRecoveryInformation(Map<String, String> claims, String tenantDomain,
                                                                  RecoveryScenarios recoveryScenario,
                                                                  Map<String, String> properties)
            throws IdentityRecoveryException {

        // Retrieve the user who matches the given set of claims.
        String username = getUsernameByClaims(claims, tenantDomain);
        if (StringUtils.isNotEmpty(username)) {
            User user = Utils.buildUser(username, tenantDomain);
            // If the account is locked or disabled, do not let the user to recover the account.
            checkAccountLockedStatus(user);
            List<NotificationChannel> notificationChannels;
            // Get the notification management mechanism.
            boolean isNotificationsInternallyManaged = Utils.isNotificationsInternallyManaged(tenantDomain, properties);

            /* If the notification is internally managed, then notification channels available for the user needs to
            be retrieved. If external notifications are enabled, external channel list should be returned.*/
            if (isNotificationsInternallyManaged) {
                notificationChannels = getInternalNotificationChannelList(username, tenantDomain,
                        recoveryScenario);
            } else {
                notificationChannels = getExternalNotificationChannelList();
            }
            // Validate whether the user account is eligible for account recovery.
            checkUserValidityForAccountRecovery(user, recoveryScenario, notificationChannels, properties);
            // This flow will be initiated only if the user has any verified channels.
            NotificationChannelDTO[] notificationChannelDTOS = getNotificationChannelsResponseDTOList(
                    tenantDomain, notificationChannels);
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            // Get the existing RESEND_CONFIRMATION_CODE details if there is any.
            UserRecoveryData recoveryDataDO = userRecoveryDataStore.loadWithoutCodeExpiryValidation(
                    user, recoveryScenario, RecoverySteps.RESEND_CONFIRMATION_CODE);
            String recoveryCode = UUIDGenerator.generateUUID();
            String notificationChannelList = getNotificationChannelListForRecovery(notificationChannels);
            /* Check whether the existing confirmation code can be used based on the email confirmation code tolerance
               with the extracted RESEND_CONFIRMATION_CODE details. */
            if (Utils.reIssueExistingConfirmationCode(recoveryDataDO,
                    NotificationChannels.EMAIL_CHANNEL.getChannelType())) {
                /* Update the existing RESEND_CONFIRMATION_CODE details with new code details without changing the
                   time created of the RESEND_CONFIRMATION_CODE. */
                userRecoveryDataStore.invalidateWithoutChangeTimeCreated(recoveryDataDO.getSecret(), recoveryCode,
                        RecoverySteps.SEND_RECOVERY_INFORMATION, notificationChannelList);
            } else {
                addRecoveryDataObject(username, tenantDomain, recoveryCode, recoveryScenario, notificationChannelList);
            }
            return buildUserRecoveryInformationResponseDTO(username, recoveryCode, notificationChannelDTOS);
        } else {
            if (log.isDebugEnabled()) {
                log.debug("No valid user found for the given claims");
            }
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND, null);
        }
    }

    /**
     * Check whether the account is locked or disabled.
     *
     * @param user User
     * @throws IdentityRecoveryException If account is in locked or disabled status.
     */
    private void checkAccountLockedStatus(User user) throws IdentityRecoveryException {

        if (Utils.isAccountDisabled(user)) {
            String errorCode = Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
            throw Utils.handleClientException(errorCode,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT.getMessage(),
                    user.getUserName());
        } else if (Utils.isAccountLocked(user)) {
            // Check user in PENDING_SR or PENDING_AP status.
            checkAccountPendingStatus(user);
            String errorCode = Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
            throw Utils.handleClientException(errorCode,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT.getMessage(), user.getUserName());
        }
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
                String errorCode = Utils.prependOperationScenarioToErrorCode(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PENDING_SELF_REGISTERED_ACCOUNT.getCode(),
                        IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
                throw Utils.handleClientException(errorCode,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PENDING_SELF_REGISTERED_ACCOUNT.getMessage(),
                        user.getUserName());
            }
            if (IdentityRecoveryConstants.PENDING_ASK_PASSWORD.equals(accountState)) {
                String errorCode = Utils.prependOperationScenarioToErrorCode(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PENDING_PASSWORD_RESET_ACCOUNT.getCode(),
                        IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
                throw Utils.handleClientException(errorCode,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PENDING_PASSWORD_RESET_ACCOUNT.getMessage(),
                        user.getUserName());
            }
        }
    }

    /**
     * Check whether the user account is eligible for account recovery.
     *
     * @param user                         The user.
     * @param recoveryScenario             Account recovery scenario.
     * @param recoveryNotificationChannels Notification channel.
     * @param metaProperties               Meta details.
     * @throws IdentityRecoveryException If account doesn't satisfy the conditions to recover.
     */
    private void checkUserValidityForAccountRecovery(User user, RecoveryScenarios recoveryScenario,
                                                     List<NotificationChannel> recoveryNotificationChannels,
                                                     Map<String, String> metaProperties)
            throws IdentityRecoveryException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER, user);
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, getUserStoreManager(user));
        properties.put(IdentityEventConstants.EventProperty.RECOVERY_SCENARIO, recoveryScenario);
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, recoveryNotificationChannels);

        if (MapUtils.isNotEmpty(metaProperties)) {
            for (Map.Entry<String, String> metaProperty : metaProperties.entrySet()) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
        }
        Event identityMgtEvent = new Event(IdentityEventConstants.Event.PRE_ACCOUNT_RECOVERY, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error occurred while validating user account " + user.getUserName() +
                        " for account recovery.");
            }
            String errorMessage = e.getMessage();
            String errorCode = IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_USER_ACCOUNT_RECOVERY_VALIDATION_FAILED.getCode();
            if (USERNAME_RECOVERY.equals(recoveryScenario)) {
                errorCode = IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_VALIDATION_FAILED
                        .getCode();
            } else if (NOTIFICATION_BASED_PW_RECOVERY.equals(recoveryScenario) ||
                    QUESTION_BASED_PWD_RECOVERY.equals(recoveryScenario)) {
                errorCode = IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_RECOVERY_VALIDATION_FAILED
                        .getCode();
            }
            throw Utils.handleClientException(errorCode, errorMessage, user.getUserName());
        }
    }

    /**
     * Get the matching username for given claims.
     *
     * @param claims       List of UserClaims
     * @param tenantDomain Tenant domain
     * @return Username (Return null if there are no users).
     * @throws IdentityRecoveryException Error while retrieving the users list.
     */
    public String getUsernameByClaims(Map<String, String> claims, String tenantDomain)
            throws IdentityRecoveryException {

        if (MapUtils.isEmpty(claims)) {
            // Get error code with scenario.
            String errorCode = Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
            throw Utils.handleClientException(errorCode,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getMessage(),
                    null);
        }
        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        try {
            List<String> userStoreDomainNames = getDomainNames(tenantId);
            AbstractUserStoreManager abstractUserStoreManager = (AbstractUserStoreManager) getUserStoreManager(tenantId);
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            ClaimManager claimManager = (ClaimManager) realmService.getTenantUserRealm(tenantId).getClaimManager();
            ArrayList<org.wso2.carbon.user.core.common.User> resultedUserList = new ArrayList<>();

            for (String domain : userStoreDomainNames) {
                List<ExpressionCondition> expressionConditionList =
                        getExpressionConditionList(claims, domain, claimManager);
                if (expressionConditionList.isEmpty()) {
                    continue;
                }
                Condition operationalCondition = getOperationalCondition(expressionConditionList);
                /* Get the users list that matches with the condition
                   limit : 2, offset : 1, sortBy : null, sortOrder : null */
                resultedUserList.addAll(abstractUserStoreManager.getUserListWithID(operationalCondition, domain,
                        UserCoreConstants.DEFAULT_PROFILE, 2, 1, null, null));
                if (resultedUserList.size() > 1) {
                    if (log.isDebugEnabled()) {
                        log.debug("Multiple users matched for given claims set : " +
                                Arrays.toString(resultedUserList.toArray()));
                    }
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_MATCHING_USERS, null);
                }
            }
            // Return empty when no users are found.
            if (resultedUserList.isEmpty()) {
                return StringUtils.EMPTY;
            }
            // When the code reaches here there only be single user match.
            return resultedUserList.get(0).getDomainQualifiedUsername();
        } catch (org.wso2.carbon.user.core.UserStoreException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while retrieving users from user store for the given claim set: " +
                        Arrays.toString(claims.keySet().toArray()));
            }
            throw new IdentityRecoveryException(e.getErrorCode(), "Error occurred while retrieving users.", e);
        } catch (UserStoreException e) {
            throw new IdentityRecoveryException(e.getMessage(), e);
        }
    }

    /**
     * Get the expression conditions for the claim set.
     *
     * @param claims   List of UserClaims
     * @param domain   User store domain
     * @param claimManager   Claim manager
     * @return expressionConditionList of the claims.
     * @throws UserStoreException Error while get attribute name.
     */
    private List<ExpressionCondition> getExpressionConditionList (Map<String, String> claims, String domain,
                                                                 ClaimManager claimManager)
            throws UserStoreException {

        List<ExpressionCondition> expressionConditionList = new ArrayList<>();
        for (Map.Entry<String,String> entry : claims.entrySet()) {
            String attributeName = claimManager.getAttributeName(domain, entry.getKey());
            if (StringUtils.isNotEmpty(entry.getKey()) && StringUtils.isNotEmpty(entry.getValue()) &&
                    StringUtils.isNotEmpty(attributeName)) {
                expressionConditionList.add(new ExpressionCondition(ExpressionOperation.EQ.toString(), attributeName,
                        entry.getValue()));
            }
        }
        return expressionConditionList;
    }

    /**
     * Get the operational condition for expression conditions.
     *
     * @param expressionConditionList   List of expression conditions
     * @return operationalCondition of the expression condition list.
     */
    private Condition getOperationalCondition (List<ExpressionCondition> expressionConditionList) {

        Condition operationalCondition = expressionConditionList.get(0);
        if (expressionConditionList.size() > 1) {
            for (int i = 1; i < expressionConditionList.size(); i++) {
                operationalCondition = new OperationalCondition(OperationalOperation.AND.toString(),
                        operationalCondition, expressionConditionList.get(i));
            }
        }
        return operationalCondition;
    }

    /**
     * Get the notification channel list when the notification channel is external.
     *
     * @return External notification channel information.
     */
    private List<NotificationChannel> getExternalNotificationChannelList() {

        NotificationChannel channelDataModel = new NotificationChannel();
        channelDataModel.setType(NotificationChannels.EXTERNAL_CHANNEL.getChannelType());
        List<NotificationChannel> notificationChannels = new ArrayList<>();
        notificationChannels.add(channelDataModel);
        return notificationChannels;
    }

    /**
     * Get the notification channel list of the user when the notifications are externally managed.
     *
     * @param username     Username
     * @param tenantDomain Tenant domain
     * @return Notification channel list
     * @throws IdentityRecoveryClientException No notification channels available for the user.
     * @throws IdentityRecoveryException       If an error occurred while getting user claim values.
     */
    private List<NotificationChannel> getInternalNotificationChannelList(String username, String tenantDomain,
                                                                         RecoveryScenarios recoveryScenarios)
            throws IdentityRecoveryClientException, IdentityRecoveryException {

        // Create a list of required claims that needs to be retrieved from the user attributes.
        String[] requiredClaims = createRequiredChannelClaimsList();
        // Get channel related claims related to the user.
        Map<String, String> claimValues = getClaimListOfUser(username, tenantDomain, requiredClaims);
        if (MapUtils.isEmpty(claimValues)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS_FOR_USER, null);
        }
        // Get the channel list with details.
        List<NotificationChannel> notificationChannels = getNotificationChannelDetails(username, tenantDomain,
                claimValues, recoveryScenarios);
        if (notificationChannels.size() == 0) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_VERIFIED_CHANNELS_FOR_USER, null);
        }
        return notificationChannels;
    }

    /**
     * Prepare the response to be sent to the recovery APIs.
     *
     * @param username                Username of the user
     * @param recoveryCode            Recovery code given to the user
     * @param notificationChannelDTOs List of NotificationChannelsResponseDTOs available for the user.
     * @return RecoveryChannelInfoDTO object.
     */
    private RecoveryChannelInfoDTO buildUserRecoveryInformationResponseDTO(String username, String recoveryCode,
                                                                    NotificationChannelDTO[] notificationChannelDTOs) {

        RecoveryChannelInfoDTO recoveryChannelInfoDTO = new RecoveryChannelInfoDTO();
        recoveryChannelInfoDTO.setUsername(username);
        recoveryChannelInfoDTO.setRecoveryCode(recoveryCode);
        recoveryChannelInfoDTO.setNotificationChannelDTOs(notificationChannelDTOs);
        return recoveryChannelInfoDTO;
    }

    /**
     * Get the list of available channels with the channel attributes associated to each channel as a list of
     * NotificationChannelsResponseDTOs.
     *
     * @param tenantDomain         Tenant domain.
     * @param notificationChannels Notification channels list.
     * @return NotificationChannelsResponseDTSs list.
     */
    private NotificationChannelDTO[] getNotificationChannelsResponseDTOList(
            String tenantDomain, List<NotificationChannel> notificationChannels) throws IdentityRecoveryException {

        ArrayList<NotificationChannelDTO> notificationChannelDTOs = new ArrayList<>();
        // Store available channels as NotificationChannelDTO objects in the array.
        int channelId = 1;
        for (NotificationChannel channel : notificationChannels) {
            NotificationChannelDTO dto = buildNotificationChannelsResponseDTO(channelId, channel.getType(),
                    channel.getChannelValue(), channel.isPreferredStatus(), tenantDomain);
            notificationChannelDTOs.add(dto);
            channelId++;
        }
        return notificationChannelDTOs.toArray(new NotificationChannelDTO[0]);
    }

    /**
     * Appends the notification channel list details for to a string and return it.
     *
     * @param notificationChannels List of notification channels for the corresponding user.
     * @return String that contains the notification channel details.
     */
    private String getNotificationChannelListForRecovery(List<NotificationChannel> notificationChannels) {

        StringBuilder recoveryChannels = new StringBuilder();
        for (NotificationChannel channel : notificationChannels) {
            // Creating the notification channel list for recovery.
            String channelEntry = channel.getType() + IdentityRecoveryConstants.CHANNEL_ATTRIBUTE_SEPARATOR + channel
                    .getChannelValue();
            recoveryChannels.append(channelEntry).append(IdentityRecoveryConstants.NOTIFY_CHANNEL_LIST_SEPARATOR);
        }
        return recoveryChannels.toString();
    }

    /**
     * Set notification channel details for each communication channels available for the user.
     *
     * @param channelId    Channel Id
     * @param channelType  Channel Type (Eg: EMAIL)
     * @param value        Channel Value (Eg: wso2@gmail.com)
     * @param preference   Whether user marked the channel as a preferred channel of communication
     * @param tenantDomain Tenant domain
     * @return NotificationChannelDTO object.
     * @throws IdentityRecoveryServerException IdentityRecoveryServerException
     */
    private NotificationChannelDTO buildNotificationChannelsResponseDTO(int channelId, String channelType, String value,
                                                                        boolean preference, String tenantDomain)
            throws IdentityRecoveryServerException {

        NotificationChannelDTO notificationChannelDTO = new NotificationChannelDTO();
        notificationChannelDTO.setId(channelId);
        notificationChannelDTO.setType(channelType);
        // Encode the channel Values.
        if (NotificationChannels.EMAIL_CHANNEL.getChannelType().equals(channelType)) {
            notificationChannelDTO.setValue(maskLocalClaim(value, IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM,
                    tenantDomain));
        } else if (NotificationChannels.SMS_CHANNEL.getChannelType().equals(channelType)) {
            notificationChannelDTO.setValue(maskLocalClaim(value, IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM,
                    tenantDomain));
        } else {
            notificationChannelDTO.setValue(value);
        }
        notificationChannelDTO.setPreferred(preference);
        return notificationChannelDTO;
    }

    /**
     * Encode the local claim of the user.
     *
     * @param claimValue    Claim Value
     * @param claimURI      Claim URI
     * @param tenantDomain  Tenant domain
     * @return Encoded claim value (Empty String if a value is not assigned to the user claim)
     * @throws IdentityRecoveryServerException IdentityRecoveryServerException
     */
    private String maskLocalClaim(String claimValue, String claimURI, String tenantDomain)
            throws IdentityRecoveryServerException {

        String localClaimMaskingRegex = getLocalClaimMaskingRegex(claimURI,tenantDomain);
        if (StringUtils.isNotEmpty(claimValue)) {
            claimValue = claimValue.replaceAll(localClaimMaskingRegex,
                    IdentityRecoveryConstants.ChannelMasking.MASKING_CHARACTER);
        }
        return claimValue;
    }

    /**
     * Retrieving masking regex pattern for local claim.
     *
     * @param claimURI      Claim URI
     * @param tenantDomain  Tenant domain
     * @return Masking regex pattern for local claim
     * @throws IdentityRecoveryServerException Error while retrieving masking regex pattern for local claim in a given
     * tenant domain.
     */
    private String getLocalClaimMaskingRegex(String claimURI, String tenantDomain)
            throws IdentityRecoveryServerException {

        String localClaimMaskingRegex;
        try {
            localClaimMaskingRegex = IdentityRecoveryServiceDataHolder.getInstance().getClaimMetadataManagementService()
                    .getMaskingRegexForLocalClaim(claimURI, tenantDomain);
        } catch (ClaimMetadataException e) {
            throw new IdentityRecoveryServerException(String.format("Error while retrieving masking regex pattern " +
                    "for claim URI: %s in tenant domain: %s", claimURI, tenantDomain), e);
        }

        if (StringUtils.isBlank(localClaimMaskingRegex)) {
            if (IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM.equals(claimURI)) {
                localClaimMaskingRegex = IdentityRecoveryConstants.ChannelMasking.EMAIL_MASKING_REGEX;
            } else if (IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM.equals(claimURI)) {
                localClaimMaskingRegex = IdentityRecoveryConstants.ChannelMasking.MOBILE_MASKING_REGEX;
            }
        }
        return localClaimMaskingRegex;
    }

    /**
     * Get the users list for a matching claim.
     *
     * @param tenantId   Tenant ID
     * @param claimUri   Claim to be searched
     * @param claimValue Claim value to be matched
     * @return Matched users list
     * @throws IdentityRecoveryServerException If an error occurred while retrieving claims from the userstore manager.
     */
    private String[] getUserList(int tenantId, String claimUri, String claimValue)
            throws IdentityRecoveryServerException {

        String[] userList = new String[0];
        UserStoreManager userStoreManager = getUserStoreManager(tenantId);
        try {
            if (userStoreManager != null) {
                if (StringUtils.isNotBlank(claimValue) && claimValue.contains(FORWARD_SLASH)) {
                    String extractedDomain = IdentityUtil.extractDomainFromName(claimValue);
                    UserStoreManager secondaryUserStoreManager = userStoreManager.
                            getSecondaryUserStoreManager(extractedDomain);
                    /*
                    Some claims (Eg:- Birth date) can have "/" in claim values. But in user store level we are trying
                    to extract the claim value and find the user store domain. Hence we are adding an extra "/" to
                    the claim value to avoid such issues.
                     */
                    if (secondaryUserStoreManager == null) {
                        claimValue = FORWARD_SLASH + claimValue;
                    }
                }
                userList = userStoreManager.getUserList(claimUri, claimValue, null);
            }
            return userList;
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                String error = String
                        .format("Unable to retrieve the claim : %1$s for the given tenant : %2$s", claimUri, tenantId);
                log.debug(error, e);
            }
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_RETRIEVING_USER_CLAIM, claimUri, e);
        }
    }

    /**
     * Get all the domain names related to user stores.
     * @param tenantId   Tenant ID
     * @return A list of all the available domain names for given tenant
     */
    private List<String> getDomainNames(int tenantId) throws IdentityRecoveryServerException {

        List<String> domainsOfUserStores = new ArrayList<>();
        // Append the primary domain name to the front of the domain list.
        domainsOfUserStores.add(UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME);
        AbstractUserStoreManager abstractUserStoreManager = (AbstractUserStoreManager) getUserStoreManager(tenantId);
        UserStoreManager secondaryUserStore = abstractUserStoreManager.getSecondaryUserStoreManager();
        while (secondaryUserStore != null) {
            String domainName = secondaryUserStore.getRealmConfiguration().
                    getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME).toUpperCase();
            secondaryUserStore = secondaryUserStore.getSecondaryUserStoreManager();
            domainsOfUserStores.add(domainName);
        }
        return domainsOfUserStores;
    }

    /**
     * Get UserStoreManager.
     *
     * @param tenantId Tenant id
     * @return UserStoreManager object
     * @throws IdentityRecoveryServerException If an error occurred while getting UserStoreManager.
     */
    private UserStoreManager getUserStoreManager(int tenantId) throws IdentityRecoveryServerException {

        UserStoreManager userStoreManager;
        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            if (realmService.getTenantUserRealm(tenantId) != null) {
                userStoreManager = (UserStoreManager) realmService.getTenantUserRealm(tenantId).
                        getUserStoreManager();
            } else {
                throw Utils.handleServerException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER, null);
            }
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                String error = String.format("Error retrieving the user store manager for the tenant : %s", tenantId);
                log.debug(error, e);
            }
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER, null, e);
        }
        return userStoreManager;
    }

    /**
     * Get the userstore manager for the user.
     *
     * @param user User.
     * @return Userstore manager.
     * @throws IdentityRecoveryException Error getting the userstore manager.
     */
    private UserStoreManager getUserStoreManager(User user) throws IdentityRecoveryException {

        RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
        try {
            UserRealm tenantUserRealm = realmService.getTenantUserRealm(IdentityTenantUtil.
                    getTenantId(user.getTenantDomain()));
            if (IdentityUtil.getPrimaryDomainName().equals(user.getUserStoreDomain())) {
                return (UserStoreManager) tenantUserRealm.getUserStoreManager();
            }
            return ((UserStoreManager) tenantUserRealm.getUserStoreManager())
                    .getSecondaryUserStoreManager(user.getUserStoreDomain());
        } catch (UserStoreException e) {
            if (log.isDebugEnabled()) {
                String error = String.format("Error retrieving the user store manager for the user : %s",
                        user.getUserName());
                log.debug(error, e);
            }
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_GETTING_USERSTORE_MANAGER, null, e);
        }
    }

    /**
     * Keep the common users list from the previously matched list and the new list.
     *
     * @param resultedUserList Already matched users for previous claims
     * @param matchedUserList  Retrieved users list for the given claim
     * @param claim            Claim used for filtering
     * @param value            Value given for the claim
     * @return Users list with no duplicates.
     */
    private String[] getCommonUserEntries(String[] resultedUserList, String[] matchedUserList, String claim,
                                          String value) {

        ArrayList<String> matchedUsers = new ArrayList<>(Arrays.asList(matchedUserList));
        ArrayList<String> resultedUsers = new ArrayList<>(Arrays.asList(resultedUserList));
        // Remove not matching users.
        resultedUsers.retainAll(matchedUsers);
        if (resultedUsers.size() > 0) {
            resultedUserList = resultedUsers.toArray(new String[0]);
            if (log.isDebugEnabled()) {
                log.debug("Current matching temporary user list :" + Arrays.toString(resultedUserList));
            }
            return resultedUserList;
        } else {
            if (log.isDebugEnabled()) {
                String message = String
                        .format("There are no common users for claim : %1$s with the value : %2$s with the "
                                + "previously filtered user list", claim, value);
                log.debug(message);
            }
            return new String[0];
        }
    }

    /**
     * Get claim values of a user for a given list of claims.
     *
     * @param username          Username of the user
     * @param tenantDomain      tenant domain
     * @param requiredClaimURLs Claims that needs to be retrieved.
     * @return Map of claims and values
     * @throws IdentityRecoveryException If an error occurred while getting the user claims of the user
     */
    private Map<String, String> getClaimListOfUser(String username, String tenantDomain, String[] requiredClaimURLs)
            throws IdentityRecoveryException {

        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        UserStoreManager userStoreManager = getUserStoreManager(tenantId);
        Map<String, String> claimValues = null;
        try {
            if (userStoreManager != null) {
                claimValues = userStoreManager.getUserClaimValues(username, requiredClaimURLs, null);
            }
        } catch (UserStoreException e) {
            String error = String
                    .format("Error getting claims of user : %1$s in tenant domain : %2$s", username, tenantDomain);
            if (log.isDebugEnabled()) {
                log.debug(error, e);
            }
            throw Utils
                    .handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_LOADING_USER_CLAIMS,
                            null);
        }
        return claimValues;
    }

    /**
     * Create required claim list from the attributes in the Notification channel list. The required claims will be
     * used to get user's attributes.
     *
     * @return Required claims list.
     */
    private String[] createRequiredChannelClaimsList() {

        List<String> requiredClaims = new ArrayList<>();
        for (NotificationChannels channel : notificationChannels) {
            requiredClaims.add(channel.getClaimUri());
            requiredClaims.add(channel.getVerifiedClaimUrl());
        }
        requiredClaims.add(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM);
        // Get the list of roles that the user has since the channel selection criteria changes with the availability
        // of INTERNAL/selfsignup role.
        requiredClaims.add(IdentityRecoveryConstants.USER_ROLES_CLAIM);
        return requiredClaims.toArray(new String[0]);
    }

    /**
     * get the available verified Notification channel objects.
     *
     * @param claimValues Claim values related to the notification channels
     * @return Verified notification channels for the user.
     */
    private List<NotificationChannel> getNotificationChannelDetails(String username, String tenantDomain, Map<String,
            String> claimValues, RecoveryScenarios recoveryScenarios) throws IdentityRecoveryServerException {

        // Check whether the user is self registered user.
        boolean isSelfRegisteredUser = isSelfSignUpUser(claimValues.get(IdentityRecoveryConstants.USER_ROLES_CLAIM));
        String preferredChannel = claimValues.get(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM);
        List<NotificationChannel> verifiedChannels = new ArrayList<>();
        for (NotificationChannels channel : notificationChannels) {
            String channelValue = claimValues.get(channel.getClaimUri());
            boolean channelVerified = Boolean.parseBoolean(claimValues.get(channel.getVerifiedClaimUrl()));
            boolean isFunctionalityLocked = isFunctionalityLocked(username, tenantDomain, channel.getChannelType(),
                    recoveryScenarios);
            NotificationChannel channelDataModel = new NotificationChannel();

            // If the user is self registered, then user has to have the verified channel claims. Check whether channel
            // is verified and not empty.
            if (!isFunctionalityLocked && isSelfRegisteredUser && channelVerified && StringUtils.isNotEmpty(channelValue)) {
                channelDataModel.setType(channel.getChannelType());
                channelDataModel.setChannelValue(channelValue);
                // Check whether the preferred channel matches the given channel.
                if (StringUtils.isNotEmpty(preferredChannel) && channel.getChannelType().equals(preferredChannel)) {
                    channelDataModel.setPreferredStatus(true);
                }
                verifiedChannels.add(channelDataModel);
            } else if (!isFunctionalityLocked && StringUtils.isNotEmpty(channelValue)) {
                channelDataModel.setType(channel.getChannelType());
                channelDataModel.setChannelValue(channelValue);
                // Check whether the preferred channel matches the given channel.
                if (StringUtils.isNotEmpty(preferredChannel) && channel.getChannelType().equals(preferredChannel)) {
                    channelDataModel.setPreferredStatus(true);
                }
                verifiedChannels.add(channelDataModel);
            }
        }
        return verifiedChannels;
    }

    /**
     * Verify whether the functionality is locked or not and return true if it is locked. Else return false.
     *
     * @param username          Username.
     * @param tenantDomain      TenantDomain.
     * @param channelType       ChannelType (SMS or EMail).
     * @param recoveryScenarios RecoveryScenarios.
     * @return Return true if it is locked. Else return false.
     * @throws IdentityRecoveryServerException
     */
    private boolean isFunctionalityLocked(String username, String tenantDomain, String channelType,
                                          RecoveryScenarios recoveryScenarios) throws IdentityRecoveryServerException {

        if (PER_USER_FUNCTIONALITY_LOCKING_ENABLED) {
            String functionalityType = FUNCTIONALITY_PREFIX + recoveryScenarios.name() + "_" + channelType;
            // Check whether the functionality is locked ot not. If it is locked, stop from sending that channel
            // for recovery.
            if (IdentityRecoveryConstants.FunctionalityTypes.getFunctionality(functionalityType) != null) {
                String functionalityIdentifier = IdentityRecoveryConstants.FunctionalityTypes.
                        getFunctionality(functionalityType).getFunctionalityIdentifier();
                FunctionalityLockStatus functionalityLockStatus = getFunctionalityStatusOfUser(username, tenantDomain,
                        functionalityIdentifier);
                return functionalityLockStatus.getLockStatus();
            }
        }
        return false;
    }

    /**
     * Get the lock status of a functionality given the tenant domain, user name and the functionality identifier.
     *
     * @param tenantDomain            Tenant domain of the user.
     * @param userName                Username of the user.
     * @param functionalityIdentifier Identifier of the the functionality.
     * @return The status of the functionality, {@link FunctionalityLockStatus}.
     */
    private FunctionalityLockStatus getFunctionalityStatusOfUser(String userName, String tenantDomain,
                                                                 String functionalityIdentifier)
            throws IdentityRecoveryServerException {

        int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
        String userId = Utils.getUserId(userName, tenantId);

        UserFunctionalityManager userFunctionalityManager =
                IdentityRecoveryServiceDataHolder.getInstance().getUserFunctionalityManagerService();

        try {
            return userFunctionalityManager.getLockStatus(userId, tenantId, functionalityIdentifier);
        } catch (UserFunctionalityManagementException e) {
            String mappedErrorCode =
                    Utils.prependOperationScenarioToErrorCode(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_GET_LOCK_STATUS_FOR_FUNCTIONALITY
                                    .getCode(), IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO);
            String message = IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_GET_LOCK_STATUS_FOR_FUNCTIONALITY
                    .getMessage();
            throw Utils.handleServerException(mappedErrorCode, message, null);
        }
    }

    /**
     * Checks whether the user is a self signed-up user or not.
     *
     * @param rolesList Roles that the user has
     * @return TRUE of the user has self sign-up role.
     */
    private boolean isSelfSignUpUser(String rolesList) {

        List<String> roles = Arrays.asList(rolesList.split(IdentityRecoveryConstants.SIGN_UP_ROLE_SEPARATOR));
        return roles.contains(IdentityRecoveryConstants.SELF_SIGNUP_ROLE);
    }

    /**
     * Validate the code.
     *
     * @param code Code given for recovery
     * @param step Recovery step
     * @throws IdentityRecoveryException If an error occurred while validating the recoveryId.
     */
    public UserRecoveryData getUserRecoveryData(String code, RecoverySteps step) throws IdentityRecoveryException {

        UserRecoveryData recoveryData;
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        try {
            // Retrieve recovery data bound to the recoveryId.
            recoveryData = userRecoveryDataStore.load(code);
        } catch (IdentityRecoveryException e) {
            // Map code expired error to new error codes for user account recovery.
            if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE.getCode().equals(e.getErrorCode())) {
                e.setErrorCode(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_CODE.getCode());
            } else if (IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode()
                    .equals(e.getErrorCode())) {
                e.setErrorCode(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_RECOVERY_CODE.getCode());
            } else {
                e.setErrorCode(Utils.prependOperationScenarioToErrorCode(e.getErrorCode(),
                        IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY));
            }
            throw e;
        }
        if (recoveryData == null) {
            throw Utils
                    .handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_ACCOUNT_RECOVERY_DATA,
                            code);
        }
        if (!step.equals(recoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_RECOVERY_CODE,
                    code);
        }
        return recoveryData;
    }

    /**
     * Add the notification channel recovery data to the store.
     *
     * @param username     Username
     * @param tenantDomain Tenant domain
     * @param secretKey    RecoveryId
     * @param scenario     RecoveryScenario
     * @param recoveryData Data to be stored as mata which are needed to evaluate the recovery data object
     * @throws IdentityRecoveryServerException If an error occurred while storing recovery data.
     */
    private void addRecoveryDataObject(String username, String tenantDomain, String secretKey,
                                       RecoveryScenarios scenario, String recoveryData)
            throws IdentityRecoveryServerException {

        // Create a user object.
        User user = Utils.buildUser(username, tenantDomain);
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, scenario,
                RecoverySteps.SEND_RECOVERY_INFORMATION);
        // Store available channels in remaining setIDs.
        recoveryDataDO.setRemainingSetIds(recoveryData);
        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.invalidate(user);
            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw Utils.handleServerException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ERROR_STORING_RECOVERY_DATA,
                    "Error Storing Recovery Data", e);
        }
    }
}
