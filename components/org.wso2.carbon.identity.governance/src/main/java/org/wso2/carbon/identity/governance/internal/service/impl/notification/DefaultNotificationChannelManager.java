/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.governance.internal.service.impl.notification;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.wso2.carbon.CarbonConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceUtil;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerClientException;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerServerException;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;

import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannelManager;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.api.UserRealm;

import java.util.ArrayList;
import java.util.Map;

/**
 * Class contains the implementation of the NotificationChannelManager.
 */
public class DefaultNotificationChannelManager implements NotificationChannelManager {

    private static final Log log = LogFactory.getLog(DefaultNotificationChannelManager.class);
    private ArrayList<NotificationChannels> channels = new ArrayList<>();

    public DefaultNotificationChannelManager() {

        this.channels.add(NotificationChannels.EMAIL_CHANNEL);
        this.channels.add(NotificationChannels.SMS_CHANNEL);
    }

    /**
     * Validate whether the user specified notification channel type is supported by the server or not.
     *
     * @param preferredChannel Type of the user preferred notification channel
     * @return True if the preferred channel is supported
     */
    @Override
    public boolean isSupportedChannel(String preferredChannel) {

        // Server supported notification channels.
        for (NotificationChannels channel : channels) {
            // If preferred channel is in the channel list, the preferred channel is supported by the server to send
            // notifications.
            if (channel.getChannelType().equalsIgnoreCase(preferredChannel)) {
                if (log.isDebugEnabled()) {
                    log.debug("Given preferred channel : " + preferredChannel + " is supported by the server");
                }
                return true;
            }
        }
        // Given preferred channel is not supported by the server.
        if (log.isDebugEnabled()) {
            log.debug("Given preferred channel : " + preferredChannel + " is not supported by the server");
        }
        return false;
    }

    /**
     * Resolve a communication channels to send notifications according to a map of claims available to the user.
     *
     * @param username        Username
     * @param tenantDomain    Tenant domain of the user
     * @param userstoreDomain Userstore domain of the user
     * @param claimsMap       Map of the user claims with claim uri as the key and claim value as the value of the map.
     * @return Communication channel
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    @Override
    public String resolveCommunicationChannel(String username, String tenantDomain, String userstoreDomain,
            Map<String, String> claimsMap) throws NotificationChannelManagerException {

        boolean isChannelResolvingEnabled = Boolean.parseBoolean(
                IdentityUtil.getProperty(IdentityMgtConstants.PropertyConfig.RESOLVE_NOTIFICATION_CHANNELS));

        // If channel resolving logic is not enabled, return the server default notification channel.
        if (!isChannelResolvingEnabled) {
            return IdentityGovernanceUtil.getDefaultNotificationChannel();
        }
        String preferredChannel = claimsMap.get(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM);
        if (StringUtils.isNotEmpty(preferredChannel)) {
            if (isSupportedChannel(preferredChannel)) {
                if (validatePreferredChannelWithValuesInClaimMap(preferredChannel, claimsMap)) {
                    return preferredChannel;
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("No value in the matching claim for preferred channel : " + preferredChannel);
                    }
                    throw new NotificationChannelManagerClientException(
                            IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL
                                    .getCode(),
                            IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL
                                    .getMessage());
                }
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Given preferred channel : " + preferredChannel + " is not supported by the server");
                }
                throw new NotificationChannelManagerClientException(
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNEL.getCode(),
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNEL.getMessage());
            }
        } else {
            preferredChannel = resolveCommunicationChannel(channels, claimsMap);
        }
        return preferredChannel;
    }

    /**
     * Resolve a communication channels to send notifications according to the available claims of the user.
     *
     * @param username        Username of the user
     * @param tenantDomain    Tenant domain of the user
     * @param userstoreDomain Userstore domain of the user
     * @return Communication channel
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    @Override
    public String resolveCommunicationChannel(String username, String tenantDomain, String userstoreDomain)
            throws NotificationChannelManagerException {

        boolean isChannelResolvingEnabled = Boolean.parseBoolean(
                IdentityUtil.getProperty(IdentityMgtConstants.PropertyConfig.RESOLVE_NOTIFICATION_CHANNELS));

        // If channel resolving logic is not enabled, return the server default notification channel.
        if (!isChannelResolvingEnabled) {
            return IdentityGovernanceUtil.getDefaultNotificationChannel();
        }
        Map<String, String> claimMap = buildChannelClaimsMap(username,tenantDomain);

        // If there are no values for channel related claims.
        if (MapUtils.isEmpty(claimMap)) {
            if (log.isDebugEnabled()) {
                String error = String.format("No notification channel for user : %1$s with domain : %2$s.",
                        userstoreDomain + CarbonConstants.DOMAIN_SEPARATOR + username, tenantDomain);
                log.debug(error);
            }
            throw new NotificationChannelManagerClientException(
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getCode(),
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getMessage());
        }
        String preferredChannel = claimMap.get(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM);

        // If the user has already specified a channel, then no need to evaluate the claims for a notification channel.
        if (StringUtils.isNotEmpty(preferredChannel)) {
            // Check whether the preferred channel has claims values.
            if (validatePreferredChannelWithValuesInClaimMap(preferredChannel, claimMap)) {
                return preferredChannel;
            }
            throw new NotificationChannelManagerClientException(
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL.getCode(),
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL.getMessage());
        }
        return resolveCommunicationChannel(this.channels, claimMap);
    }

    /**
     * Checks whether the preferred communication channel matches the given channel values.
     *
     * @param claimsMap User's claims
     * @return True of preferred channel has a value in the claims map.
     */
    private boolean validatePreferredChannelWithValuesInClaimMap(String preferredChannel, Map<String, String> claimsMap)
            throws NotificationChannelManagerClientException {

        // Get the notification channel which matches the given channel type
        NotificationChannels channel = NotificationChannels.getNotificationChannel(preferredChannel);
        return StringUtils.isNotEmpty(claimsMap.get(channel.getClaimUri()));
    }

    /**
     * Get a notification channel for the given set of claims which are belong to a user.
     *
     * @param channels  notification channels
     * @param claimsMap Claims
     * @return notification channel
     */
    private String resolveCommunicationChannel(ArrayList<NotificationChannels> channels,
            Map<String, String> claimsMap) {

        ArrayList<String> availableChannels = new ArrayList<>();
        for (NotificationChannels channel : channels) {
            // Checks whether there is a value value for given communication channel url.
            String value = claimsMap.get(channel.getClaimUri());
            if (StringUtils.isNotEmpty(value)) {

                // Add the channel type as a available channel.
                availableChannels.add(channel.getChannelType());
            }
        }
        if (!availableChannels.isEmpty()) {
            if (availableChannels.size() > 1) {
                if (log.isDebugEnabled()) {
                    log.debug("Multiple notification channels available for the user");
                }
                return resolveMultipleAvailableChannels(availableChannels);
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("Single notification channel is available : " + availableChannels.get(0));
                }
                return availableChannels.get(0);
            }
        } else {
            return null;
        }
    }

    /**
     * Resolve a communication channel when multiple channels are available for the user.
     *
     * @param availableChannels Available channels for the user
     * @return Resolved notification channel
     */
    private String resolveMultipleAvailableChannels(ArrayList<String> availableChannels) {

        // Get server default configurations.
        String serverDefaultChannel = IdentityGovernanceUtil.getDefaultNotificationChannel();

        // Check whether the server default communication channel is available for the user.
        if (availableChannels.contains(serverDefaultChannel)) {
            if (log.isDebugEnabled()) {
                log.debug("User has multiple communication channels. Using server default configuration : "
                        + serverDefaultChannel + " channel as the communication channel");
            }
            return serverDefaultChannel;
        }

        // Reaching this point will imply that user does not have server default channel in his/her available
        // channel list. Therefore, sending the first channel as in the list as preferred channel.
        String availableChannel = availableChannels.get(0);
        if (log.isDebugEnabled()) {
            String message = String.format("User does not have server default channel : %1$s. Therefore,"
                    + "communication channel is set to : %2$s", serverDefaultChannel, availableChannel);
            log.debug(message);
        }
        return availableChannel;
    }

    /**
     * Create a claims list related to notification channels using the notification channel objects.
     *
     * @param channels Notification channel objects
     * @return List of claims associated with the notification channels
     */
    private ArrayList<String> getChannelsClaimList(ArrayList<NotificationChannels> channels) {

        ArrayList<String> claimList = new ArrayList<>();
        for (NotificationChannels channel : channels) {
            claimList.add(channel.getClaimUri());
        }
        return claimList;
    }

    /**
     * Build the user claims map with notification channel information.
     *
     * @param username     Username
     * @param tenantDomain Tenant domain
     * @return User claims map with notification channel information
     * @throws NotificationChannelManagerServerException Error while getting user claims
     */
    private Map<String, String> buildChannelClaimsMap(String username, String tenantDomain)
            throws NotificationChannelManagerServerException {

        ArrayList<String> claimUrls = getChannelsClaimList(this.channels);
        claimUrls.add(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM);
        UserStoreManager userStoreManager = getUserstoreManager(username, tenantDomain);
        Map<String, String> claimValues;
        try {
            claimValues = userStoreManager.getUserClaimValues(username, claimUrls.toArray(new String[0]), null);
        } catch (UserStoreException exception) {

            // This error will occur due to unavailability of identity claims.
            // Retrieving email address and mobile claims for user.
            String[] channelClaims = new String[] {
                    NotificationChannels.SMS_CHANNEL.getClaimUri(), NotificationChannels.EMAIL_CHANNEL.getClaimUri()
            };
            try {
                claimValues = userStoreManager.getUserClaimValues(username, channelClaims, null);
            } catch (UserStoreException e) {
                throw new NotificationChannelManagerServerException(
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getCode(),
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getMessage(), e);
            }
        }
        return claimValues;
    }

    /**
     * Get the UserStoreManager.
     *
     * @param username     Username
     * @param tenantDomain Tenant domain
     * @return UserStoreManager
     * @throws NotificationChannelManagerServerException Error getting UserStoreManager
     */
    private UserStoreManager getUserstoreManager(String username, String tenantDomain)
            throws NotificationChannelManagerServerException {

        try {
            int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);
            RealmService realmService = IdentityMgtServiceDataHolder.getInstance().getRealmService();
            UserRealm userRealm = realmService.getTenantUserRealm(tenantId);
            if (userRealm != null) {
                return userRealm.getUserStoreManager();
            } else {
                if (log.isDebugEnabled()) {
                    String error = String
                            .format("User Realm returned NULL for user : %1$s with in " + "domain : %2$s", username,
                                    tenantDomain);
                    log.debug(error);
                }
                throw new NotificationChannelManagerServerException(
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getCode(),
                        IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getMessage());
            }
        } catch (UserStoreException e) {
            throw new NotificationChannelManagerServerException(
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getCode(),
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_DEFAULT_SERVER_ERROR.getMessage(), e);
        }
    }
}
