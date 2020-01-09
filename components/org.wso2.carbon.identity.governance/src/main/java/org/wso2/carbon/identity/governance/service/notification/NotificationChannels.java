/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.governance.service.notification;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerClientException;

/**
 * Enum contains the supported notification channels by the identity server.
 */
public enum NotificationChannels {

    EMAIL_CHANNEL("EMAIL", "http://wso2.org/claims/emailaddress",
            "http://wso2.org/claims/identity/emailVerified"),
    SMS_CHANNEL("SMS", "http://wso2.org/claims/mobile",
            "http://wso2.org/claims/identity/phoneVerified"),
    EXTERNAL_CHANNEL("EXTERNAL", StringUtils.EMPTY, StringUtils.EMPTY);

    // Type of the channel. Eg: EMAIL or SMS.
    private String channelType;

    // Verification claim url of the channel.
    private String verifiedClaimUrl;

    // Claim url of the channel.
    private String claimUrl;

    /**
     * Get claim url of the channel.
     *
     * @return Claim url of the channel
     */
    public String getClaimUri() {
        return claimUrl;
    }


    /**
     * Get claim url of the channel.
     *
     * @return Claim url of the channel
     */
    public String getVerifiedClaimUrl() {
        return verifiedClaimUrl;
    }

    /**
     * Get channel type/name of the channel.
     *
     * @return Claim url of the channel
     */
    public String getChannelType() {
        return channelType;
    }

    NotificationChannels(String type, String claimUrl, String verifiedClaimUrl) {

        this.channelType = type;
        this.claimUrl = claimUrl;
        this.verifiedClaimUrl = verifiedClaimUrl;
    }

    /**
     * Get NotificationChannels enum which matches the channel type.
     *
     * @param channelType Type of the channel
     * @return NotificationChannel which matches the given channel type
     */
    public static NotificationChannels getNotificationChannel(String channelType)
            throws NotificationChannelManagerClientException {

        if (EMAIL_CHANNEL.getChannelType().equals(channelType)) {
            return EMAIL_CHANNEL;
        } else if (SMS_CHANNEL.getChannelType().equals(channelType)) {
            return SMS_CHANNEL;
        } else if (EXTERNAL_CHANNEL.getChannelType().equals(channelType)) {
            return EXTERNAL_CHANNEL;
        } else {
            throw new NotificationChannelManagerClientException(
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getCode(),
                    IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getMessage());
        }
    }
}
