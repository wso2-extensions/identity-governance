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
package org.wso2.carbon.identity.recovery.model;

/**
 * Model that encapsulates the attributes related to the communication channels.
 */
public class NotificationChannel {

    /**
     * Channel Type (Eg: EMAIL, SMS).
     */
    private String type;

    /**
     * Claim uri of the channel (Eg: http://wso2.org/claims/identity/verifyEmail).
     */
    private String claimUri;

    /**
     * Verified claim uri of the channel (Eg: http://wso2.org/claims/identity/emailVerified).
     */
    private String channelVerifiedClaimUri;

    /**
     * Value of the channel stored at the claim uri (Eg: wso2@gmail.com).
     */
    private String channelValue;

    /**
     * Whether the channel is stored as a user preferred channel to get notifications.
     */
    private boolean preferredStatus;

    /**
     * Notification channel builder.
     */
    public NotificationChannel() {

        this.type = "";
        this.channelVerifiedClaimUri = "";
        this.claimUri = "";
        this.channelValue = "";
        this.preferredStatus = false;
    }

    /**
     * Get the verified status of the preferred notification channel.
     *
     * @return TRUE if the preferred channel is verified.
     */
    public boolean isPreferredStatus() {

        return preferredStatus;
    }

    /**
     * Set the verified status of the preferred notification channel.
     *
     * @param preferredStatus TRUE if the preferred channel is verified.
     */
    public void setPreferredStatus(boolean preferredStatus) {

        this.preferredStatus = preferredStatus;
    }

    /**
     * Get Notification channel type.
     *
     * @return Notification channel type (Eg: EMAIL or SMS)
     */
    public String getType() {

        return type;
    }

    /**
     * Set the notification channel type.
     *
     * @param type Notification channel type (Eg: EMAIL or SMS)
     */
    public void setType(String type) {

        this.type = type;
    }

    /**
     * Get notification channel value.
     *
     * @return Notification channel value (Eg: user@wso2.com)
     */
    public String getChannelValue() {

        return channelValue;
    }

    /**
     * Set notification channel value.
     *
     * @param channelValue Notification channel value (Eg: user@wso2.com)
     */
    public void setChannelValue(String channelValue) {

        this.channelValue = channelValue;
    }

    /**
     * Get the value claim uri the notification channel.
     *
     * @return Claim uri (Eg: http://wso2.org/claims/emailaddress)
     */
    public String getClaimUri() {

        return claimUri;
    }

    /**
     * Set the value claim of the notification channel.
     *
     * @param claimUri Claim uri (Eg: http://wso2.org/claims/emailaddress)
     */
    public void setClaimUri(String claimUri) {

        this.claimUri = claimUri;
    }

    /**
     * Get the verified claim of the notification channel.
     *
     * @return Verified claim uri (Eg: http://wso2.org/claims/identity/emailVerified)
     */
    public String getChannelVerifiedClaimUri() {

        return channelVerifiedClaimUri;
    }

    /**
     * Set the verified claim of the notification channel.
     *
     * @param channelVerifiedClaimUri Verified claim uri (Eg: http://wso2.org/claims/identity/emailVerified)
     */
    public void setChannelVerifiedClaimUri(String channelVerifiedClaimUri) {

        this.channelVerifiedClaimUri = channelVerifiedClaimUri;
    }
}
