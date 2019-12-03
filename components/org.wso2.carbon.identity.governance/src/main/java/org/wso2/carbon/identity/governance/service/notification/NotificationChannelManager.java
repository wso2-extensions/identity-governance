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

package org.wso2.carbon.identity.governance.service.notification;

import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;

import java.util.Map;

/**
 * Service Interface for resolving notification channel.
 */
public interface NotificationChannelManager {

    /**
     * Validate whether the user specified notification channel type is supported by the server or not.
     *
     * @param preferredChannel Type of the user preferred notification channel
     * @return True if the preferred channel is supported
     */
    boolean isSupportedChannel(String preferredChannel);

    /**
     * Resolve a communication channels to send notifications according to the available claims of the user.
     *
     * @param username        Username of the user
     * @param tenantDomain    Tenant domain of the user
     * @param userstoreDomain Userstore domain of the user
     * @return Communication channel
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    String resolveCommunicationChannel(String username, String tenantDomain, String userstoreDomain)
            throws NotificationChannelManagerException;

    /**
     * Resolve a communication channels to send notifications according to a map of claims available to the user.
     *
     * @param username        Username of the user
     * @param tenantDomain    Tenant domain of the user
     * @param userstoreDomain Userstore domain of the user
     * @param claimsMap       Map of the user claims with claim uri as the key and claim value as the value of the ma.
     * @return Communication channel
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    String resolveCommunicationChannel(String username, String tenantDomain, String userstoreDomain,
            Map<String, String> claimsMap) throws NotificationChannelManagerException;

}
