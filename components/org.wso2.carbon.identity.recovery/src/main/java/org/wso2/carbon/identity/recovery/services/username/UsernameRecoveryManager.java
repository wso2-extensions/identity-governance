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
package org.wso2.carbon.identity.recovery.services.username;

import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.UsernameRecoverDTO;

import java.util.Map;

/**
 * Service Interface for username recovery.
 */
public interface UsernameRecoveryManager {

    /**
     * Get the username recovery information with available verified channel details.
     *
     * @param claims       User Claims
     * @param tenantDomain Tenant domain
     * @param properties   Meta properties
     * @return RecoveryInformationDTO {@link RecoveryInformationDTO} object that contains
     * recovery information of a  verified user
     * @throws IdentityRecoveryException Error while initiating username recovery
     */
    RecoveryInformationDTO initiate(Map<String, String> claims, String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException;

    /**
     * Verify the recovery code and send recovery information via channel which matches the given channel id.
     *
     * @param recoveryId   RecoveryId of the user
     * @param channelId    Channel Id of the user
     * @param tenantDomain Tenant Domain
     * @param properties   Meta properties in the recovery request
     * @return UsernameRecoverDTO {@link UsernameRecoverDTO} object that contains notified
     * channel details and success status code
     * @throws IdentityRecoveryException Error while notifying user
     */
    UsernameRecoverDTO notify(String recoveryId, String channelId, String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException;
}
