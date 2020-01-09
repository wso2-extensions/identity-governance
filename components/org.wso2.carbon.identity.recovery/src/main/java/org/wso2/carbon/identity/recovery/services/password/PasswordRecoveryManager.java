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
package org.wso2.carbon.identity.recovery.services.password;

import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.dto.PasswordRecoverDTO;
import org.wso2.carbon.identity.recovery.dto.PasswordResetCodeDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.ResendConfirmationDTO;
import org.wso2.carbon.identity.recovery.dto.SuccessfulPasswordResetDTO;

import java.util.Map;

/**
 * Service Interface for password recovery.
 */
public interface PasswordRecoveryManager {

    /**
     * Get the username recovery information with available verified channel details.
     *
     * @param claims       User Claims
     * @param tenantDomain Tenant domain
     * @param properties   Meta properties
     * @return RecoveryInformationDTO {@link RecoveryInformationDTO} object that contains
     * recovery information of a  verified user
     * @throws IdentityRecoveryException Error while initiating password recovery
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
     * @return UsernameRecoverDTO {@link PasswordRecoverDTO} object that contains notified
     * channel details and success status code
     * @throws IdentityRecoveryException Error while notifying user
     */
    PasswordRecoverDTO notify(String recoveryId, String channelId, String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException;

    /**
     * Validate the confirmation code given for password recovery and return the password reset token.
     *
     * @param confirmationCode Confirmation code
     * @param tenantDomain     Tenant domain
     * @param properties       Meta properties in the confirmation request
     * @return PasswordResetCodeDTO {@link PasswordResetCodeDTO} object which contains password reset code
     * @throws IdentityRecoveryException Error while confirming password recovery
     */
    PasswordResetCodeDTO confirm(String confirmationCode, String tenantDomain, Map<String, String> properties)
            throws IdentityRecoveryException;

    /**
     * Update the password for password recovery, if the password reset code is valid.
     *
     * @param resetCode  Password reset code
     * @param password   New password
     * @param properties Properties
     * @return SuccessfulPasswordResetDTO {@link SuccessfulPasswordResetDTO} object which contain the information
     * for a successful password update
     * @throws IdentityRecoveryException Error while resetting the password
     */
    SuccessfulPasswordResetDTO reset(String resetCode, char[] password, Map<String, String> properties)
            throws IdentityRecoveryException;

    /**
     * Resend the password recovery information to the user via user specified channel.
     *
     * @param tenantDomain Tenant Domain
     * @param resendCode   Resend code
     * @param properties   Meta properties
     * @return ResendConfirmationDTO {@link ResendConfirmationDTO} which wraps the information for a successful
     * recovery information resend
     * @throws IdentityRecoveryException Error while sending recovery information
     */
    ResendConfirmationDTO resend(String tenantDomain, String resendCode, Map<String, String> properties)
            throws IdentityRecoveryException;
}
