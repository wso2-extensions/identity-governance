/*
 *  Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.claim.verification.core.verifier;

import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;

import java.util.Map;

/**
 * Interface for implementing the claim verifiers.
 */
public interface ClaimVerifier {

    /**
     * Used to send a notification initiating the claim verification process.
     *
     * @param user       User of the claim.
     * @param claim      The claim.
     * @param properties Additional properties that may be required by the verifier.
     * @throws ClaimVerificationException
     */
    void sendNotification(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException;

    /**
     * Used to perform the claim verification.
     *
     * @param user       User of the claim.
     * @param claim      The claim.
     * @param properties Additional properties that may be required by the verifier.
     * @return Returns true if the claim is verified else returns false.
     * @throws ClaimVerificationException
     */
    boolean isVerified(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException;

    /**
     * Used to check if the verifier can handler a particular claim verification request.
     *
     * @param verificationMethod Verification method.
     * @return Returns true is the verifier can handle the claim verification request else returns false.
     * @throws ClaimVerificationException
     */
    boolean canHandle(String verificationMethod) throws ClaimVerificationException;

    /**
     * Used to notify the verifier that the claim verification process is being terminated.
     *
     * @param user  User of the claim.
     * @param claim The claim.
     * @throws ClaimVerificationException
     */
    void revokeProcess(User user, Claim claim) throws ClaimVerificationException;

    /**
     * Used to get the confirmation validity period.
     *
     * @param codeType Type of confirmation code. Input values "CODE_TYPE_VALIDATION" and "CODE_TYPE_CONFIRMATION".
     * @param tenantId The tenant id.
     * @return Returns the confirmation code validity period in minutes.
     * @throws ClaimVerificationException
     */
    int getConfirmationCodeValidityPeriod(String codeType, int tenantId) throws ClaimVerificationException;
}
