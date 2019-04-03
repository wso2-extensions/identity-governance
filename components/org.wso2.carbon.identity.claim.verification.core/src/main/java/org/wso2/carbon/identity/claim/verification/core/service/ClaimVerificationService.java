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

package org.wso2.carbon.identity.claim.verification.core.service;

import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.model.ValidationResponse;

import java.util.List;
import java.util.Map;
// TODO: 3/8/19 Only allow local claims? Or validate the mapped local claim for external claims?. If so, a validation
//  needed for this.
/**
 * Interface for implementing the claim verification core.
 */
public interface ClaimVerificationService {

    /**
     * Used to initiate the claim verification process.
     *
     * @param user       User of the claim.
     * @param claim      The claim.
     * @param properties Additional properties that may be required by the claim verifier.
     * @return Confirmation code to be used for the claim validation step.
     * @throws ClaimVerificationException
     */
    String initVerification(User user, Claim claim, Map<String, String> properties)
            throws ClaimVerificationException;

    /**
     * Used to initiate claim validation. Used when additional validation is not required.
     * This will also end the claim verification process.
     *
     * @param code       Confirmation code.
     * @param properties Additional properties that may be required by the claim verifier.
     * @return ValidationResponse response object with claim validation result.
     * @throws ClaimVerificationException
     */
    ValidationResponse validateClaim(String code, Map<String, String> properties) throws ClaimVerificationException;

    /**
     * Used to initiate claim validation.
     * Will end the claim verification process if additional validation is not required.
     *
     * @param code                           Confirmation code.
     * @param properties                     Additional properties that may be required by the claim verifier.
     * @param isAdditionalValidationRequired Boolean value stating whether or not additional validation is required.
     * @return ValidationResponse response object with claim validation result. May contain additional properties
     * requires for additional external validation.
     * @throws ClaimVerificationException
     */
    ValidationResponse validateClaim(String code, Map<String, String> properties,
                                     boolean isAdditionalValidationRequired) throws ClaimVerificationException;

    /**
     * Used to gracefully end the verification process.
     * Will determine what happened to the temporary claim data based on whether or not validation is successful.
     *
     * @param code                Confirmation code.
     * @param isValidationSuccess Boolean value stating whether or not the validation is successful.
     * @throws ClaimVerificationException
     */
    void confirmVerification(String code, boolean isValidationSuccess) throws ClaimVerificationException;

    /**
     * Used to terminate the claim verification process.
     *
     * @param code Confirmation code.
     * @throws ClaimVerificationException
     */
    void revokeVerification(String code) throws ClaimVerificationException;

    /**
     * Used to retrieve all the available claim verifiers.
     *
     * @return A list with all available claim verifiers.
     * @throws ClaimVerificationException
     */
    List<ClaimVerifier> getAvailableClaimVerifiers() throws ClaimVerificationException;
}
