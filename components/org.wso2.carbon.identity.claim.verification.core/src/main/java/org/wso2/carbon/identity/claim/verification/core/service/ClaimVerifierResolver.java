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

import java.util.List;

/**
 * Interface for implementing resolvers to resolve a {@link ClaimVerifier} to a given local claim.
 */
public interface ClaimVerifierResolver {

    /**
     * Used to get the priority value of the {@link ClaimVerifierResolver}.
     *
     * @return Priority value of the {@link ClaimVerifierResolver}.
     */
    int getPriority();

//    /**
//     * Used to validate the verifiability of the local claim with the {@link ClaimVerifierResolver}.
//     *
//     * @param localClaimUri Id value of the claim.
//     * @return True if the claim is verifiable. False otherwise.
//     * @throws ClaimVerificationException
//     */
//    boolean isVerifiable(String localClaimUri) throws ClaimVerificationException;

    /**
     * Used to resolve a claim verifier for the given local claim, from the available claim verifiers.
     *
     * @param localClaimUri Id value of the claim.
     * @return A {@link ClaimVerifier} for the given claim.
     * @throws ClaimVerificationException
     */
    ClaimVerifier resolveClaimUri(String localClaimUri) throws ClaimVerificationException;

    /**
     * Used to resolve a claim verifier for the given claim verifier id value, from the available claim verifiers.
     *
     * @param claimVerifierId Id value of the claim verifier.
     * @return A {@link ClaimVerifier} for the given claim.
     * @throws ClaimVerificationException
     */
    ClaimVerifier resolveClaimVerifierId(String claimVerifierId) throws ClaimVerificationException;

    /**
     * Used to get all the available {@link ClaimVerifier} components.
     *
     * @return A list of available {@link ClaimVerifier} components.
     */
    List<ClaimVerifier> getAvailableClaimVerifiers();
}
