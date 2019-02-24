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

package org.wso2.carbon.identity.claim.verification.core.store;

import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.ClaimData;
import org.wso2.carbon.identity.claim.verification.core.model.ConfirmationCodeData;
import org.wso2.carbon.identity.claim.verification.core.model.User;

/**
 * Interface for implementing the persistence layer for claim verification.
 */
public interface ClaimVerificationStore {

    /**
     * Used to store confirmation codes.
     *
     * @param codeData ConfirmationCodeData object.
     * @throws ClaimVerificationException
     */
    void storeConfirmationCode(ConfirmationCodeData codeData) throws ClaimVerificationException;

    /**
     * Used to store temporary claim data.
     *
     * @param claimData ClaimData object.
     * @throws ClaimVerificationException
     */
    void storeClaimData(ClaimData claimData) throws ClaimVerificationException;

    /**
     * Used to invalidate confirmation code when the code value is not available.
     *
     * @param codeData ConfirmationCodeData object containing user, scenario and step.
     * @throws ClaimVerificationException
     */
    void invalidateConfirmationCode(ConfirmationCodeData codeData) throws ClaimVerificationException;

    /**
     * Used to invalidate confirmation code with a confirmation code.
     *
     * @param code confirmation code.
     * @throws ClaimVerificationException
     */
    void invalidateConfirmationCode(String code) throws ClaimVerificationException;

    /**
     * Used to update the claim verification status.
     *
     * @param claimData ClaimData object with user, claimId and verificationStatus.
     * @throws ClaimVerificationException
     */
    void updateClaimVerificationStatus(ClaimData claimData) throws ClaimVerificationException;

    /**
     * Used to update claim data.
     *
     * @param claimData ClaimData object.
     * @throws ClaimVerificationException
     */
    void updateClaimData(ClaimData claimData) throws ClaimVerificationException;

    /**
     * Used to remove claim data.
     *
     * @param claimId Claim id.
     * @param user    User related to the claim.
     * @throws ClaimVerificationException
     */
    void clearClaimData(int claimId, User user) throws ClaimVerificationException;

    /**
     * Used to load confirmation code data.
     *
     * @param code Confirmation code.
     * @return ConfirmationCodeData object.
     * @throws ClaimVerificationException
     */
    ConfirmationCodeData loadConfirmationCodeData(String code) throws ClaimVerificationException;

    /**
     * Used to load the claim id.
     *
     * @param claimUri Claim URI for the claim id.
     * @param tenantId Tenant id for the claim id.
     * @return Claim id.
     * @throws ClaimVerificationException
     */
    int loadClaimId(String claimUri, int tenantId) throws ClaimVerificationException;

    /**
     * Used to load the claim URI.
     *
     * @param claimId  Claim id for the claim URI.
     * @param tenantId Tenant id for the claim URI.
     * @return Claim URI.
     * @throws ClaimVerificationException
     */
    String loadClaimUri(int claimId, int tenantId) throws ClaimVerificationException;

    /**
     * Used to load claim data.
     *
     * @param claimId Claim id.
     * @param user    User of the claim.
     * @return ClaimData object.
     * @throws ClaimVerificationException
     */
    ClaimData loadClaimData(int claimId, User user) throws ClaimVerificationException;
}
