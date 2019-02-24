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

package org.wso2.carbon.identity.claim.verification.core;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.internal.ClaimVerificationServiceDataHolder;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.ClaimData;
import org.wso2.carbon.identity.claim.verification.core.model.ConfirmationCodeData;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.model.ValidationResponse;
import org.wso2.carbon.identity.claim.verification.core.store.ClaimVerificationStore;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;
import org.wso2.carbon.identity.claim.verification.core.verifier.ClaimVerifier;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.ErrorMessages;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.PROP_IS_RETRY_ATTEMPT;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.Step;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.ClaimVerificationStatus;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.CodeType;

/**
 * The claim verification core. Handles all necessary actions for the claim verification process.
 */
public class ClaimVerificationHandlerImpl implements ClaimVerificationHandler {

    private static final Log LOG = LogFactory.getLog(ClaimVerificationHandlerImpl.class);
    private static final String CLAIM_VERIFICATION = "CLAIM_VERIFICATION";
    private static final String SCENARIO_APPENDER = "-";

    @Override
    public String initVerification(User user, Claim claim, String verificationMethod, Map<String, String> properties)
            throws ClaimVerificationException {

        ClaimVerifier claimVerifier = getClaimVerifier(verificationMethod);
        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();

        // Validate incoming user data.
        validateUser(user);

        int claimId = claimVerificationStore.loadClaimId(claim.getClaimUri(), user.getTenantId());
        org.wso2.carbon.user.api.Claim claimMetaData =
                ClaimVerificationCoreUtils.getClaimMetaData(user.getTenantId(), claim.getClaimUri());

        // Validate incoming claim data.
        validateClaimData(claim, claimId, user.getTenantId());

        ClaimData claimData = new ClaimData(user, claimId, claim.getClaimValue(), ClaimVerificationStatus.INITIATED);

        // Check whether retry attempt.
        ClaimData loadedClaimData = claimVerificationStore.loadClaimData(claimId, user);
        if (loadedClaimData != null) {

            Enum verificationStatus;
            if (loadedClaimData.getVerificationStatus() == ClaimVerificationStatus.INITIATED) {
                verificationStatus = Step.CLAIM_VALIDATION;
            } else {
                verificationStatus = Step.CLAIM_CONFIRMATION;
            }

            ConfirmationCodeData codeData = new ConfirmationCodeData(user,
                    getScenarioForCodeData(claimId, verificationMethod), verificationStatus);
            claimVerificationStore.invalidateConfirmationCode(codeData);

            if ((loadedClaimData.getVerificationStatus() !=
                    ClaimVerificationStatus.INITIATED) ||
                    loadedClaimData.getClaimValue().equals(claimData.getClaimValue())) {
                claimVerificationStore.updateClaimData(claimData);
            }

            // Let claim verifier know this is a retry attempt.
            // Example usage: ClaimVerifier invalidates previous OTPs.
            properties.put(PROP_IS_RETRY_ATTEMPT, "true");

        } else {
            claimVerificationStore.storeClaimData(claimData);
        }

        // Call claim verifier.
        claimVerifier.sendNotification(user, claim, properties);

        String confirmationCode = ClaimVerificationCoreUtils.getConfirmationCode();
        ConfirmationCodeData codeData = new ConfirmationCodeData(user, confirmationCode,
                getScenarioForCodeData(claimId, verificationMethod), Step.CLAIM_VALIDATION);
        claimVerificationStore.storeConfirmationCode(codeData);

        return confirmationCode;
    }

    @Override
    public ValidationResponse validateClaim(String code, Map<String, String> properties)
            throws ClaimVerificationException {

        return validateClaim(code, properties, false);
    }

    @Override
    public ValidationResponse validateClaim(String code, Map<String, String> properties,
                                            boolean isAdditionalValidationRequired) throws ClaimVerificationException {

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();
        ConfirmationCodeData codeData = claimVerificationStore.loadConfirmationCodeData(code);

        if (codeData == null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("No code data received.");
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
        }

        String verificationMethod = getVerificationMethod(codeData);
        ClaimVerifier claimVerifier = getClaimVerifier(verificationMethod);

        validateConfirmationCode(codeData, claimVerifier.getConfirmationCodeValidityPeriod(
                CodeType.VALIDATION, codeData.getUser().getTenantId()), Step.CLAIM_VALIDATION);

        claimVerificationStore.invalidateConfirmationCode(codeData.getCode());

        Claim claim = getClaim(codeData, claimVerificationStore);
        // Call claim verifier.
        boolean isVerified = claimVerifier.isVerified(codeData.getUser(), claim, properties);
        ValidationResponse validationResponse = new ValidationResponse();

        int claimId = getClaimId(codeData);
        if (!isVerified) {
            // Remove temporary claim data as claim verification failed.
            claimVerificationStore.clearClaimData(claimId, codeData.getUser());
            validationResponse.setValidationSuccess(false);
            validationResponse.setVerificationStatus(String.valueOf(ClaimVerificationStatus.FAILED));
            return validationResponse;
        } else if (isAdditionalValidationRequired) {
            ClaimData claimData = new ClaimData(codeData.getUser(), claimId, ClaimVerificationStatus.PENDING);
            claimVerificationStore.updateClaimVerificationStatus(claimData);
            String confirmationCode = ClaimVerificationCoreUtils.getConfirmationCode();
            ConfirmationCodeData newCodeData = new ConfirmationCodeData(codeData.getUser(), confirmationCode,
                    codeData.getScenario(), Step.CLAIM_CONFIRMATION);
            claimVerificationStore.storeConfirmationCode(newCodeData);
            validationResponse.setValidationSuccess(true);
            validationResponse.setVerificationStatus(String.valueOf(ClaimVerificationStatus.PENDING));
            validationResponse.setCode(confirmationCode);
            return validationResponse;
        } else {
            // Add claim data to user store as claim verification is successful.
            addClaimDataToUserStore(codeData, claimVerificationStore);
            // Remove temporary claim data.
            claimVerificationStore.clearClaimData(getClaimId(codeData), codeData.getUser());
            validationResponse.setValidationSuccess(true);
            validationResponse.setVerificationStatus(String.valueOf(ClaimVerificationStatus.SUCCESSFUL));
            return validationResponse;
        }
    }

    @Override
    public void confirmVerification(String code, boolean isValidationSuccess) throws ClaimVerificationException {

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();
        ConfirmationCodeData codeData = claimVerificationStore.loadConfirmationCodeData(code);

        if (codeData == null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("No code data received.");
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
        }

        String verificationMethod = getVerificationMethod(codeData);
        ClaimVerifier claimVerifier = getClaimVerifier(verificationMethod);

        validateConfirmationCode(codeData, claimVerifier.getConfirmationCodeValidityPeriod(CodeType.CONFIRMATION,
                codeData.getUser().getTenantId()), Step.CLAIM_CONFIRMATION);

        claimVerificationStore.invalidateConfirmationCode(codeData.getCode());
        if (isValidationSuccess) {
            addClaimDataToUserStore(codeData, claimVerificationStore);
        }
        claimVerificationStore.clearClaimData(getClaimId(codeData), codeData.getUser());
    }

    @Override
    public void revokeVerification(String code) throws ClaimVerificationException {

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();
        ConfirmationCodeData codeData = claimVerificationStore.loadConfirmationCodeData(code);

        if (codeData == null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("No code data received.");
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
        }

        String verificationMethod = getVerificationMethod(codeData);
        ClaimVerifier claimVerifier = getClaimVerifier(verificationMethod);

        validateConfirmationCode(codeData, claimVerifier.getConfirmationCodeValidityPeriod(
                getCodeType(codeData.getStep()), codeData.getUser().getTenantId()), null);

        claimVerificationStore.invalidateConfirmationCode(codeData.getCode());

        Claim claim = getClaim(codeData, claimVerificationStore);
        // Call claim verifier.
        claimVerifier.revokeProcess(codeData.getUser(), claim);

        // Remove temporary claim data as process is being terminated.
        claimVerificationStore.clearClaimData(getClaimId(codeData), codeData.getUser());
    }

    /**
     * Used to get claim details from the confirmation code.
     *
     * @param codeData               ConfirmationCodeData.
     * @param claimVerificationStore ClaimVerificationStore.
     * @return Claim object.
     * @throws ClaimVerificationException
     */
    private Claim getClaim(ConfirmationCodeData codeData, ClaimVerificationStore claimVerificationStore)
            throws ClaimVerificationException {

        int claimId = getClaimId(codeData);
        Claim claim = new Claim();
        claim.setClaimUri(claimVerificationStore.loadClaimUri(claimId, codeData.getUser().getTenantId()));
        claim.setClaimValue(claimVerificationStore.loadClaimData(claimId, codeData.getUser()).getClaimValue());

        return claim;
    }

    /**
     * Used to get the claim verifier.
     *
     * @param verificationMethod Identifier for the claim verifier to be used for claim verification.
     * @return ClaimVerifier.
     * @throws ClaimVerificationException
     */
    private ClaimVerifier getClaimVerifier(String verificationMethod) throws ClaimVerificationException {

        List<ClaimVerifier> claimVerifiers = ClaimVerificationServiceDataHolder.getInstance().getClaimVerifiers();

        for (ClaimVerifier claimVerifier : claimVerifiers) {
            if (claimVerifier.canHandle(verificationMethod)) {
                return claimVerifier;
            }
        }
        throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                ErrorMessages.ERROR_MSG_NO_MATCHING_CLAIM_VERIFIER_FOUND);
    }

    /**
     * Validate the user exists.
     *
     * @param user User.
     * @throws ClaimVerificationException
     */
    private void validateUser(User user) throws ClaimVerificationException {

        String[] userList = ClaimVerificationCoreUtils.getUserList(user.getTenantId(),
                IdentityUtil.addDomainToName(user.getUsername(), user.getRealm()));

        if (ArrayUtils.isEmpty(userList)) {
            String msg = "Unable to find an user for username: " + user.getUsername()
                    + ", userStore:" + user.getRealm() + "and tenantId:" + user.getTenantId();
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_NO_MATCHING_USER_FOUND);

        } else if (userList.length == 1 && StringUtils.isBlank(user.getRealm())) {
            user.setRealm(IdentityUtil.extractDomainFromName(userList[0]));
        } else if (userList.length > 1) {
            String msg =
                    "Found multiple users for username: " + user.getUsername() + ", userStore:" + user.getRealm() +
                            "and tenantId:" + user.getTenantId();
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_MULTIPLE_MATCHING_USERS_FOUND);
        }

        if (LOG.isDebugEnabled()) {
            String msg = "Found user for username: " + user.getUsername() + ", userStore:" + user.getRealm() + "and " +
                    "tenantId:" + user.getTenantId();
            LOG.debug(msg);
        }
    }

    /**
     * Validate the claim uri exists.
     *
     * @param claim    Claim.
     * @param claimId  Claim id.
     * @param tenantId Tenant id.
     * @throws ClaimVerificationException
     */
    private void validateClaimData(Claim claim, int claimId, int tenantId) throws ClaimVerificationException {

        if (claimId == -1) {
            String msg = "Unable to find a claim with claim uri: " + claim.getClaimUri() + " and tenantId:" + tenantId;
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_NO_MATCHING_CLAIM_FOUND);
        }
    }

    /**
     * Used to get the scenario string related to the confirmation code.
     * String format: CLAIM_VERIFICATION-claimId-verificationMethod
     *
     * @param claimId            Claim id.
     * @param verificationMethod Verification method.
     * @return
     */
    private String getScenarioForCodeData(int claimId, String verificationMethod) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verificationMethod>
        return CLAIM_VERIFICATION + SCENARIO_APPENDER + claimId + SCENARIO_APPENDER + verificationMethod;
    }

    /**
     * Used to get the verification method from the confirmation code.
     *
     * @param codeData ConfirmationCodeData.
     * @return Verification method.
     */
    private String getVerificationMethod(ConfirmationCodeData codeData) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verificationMethod>
        return codeData.getScenario().split(SCENARIO_APPENDER, 3)[2];
    }

    /**
     * Used to get the claim id from the confirmation code.
     *
     * @param codeData ConfirmationCodeData.
     * @return Claim id.
     */
    private int getClaimId(ConfirmationCodeData codeData) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verificationMethod>
        String claimId = codeData.getScenario().split(SCENARIO_APPENDER, 3)[1];
        return Integer.parseInt(claimId);
    }

    /**
     * Used to validate the received confirmation code.
     *
     * @param codeData           ConfirmationCodeData
     * @param codeValidityPeriod Confirmation code validity period in minutes.
     * @param step               The step the confirmation code was issued for.
     * @throws ClaimVerificationBadRequestException
     */
    private static void validateConfirmationCode(ConfirmationCodeData codeData, int codeValidityPeriod, Step step)
            throws ClaimVerificationBadRequestException {

        if (step == null) {
            if (codeData.getStep() != Step.CLAIM_VALIDATION
                    && codeData.getStep() != Step.CLAIM_CONFIRMATION) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Invalid code. Step verification failed. code:" + codeData.getCode());
                }
                throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                        ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
            }
        } else if (step != codeData.getStep()) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Invalid code. Step verification failed. code:" + codeData.getCode());
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
        }

        if (codeValidityPeriod < 0) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Code:" + codeData.getCode() + "has an unlimited validity period.");
            }
            return;
        }

        long createdTimeStamp = codeData.getTimeCreated().getTime();
        long expiryTime = createdTimeStamp + TimeUnit.MINUTES.toMillis(codeValidityPeriod);

        if (System.currentTimeMillis() > expiryTime) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Expired code. code:" + codeData.getCode());
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_EXPIRED_CONFIRMATION_CODE);
        }
    }

    /**
     * Used to move the temporary claim data to the user store.
     *
     * @param codeData               ConfirmationCodeData.
     * @param claimVerificationStore ClaimVerificationStore.
     * @throws ClaimVerificationException
     */
    private void addClaimDataToUserStore(ConfirmationCodeData codeData, ClaimVerificationStore claimVerificationStore)
            throws ClaimVerificationException {

        UserStoreManager userStoreManager =
                ClaimVerificationCoreUtils.getUserStoreManager(codeData.getUser().getTenantId());
        String usernameWithRealm = IdentityUtil.addDomainToName(codeData.getUser().getUsername(),
                codeData.getUser().getRealm());
        String claimUri = claimVerificationStore.loadClaimUri(getClaimId(codeData), codeData.getUser().getTenantId());
        ClaimData claimData = claimVerificationStore.loadClaimData(getClaimId(codeData), codeData.getUser());
        try {
            userStoreManager.setUserClaimValue(usernameWithRealm, claimUri, claimData.getClaimValue(), null);
        } catch (UserStoreException e) {
            String msg = "Error when adding claim to userstore for user:" + codeData.getUser().getUsername() +
                    " in userStore:" + codeData.getUser().getRealm() + "in tenantId:" + codeData.getUser().getTenantId()
                    + " and claimUri:" + claimUri;
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_ADD_CLAIM_TO_USER_STORE, e);
        }
    }

    /**
     * Used to get confirmation code type for the relevant step, validation or confirmation.
     *
     * @param step Step.
     * @return Code type.
     * @throws ClaimVerificationException
     */
    private String getCodeType(Enum step) throws ClaimVerificationException {

        if (step == Step.CLAIM_VALIDATION) {
            return CodeType.VALIDATION;
        } else if (step == Step.CLAIM_CONFIRMATION) {
            return CodeType.CONFIRMATION;
        }

        String msg = "Invalid step received to get codeType. step:" + String.valueOf(step);
        LOG.error(msg);
        throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_UNEXPECTED_ERROR);
    }
}
