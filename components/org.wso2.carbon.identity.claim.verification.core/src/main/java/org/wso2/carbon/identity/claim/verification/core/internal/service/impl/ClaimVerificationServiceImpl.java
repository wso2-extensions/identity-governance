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

package org.wso2.carbon.identity.claim.verification.core.internal.service.impl;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.ClaimData;
import org.wso2.carbon.identity.claim.verification.core.model.ConfirmationCodeData;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.model.ValidationResponse;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerificationService;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerifier;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerifierResolver;
import org.wso2.carbon.identity.claim.verification.core.store.ClaimVerificationStore;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationConfigParser;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ClaimVerificationStatus;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.CodeType;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ErrorMessages;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.PROP_IS_RETRY_ATTEMPT;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.Step;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationConfigs.CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR;

/**
 * The claim verification core. Handles all necessary actions for the claim verification process.
 */
@Component(
        name = "org.wso2.carbon.identity.claim.verification.service",
        immediate = true,
        service = ClaimVerificationService.class
)
public class ClaimVerificationServiceImpl implements ClaimVerificationService {

    private static final Log LOG = LogFactory.getLog(ClaimVerificationServiceImpl.class);
    private static final String CLAIM_VERIFICATION = "CLAIM_VERIFICATION";
    private static final String SCENARIO_APPENDER = "-";
    protected RealmService realmService;
    protected List<ClaimVerifierResolver> claimVerifierResolvers = new ArrayList<>();
    private ClaimVerificationConfigParser claimVerificationConfigParser;

    public ClaimVerificationServiceImpl() {

        initConfigParser();
    }

    @Override
    public List<ClaimVerifier> getAvailableClaimVerifiers() throws ClaimVerificationException {

        return getClaimVerifierResolver().getAvailableClaimVerifiers();
    }

    @Override
    public String initVerification(User user, Claim claim, Map<String, String> properties)
            throws ClaimVerificationException {

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();

        // Validate incoming user data.
        validateUser(user);

        // Claim verification strictly requires incoming claim data to be a local claim.
        int claimId = claimVerificationStore.loadLocalClaimId(claim.getClaimUri(), user.getTenantId());

        // Validate incoming claim data.
        validateClaimData(claim, claimId, user.getTenantId());

        ClaimVerifier claimVerifier = getClaimVerifierByClaimUri(claim.getClaimUri());

        ClaimData claimData = new ClaimData(user, claimId, claim.getClaimValue(), ClaimVerificationStatus.INITIATED);

        // Check whether retry attempt.
        ClaimData loadedClaimData = claimVerificationStore.loadClaimData(claimId, user);
        if (loadedClaimData != null) {

            // Invalidate existing confirmation code data for the retry attempt.
            Enum verificationStatus;
            if (loadedClaimData.getVerificationStatus() == ClaimVerificationStatus.INITIATED) {
                verificationStatus = Step.CLAIM_VALIDATION;
            } else {
                verificationStatus = Step.CLAIM_CONFIRMATION;
            }

            ConfirmationCodeData codeData = new ConfirmationCodeData(user,
                    getScenarioForCodeData(claimId, claimVerifier.getId()), verificationStatus);
            claimVerificationStore.invalidateConfirmationCode(codeData);

            // Update existing claim data for the retry attempt if requires.
            if ((loadedClaimData.getVerificationStatus() !=
                    ClaimVerificationStatus.INITIATED) ||
                    !loadedClaimData.getClaimValue().equals(claimData.getClaimValue())) {
                claimVerificationStore.updateClaimData(claimData);
            }

            // Let claim verifier know this is a retry attempt.
            // Example usage: ClaimVerifier invalidates previous OTPs.
            properties.put(PROP_IS_RETRY_ATTEMPT, "true");

        } else {
            claimVerificationStore.storeClaimData(claimData);
        }

        // Retrieve claim verifier configurations and populate properties if not already overridden in the request.
        populatePropertiesFromConfigFile(properties, claimVerifier);

        // Call claim verifier.
        claimVerifier.sendNotification(user, claim, properties);

        // Generate a confirmation code for the validation scenario and store code data.
        String confirmationCode = ClaimVerificationCoreUtils.getConfirmationCode();
        ConfirmationCodeData codeData = new ConfirmationCodeData(user, confirmationCode,
                getScenarioForCodeData(claimId, claimVerifier.getId()), Step.CLAIM_VALIDATION);
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

        String verifierId = getVerifierId(codeData);
        ClaimVerifier claimVerifier = getClaimVerifierById(verifierId);

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
            String newConfirmationCode = ClaimVerificationCoreUtils.getConfirmationCode();
            ConfirmationCodeData newCodeData = new ConfirmationCodeData(codeData.getUser(), newConfirmationCode,
                    codeData.getScenario(), Step.CLAIM_CONFIRMATION);
            claimVerificationStore.storeConfirmationCode(newCodeData);
            validationResponse.setValidationSuccess(true);
            validationResponse.setVerificationStatus(String.valueOf(ClaimVerificationStatus.PENDING));
            validationResponse.setCode(newConfirmationCode);
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

        String verifierId = getVerifierId(codeData);
        ClaimVerifier claimVerifier = getClaimVerifierById(verifierId);

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

        String verifierId = getVerifierId(codeData);
        ClaimVerifier claimVerifier = getClaimVerifierById(verifierId);

        validateConfirmationCode(codeData, claimVerifier.getConfirmationCodeValidityPeriod(
                getCodeType(codeData.getStep()), codeData.getUser().getTenantId()), null);

        claimVerificationStore.invalidateConfirmationCode(codeData.getCode());

        Claim claim = getClaim(codeData, claimVerificationStore);

        // Call claim verifier.
        claimVerifier.revokeProcess(codeData.getUser(), claim);

        // Remove temporary claim data as process is being terminated.
        claimVerificationStore.clearClaimData(getClaimId(codeData), codeData.getUser());
    }

    protected ClaimVerifierResolver getClaimVerifierResolver() {

        return this.claimVerifierResolvers.get(this.claimVerifierResolvers.size() - 1);
    }

    protected void unsetRealmService(RealmService realmService) {

        this.realmService = null;
        if (LOG.isDebugEnabled()) {
            LOG.debug("RealmService is unset in claim verification service");
        }
    }

    protected RealmService getRealmService() {

        if (this.realmService == null) {
            throw new RuntimeException("RealmService not available. Component is not started properly.");
        }
        return realmService;
    }

    @Reference(
            name = "realm.service",
            service = org.wso2.carbon.user.core.service.RealmService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetRealmService"
    )
    protected void setRealmService(RealmService realmService) {

        this.realmService = realmService;
        if (LOG.isDebugEnabled()) {
            LOG.debug("RealmService is set in claim verification service");
        }
    }

    @Reference(
            name = "claim.verifier.resolver",
            service = ClaimVerifierResolver.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimVerifierResolver"
    )
    protected void setClaimVerifierResolvers(ClaimVerifierResolver claimVerifierResolver) {

        claimVerifierResolvers.add(claimVerifierResolver);
        claimVerifierResolvers.sort(Comparator.comparingInt(ClaimVerifierResolver::getPriority));
        if (LOG.isDebugEnabled()) {
            LOG.debug("ClaimVerifierResolver with priority: " + claimVerifierResolver.getPriority() + " is set in " +
                    "claim verification service");
        }
    }

    protected void unsetClaimVerifierResolver(ClaimVerifierResolver claimVerifierResolver) {

        this.claimVerifierResolvers.remove(claimVerifierResolver);
        if (LOG.isDebugEnabled()) {
            LOG.debug("ClaimVerifierResolver with priority: " + claimVerifierResolver.getPriority() + " is unset " +
                    "in claim verification service");
        }
    }

    private void populatePropertiesFromConfigFile(Map<String, String> properties, ClaimVerifier claimVerifier) {

        Map<String, Object> claimVerifierConfigs = claimVerificationConfigParser.getClaimVerifierConfigs(
                ClaimVerificationCoreUtils.getTenantDomainFromContext(), claimVerifier.getId());

        claimVerifierConfigs.forEach((key, val) -> {
            String configName =
                    key.substring(getConfigNameStartIndex(key));

            // Properties can override config file configurations.
            if (!properties.keySet().contains(configName)) {
                if (val instanceof String) {
                    properties.put(configName, (String) val);
                    LOG.debug("Added the new config value: " + val + "for the configuration: " + key);
                }
                LOG.debug("Skip adding non-string config value: " + val + "for the configuration: " + key);
            }
        });
    }

    private int getConfigNameStartIndex(String key) {

        // ::{configName} => Index of the first character is the key.lastIndexOf(domain)
        return key.lastIndexOf(CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR)
                + (CLAIM_VERIFICATION_CONFIG_DOMAIN_SEPARATOR.length());
    }

    private void initConfigParser() {

        claimVerificationConfigParser = new ClaimVerificationConfigParser();
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
                    LOG.debug("Invalid code. Step verification failed. code: " + codeData.getCode());
                }
                throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                        ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
            }
        } else if (step != codeData.getStep()) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Invalid code. Step verification failed. code: " + codeData.getCode());
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_INVALID_CONFIRMATION_CODE);
        }

        if (codeValidityPeriod < 0) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Code: " + codeData.getCode() + " has an unlimited validity period.");
            }
            return;
        }

        long createdTimeStamp = codeData.getTimeCreated().getTime();
        long expiryTime = createdTimeStamp + TimeUnit.MINUTES.toMillis(codeValidityPeriod);

        if (System.currentTimeMillis() > expiryTime) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Expired code. code: " + codeData.getCode());
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_EXPIRED_CONFIRMATION_CODE);
        }
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
        claim.setClaimUri(claimVerificationStore.loadLocalClaimUri(claimId, codeData.getUser().getTenantId()));
        claim.setClaimValue(claimVerificationStore.loadClaimData(claimId, codeData.getUser()).getClaimValue());

        return claim;
    }

    /**
     * Used to get the claim verifier by properties.
     *
     * @return ClaimVerifier.
     * @throws ClaimVerificationException
     */
    private ClaimVerifier getClaimVerifierByClaimUri(String localClaimUri)
            throws ClaimVerificationException {

        return getClaimVerifierResolver().resolveClaimUri(localClaimUri);
    }

    /**
     * Used to get the claim verifier by the id.
     *
     * @return ClaimVerifier.
     * @throws ClaimVerificationException
     */
    private ClaimVerifier getClaimVerifierById(String id) throws ClaimVerificationException {

        return getClaimVerifierResolver().resolveClaimVerifierId(id);
    }

    /**
     * Validate the user exists.
     *
     * @param user User.
     * @throws ClaimVerificationException
     */
    private void validateUser(User user) throws ClaimVerificationException {

        String[] userList = ClaimVerificationCoreUtils.getUserList(user.getTenantId(),
                IdentityUtil.addDomainToName(user.getUsername(), user.getRealm()), getRealmService());

        if (ArrayUtils.isEmpty(userList)) {
            String msg = "Unable to find an user for username: " + user.getUsername()
                    + ", userStore: " + user.getRealm() + "and tenantId: " + user.getTenantId();
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_NO_MATCHING_USER_FOUND);

        } else if (userList.length == 1 && StringUtils.isBlank(user.getRealm())) {
            user.setRealm(IdentityUtil.extractDomainFromName(userList[0]));
        } else if (userList.length > 1) {
            String msg =
                    "Found multiple users for username: " + user.getUsername() + ", userStore: " + user.getRealm() +
                            "and tenantId: " + user.getTenantId();
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_MULTIPLE_MATCHING_USERS_FOUND);
        }

        if (LOG.isDebugEnabled()) {
            String msg = "Found user for username: " + user.getUsername() + ", userStore: " + user.getRealm() + "and " +
                    "tenantId: " + user.getTenantId();
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
            String msg =
                    "Unable to find a local claim with claim uri: " + claim.getClaimUri() + " and tenantId: " + tenantId;
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_NO_MATCHING_LOCAL_CLAIM_FOUND);
        }
    }

    /**
     * Used to get the scenario string related to the confirmation code.
     * String format: CLAIM_VERIFICATION-claimId-verifierId
     *
     * @param claimId    Claim id.
     * @param verifierId Verification method.
     * @return
     */
    private String getScenarioForCodeData(int claimId, String verifierId) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verifierId>
        return CLAIM_VERIFICATION + SCENARIO_APPENDER + claimId + SCENARIO_APPENDER + verifierId;
    }

    /**
     * Used to get the id value of the verifier used for verification, from the confirmation code.
     *
     * @param codeData ConfirmationCodeData.
     * @return Identifier value of the verifier.
     */
    private String getVerifierId(ConfirmationCodeData codeData) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verifierId>
        return codeData.getScenario().split(SCENARIO_APPENDER, 3)[2];
    }

    /**
     * Used to get the claim id from the confirmation code.
     *
     * @param codeData ConfirmationCodeData.
     * @return Claim id.
     */
    private int getClaimId(ConfirmationCodeData codeData) {

        // scenario -> CLAIM_VERIFICATION-<claimId>-<verifierId>
        String claimId = codeData.getScenario().split(SCENARIO_APPENDER, 3)[1];
        return Integer.parseInt(claimId);
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
                ClaimVerificationCoreUtils.getUserStoreManager(codeData.getUser().getTenantId(), getRealmService());
        String usernameWithRealm = IdentityUtil.addDomainToName(codeData.getUser().getUsername(),
                codeData.getUser().getRealm());
        String claimUri = claimVerificationStore.loadLocalClaimUri(
                getClaimId(codeData), codeData.getUser().getTenantId());
        ClaimData claimData = claimVerificationStore.loadClaimData(getClaimId(codeData), codeData.getUser());
        try {
            // Set the user claim value using the default profile.
            userStoreManager.setUserClaimValue(usernameWithRealm, claimUri, claimData.getClaimValue(), null);
        } catch (UserStoreException e) {
            String msg = "Error when adding claim to userstore for user: " + codeData.getUser().getUsername() +
                    " in userStore: " + codeData.getUser().getRealm() + "in tenantId: " + codeData.getUser().getTenantId()
                    + " and claimUri: " + claimUri;
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

        String msg = "Invalid step received to get codeType. step: " + String.valueOf(step);
        LOG.error(msg);
        throw ClaimVerificationCoreUtils.getClaimVerificationException(ErrorMessages.ERROR_MSG_UNEXPECTED_ERROR);
    }
}
