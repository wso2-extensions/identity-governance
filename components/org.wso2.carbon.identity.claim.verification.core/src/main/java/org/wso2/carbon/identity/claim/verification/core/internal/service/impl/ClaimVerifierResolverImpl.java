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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerifier;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerifierResolver;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.VerificationClaimMetaData.VERIFICATION_METHOD_PROPERTY;

/**
 * The claim verification resolver. This will maintain all the {@link ClaimVerifier} components to resolve against
 * claims.
 */
@Component(
        name = "org.wso2.carbon.identity.claim.verification.resolver.claim",
        immediate = true,
        service = ClaimVerifierResolver.class
)
public class ClaimVerifierResolverImpl implements ClaimVerifierResolver {

    private static final Log LOG = LogFactory.getLog(ClaimVerifierResolverImpl.class);
    private final List<ClaimVerifier> claimVerifiers = new ArrayList<>();
    protected ClaimMetadataManagementService claimMetadataManagementService;

    /**
     * {@inheritDoc}
     */
    @Override
    public int getPriority() {

        return 1;
    }

//    /**
//     * Used to validate the verifiability of the local claim with this resolver.
//     *
//     * @param localClaimUri Id value of the claim.
//     * @return True if the claim is verifiable. False otherwise.
//     * @throws ClaimVerificationException
//     */
//    @Override
//    public boolean isVerifiable(String localClaimUri) throws ClaimVerificationException {
//
//        try {
//            List<LocalClaim> localClaims = claimMetadataManagementService.getLocalClaims(getTenantDomainFromContext());
//
//            for (LocalClaim localClaim : localClaims) {
//                if (localClaim.getClaimURI().equals(localClaimUri)) {
//                    Map<String, String> claimProperties = localClaim.getClaimProperties();
//                    if (claimProperties.keySet().contains(VERIFIABLE_PROPERTY)
//                            && Boolean.parseBoolean(claimProperties.get(VERIFIABLE_PROPERTY))) {
//                        return true;
//                    }
//                    break;
//                }
//            }
//            return false;
//        } catch (ClaimMetadataException e) {
//            String msg = "Error occurred while retrieving local claims for the tenant domain: " + getTenantDomainFromContext();
//            LOG.error(msg, e);
//            throw ClaimVerificationCoreUtils.getClaimVerificationException(
//                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER, e);
//        }
//    }

    /**
     * Used to resolve a claim verifier for the given claim verifier id value, from the available claim verifiers.
     *
     * @param claimVerifierId Id value of the claim verifier.
     * @return A {@link ClaimVerifier} for the given claim.
     * @throws ClaimVerificationException
     */
    @Override
    public ClaimVerifier resolveClaimVerifierId(String claimVerifierId) throws ClaimVerificationException {

        for (ClaimVerifier claimVerifier : claimVerifiers) {
            if (claimVerifier.getId().equals(claimVerifierId)) {
                return claimVerifier;
            }
        }
        String msg = "Could not find a matching claim verifier for the id value: " + claimVerifierId;
        LOG.error(msg);
        throw ClaimVerificationCoreUtils.getClaimVerificationException(
                ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER);
    }

    /**
     * Used to resolve the {@link ClaimVerifier} for the given local claim id value, using pre-configured claim
     * properties.
     *
     * @param localClaimUri Id value of the claim.
     * @return
     * @throws ClaimVerificationException
     */
    @Override
    public ClaimVerifier resolveClaimUri(String localClaimUri) throws ClaimVerificationException {

        try {
            LocalClaim localClaim = ClaimVerificationCoreUtils.getLocalClaimFromService(claimMetadataManagementService,
                    ClaimVerificationCoreUtils.getTenantDomainFromContext(), localClaimUri);

            if (localClaim == null) {
                String msg = "Could not find a matching local claim for the claim uri: " + localClaimUri;
                LOG.error(msg);
                throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                        ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER);
            }

            Map<String, String> claimProperties = localClaim.getClaimProperties();

//            validateLocalClaimProperties(claimProperties, localClaim);
            for (ClaimVerifier claimVerifier : claimVerifiers) {
                if (claimVerifier.getId().equals(claimProperties.get(VERIFICATION_METHOD_PROPERTY))) {
                    return claimVerifier;
                }
            }
            String msg = "Could not find a matching claim verifier for the claim: " + localClaimUri + " with " +
                    "the verification method: " + claimProperties.get(VERIFICATION_METHOD_PROPERTY);
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER);
        } catch (ClaimMetadataException e) {
            String msg = "Error occurred while retrieving the local claim for the tenant domain: " + ClaimVerificationCoreUtils.getTenantDomainFromContext();
            LOG.error(msg, e);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ClaimVerifier> getAvailableClaimVerifiers() {

        return this.claimVerifiers;
    }

    @Reference(
            name = "claim.verifier",
            service = ClaimVerifier.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimVerifier"
    )
    protected void setClaimVerifier(ClaimVerifier claimVerifier) {

        if (claimVerifier != null) {
            this.claimVerifiers.add(claimVerifier);
            if (LOG.isDebugEnabled()) {
                LOG.debug("Claim verifier:" + claimVerifier.getClass().getSimpleName() + " is set in claim " +
                        "verification service.");
            }
        }
    }

    protected void unsetClaimVerifier(ClaimVerifier claimVerifier) {

        this.claimVerifiers.remove(claimVerifier);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Claim verifier:" + claimVerifier.getClass().getSimpleName() + " is unset in claim " +
                    "verification service.");
        }
    }

    @Reference(
            name = "claim.metadata.management.service",
            service = ClaimMetadataManagementService.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetClaimMetadataManagementService"
    )
    protected void setClaimMetadataManagementService(ClaimMetadataManagementService claimMetadataManagementService) {

        if (claimMetadataManagementService != null) {
            this.claimMetadataManagementService = claimMetadataManagementService;
            if (LOG.isDebugEnabled()) {
                LOG.debug("ClaimMetadataManagementService is set in claim " +
                        "verification service.");
            }
        }
    }

    protected void unsetClaimMetadataManagementService(ClaimMetadataManagementService claimMetadataManagementService) {

        this.claimMetadataManagementService = null;
        if (LOG.isDebugEnabled()) {
            LOG.debug("ClaimMetadataManagementService is unset in claim " +
                    "verification service.");
        }
    }

    private void validateLocalClaimProperties(Map<String, String> claimProperties, LocalClaim localClaim)
            throws ClaimVerificationBadRequestException {

        String errMsg = null;

        // Verifiable property can be unavailable in the db, if it has not been configured previously.
        // TODO: 3/11/19 [Review Required] vverification method claims will only be available in
        //  the db once they are used at the first run.
        // It is mandatory to have the property, "verification method" for a verifiable claim.
        if (!claimProperties.keySet().contains(VERIFICATION_METHOD_PROPERTY)) {
            errMsg = "The claim: " + localClaim.getClaimURI() + ", does not contain mandatory property: "
                    + VERIFICATION_METHOD_PROPERTY + " to " +
                    "resolve a claim verifier.";
            LOG.error(errMsg);
        }

        if (errMsg != null) {
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_RESOLVING_CLAIM_VERIFIER);
        }
    }

}
