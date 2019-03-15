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
// TODO: 3/6/19 [Review Required] Changed package name from "emailclaimverifier" to "email"
package org.wso2.carbon.identity.claim.verification.core.internal.service.impl.email;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.osgi.service.component.annotations.ReferencePolicy;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants;
import org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.service.ClaimVerifier;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.CodeType;
import static org.wso2.carbon.identity.claim.verification.core.constant.ClaimVerificationCoreConstants.ErrorMessages;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.ClaimVerifierConfig.EMAIL_VERIFIER_PROPERTY_TEMPLATE;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.ClaimVerifierConfig.EMAIL_VERIFIER_PROPERTY_VALIDATION_URL;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.ConnectorConfig;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.MessageParameter.EMAIL_VERIFIER_PARAMETER_NONCE_VALUE;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_NONCE_VALUE;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_TEMPLATE_TYPE;
import static org.wso2.carbon.identity.claim.verification.core.constant.EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_VALIDATION_URL;

/**
 * Claim verifier for email claims.
 */
@Component(
        name = "org.wso2.carbon.identity.claim.verification.verifier.email",
        immediate = true,
        service = ClaimVerifier.class
)
public class EmailClaimVerifier implements ClaimVerifier {

    private static final Log LOG = LogFactory.getLog(EmailClaimVerifier.class);

    private final String ID = "EmailClaimVerifier";

    private final String CLAIM_PROPERTY_DISPLAY_NAME = "DisplayName";

    private IdentityEventService identityEventService;
    private IdentityGovernanceService identityGovernanceService;
    private RealmService realmService;
    private ClaimMetadataManagementService claimMetadataManagementService;

    @Override
    public String getId() {

        return ID;
    }

    @Override
    public void sendNotification(User user, Claim claim, Map<String, String> properties) throws
            ClaimVerificationException {

        verifyRequiredPropertyExists(properties, EMAIL_VERIFIER_PARAMETER_NONCE_VALUE);
        verifyRequiredPropertyExists(properties, EMAIL_VERIFIER_PROPERTY_TEMPLATE);
        verifyRequiredPropertyExists(properties, EMAIL_VERIFIER_PROPERTY_VALIDATION_URL);

        try {
            String claimName = getLocalClaimDisplayName(user, claim);

            // Build email notification.
            LOG.debug("Building the e-mail notification.");
            Map<String, Object> notificationProps = new HashMap<>();
            notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER,
                    ClaimVerificationCoreUtils.getUserStoreManager(user.getTenantId(), getRealmService()));
            notificationProps.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUsername());
            notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getRealm());
            notificationProps.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN,
                    IdentityTenantUtil.getTenantDomain(user.getTenantId()));
            notificationProps.put(EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_SEND_TO, claim.getClaimValue());
            notificationProps.put(EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_CLAIM_NAME, claimName);
            notificationProps.put(EmailClaimVerifierConstants.NotificationProperty.NOTIFICATION_PROPERTY_CLAIM_VALUE, claim.getClaimValue());

            // Naming consistence for event notifications and verifier configurations.
            notificationProps.put(
                    NOTIFICATION_PROPERTY_TEMPLATE_TYPE, properties.get(EMAIL_VERIFIER_PROPERTY_TEMPLATE));
            notificationProps.put(
                    NOTIFICATION_PROPERTY_VALIDATION_URL, properties.get(EMAIL_VERIFIER_PROPERTY_VALIDATION_URL));
            notificationProps.put(
                    NOTIFICATION_PROPERTY_NONCE_VALUE, properties.get(EMAIL_VERIFIER_PARAMETER_NONCE_VALUE));

            Event identityMgtEvent = new Event(IdentityEventConstants.Event.TRIGGER_NOTIFICATION, notificationProps);
            try {
                LOG.debug("Sending the e-mail notification.");
                getIdentityEventService().handleEvent(identityMgtEvent);
            } catch (IdentityEventException e) {
                String msg = "Error occurred while sending email-claim verification email to: " + user.getUsername();
                LOG.error(msg, e);
                throw ClaimVerificationCoreUtils.getClaimVerificationException(
                        ErrorMessages.ERROR_MSG_SENDING_NOTIFICATION, e);
            }
        } catch (ClaimMetadataException e) {
            String msg = "Error occurred while retrieving the local claim for the tenant domain: "
                    + getTenantDomainFromId(user.getTenantId());
            LOG.error(msg, e);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_SENDING_NOTIFICATION, e);
        }
    }

    @Override
    public boolean isVerified(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException {

        return true;
    }

    @Override
    public void revokeProcess(User user, Claim claim) throws ClaimVerificationException {

    }

    @Override
    public int getConfirmationCodeValidityPeriod(String codeType, int tenantId) throws ClaimVerificationException {

        if (CodeType.VALIDATION.equals(codeType)) {
            return getCodeValidityPeriod(tenantId, ConnectorConfig.VALIDATION_STEP_CODE_EXPIRY_TIME);
        } else {
            return getCodeValidityPeriod(tenantId, ConnectorConfig.CONFIRMATION_STEP_EXPIRY_TIME);
        }
    }

    protected void unsetIdentityEventService(IdentityEventService identityEventService) {

        this.identityEventService = null;
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityEventService is unset in claim verification service");
        }
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {

        this.identityGovernanceService = null;
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityGovernanceService is unset in claim verification service");
        }
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

    protected IdentityEventService getIdentityEventService() {

        if (this.identityEventService == null) {
            throw new RuntimeException("IdentityEventService not available. Component is not started properly.");
        }
        return this.identityEventService;
    }

    @Reference(
            name = "IdentityEventService",
            service = org.wso2.carbon.identity.event.services.IdentityEventService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityEventService"
    )
    protected void setIdentityEventService(IdentityEventService identityEventService) {

        this.identityEventService = identityEventService;
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityEventService is set in claim verification service");
        }
    }

    protected IdentityGovernanceService getIdentityGovernanceService() {

        if (this.identityGovernanceService == null) {
            throw new RuntimeException("IdentityGovernanceService not available. Component is not started properly.");
        }
        return this.identityGovernanceService;
    }

    @Reference(
            name = "IdentityGovernanceService",
            service = org.wso2.carbon.identity.governance.IdentityGovernanceService.class,
            cardinality = ReferenceCardinality.MANDATORY,
            policy = ReferencePolicy.DYNAMIC,
            unbind = "unsetIdentityGovernanceService"
    )
    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {

        this.identityGovernanceService = idpManager;
        if (LOG.isDebugEnabled()) {
            LOG.debug("IdentityGovernanceService is set in claim verification service");
        }
    }

    protected void unsetClaimMetadataManagementService(ClaimMetadataManagementService claimMetadataManagementService) {

        this.claimMetadataManagementService = null;
        if (LOG.isDebugEnabled()) {
            LOG.debug("ClaimMetadataManagementService is unset in claim " +
                    "verification service.");
        }
    }

    protected ClaimMetadataManagementService getClaimMetadataManagementService() {

        if (this.claimMetadataManagementService == null) {
            throw new RuntimeException("ClaimMetadataManagementService not available. Component is not started " +
                    "properly.");
        }
        return this.claimMetadataManagementService;
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

    private String getLocalClaimDisplayName(User user, Claim claim)
            throws ClaimMetadataException, ClaimVerificationBadRequestException {

        LocalClaim localClaim =
                ClaimVerificationCoreUtils.getLocalClaimFromService(getClaimMetadataManagementService(),
                        getTenantDomainFromId(user.getTenantId()), claim.getClaimUri());

        if (localClaim == null) {
            String msg = "Could not find a matching local claim for the claim uri: " + claim.getClaimUri();
            LOG.error(msg);
            throw ClaimVerificationCoreUtils.getClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_SENDING_NOTIFICATION);
        }

        return localClaim.getClaimProperties().get(CLAIM_PROPERTY_DISPLAY_NAME);
    }

    /**
     * Used to verify that a required property exists, if it does not, throws an exception.
     *
     * @param properties Properties map.
     * @param property   Property to be checked.
     * @throws ClaimVerificationBadRequestException
     */
    private void verifyRequiredPropertyExists(Map<String, String> properties, String property) throws
            ClaimVerificationBadRequestException {

        if (!properties.containsKey(property) || StringUtils.isBlank(properties.get(property))) {
            String msg = "Required property not found. Property: " + property;
            if (LOG.isDebugEnabled()) {
                LOG.debug(msg);
            }
            throw new ClaimVerificationBadRequestException(
                    ErrorMessages.ERROR_MSG_REQUIRED_PROPERTY_NOT_FOUND.getCode(), msg);
        }
    }

    /**
     * Used to get the confirmation code validity period from the configuration connector.
     *
     * @param tenantId     Tenant id.
     * @param confProperty Configuration property.
     * @return The confirmation code validity period in minutes.
     * @throws ClaimVerificationException
     */
    private int getCodeValidityPeriod(int tenantId, String confProperty) throws ClaimVerificationException {

        IdentityGovernanceService identityGovernanceService = getIdentityGovernanceService();
        try {
            Property[] properties = identityGovernanceService.getConfiguration(new String[]{confProperty},
                    IdentityTenantUtil.getTenantDomain(tenantId));

            if (ArrayUtils.isEmpty(properties)) {
                String msg = "Required property:" + confProperty + " not found in configuration.";
                LOG.error(msg);
                throw ClaimVerificationCoreUtils.getClaimVerificationException(
                        ErrorMessages.ERROR_MSG_READING_CONFIGURATION);
            }
            String codeValidityPeriod = properties[0].getValue();
            return Integer.parseInt(codeValidityPeriod);
        } catch (IdentityGovernanceException e) {
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_READING_CONFIGURATION, e);
        }
    }

    /**
     * Used to retrieve tenant domain value from the given tenant id.
     *
     * @param tenantId Tenant Id.
     * @return Corresponding tenant domain value for the tenant id.
     */
    private String getTenantDomainFromId(int tenantId) {

        return IdentityTenantUtil.getTenantDomain(tenantId);
    }
}
