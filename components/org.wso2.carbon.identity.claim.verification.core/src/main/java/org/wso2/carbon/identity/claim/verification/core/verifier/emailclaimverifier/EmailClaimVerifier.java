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

package org.wso2.carbon.identity.claim.verification.core.verifier.emailclaimverifier;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationBadRequestException;
import org.wso2.carbon.identity.claim.verification.core.exception.ClaimVerificationException;
import org.wso2.carbon.identity.claim.verification.core.internal.ClaimVerificationServiceDataHolder;
import org.wso2.carbon.identity.claim.verification.core.model.Claim;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreUtils;
import org.wso2.carbon.identity.claim.verification.core.verifier.ClaimVerifier;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import java.util.HashMap;
import java.util.Map;

import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.CodeType;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.ConnectorConfig;
import static org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants.ErrorMessages;

/**
 * Claim verifier for email claims.
 */
public class EmailClaimVerifier implements ClaimVerifier {

    private static final Log LOG = LogFactory.getLog(EmailClaimVerifier.class);

    private final String IDENTIFIER = "EmailClaimVerifier";

    private final String PROPERTY_SEND_TO = "send-to";
    private final String PROPERTY_NONCE_VALUE = "nonce-value";
    private final String PROPERTY_CLAIM_NAME = "claim-name";
    private final String PROPERTY_CLAIM_VALUE = "claim-value";
    private final String PROPERTY_VALIDATION_URL = "validation-url";
    private final String PROPERTY_TEMPLATE_TYPE = "TEMPLATE_TYPE";
    private final String PROPERTY_TEMPLATE_TYPE_VALUE = "emailVerification";

    @Override
    public void sendNotification(User user, Claim claim, Map<String, String> properties) throws
            ClaimVerificationException {

        verifyRequiredPropertyExists(properties, PROPERTY_VALIDATION_URL);
        verifyRequiredPropertyExists(properties, PROPERTY_NONCE_VALUE);

        String claimName = ClaimVerificationCoreUtils.getClaimMetaData(user.getTenantId(), claim.getClaimUri())
                .getDisplayTag();

        // Build email notification.
        Map<String, Object> notificationProps = new HashMap<>();
        notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER,
                ClaimVerificationCoreUtils.getUserStoreManager(user.getTenantId()));
        notificationProps.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUsername());
        notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getRealm());
        notificationProps.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN,
                IdentityTenantUtil.getTenantDomain(user.getTenantId()));
        notificationProps.put(PROPERTY_SEND_TO, claim.getClaimValue());
        notificationProps.put(PROPERTY_CLAIM_NAME, claimName);
        notificationProps.put(PROPERTY_NONCE_VALUE, properties.get(PROPERTY_NONCE_VALUE));
        notificationProps.put(PROPERTY_CLAIM_VALUE, claim.getClaimValue());
        notificationProps.put(PROPERTY_VALIDATION_URL, properties.get(PROPERTY_VALIDATION_URL));
        notificationProps.put(PROPERTY_TEMPLATE_TYPE, PROPERTY_TEMPLATE_TYPE_VALUE);

        Event identityMgtEvent = new Event(IdentityEventConstants.Event.TRIGGER_NOTIFICATION, notificationProps);
        try {
            ClaimVerificationServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            String msg = "Error occurred while sending email-claim verification email to: " + user.getUsername();
            LOG.error(msg, e);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ErrorMessages.ERROR_MSG_SENDING_NOTIFICATION, e);
        }
    }

    @Override
    public boolean isVerified(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException {

        return true;
    }

    @Override
    public boolean canHandle(String verificationMethod) throws ClaimVerificationException {

        return IDENTIFIER.equalsIgnoreCase(verificationMethod);
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
            String msg = "Required property not found. Property:" + property;
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

        IdentityGovernanceService identityGovernanceService =
                ClaimVerificationServiceDataHolder.getInstance().getIdentityGovernanceService();
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
}
