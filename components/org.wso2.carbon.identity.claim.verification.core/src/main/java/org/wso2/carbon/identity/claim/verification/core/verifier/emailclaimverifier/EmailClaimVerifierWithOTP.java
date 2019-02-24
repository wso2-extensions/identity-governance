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
import org.wso2.carbon.identity.claim.verification.core.model.ConfirmationCodeData;
import org.wso2.carbon.identity.claim.verification.core.model.User;
import org.wso2.carbon.identity.claim.verification.core.store.ClaimVerificationStore;
import org.wso2.carbon.identity.claim.verification.core.util.ClaimVerificationCoreConstants;
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
import java.util.concurrent.TimeUnit;

public class EmailClaimVerifierWithOTP implements ClaimVerifier {

    private static final Log LOG = LogFactory.getLog(EmailClaimVerifierWithOTP.class);
    private final String IDENTIFIER = "EmailClaimVerifierWithOTP";

    private final String PROPERTY_SEND_TO="send-to";
    private final String PROPERTY_NONCE_VALUE="nonce-value";
    private final String PROPERTY_CLAIM_NAME="claim-name";
    private final String PROPERTY_CLAIM_VALUE="claim-value";
    private final String PROPERTY_VALIDATION_URL="validation-url";
    private final String PROPERTY_CONFIRMATION_CODE="confirmation-code";
    private final String PROPERTY_TEMPLATE_TYPE="TEMPLATE_TYPE";
    private final String PROPERTY_TEMPLATE_TYPE_VALUE= "emailVerification";


    @Override
    public void sendNotification(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException {

        verifyRequiredPropertyExists(properties, PROPERTY_VALIDATION_URL);
        verifyRequiredPropertyExists(properties, PROPERTY_NONCE_VALUE);

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();

        if (properties.containsKey(ClaimVerificationCoreConstants.PROP_IS_RETRY_ATTEMPT)){

            ConfirmationCodeData codeData = new ConfirmationCodeData(user, claim.getClaimUri(),
                    ClaimVerificationCoreConstants.Step.EMAIL_VERIFICATION );

            claimVerificationStore.invalidateConfirmationCode(codeData);
        }

        String confirmationCode = ClaimVerificationCoreUtils.getConfirmationCode();

        ConfirmationCodeData codeData = new ConfirmationCodeData(user, confirmationCode, claim.getClaimUri(),
                ClaimVerificationCoreConstants.Step.EMAIL_VERIFICATION );

        claimVerificationStore.storeConfirmationCode(codeData);

        Map <String, Object> notificationProps = new HashMap<>();
        notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER,
                ClaimVerificationCoreUtils.getUserStoreManager(user.getTenantId()));
        notificationProps.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUsername());
        notificationProps.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getRealm());
        notificationProps.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN,
                IdentityTenantUtil.getTenantDomain(user.getTenantId()));
        notificationProps.put(PROPERTY_SEND_TO, claim.getClaimValue());
        notificationProps.put(PROPERTY_CLAIM_NAME, claim.getClaimDisplayTag());
        notificationProps.put(PROPERTY_NONCE_VALUE, properties.get(PROPERTY_NONCE_VALUE));
        notificationProps.put(PROPERTY_CLAIM_VALUE, claim.getClaimValue());
        notificationProps.put(PROPERTY_VALIDATION_URL, properties.get(PROPERTY_VALIDATION_URL));
        notificationProps.put(PROPERTY_CONFIRMATION_CODE, confirmationCode);
        notificationProps.put(PROPERTY_TEMPLATE_TYPE, PROPERTY_TEMPLATE_TYPE_VALUE);


        Event identityMgtEvent = new Event(IdentityEventConstants.Event.TRIGGER_NOTIFICATION, notificationProps);
        try {
            ClaimVerificationServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            String msg = "Error occurred while sending email-claim verification email to: " + user.getUsername();
            LOG.error(msg, e);
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_SENDING_NOTIFICATION, e);
        }

    }

    @Override
    public boolean isVerified(User user, Claim claim, Map<String, String> properties) throws ClaimVerificationException {

        verifyRequiredPropertyExists(properties, PROPERTY_CONFIRMATION_CODE);

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();

        ConfirmationCodeData codeData = claimVerificationStore.loadConfirmationCodeData(
                properties.get(PROPERTY_CONFIRMATION_CODE));

        boolean isCodeValid = isCodeValid(codeData,
                getCodeValidityPeriod(ClaimVerificationCoreConstants.ConnectorConfig.EMAIL_VERIFICATION_CODE_EXPIRY_TIME,
                codeData.getUser().getTenantId()), user, claim);

        if (isCodeValid){
            claimVerificationStore.invalidateConfirmationCode(codeData.getCode());
        }

        return isCodeValid;
    }

    @Override
    public boolean canHandle(String verificationMethod) throws ClaimVerificationException {

        return IDENTIFIER.equalsIgnoreCase(verificationMethod);

    }

    @Override
    public void revokeProcess(User user, Claim claim) throws ClaimVerificationException {

        ClaimVerificationStore claimVerificationStore = ClaimVerificationCoreUtils.getClaimVerificationStore();
        ConfirmationCodeData codeData = new ConfirmationCodeData(user, claim.getClaimUri(),
                ClaimVerificationCoreConstants.Step.EMAIL_VERIFICATION );
        claimVerificationStore.invalidateConfirmationCode(codeData);

    }

    @Override
    public int getConfirmationCodeValidityPeriod(int tenantId) throws ClaimVerificationException {

        return getCodeValidityPeriod(ClaimVerificationCoreConstants.ConnectorConfig.CONFIRMATION_CODE_EXPIRY_TIME,
                tenantId);
    }

    /**
     * Used to verify that a required property exists,
     * if it does not, throws an exception.
     *
     * @param properties Properties map.
     * @param property Property to be checked.
     * @throws ClaimVerificationBadRequestException
     */
    private void verifyRequiredPropertyExists(Map<String, String> properties, String property) throws
            ClaimVerificationBadRequestException {

        if (!properties.containsKey(property) || StringUtils.isBlank(properties.get(property))){
            String msg = "Required property not found. Property:" + property;
            if (LOG.isDebugEnabled()){
                LOG.debug(msg);
            }
            throw new ClaimVerificationBadRequestException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_REQUIRED_PROPERTY_NOT_FOUND.getCode(), msg);
        }
    }

    private int getCodeValidityPeriod(String codeProperty,int tenantId) throws ClaimVerificationException {

        IdentityGovernanceService identityGovernanceService = ClaimVerificationServiceDataHolder.getInstance().getIdentityGovernanceService();
        try {

            Property[] properties = identityGovernanceService.getConfiguration(new String[]{ClaimVerificationCoreConstants.ConnectorConfig.EMAIL_VERIFICATION_CODE_EXPIRY_TIME},
                    IdentityTenantUtil.getTenantDomain(tenantId));

            if (ArrayUtils.isEmpty(properties) ) {
                String msg = "Required property:"+ codeProperty +" not found in configuration.";
                LOG.error(msg);
                throw ClaimVerificationCoreUtils.getClaimVerificationException(
                        ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_READING_CONFIGURATION);
            }

            String codeValidityPeriod = properties[0].getValue();
            return Integer.parseInt(codeValidityPeriod);

        } catch (IdentityGovernanceException e) {
            throw ClaimVerificationCoreUtils.getClaimVerificationException(
                    ClaimVerificationCoreConstants.ErrorMessages.ERROR_MSG_READING_CONFIGURATION, e);

        }

    }

    private static boolean isCodeValid(ConfirmationCodeData codeData, int codeValidityPeriod, User user, Claim claim){

        if (codeData == null) {
            if (LOG.isDebugEnabled()){
                LOG.debug("No code data received.");
            }
            return false;
        }

        if (ClaimVerificationCoreConstants.Step.EMAIL_VERIFICATION!=codeData.getStep()){
            if (LOG.isDebugEnabled()){
                LOG.debug("Invalid code. Step verification failed. code:"+codeData.getCode());
            }
            return false;
        }

        if (!user.equals(codeData.getUser())){
            if (LOG.isDebugEnabled()){
                LOG.debug("Invalid code. Mismatch in request initiated user and user in the confirmation code.");
            }
            return false;
        }

        if (!claim.getClaimUri().equals(codeData.getScenario())){
            if (LOG.isDebugEnabled()){
                LOG.debug("Invalid code. Mismatch in request initiated scenario and scenario in the confirmation code");
            }
            return false;
        }

        if (codeValidityPeriod < 0){
            if (LOG.isDebugEnabled()){
                LOG.debug("Code:"+codeData.getCode()+ "has an unlimited validity period.");
            }
            return true;
        }

        long createdTimeStamp = codeData.getTimeCreated().getTime();
        long expiryTime = createdTimeStamp + TimeUnit.MINUTES.toMillis(codeValidityPeriod);

        return expiryTime > System.currentTimeMillis();
    }

}
