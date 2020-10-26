/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityRuntimeException;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.handler.InitConfig;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreConfigConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

/**
 * This event handler is used to send a verification SMS when a claim update event to update the mobile number
 * is triggered.
 */
public class MobileNumberVerificationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(MobileNumberVerificationHandler.class);

    public String getName() {

        return "userMobileVerification";
    }

    public String getFriendlyName() {

        return "User Mobile Number Verification";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String eventName = event.getEventName();
        UserStoreManager userStoreManager = (UserStoreManager) eventProperties.get(IdentityEventConstants.
                EventProperty.USER_STORE_MANAGER);
        User user = getUser((String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME),
                (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN),
                userStoreManager.getRealmConfiguration().
                        getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME));
        Map<String, String> claims = (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty
                .USER_CLAIMS);

        boolean enable = isMobileVerificationOnUpdateEnabled(user.getTenantDomain());

        if (!enable) {
            // Mobile Number Verification feature is disabled.
            if (log.isDebugEnabled()) {
                log.debug("Mobile number verification handler is disabled in tenant: " + user.getTenantDomain() +
                        " for event: " + eventName);
            }
            /* We need to empty 'MOBILE_NUMBER_PENDING_VALUE_CLAIM' because having a value in that claim implies
            a verification is pending. But verification is not enabled anymore. */
            if (claims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM)) {
                invalidatePendingMobileVerification(user, userStoreManager, claims);
            }
            return;
        }

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(eventName)) {
            preSetUserClaimOnMobileNumberUpdate(claims, userStoreManager, user);
        }

        if (IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(eventName)) {
            postSetUserClaimOnMobileNumberUpdate(user, userStoreManager);
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {

        super.init(configuration);
    }

    @Override
    public int getPriority(MessageContext messageContext) {

        return 50;
    }

    /**
     * Store verification details in the recovery data store and initiate notification.
     *
     * @param user  User.
     * @param verificationPendingMobileNumber Updated mobile number that is pending verification.
     * @throws IdentityEventException
     */
    private void initNotificationForMobileNumberVerificationOnUpdate(User user, String verificationPendingMobileNumber)
            throws IdentityEventException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user, RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                    RecoverySteps.VERIFY_MOBILE_NUMBER);
            String secretKey = Utils.generateSecretKey(NotificationChannels.SMS_CHANNEL.getChannelType(),
                    user.getTenantDomain(), String.valueOf(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE));
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER);
            /* Mobile number is persisted in remaining set ids to maintain context information about the mobile number
            associated with the verification code generated. */
            recoveryDataDO.setRemainingSetIds(verificationPendingMobileNumber);
            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, secretKey, Utils.getArbitraryProperties(), verificationPendingMobileNumber);
        } catch (IdentityRecoveryException e) {
            throw new IdentityEventException("Error while sending notification to user: " +
                    user.toFullQualifiedUsername() + " for mobile verification on update.", e);
        }
    }

    /**
     * Trigger the SMS notification.
     *
     * @param user      User.
     * @param code      SMS OTP.
     * @param props     Other properties.
     * @param verificationPendingMobileNumber Mobile number to which the SMS should be sent.
     * @throws IdentityRecoveryException
     */
    private void triggerNotification(User user, String code, Property[] props, String
            verificationPendingMobileNumber) throws IdentityRecoveryException {

        String notificationType = IdentityRecoveryConstants.NOTIFICATION_TYPE_VERIFY_MOBILE_ON_UPDATE;

        if (log.isDebugEnabled()) {
            log.debug("Sending: " + notificationType + " notification to user: " + user.toFullQualifiedUsername());
        }

        String eventName = IdentityEventConstants.Event.TRIGGER_SMS_NOTIFICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL,
                NotificationChannels.SMS_CHANNEL.getChannelType());
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, notificationType);
        if (StringUtils.isNotBlank(verificationPendingMobileNumber)) {
            properties.put(IdentityRecoveryConstants.SEND_TO, verificationPendingMobileNumber);
        }
        if (props != null && props.length > 0) {
            for (Property prop : props) {
                properties.put(prop.getKey(), prop.getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.toFullQualifiedUsername(), e);
        }
    }

    /**
     * Form User object from username, tenant domain, and user store domain.
     *
     * @param userName          UserName.
     * @param tenantDomain      Tenant Domain.
     * @param userStoreDomain   User Domain.
     * @return User.
     */
    private User getUser(String userName, String tenantDomain, String userStoreDomain) {

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(userStoreDomain);
        return user;
    }

    /**
     * If the mobile claim is updated, set it to the 'MOBILE_NUMBER_PENDING_VALUE_CLAIM' claim.
     * Set thread local state to skip sending verification notification in inapplicable claim update scenarios.
     *
     * @param claims            Map of claims to be updated.
     * @param userStoreManager  User store manager.
     * @param user              User.
     * @throws IdentityEventException
     */
    private void preSetUserClaimOnMobileNumberUpdate(Map<String, String> claims, UserStoreManager userStoreManager,
                                                     User user) throws IdentityEventException {

        if (IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                (Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate())) {
            // Not required to handle in this handler.
            return;
        }

        if (Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate() != null) {
            Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        }

        if (!isInvokedByUser(user) || MapUtils.isEmpty(claims)) {
            // Not required to handle in this handler.
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
            return;
        }

        String mobileNumber = claims.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);

        if (StringUtils.isNotBlank(mobileNumber)) {
            String existingMobileNumber;
            String username = user.getUserName();
            try {
                existingMobileNumber = userStoreManager.getUserClaimValue(username, IdentityRecoveryConstants.
                        MOBILE_NUMBER_CLAIM, null);
            } catch (UserStoreException e) {
                String error = String.format("Error occurred while retrieving existing mobile number for user: %s in " +
                        "domain: %s and user store: %s", username, user.getTenantDomain(), user.getUserStoreDomain());
                throw new IdentityEventException(error, e);
            }

            if (StringUtils.equals(mobileNumber, existingMobileNumber)) {
                if (log.isDebugEnabled()) {
                    log.debug(String.format("The mobile number to be updated: %s is same as the existing mobile " +
                            "number for user: %s in domain: %s and user store: %s. Hence an SMS OTP verification " +
                            "will not be triggered.", mobileNumber, username, user.getTenantDomain(),
                            user.getUserStoreDomain()));
                }
                Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                        .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_EXISTING_MOBILE_NUM.toString());
                invalidatePendingMobileVerification(user, userStoreManager, claims);
                return;
            }
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, mobileNumber);
            claims.remove(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM);
        } else {
            Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString());
        }
    }

    /**
     * Verify whether the mobile number update is invoked by the user himself, but not by another privileged user
     * on behalf.
     *
     * @param user User whose claims are being updates.
     * @return True if the user in the context is the same as the user whose claims are being updated, false otherwise.
     */
    private boolean isInvokedByUser(User user) {

        String usernameFromContext = PrivilegedCarbonContext.getThreadLocalCarbonContext().getUsername();
        String tenantDomainFromContext = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userRealm = UserStoreConfigConstants.PRIMARY;
        String[] strComponent = usernameFromContext.split("/");
        if (usernameFromContext.split("/").length == 2) {
            userRealm = strComponent[0];
            usernameFromContext = strComponent[1];
        }
        User invokingUser = getUser(usernameFromContext, tenantDomainFromContext, userRealm);
        return user.equals(invokingUser);
    }

    /**
     * Initiate notification sending process if the thread local is not set to skip verification process.
     *
     * @param user              User.
     * @param userStoreManager  User store manager.
     * @throws IdentityEventException
     */
    private void postSetUserClaimOnMobileNumberUpdate(User user, UserStoreManager userStoreManager) throws
            IdentityEventException {

        try {
            String skipMobileNumVerificationOnUpdateState =
                    Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
            if (!IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_CONFIRM.toString().equals
                    (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants.
                    SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_EXISTING_MOBILE_NUM.toString().equals
                    (skipMobileNumVerificationOnUpdateState) && !IdentityRecoveryConstants
                    .SkipMobileNumberVerificationOnUpdateStates.SKIP_ON_INAPPLICABLE_CLAIMS.toString().equals
                            (skipMobileNumVerificationOnUpdateState)) {

                String verificationPendingMobileNumClaim = getVerificationPendingMobileNumValue(userStoreManager, user);

                if (StringUtils.isNotBlank(verificationPendingMobileNumClaim)) {
                    initNotificationForMobileNumberVerificationOnUpdate(user, verificationPendingMobileNumClaim);
                }
            }
        } finally {
            Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        }
    }

    /**
     * Get the 'http://wso2.org/claims/identity/mobileNumber.pendingValue' claim value.
     *
     * @param userStoreManager  User store manager.
     * @param user              User.
     * @return Claim value.
     * @throws IdentityEventException
     */
    private String getVerificationPendingMobileNumValue(UserStoreManager userStoreManager, User user) throws
            IdentityEventException {

        Map<String, String> verificationPendingMobileNumClaimMap;
        try {
            verificationPendingMobileNumClaimMap = userStoreManager.getUserClaimValues(user.getUserName(), new String[]{
                    IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM}, null);
        } catch (UserStoreException e) {
            throw new IdentityEventException("Error while retrieving verification pending mobile number claim value " +
                    "for user: " + user.toFullQualifiedUsername(), e);
        }

        if (MapUtils.isEmpty(verificationPendingMobileNumClaimMap)) {
            return null;
        }

        for (Map.Entry<String, String> entry : verificationPendingMobileNumClaimMap.entrySet()) {
            String pendingVerificationMobileNumClaimURI = entry.getKey();
            if (IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM
                    .equals(pendingVerificationMobileNumClaimURI)) {
                return entry.getValue();
            }
        }
        return null;
    }

    /**
     * Check whether mobile verification on update feature is enabled via connector configuration.
     *
     * @param userTenantDomain      Tenant domain of the user.
     * @return True if the feature is enabled, false otherwise.
     * @throws IdentityEventException
     */
    private boolean isMobileVerificationOnUpdateEnabled(String userTenantDomain) throws IdentityEventException {

        return Boolean.parseBoolean(Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, userTenantDomain));
    }

    /**
     * Invalidate pending mobile number verification.
     *
     * @param user              User.
     * @param userStoreManager  User store manager.
     * @param claims            User claims.
     * @throws IdentityEventException
     */
    private void invalidatePendingMobileVerification(User user, UserStoreManager userStoreManager,
                                                    Map<String, String> claims ) throws IdentityEventException {

        if (StringUtils.isNotBlank(getVerificationPendingMobileNumValue(userStoreManager, user))) {
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, StringUtils.EMPTY);
            try {
                UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                userRecoveryDataStore.invalidate(user, RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER);
            } catch (IdentityRecoveryException e) {
                throw new IdentityEventException("Error while invalidating previous mobile verification data " +
                        "from recovery store for user: " + user.toFullQualifiedUsername(), e);
            }
        }
    }
}
