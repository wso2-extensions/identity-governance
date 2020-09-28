/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */
package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

/**
 * Event handler class to send self registration confirmation notification.
 */
public class UserSelfRegistrationConfirmationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationConfirmationHandler.class);

    private static ThreadLocal<String> accountState = new ThreadLocal<>();

    private enum accountStates {PENDING_SR, UNLOCKED}

    public String getName() {

        return "userSelfRegistrationConfirm";
    }

    public String getFriendlyName() {

        return "User Self Registration Confirm";
    }

    @Override
    public void handleEvent(Event event) throws IdentityEventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String userName = (String) eventProperties.get(IdentityEventConstants.EventProperty.USER_NAME);
        UserStoreManager userStoreManager =
                (UserStoreManager) eventProperties.get(IdentityEventConstants.EventProperty.USER_STORE_MANAGER);
        String tenantDomain = (String) eventProperties.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
        String domainName = userStoreManager.getRealmConfiguration().getUserStoreProperty(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME);

        User user = new User();
        user.setUserName(userName);
        user.setTenantDomain(tenantDomain);
        user.setUserStoreDomain(domainName);

        boolean enableSelfSignUp = Boolean.parseBoolean(Utils.getConnectorConfig(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        boolean isSelfRegistrationConfirmationNotify = Boolean.parseBoolean(Utils.getConnectorConfig
                (IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_NOTIFY_ACCOUNT_CONFIRMATION,
                        user.getTenantDomain()));

        if (!enableSelfSignUp && !isSelfRegistrationConfirmationNotify) {
            //self sign up feature  or self is disabled
            if (!enableSelfSignUp) {
                if (log.isDebugEnabled()) {
                    log.debug("Self signup feature is disabled in " + tenantDomain);
                }
            }
            if (!isSelfRegistrationConfirmationNotify) {
                if (log.isDebugEnabled()) {
                    log.debug("Self signup succesful completion notification is disabled in " + tenantDomain);
                }
            }
            return; //this handler will not do anything. just return
        }

        if (IdentityEventConstants.Event.PRE_SET_USER_CLAIMS.equals(event.getEventName())) {
            if (log.isDebugEnabled()) {
                log.debug("Handling PRE_SET_USER_CLAIMS event for user " + userName);
            }
            handlePreSetUserClaimValues(user, userStoreManager, tenantDomain, userName);
        } else if (IdentityEventConstants.Event.POST_SET_USER_CLAIMS.equals(event.getEventName())) {
            if (log.isDebugEnabled()) {
                log.debug("Handling POST_SET_USER_CLAIMS event for user " + userName);
            }
            handlePostSetUserClaimValues(user, userStoreManager, tenantDomain, userName);
        }
    }

    /**
     * This method will store the account state in a thread local variable to identify whether this claims update
     * method is triggered during a self sign up flow.
     *
     * @param user
     * @param userStoreManager
     * @param tenantDomain
     * @param userName
     * @return
     * @throws IdentityEventException
     */
    protected boolean handlePreSetUserClaimValues(User user, UserStoreManager userStoreManager, String tenantDomain,
                                                  String userName) throws IdentityEventException {

        if (accountState.get() != null) {
            return true;
        }
        String currentAccountStateClaimValue = getAccountState(userStoreManager, tenantDomain, userName);
        boolean isPendingSelfRegistration =
                IdentityRecoveryConstants.PENDING_SELF_REGISTRATION.equals(currentAccountStateClaimValue);
        if (isPendingSelfRegistration) {
            accountState.set(accountStates.PENDING_SR.toString());
        }

        return true;
    }

    protected boolean handlePostSetUserClaimValues(User user, UserStoreManager userStoreManager, String tenantDomain,
                                                   String userName) throws IdentityEventException {

        try {
            String existingAccountStateClaimValue = getAccountState(userStoreManager, tenantDomain, userName);
            boolean notificationInternallyManage = Boolean.parseBoolean(Utils.getConnectorConfig
                    (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                            tenantDomain));
            boolean isAccountUnlocked =
                    IdentityRecoveryConstants.ACCOUNT_STATE_UNLOCKED.equals(existingAccountStateClaimValue);
            if (accountStates.PENDING_SR.toString().equals(accountState.get())) {
                if (notificationInternallyManage && isAccountUnlocked) {
                    try {
                        triggerNotification(user);
                    } catch (IdentityRecoveryServerException e) {
                        throw new IdentityEventException("Error while sending self sign up successful " +
                                "completion notification ", e);
                    }
                }
            }
        } finally {
            accountState.remove();
        }

        return true;
    }

    private String getAccountState(UserStoreManager userStoreManager, String tenantDomain, String userName)
            throws IdentityEventException {

        String accountStateClaimValue = null;
        try {
            boolean isAccountStateClaimExist = Utils.isAccountStateClaimExisting(tenantDomain);

            if (isAccountStateClaimExist) {
                Map<String, String> claimValues = userStoreManager.getUserClaimValues(userName, new String[]{
                        IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI}, UserCoreConstants.DEFAULT_PROFILE);

                accountStateClaimValue = claimValues.get(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI);
            }

        } catch (UserStoreException e) {
            throw new IdentityEventException("Error occurred while retrieving account state claim value", e);
        }
        return accountStateClaimValue;
    }

    protected void triggerNotification(User user) throws IdentityRecoveryServerException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;
        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE,
                IdentityRecoveryConstants.NOTIFICATION_TYPE_SELF_SIGNUP_SUCCESS);

        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }
}
