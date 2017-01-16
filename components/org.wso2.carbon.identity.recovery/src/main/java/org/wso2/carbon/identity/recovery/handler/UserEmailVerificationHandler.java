/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.common.base.exception.IdentityRuntimeException;
import org.wso2.carbon.identity.common.base.handler.InitConfig;
import org.wso2.carbon.identity.event.AbstractEventHandler;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.claim.Claim;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * UserEmail Verification Handler.
 */
public class UserEmailVerificationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserEmailVerificationHandler.class);

    public String getName() {
        return "userEmailVerification";
    }

    public String getFriendlyName() {
        return "User Email Verification";
    }

    @Override
    public void handleEvent(Event event) throws EventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        IdentityStore userStoreManager =
                (IdentityStore) eventProperties.get(EventConstants.EventProperty.USER_STORE_MANAGER);
        User user = getUser(eventProperties, userStoreManager);
        String[] roleList = (String[]) eventProperties.get(EventConstants.EventProperty.ROLE_LIST);
        Map<String, String> claims =
                (Map<String, String>) eventProperties.get(EventConstants.EventProperty.USER_CLAIMS);
        boolean enable = Boolean.parseBoolean(
                Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION));

        if (!enable) {
            //Email Verification feature is disabled
            return;
        }

        if (roleList != null) {
            List<String> roles = Arrays.asList(roleList);
            if (roles.contains(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                //This is a self signup request. Will be handled in self signup handler
                return;
            }
        }

        boolean isAccountLockOnCreation = Boolean.parseBoolean(
                Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION));

        boolean isNotificationInternallyManage = Boolean.parseBoolean(
                Utils.getConnectorConfig(
                        IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE));


        if (EventConstants.Event.PRE_ADD_USER.equals(event.getEventName())) {
            if (claims == null || claims.isEmpty()) {
                //Not required to handle in this org.wso2.carbon.identity.recovery.handler
                return;
            } else if (claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM) != null) {
                Claim claim = new Claim();
                claim.setClaimUri(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
                claim.setValue(claims.get(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));
                Utils.clearEmailVerifyTemporaryClaim();
                Utils.setEmailVerifyTemporaryClaim(claim);
                claims.remove(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);

            } else if (claims.get(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM) != null) {
                Claim claim = new Claim();
                claim.setClaimUri(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);
                claim.setValue(claims.get(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM));
                Utils.clearEmailVerifyTemporaryClaim();
                Utils.setEmailVerifyTemporaryClaim(claim);
                claims.remove(IdentityRecoveryConstants.ASK_PASSWORD_CLAIM);

            } else {
                return;
                //Not required to handle in this org.wso2.carbon.identity.recovery.handler
            }
        }

        if (EventConstants.Event.POST_ADD_USER.equals(event.getEventName())) {
            Claim claim = Utils.getEmailVerifyTemporaryClaim();
            if (claim == null) {
                //Not required to handle in this org.wso2.carbon.identity.recovery.handler
            } else if (IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM.equals(claim.getClaimUri())) {
                if (isNotificationInternallyManage) {
                    initNotification(user, RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP,
                            IdentityRecoveryConstants.NOTIFICATION_TYPE_EMAIL_CONFIRM);
                }

                //Need to lock user account
                if (isAccountLockOnCreation) {
                    lockAccount(user, userStoreManager);
                }
            } else if (IdentityRecoveryConstants.ASK_PASSWORD_CLAIM.equals(claim.getClaimUri())) {
                if (isNotificationInternallyManage) {
                    initNotification(user, RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD,
                            IdentityRecoveryConstants.NOTIFICATION_TYPE_ASK_PASSWORD);
                }
            }
        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }

    public void lockAccount(User user, IdentityStore userStoreManager) throws EventException {
        try {
            Utils.setClaimInIdentityStore(user, IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                                          Boolean.TRUE.toString());
        } catch (IdentityStoreException e) {
            throw new EventException("Error locking account", e);
        } catch (UserNotFoundException e) {
            throw new EventException("Error locking account", e);
        }
    }

    protected void initNotification(User user, Enum recoveryScenario, Enum recoveryStep, String notificationType)
            throws EventException {
            String secretKey = Utils.generateUUID();
            initNotification(user, recoveryScenario, recoveryStep, notificationType, secretKey);
    }

    protected void initNotification(User user, Enum recoveryScenario, Enum recoveryStep, String notificationType,
                                    String secretKey) throws EventException {
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

            userRecoveryDataStore.store(recoveryDataDO);
            triggerNotification(user, notificationType, secretKey, Utils.getArbitraryProperties());
        } catch (IdentityRecoveryException e) {
            throw new EventException("Error while sending  notification ", e);
        }
    }

    protected void setRecoveryData(User user, Enum recoveryScenario, Enum recoveryStep, String secretKey)
            throws EventException {
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        try {
            userRecoveryDataStore.invalidate(user);
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, recoveryScenario, recoveryStep);

            userRecoveryDataStore.store(recoveryDataDO);
        } catch (IdentityRecoveryException e) {
            throw new EventException("Error while setting recovery data for user ", e);
        }
    }

    protected UserRecoveryData getRecoveryData(User user) throws EventException {
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData recoveryData;
        try {
            recoveryData = userRecoveryDataStore.load(user);
        } catch (IdentityRecoveryException e) {
            throw new EventException("Error while loading recovery data for user ", e);
        }
        return recoveryData;
    }

    protected void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

        if (log.isDebugEnabled()) {
            log.debug("Sending : " + type + " notification to user : " + user.toString());
        }

        String eventName = EventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventConstants.EventProperty.USER_NAME, user.getUniqueUserId());

        if (props != null && props.length > 0) {
            for (int i = 0; i < props.length; i++) {
                properties.put(props[i].getKey(), props[i].getValue());
            }
        }
        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (EventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUniqueUserId(), e);
        }
    }

    protected User getUser(Map eventProperties, IdentityStore identityStore) throws EventException {

        String uniqueUserID = (String) eventProperties.get(EventConstants.EventProperty.USER_NAME);
        try {
            return identityStore.getUser(uniqueUserID);
        } catch (IdentityStoreException e) {
            throw new EventException("Error retrieving user", e);
        } catch (UserNotFoundException e) {
            throw new EventException("Error retrieving user", e);
        }

    }


}
