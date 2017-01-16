/*
 *
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.signup;


import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.claim.Claim;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.util.HashMap;

/**
 * Manager class which can be used to recover passwords using a notification.
 */
public class UserSelfRegistrationManager {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationManager.class);

    private static UserSelfRegistrationManager instance = new UserSelfRegistrationManager();


    private UserSelfRegistrationManager() {

    }

    public static UserSelfRegistrationManager getInstance() {
        return instance;
    }


    public NotificationResponseBean registerUser(User user, String password, Claim[] claims, Property[] properties)
            throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                                              user.getUniqueUserId(),
                                              new IdentityRecoveryException("Domain not specified in the request"));
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP,
                                              user.getUniqueUserId());
        }

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE));

        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        try {

            //Set arbitrary properties to use in UserSelfRegistrationHandler
            Utils.setArbitraryProperties(properties);

            //TODO : Implement using C5 apis
//            try {
////                identityStore.addUser(user);
//
//            } catch (UserStoreException e) {
//                Throwable cause = e;
//
//                while (cause != null) {
//                    if (cause instanceof PolicyViolationException) {
//                        throw IdentityException.error(IdentityRecoveryClientException.class,
//                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
//                                                      cause.getMessage(), e);
//                    }
//                    cause = cause.getCause();
//                }
//
//                if (e.getMessage() != null && e.getMessage().contains("UserAlreadyExisting:")) {
//                    throw Utils.handleClientException(
//                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS,
            // user.getUserName(),
//                            e);
//                } else {
//                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.
            // ERROR_CODE_ADD_SELF_USER,
//                                                      user.getUserName(), e);
//                }
//            }

            if (!isNotificationInternallyManage) {
                UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                userRecoveryDataStore.invalidate(user);

                String secretKey = Utils.generateUUID();
                UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, 
                                                                       RecoveryScenarios.SELF_SIGN_UP, 
                                                                       RecoverySteps.CONFIRM_SIGN_UP);

                userRecoveryDataStore.store(recoveryDataDO);
                notificationResponseBean.setKey(secretKey);
            }

            //isNotificationInternallyManage == true,  will be handled in UserSelfRegistration Handler


        } finally {
            Utils.clearArbitraryProperties();
            PrivilegedCarbonContext.endTenantFlow();
        }
        return notificationResponseBean;
    }

    /**
     * Check whether user is already confirmed or not.
     *
     * @param user
     * @return
     * @throws IdentityRecoveryException
     */
    public boolean isUserConfirmed(User user) throws IdentityRecoveryException {
        boolean isUserConfirmed = false;
        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                                              user.getUniqueUserId(),
                                              new IdentityRecoveryException("ConfirmUserSelfRegistration : " +
                                                                            "Domain is not in the request." + 
                                                                            user.getUniqueUserId()));
        }
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData load = userRecoveryDataStore.load(user);

        if (load == null || !RecoveryScenarios.SELF_SIGN_UP.equals(load.getRecoveryScenario())) {
            isUserConfirmed = true;
        }
        return isUserConfirmed;

    }

    public void confirmUserSelfRegistration(String code) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        UserRecoveryData recoveryData = userRecoveryDataStore.load(code);
        User user = recoveryData.getUser();

        boolean enable = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP,
                    user.getUniqueUserId());
        }

        if (!RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        try {

            Utils.setClaimInIdentityStore(user,
                                          IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                                          Boolean.FALSE.toString());
            Utils.setClaimInIdentityStore(user,
                                          IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM,
                                          Boolean.TRUE.toString());

            //Invalidate code
            userRecoveryDataStore.invalidate(code);

        } catch (UserNotFoundException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNLOCK_USER_USER,
                                              user.getUniqueUserId(), e);
        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNLOCK_USER_USER,
                                              user.getUniqueUserId(), e);
        }

    }

    public NotificationResponseBean resendConfirmationCode(User user, Property[] properties) throws
            IdentityRecoveryException {

        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                                              user.getUniqueUserId(),
                                              new IdentityRecoveryException("ConfirmUserSelfRegistration : " +
                                                                            "Domain is not in the request." +
                                                                            user.getUniqueUserId()));
        }

        boolean enable = Boolean.parseBoolean(
                Utils.getSignUpConfigs(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP, 
                                              user.getUniqueUserId());
        }

        boolean isNotificationInternallyManage = 
                Boolean.parseBoolean(
                        Utils.getSignUpConfigs(
                                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE));


        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(user);

        if (userRecoveryData == null || StringUtils.isBlank(userRecoveryData.getSecret()) || !RecoverySteps
                .CONFIRM_SIGN_UP.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_OLD_CODE_NOT_FOUND,
                                              null);
        }
        //Invalid old code
        userRecoveryDataStore.invalidate(userRecoveryData.getSecret());

        String secretKey = Utils.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

        userRecoveryDataStore.store(recoveryDataDO);

        if (isNotificationInternallyManage) {
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM, secretKey,
                                properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;

    }

    private void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

        String eventName = EventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventConstants.EventProperty.USER_NAME, user.getUniqueUserId());
        properties.put(EventConstants.EventProperty.USER_STORE_DOMAIN, user.getDomainName());

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
}
