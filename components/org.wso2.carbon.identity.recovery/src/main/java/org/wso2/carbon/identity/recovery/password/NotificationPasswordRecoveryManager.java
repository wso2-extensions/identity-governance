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

package org.wso2.carbon.identity.recovery.password;


import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.event.EventConstants;
import org.wso2.carbon.identity.event.EventException;
import org.wso2.carbon.identity.event.model.Event;
import org.wso2.carbon.identity.mgt.IdentityStore;
import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.mgt.exception.IdentityStoreException;
import org.wso2.carbon.identity.mgt.exception.UserNotFoundException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
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
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);
    private static NotificationPasswordRecoveryManager instance = new NotificationPasswordRecoveryManager();

    private NotificationPasswordRecoveryManager() {

    }

    public static NotificationPasswordRecoveryManager getInstance() {
        return instance;
    }


    public NotificationResponseBean sendRecoveryNotification(User user, String type, Boolean notify, Property[]
            properties) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getDomainName())) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED,
                                              user.getUniqueUserId(),
                                              new IdentityRecoveryException("SendRecoveryNotification : Identity " +
                                                                            "store domain is not in the request"));
        }

        boolean isRecoveryEnable = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_BASED_PASSWORD_RECOVERY_NOT_ENABLE,
                    null);
        }

        boolean isNotificationInternallyManage;
        if (notify == null) {
            isNotificationInternallyManage = Boolean.parseBoolean(
                    Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE));
        } else {
            isNotificationInternallyManage = notify.booleanValue();
        }

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        IdentityStore identityStore = IdentityRecoveryServiceDataHolder.getInstance()
                                                                       .getRealmService().getIdentityStore();
        try {
            identityStore.getUser(user.getUniqueUserId());

        } catch (IdentityStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } catch (UserNotFoundException e) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER,
                                              user.getUniqueUserId());
        }

        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, user.getUniqueUserId());
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, user.getUniqueUserId());
        }

        userRecoveryDataStore.invalidate(user);

        String secretKey = Utils.generateUUID();
        UserRecoveryData recoveryDataDO =
                new UserRecoveryData(user, secretKey, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY,
                                     RecoverySteps.UPDATE_PASSWORD);

        userRecoveryDataStore.store(recoveryDataDO);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        if (isNotificationInternallyManage) {
            triggerNotification(user,
                    IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET, secretKey, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;
    }


    public void updatePassword(String code, String password, Property[] properties) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(code);
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        if (!RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        try {
            IdentityRecoveryServiceDataHolder.getInstance().getRealmService().getIdentityStore()
                                             .updateUserCredentials(userRecoveryData.getUser().getUniqueUserId(), null);
            if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(
                    userRecoveryData.getRecoveryScenario()) ||
                    RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(
                            userRecoveryData.getRecoveryScenario())) {
                Utils.setClaimInIdentityStore(userRecoveryData.getUser(),
                                              IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                                              Boolean.FALSE.toString());
            }
        } catch (IdentityStoreException e) {
            checkPasswordValidity(e);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        } catch (UserNotFoundException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }

        userRecoveryDataStore.invalidate(code);

        boolean isNotificationInternallyManaged = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE));
        boolean isNotificationSendWhenSuccess = Boolean.parseBoolean(
                Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS));


        if (isNotificationInternallyManaged && isNotificationSendWhenSuccess) {
            try {
                triggerNotification(userRecoveryData.getUser(), IdentityRecoveryConstants
                        .NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS, null, properties);
            } catch (IdentityRecoveryException e) {
                log.warn("Error while sending password reset success notification to user :" +
                        userRecoveryData.getUser().getUniqueUserId());
            }
        }

        if (log.isDebugEnabled()) {
            String msg = "Password is updated for  user: " + userRecoveryData.getUser().getUniqueUserId();
            log.debug(msg);
        }

    }

    private void checkPasswordValidity(IdentityStoreException e) throws IdentityRecoveryClientException {

        Throwable cause = e.getCause();
        while (cause != null) {
            if (cause instanceof EventException) {
                String errorCode = ((EventException) cause).getErrorCode();
                if (StringUtils.equals(errorCode, "22001")) {
                    throw Utils.handleClientException(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_HISTORY_VIOLATE, null, e);
                }
            }
            cause = cause.getCause();
        }
    }

    private void triggerNotification(User user, String type, String code, Property[] metaProperties) throws
            IdentityRecoveryException {

        String eventName = EventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventConstants.EventProperty.USER_NAME, user.getUniqueUserId());
        properties.put(EventConstants.EventProperty.USER_STORE_DOMAIN, user.getDomainName());

        if (StringUtils.isNotBlank(code)) {
            properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        }

        if (metaProperties != null) {
            for (Property metaProperty : metaProperties) {
                if (StringUtils.isNotBlank(metaProperty.getValue()) && StringUtils.isNotBlank(metaProperty.getKey())) {
                    properties.put(metaProperty.getKey(), metaProperty.getValue());
                }
            }
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
