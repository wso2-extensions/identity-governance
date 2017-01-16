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
 * User SelfRegistration Handler.
 */
public class UserSelfRegistrationHandler extends AbstractEventHandler {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationHandler.class);

    public String getName() {
        return "userSelfRegistration";
    }

    public String getFriendlyName() {
        return "User Self Registration";
    }

    @Override
    public void handleEvent(Event event) throws EventException {

        Map<String, Object> eventProperties = event.getEventProperties();
        String uniqueUserID = (String) eventProperties.get(EventConstants.EventProperty.USER_NAME);
        IdentityStore identityStore =
                (IdentityStore) eventProperties.get(EventConstants.EventProperty.USER_STORE_MANAGER);
        String[] roleList = (String[]) eventProperties.get(EventConstants.EventProperty.ROLE_LIST);

        User user = null;
        try {
            user = identityStore.getUser(uniqueUserID);
        } catch (IdentityStoreException e) {
            throw new EventException("Error occurred while retrieving user");
        } catch (UserNotFoundException e) {
            throw new EventException("Error occurred while retrieving user");
        }

        boolean enable = Boolean.parseBoolean(
                Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP));

        if (!enable) {
            //Self signup feature is disabled
            return;
        }

        //Check selfSignupRole is in the request. If it is not there, this handler will not do anything. just retrun
        if (roleList == null) {
            return;
        } else {
            List<String> roles = Arrays.asList(roleList);
            if (!roles.contains(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                return;
            }
        }

        boolean isAccountLockOnCreation = Boolean.parseBoolean(
                Utils.getConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION));

        boolean isNotificationInternallyManage = Boolean.parseBoolean(
                Utils.getConnectorConfig(
                        IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE));

        if (EventConstants.Event.POST_ADD_USER.equals(event.getEventName())) {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            try {

                if (isNotificationInternallyManage) {
                    userRecoveryDataStore.invalidate(user);
                    String secretKey = Utils.generateUUID();
                    UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey,
                                                                           RecoveryScenarios.SELF_SIGN_UP,
                                                                           RecoverySteps.CONFIRM_SIGN_UP);

                    userRecoveryDataStore.store(recoveryDataDO);
                    triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM,
                            secretKey, Utils.getArbitraryProperties());
                }
            } catch (IdentityRecoveryException e) {
                throw new EventException("Error while sending self sign up notification ", e);
            }
            if (isAccountLockOnCreation) {
                try {
                    Utils.setClaimInIdentityStore(user, IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM,
                                                  Boolean.TRUE.toString());
                } catch (IdentityStoreException e) {
                    throw new EventException("Error while lock user account :" + user.getUniqueUserId(), e);
                } catch (UserNotFoundException e) {
                    throw new EventException("Error while lock user account :" + user.getUniqueUserId(), e);
                }
            }

        }
    }

    @Override
    public void init(InitConfig configuration) throws IdentityRuntimeException {
        super.init(configuration);
    }


    private void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

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

}
