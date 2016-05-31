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

package org.wso2.carbon.identity.recovery;


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.EventMgtException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.bean.ResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.model.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.HashMap;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);

    public ResponseBean sendRecoveryNotification(User user) throws IdentityRecoveryException {
        boolean isNotificationInternallyManaged = true;
        //TODO Read from configuraion

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        userRecoveryDataStore.invalidate(user);

        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceComponent.getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String fullUserName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(fullUserName)) {
                String message = "User does not exist :" + fullUserName;
                throw handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_INVALID_USER);
            }

        } catch (UserStoreException e) {
            String message = "Error while user validation :" + user.getUserName();
            throw handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED);
        }

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.NOTIFY);

        userRecoveryDataStore.store(recoveryDataDO);
        ResponseBean responseBean = new ResponseBean(user);

        if (isNotificationInternallyManaged) {
            triggerNotification(user, "passwordreset", secretKey);
        } else {
            responseBean.setKey(secretKey);
        }

        return responseBean;
    }


    public void updatePassword(User user, String code, String password) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        userRecoveryDataStore.load(user, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY,
                RecoverySteps.NOTIFY, code);
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions


        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        String fullName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        try {
            UserStoreManager userStoreManager = IdentityRecoveryServiceComponent.getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(fullName, password);
        } catch (UserStoreException e) {
            String message = "Error while updating password :" + fullName;
            throw handleException(message, IdentityRecoveryConstants.ErrorCode.ERROR_CODE_UNEXPECTED, e);
        }

        userRecoveryDataStore.invalidate(code);

        if (log.isDebugEnabled()) {
            String msg = "Password is updated for  user: " + fullName;
            log.debug(msg);
        }

    }

    private void triggerNotification(User user, String type, String code) throws IdentityRecoveryException {

        String eventName = EventMgtConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(EventMgtConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(EventMgtConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(EventMgtConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        properties.put("confirmation-code", code);
        properties.put("TEMPLATE_TYPE", type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getEventMgtService().handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            String message = "Error while trigger notification :" + user.getUserName();
            throw handleException(message, "", e);
        }
    }

    private IdentityRecoveryException handleException(String errorDescription, String errorCode) throws
            IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        log.error(errorDescription + "    Code :" + errorCode);
        return identityRecoveryException;
    }

    private IdentityRecoveryException handleException(String errorDescription, String errorCode, Throwable e) throws IdentityRecoveryException {
        IdentityRecoveryException identityRecoveryException = new IdentityRecoveryException(errorDescription, e);
        IdentityRecoveryException.ErrorInfo.ErrorInfoBuilder errorInfoBuilder = new IdentityRecoveryException
                .ErrorInfo.ErrorInfoBuilder(errorDescription);
        errorInfoBuilder.cause(e);
        errorInfoBuilder.errorCode(errorCode);
        identityRecoveryException.addErrorInfo(errorInfoBuilder.build());
        log.error(errorDescription + "    Code :" + errorCode, e);
        return identityRecoveryException;
    }
}
