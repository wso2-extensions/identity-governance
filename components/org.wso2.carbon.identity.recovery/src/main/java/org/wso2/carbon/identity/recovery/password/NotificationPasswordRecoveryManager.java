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


import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.EventMgtConstants;
import org.wso2.carbon.identity.event.EventMgtException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);

    public NotificationResponseBean sendRecoveryNotification(User user) throws IdentityRecoveryException {
        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("SendRecoveryNotification :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("SendRecoveryNotification :User store domain is not in the request. set to default for user" +
                    " : " + user.getUserName());

        }

        boolean isRecoveryEnable = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, user.getTenantDomain()));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_BASED_RECOVERY_NOT_ENABLE,
                    null);
        }

        boolean isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceComponent.getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER, user
                        .getUserName());
            }

        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }

        userRecoveryDataStore.invalidate(user);

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);

        userRecoveryDataStore.store(recoveryDataDO);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        if (isNotificationInternallyManaged) {
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET, secretKey);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;
    }


    public void updatePassword(User user, String code, String password) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("UpdatePassword :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("UpdatePassword :User store domain is not in the request. set to default for user" +
                    " : " + user.getUserName());

        }

        boolean isRecoveryEnable = Boolean.parseBoolean(Utils.getRecoveryConfigs(IdentityRecoveryConstants
                .ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, user.getTenantDomain()));
        if (!isRecoveryEnable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PASSWORD_BASED_RECOVERY_NOT_ENABLE, null);
        }

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        userRecoveryDataStore.load(user, RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY,
                RecoverySteps.UPDATE_PASSWORD, code);
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions


        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        String fullName = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
        try {
            UserStoreManager userStoreManager = IdentityRecoveryServiceComponent.getRealmService().getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(fullName, password);
        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
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

        properties.put(IdentityRecoveryConstants.CONFIRMATION_CODE, code);
        properties.put(IdentityRecoveryConstants.TEMPLATE_TYPE, type);
        Event identityMgtEvent = new Event(eventName, properties);
        try {
            IdentityRecoveryServiceDataHolder.getInstance().getEventMgtService().handleEvent(identityMgtEvent);
        } catch (EventMgtException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
    }
}
