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
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
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
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;

import java.util.HashMap;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class NotificationPasswordRecoveryManager {

    private static final Log log = LogFactory.getLog(NotificationPasswordRecoveryManager.class);
    private static NotificationPasswordRecoveryManager instance = new NotificationPasswordRecoveryManager();

    private NotificationPasswordRecoveryManager() {

    }

    public static NotificationPasswordRecoveryManager getInstance() {
        return instance;
    }


    public NotificationResponseBean sendRecoveryNotification(User user, String type, Boolean notify, Property[] properties)
            throws IdentityRecoveryException {
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
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_BASED_PASSWORD_RECOVERY_NOT_ENABLE, null);
        }

        boolean isNotificationInternallyManage;
        if (notify == null) {
            isNotificationInternallyManage = Boolean.parseBoolean(Utils.getRecoveryConfigs
                    (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));
        } else {
            isNotificationInternallyManage = notify.booleanValue();
        }

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        int tenantId = IdentityTenantUtil.getTenantId(user.getTenantDomain());
        UserStoreManager userStoreManager;
        try {
            userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            String domainQualifiedUsername = IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain());
            if (!userStoreManager.isExistingUser(domainQualifiedUsername)) {
                throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER, user
                        .getUserName());
            }

        } catch (UserStoreException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }

        if (Utils.isAccountDisabled(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLED_ACCOUNT, user.getUserName());
        } else if (Utils.isAccountLocked(user)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_LOCKED_ACCOUNT, user.getUserName());
        }

        userRecoveryDataStore.invalidate(user);

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);

        userRecoveryDataStore.store(recoveryDataDO);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        if (isNotificationInternallyManage) {
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET, secretKey, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;
    }


    public void updatePassword(String code, String password, Property[] properties) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(code);

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain() ;
        String userTenantDomain = userRecoveryData.getUser().getTenantDomain();

        if(!StringUtils.equals(contextTenantDomain, userTenantDomain)){
            throw new IdentityRecoveryClientException("invalid tenant domain: "+ userTenantDomain);
        }
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        if (!RecoverySteps.UPDATE_PASSWORD.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        int tenantId = IdentityTenantUtil.getTenantId(userRecoveryData.getUser().getTenantDomain());
        String domainQualifiedName = IdentityUtil.addDomainToName(userRecoveryData.getUser().getUserName(), userRecoveryData.getUser().getUserStoreDomain());
        try {

            UserStoreManager userStoreManager = IdentityRecoveryServiceDataHolder.getInstance().getRealmService().
                    getTenantUserRealm(tenantId).getUserStoreManager();
            userStoreManager.updateCredentialByAdmin(domainQualifiedName, password);
            if (RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK.equals(userRecoveryData.getRecoveryScenario()) || RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP.equals(userRecoveryData.getRecoveryScenario())) {
                HashMap<String, String> userClaims = new HashMap<>();
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
                userStoreManager.setUserClaimValues(domainQualifiedName, userClaims, null);
            }
        } catch (UserStoreException e) {
            checkPasswordValidity(e);
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, null, e);
        }

        userRecoveryDataStore.invalidate(code);

        boolean isNotificationInternallyManaged = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, userRecoveryData.getUser().getTenantDomain()));
        boolean isNotificationSendWhenSuccess = Boolean.parseBoolean(Utils.getRecoveryConfigs
                (IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_SEND_RECOVERY_NOTIFICATION_SUCCESS, userRecoveryData.getUser().getTenantDomain()));


        if (isNotificationInternallyManaged && isNotificationSendWhenSuccess) {
            try {
                triggerNotification(userRecoveryData.getUser(), IdentityRecoveryConstants
                        .NOTIFICATION_TYPE_PASSWORD_RESET_SUCCESS, null, properties);
            } catch (Exception e) {
                log.warn("Error while sending password reset success notification to user :" + userRecoveryData.getUser().getUserName());
            }
        }

        if (log.isDebugEnabled()) {
            String msg = "Password is updated for  user: " + domainQualifiedName;
            log.debug(msg);
        }

    }

    private void checkPasswordValidity(UserStoreException e) throws IdentityRecoveryClientException {
        Throwable cause = e.getCause();
        while (cause != null) {
            if (cause instanceof IdentityEventException) {
                String errorCode = ((IdentityEventException) cause).getErrorCode();
                if (StringUtils.equals(errorCode, "22001")) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_HISTORY_VIOLATE, null, e);
                }
            }

            if (cause instanceof PolicyViolationException) {
                throw IdentityException.error(IdentityRecoveryClientException.class,
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(), cause.getMessage(), e);
            }
            cause = cause.getCause();
        }
    }

    private void triggerNotification(User user, String type, String code, Property[] metaProperties) throws
            IdentityRecoveryException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

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
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION,
                    user.getUserName(), e);
        }
    }
}
