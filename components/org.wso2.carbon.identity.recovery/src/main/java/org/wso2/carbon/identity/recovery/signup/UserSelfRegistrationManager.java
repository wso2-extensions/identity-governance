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


import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.*;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceComponent;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserClaim;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.HashMap;
import java.util.Map;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class UserSelfRegistrationManager {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationManager.class);

    public NotificationResponseBean registerUser(User user, String password, UserClaim[] claims, Property[]
            properties) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("registerUser :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("registerUser :User store domain is not in the request. set to default for user : " + user
                    .getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }

        boolean isAccountLockOnCreation = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION, user.getTenantDomain()));

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, user
                        .getTenantDomain()));

        String roles = String.valueOf(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SELF_SIGN_UP_ROLES, user.getTenantDomain()));

        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        try {
            RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user
                        .getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());


            Map<String, String> claimsMap = new HashMap<String, String>();
            for (UserClaim claim : claims) {
                claimsMap.put(claim.getClaimURI(), claim.getClaimValue());
            }

            try {
                String[] userRoles = null;
                if (StringUtils.isNotBlank(roles)) {
                    userRoles = roles.split(IdentityRecoveryConstants.SIGN_UP_ROLE_SEPARATOR);
                }
                userStoreManager.addUser(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()),
                        password, userRoles,
                        claimsMap, null);

            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ADD_SELF_USER,
                        user.getUserName(), e);
            }

            if (isAccountLockOnCreation) {
                HashMap<String, String> userClaims = new HashMap<>();
                //Need to lock user account
                userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
                try {
                    userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                            user.getUserStoreDomain()), userClaims, null);
                } catch (UserStoreException e) {
                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                            .ERROR_CODE_LOCK_USER_USER, user.getUserName(), e);
                }
            }

            UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
            userRecoveryDataStore.invalidate(user);

            String secretKey = UUIDGenerator.generateUUID();
            UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                    .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

            userRecoveryDataStore.store(recoveryDataDO);

            if (isNotificationInternallyManage) {
                triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM.toString(),
                        secretKey, properties);
            } else {
                notificationResponseBean.setKey(secretKey);
            }

        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
        return notificationResponseBean;
    }

    public void confirmUserSelfRegistration(User user, String code) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default for user :" +
                    " " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }

        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();

        userRecoveryDataStore.load(user, RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP, code);
        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        try {
            RealmService realmService = IdentityRecoveryServiceComponent.getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user
                        .getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());

            HashMap<String, String> userClaims = new HashMap<>();
            //Need to lock user account
            userClaims.put(IdentityRecoveryConstants.ACCOUNT_LOCKED_CLAIM, Boolean.FALSE.toString());
            try {
                userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(user.getUserName(),
                        user.getUserStoreDomain()), userClaims, null);
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNLOCK_USER_USER,
                        user.getUserName(), e);
            }

            //Invalidate code
            userRecoveryDataStore.invalidate(code);

        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }

    }

    public NotificationResponseBean resendConfirmationCode(User user, Property[] properties) throws
            IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default for user :" +
                    " " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, user
                        .getTenantDomain()));


        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        UserRecoveryDataStore userRecoveryDataStore = new JDBCRecoveryDataStore();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.load(user);

        if (userRecoveryData == null || StringUtils.isBlank(userRecoveryData.getSecret())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_OLD_CODE_NOT_FOUND,
                    null);
        }
        //Invalid old code
        userRecoveryDataStore.invalidate(userRecoveryData.getSecret());

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

        userRecoveryDataStore.store(recoveryDataDO);

        if (isNotificationInternallyManage) {
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_ACCOUNT_CONFIRM.toString(),
                    secretKey, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;

    }


    private void triggerNotification(User user, String type, String code, Property[] props) throws
            IdentityRecoveryException {

        String eventName = IdentityEventConstants.Event.TRIGGER_NOTIFICATION;

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

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
        } catch (IdentityEventException e) {
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
    }
}
