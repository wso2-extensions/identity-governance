/*
 * Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
 *  in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.confirmation;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;

import java.util.HashMap;

/**
 * Generic Manager class which can be used to resend confirmation code for any recovery scenario and self registration
 * using a notification
 */
public class ResendConfirmationManager {

    private static final Log log = LogFactory.getLog(ResendConfirmationManager.class);
    private static ResendConfirmationManager instance = new ResendConfirmationManager();

    private ResendConfirmationManager() {
    }

    public static ResendConfirmationManager getInstance() {
        return instance;
    }

    /**
     * Generic method to resend confirmation email for account recovery and self registration
     *
     * @param user             User who receives the confirmation email
     * @param recoveryScenario name of the recovery scenario (mandatory)
     *                         scenarios in org.wso2.carbon.identity.recovery.RecoveryScenarios
     * @param recoveryStep     name of the recovery step (mandatory)
     *                         steps in org.wso2.carbon.identity.recovery.RecoverySteps
     * @param notificationType email notification type (mandatory)
     *                         Ex: org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET
     * @param properties       other properties
     * @return notificationResponseBean
     * @throws org.wso2.carbon.identity.recovery.IdentityRecoveryException
     */
    public NotificationResponseBean resendConfirmationCode(User user, String recoveryScenario,
                                                           String recoveryStep, String notificationType,
                                                           Property[] properties) throws IdentityRecoveryException {
        return validateAndResendNotification(user, null, recoveryScenario, recoveryStep, notificationType, properties);
    }

    /**
     * Generic method to resend confirmation email for account recovery and self registration with provided
     * confirmation code validation
     *
     * @param user             User who receives the confirmation email
     * @param code             Previously issued confirmation code (mandatory)
     * @param recoveryScenario name of the recovery scenario (mandatory)
     *                         scenarios in org.wso2.carbon.identity.recovery.RecoveryScenarios
     * @param recoveryStep     name of the recovery step (mandatory)
     *                         steps in org.wso2.carbon.identity.recovery.RecoverySteps
     * @param notificationType email notification type (mandatory)
     *                         Ex: org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.NOTIFICATION_TYPE_PASSWORD_RESET
     * @param properties       other properties
     * @return notificationResponseBean
     * @throws IdentityRecoveryException
     */
    public NotificationResponseBean resendConfirmationCode(User user, String code, String recoveryScenario,
                                                           String recoveryStep, String notificationType,
                                                           Property[] properties) throws IdentityRecoveryException {
        if (StringUtils.isBlank(code)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CONFIRMATION_CODE_NOT_PROVIDED,
                    user.getUserName());
        }
        return validateAndResendNotification(user, code, recoveryScenario, recoveryStep, notificationType, properties);
    }

    private NotificationResponseBean validateAndResendNotification(User user, String code, String recoveryScenario,
                                                                   String recoveryStep, String notificationType,
                                                                   Property[] properties)
            throws IdentityRecoveryException {
        validateProvidedRecoveryInfo(user, recoveryScenario, recoveryStep);
        validateProvidedNotificationInfo(user, notificationType);
        setTenantDomainForUser(user);
        setUserStoreDomainForUser(user);
        boolean notificationInternallyManage = isNotificationInternallyManage(user);
        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        String secretKey = UUIDGenerator.generateUUID();
        validateAndStoreRecoveryData(user, secretKey, recoveryScenario, recoveryStep, code);
        if (notificationInternallyManage) {
            triggerNotification(user, notificationType, secretKey, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }
        return notificationResponseBean;
    }

    private void validateProvidedNotificationInfo(User user, String notificationType)
            throws IdentityRecoveryClientException {
        if (StringUtils.isBlank(notificationType)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NOTIFICATION_TYPE_NOT_PROVIDED,
                    user.getUserName());
        }
    }

    private void validateProvidedRecoveryInfo(User user, String recoveryScenario, String recoveryStep)
            throws IdentityRecoveryClientException {
        if (StringUtils.isBlank(recoveryScenario)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_RECOVERY_SCENARIO_NOT_PROVIDED,
                    user.getUserName());
        }
        if (StringUtils.isBlank(recoveryStep)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_RECOVERY_STEP_NOT_PROVIDED,
                    user.getUserName());
        }
    }

    private boolean isNotificationInternallyManage(User user) throws IdentityRecoveryServerException {
        return Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                        user.getTenantDomain()));
    }

    private void setTenantDomainForUser(User user) {
        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            if (log.isDebugEnabled()) {
                log.debug("Tenant domain is not in the request. Set super tenant domain for user : " +
                        user.getUserName());
            }
        }
    }

    private void setUserStoreDomainForUser(User user) {
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            if (log.isDebugEnabled()) {
                log.debug("User store domain is not in the request. Set primary user store domain for user : " +
                        user.getUserName());
            }
        }
    }

    private void validateAndStoreRecoveryData(User user, String secretKey, String recoveryScenario, String recoveryStep,
                                              String code) throws IdentityRecoveryException {
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);
        validateWithOldConfirmationCode(code, recoveryScenario, recoveryStep, userRecoveryData);

        //Invalidate the old confirmation code
        userRecoveryDataStore.invalidate(userRecoveryData.getSecret());
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, userRecoveryData.getRecoveryScenario(),
                userRecoveryData.getRecoveryStep());
        userRecoveryDataStore.store(recoveryDataDO);
    }

    private void validateWithOldConfirmationCode(String code, String recoveryScenario, String recoveryStep,
                                                 UserRecoveryData userRecoveryData)
            throws IdentityRecoveryClientException {
        if (userRecoveryData == null || StringUtils.isBlank(userRecoveryData.getSecret()) ||
                !recoveryScenario.equals(userRecoveryData.getRecoveryScenario().toString()) ||
                !recoveryStep.equals(userRecoveryData.getRecoveryStep().toString())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_OLD_CODE_NOT_FOUND,
                    null);
        }
        //Validate the provided confirmation code with previously issued code
        if (code != null && !userRecoveryData.getSecret().equals(code)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PROVIDED_CONFIRMATION_CODE_NOT_VALID,
                    code);
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
