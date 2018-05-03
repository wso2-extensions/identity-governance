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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONArray;
import org.json.JSONObject;
import org.wso2.carbon.CarbonException;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.PIICategoryValidity;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.consent.mgt.core.model.ReceiptPurposeInput;
import org.wso2.carbon.consent.mgt.core.model.ReceiptServiceInput;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.core.util.AnonymousSessionUtil;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.mgt.policy.PolicyViolationException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;
import org.wso2.carbon.registry.core.utils.UUIDGenerator;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.Permission;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Manager class which can be used to recover passwords using a notification
 */
public class UserSelfRegistrationManager {

    private static final Log log = LogFactory.getLog(UserSelfRegistrationManager.class);

    private static UserSelfRegistrationManager instance = new UserSelfRegistrationManager();

    private UserSelfRegistrationManager() {

    }

    public static UserSelfRegistrationManager getInstance() {

        return instance;
    }

    public NotificationResponseBean registerUser(User user, String password, Claim[] claims, Property[] properties) throws IdentityRecoveryException {

        String consent = getPropertyValue(properties, IdentityRecoveryConstants.Consent.CONSENT);
        String tenantDomain = user.getTenantDomain();

        if (StringUtils.isEmpty(tenantDomain)) {
            tenantDomain = MultitenantConstants.SUPER_TENANT_DOMAIN_NAME;
        }

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("registerUser :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("registerUser :User store domain is not in the request. set to default for user : " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));
        boolean isAccountLockOnCreation = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION, user.getTenantDomain()));

        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);

        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain())).getUserStoreManager();
            } catch (UserStoreException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED, user
                        .getUserName(), e);
            }

            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext carbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            carbonContext.setTenantId(IdentityTenantUtil.getTenantId(user.getTenantDomain()));
            carbonContext.setTenantDomain(user.getTenantDomain());

            Map<String, String> claimsMap = new HashMap<>();
            for (Claim claim : claims) {
                claimsMap.put(claim.getClaimUri(), claim.getValue());
            }

            //Set arbitrary properties to use in UserSelfRegistrationHandler
            Utils.setArbitraryProperties(properties);

            try {

                //TODO It is required to add this role before tenant creation. And also, this role should not not be able remove.
                if (!userStoreManager.isExistingRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE)) {
                    Permission permission = new Permission("/permission/admin/login", IdentityRecoveryConstants.EXECUTE_ACTION);
                    userStoreManager.addRole(IdentityRecoveryConstants.SELF_SIGNUP_ROLE, null, new Permission[]{permission});
                }

                String[] userRoles = new String[]{IdentityRecoveryConstants.SELF_SIGNUP_ROLE};

                userStoreManager.addUser(IdentityUtil.addDomainToName(user.getUserName(), user.getUserStoreDomain()),
                        password, userRoles, claimsMap, null);

            } catch (UserStoreException e) {
                Throwable cause = e;

                while (cause != null) {
                    if (cause instanceof PolicyViolationException) {
                        throw IdentityException.error(IdentityRecoveryClientException.class,
                                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(), cause.getMessage(), e);
                    }
                    cause = cause.getCause();
                }

                if (e.getMessage() != null && e.getMessage().contains("UserAlreadyExisting:")) {
                    throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS, user.getUserName(), e);
                } else {
                    throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ADD_SELF_USER, user.getUserName(), e);
                }
            }
            addUserConsent(consent, tenantDomain);

            if (!isNotificationInternallyManage && isAccountLockOnCreation) {
                UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
                userRecoveryDataStore.invalidate(user);

                String secretKey = UUIDGenerator.generateUUID();
                UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                        .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

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
     * Adds user consent.
     *
     * @param consent      Consent String.
     * @param tenantDomain Tenant Domain.
     * @throws IdentityRecoveryServerException IdentityRecoveryServerException.
     */
    public void addUserConsent(String consent, String tenantDomain) throws IdentityRecoveryServerException {

        if (StringUtils.isNotEmpty(consent)) {
            if (log.isDebugEnabled()) {
                log.debug(String.format("Adding consent to tenant domain : %s : %s", tenantDomain, consent));
            }
            try {
                addConsent(consent, tenantDomain);
            } catch (ConsentManagementException e) {
                throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ADD_USER_CONSENT,
                        "", e);
            }
        } else {
            if(log.isDebugEnabled()) {
                log.debug("Consent string is empty. Hence not adding consent");
            }
        }
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
        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }
        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default for user : " + user.getUserName());
        }
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData load =
                userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);

        if (load == null || !RecoveryScenarios.SELF_SIGN_UP.equals(load.getRecoveryScenario())) {
            isUserConfirmed = true;
        }
        return isUserConfirmed;

    }

    public void confirmUserSelfRegistration(String code) throws IdentityRecoveryException {

        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        UserRecoveryData recoveryData = userRecoveryDataStore.load(code);
        User user = recoveryData.getUser();

        String contextTenantDomain = PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain();
        String userTenantDomain = user.getTenantDomain();
        if (!StringUtils.equalsIgnoreCase(contextTenantDomain, userTenantDomain)) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_TENANT, contextTenantDomain);
        }

        if (!RecoverySteps.CONFIRM_SIGN_UP.equals(recoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CODE, null);
        }

        //if return data from load method, it means the code is validated. Otherwise it returns exceptions

        try {
            RealmService realmService = IdentityRecoveryServiceDataHolder.getInstance().getRealmService();
            UserStoreManager userStoreManager;
            try {
                userStoreManager = realmService.getTenantUserRealm(IdentityTenantUtil.getTenantId(user.getTenantDomain())).getUserStoreManager();
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
            userClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, Boolean.TRUE.toString());
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

    public NotificationResponseBean resendConfirmationCode(User user, Property[] properties) throws IdentityRecoveryException {

        if (StringUtils.isBlank(user.getTenantDomain())) {
            user.setTenantDomain(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            log.info("confirmUserSelfRegistration :Tenant domain is not in the request. set to default for user : " +
                    user.getUserName());
        }

        if (StringUtils.isBlank(user.getUserStoreDomain())) {
            user.setUserStoreDomain(IdentityUtil.getPrimaryDomainName());
            log.info("confirmUserSelfRegistration :User store domain is not in the request. set to default for user : " + user.getUserName());
        }

        boolean enable = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, user.getTenantDomain()));

        if (!enable) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DISABLE_SELF_SIGN_UP, user
                    .getUserName());
        }

        boolean isNotificationInternallyManage = Boolean.parseBoolean(Utils.getSignUpConfigs
                (IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE, user.getTenantDomain()));

        NotificationResponseBean notificationResponseBean = new NotificationResponseBean(user);
        UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
        UserRecoveryData userRecoveryData = userRecoveryDataStore.loadWithoutCodeExpiryValidation(user);

        if (userRecoveryData == null || StringUtils.isBlank(userRecoveryData.getSecret()) || !RecoverySteps
                .CONFIRM_SIGN_UP.equals(userRecoveryData.getRecoveryStep())) {
            throw Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_OLD_CODE_NOT_FOUND, null);
        }
        //Invalid old code
        userRecoveryDataStore.invalidate(userRecoveryData.getSecret());

        String secretKey = UUIDGenerator.generateUUID();
        UserRecoveryData recoveryDataDO = new UserRecoveryData(user, secretKey, RecoveryScenarios
                .SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP);

        userRecoveryDataStore.store(recoveryDataDO);

        if (isNotificationInternallyManage) {
            triggerNotification(user, IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ACCOUNT_CONFIRM.toString(), secretKey, properties);
        } else {
            notificationResponseBean.setKey(secretKey);
        }

        return notificationResponseBean;

    }

    /**
     * Checks whether the given tenant domain of a username is valid / exists or not.
     *
     * @param tenantDomain Tenant domain.
     * @return True if the tenant domain of the user is valid / available, else false.
     */
    public boolean isValidTenantDomain(String tenantDomain) throws IdentityRecoveryException {

        boolean isValidTenant = false;
        try {
            UserRealm userRealm = getUserRealm(tenantDomain);
            isValidTenant = userRealm != null;

        } catch (CarbonException e) {
            if (log.isDebugEnabled()) {
                log.debug("Error while getting user realm for user " + tenantDomain);
            }
            // In a case of a non existing tenant.
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        }
        return isValidTenant;
    }

    /**
     * Returns whether a given username is already taken by a user or not.
     *
     * @param username Username.
     * @return True if the username is already taken, else false.
     */
    public boolean isUsernameAlreadyTaken(String username) throws IdentityRecoveryException {

        boolean isUsernameAlreadyTaken = true;
        String tenantDomain = MultitenantUtils.getTenantDomain(username);
        try {
            String tenantAwareUsername = MultitenantUtils.getTenantAwareUsername(username);

            UserRealm userRealm = getUserRealm(tenantDomain);
            if (userRealm != null) {
                isUsernameAlreadyTaken = userRealm.getUserStoreManager().isExistingUser(tenantAwareUsername);
            }
        } catch (CarbonException | org.wso2.carbon.user.core.UserStoreException e) {
            throw new IdentityRecoveryException("Error while retrieving user realm for tenant : " + tenantDomain, e);
        }
        return isUsernameAlreadyTaken;
    }

    /**
     * Checks whether self registration is enabled or not for a given tenant domain
     *
     * @param tenantDomain Tenant domain.
     * @return True if self registration is enabled for a tenant domain. If not returns false.
     */
    public boolean isSelfRegistrationEnabled(String tenantDomain) throws IdentityRecoveryException {

        String selfSignUpEnalbed = getIDPProperty(tenantDomain,
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        return Boolean.parseBoolean(selfSignUpEnalbed);
    }

    private String getIDPProperty(String tenantDomain, String propertyName) throws
            IdentityRecoveryException {

        String propertyValue = "";
        try {
            org.wso2.carbon.identity.application.common.model.Property[] configuration =
                    IdentityRecoveryServiceDataHolder.getInstance().getIdentityGovernanceService().
                            getConfiguration(new String[]{IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP},
                                    tenantDomain);
            for (org.wso2.carbon.identity.application.common.model.Property configProperty : configuration) {
                if (configProperty != null && propertyName.equalsIgnoreCase(configProperty.getName())) {
                    propertyValue = configProperty.getValue();
                }
            }
        } catch (IdentityGovernanceException e) {
            throw new IdentityRecoveryException("Error while retrieving resident identity provider for tenant : "
                    + tenantDomain, e);
        }
        return propertyValue;
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
            throw Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_TRIGGER_NOTIFICATION, user
                    .getUserName(), e);
        }
    }

    private void addConsent(String consent, String tenantDomain) throws ConsentManagementException, IdentityRecoveryServerException {

        Gson gson = new Gson();
        ReceiptInput receiptInput = gson.fromJson(consent, ReceiptInput.class);
        ConsentManager consentManager = IdentityRecoveryServiceDataHolder.getInstance().getConsentManager();

        if (receiptInput.getServices().size() < 0) {
            throw new IdentityRecoveryServerException("A service should be available in a receipt");
        }
        // There should be a one receipt
        ReceiptServiceInput receiptServiceInput = receiptInput.getServices().get(0);
        receiptServiceInput.setTenantDomain(tenantDomain);
        try {
            setIDPData(tenantDomain, receiptServiceInput);
        } catch (IdentityProviderManagementException e) {
            throw new ConsentManagementException("Error while retrieving identity provider data", "Error while " +
                    "setting IDP data", e);
        }
        receiptInput.setTenantDomain(tenantDomain);
        consentManager.addConsent(receiptInput);
    }

    private void setIDPData(String tenantDomain, ReceiptServiceInput receiptServiceInput)
            throws IdentityProviderManagementException {

        IdentityProviderManager idpManager = IdentityProviderManager.getInstance();
        IdentityProvider residentIdP = idpManager.getResidentIdP(tenantDomain);

        if (StringUtils.isEmpty(receiptServiceInput.getService())) {
            if (log.isDebugEnabled()) {
                log.debug("No service name found. Hence adding resident IDP home realm ID");
            }
            receiptServiceInput.setService(residentIdP.getHomeRealmId());
        }
        if (StringUtils.isEmpty(receiptServiceInput.getTenantDomain())) {
            receiptServiceInput.setTenantDomain(tenantDomain);
        }
        if (StringUtils.isEmpty(receiptServiceInput.getSpDescription())) {
            if (StringUtils.isNotEmpty(residentIdP.getIdentityProviderDescription())) {
                receiptServiceInput.setSpDescription(residentIdP.getIdentityProviderDescription());
            } else {
                receiptServiceInput.setSpDescription(IdentityRecoveryConstants.Consent.RESIDENT_IDP);
            }
        }
        if (StringUtils.isEmpty(receiptServiceInput.getSpDisplayName())) {
            if (StringUtils.isNotEmpty(residentIdP.getDisplayName())) {
                receiptServiceInput.setSpDisplayName(residentIdP.getDisplayName());
            } else {
                receiptServiceInput.setSpDisplayName(IdentityRecoveryConstants.Consent.RESIDENT_IDP);
            }
        }
    }

    private String getPropertyValue(Property[] properties, String key) {

        String propertyValue = "";
        if (properties != null && StringUtils.isNotEmpty(key)) {
            for (int index = 0; index < properties.length; index++) {
                Property property = properties[index];
                if (key.equalsIgnoreCase(property.getKey())) {
                    propertyValue = property.getValue();
                    ArrayUtils.removeElement(properties, property);
                    break;
                }
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("Returning value for key : " + key + " - " + propertyValue);
        }
        return propertyValue;
    }

    private UserRealm getUserRealm(String tenantDomain) throws CarbonException {

        return AnonymousSessionUtil.getRealmByTenantDomain(IdentityRecoveryServiceDataHolder.getInstance()
                        .getRegistryService(), IdentityRecoveryServiceDataHolder.getInstance().getRealmService(),
                tenantDomain);
    }
}
