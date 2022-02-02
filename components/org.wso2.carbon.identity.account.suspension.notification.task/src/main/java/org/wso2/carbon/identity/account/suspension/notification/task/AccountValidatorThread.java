/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.account.suspension.notification.task;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.EmailUtil;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.user.api.Tenant;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AccountValidatorThread implements Runnable {

    private static final Log log = LogFactory.getLog(AccountValidatorThread.class);

    public AccountValidatorThread() {

    }

    @Override
    public void run() {

        if (log.isDebugEnabled()) {
            log.debug("Idle account suspension task started.");
        }

        RealmService realmService = NotificationTaskDataHolder.getInstance().getRealmService();

        Tenant[] tenants = new Tenant[0];

        try {
            tenants = realmService.getTenantManager().getAllTenants();
        } catch (UserStoreException e) {
            log.error("Error occurred while retrieving tenants", e);
        }

        handleTask(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        for (Tenant tenant : tenants) {
            handleTask(tenant.getDomain());
        }

    }

    private void handleTask(String tenantDomain) {

        if (log.isDebugEnabled()) {
            log.debug("Handling idle account suspension task for tenant: " + tenantDomain);
        }

        Property[] identityProperties;
        try {
            // Start Tenant flow
            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext privilegedCarbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
            privilegedCarbonContext.setTenantId(IdentityTenantUtil.getTenantId(tenantDomain));
            privilegedCarbonContext.setTenantDomain(tenantDomain);

            identityProperties = NotificationTaskDataHolder.getInstance().getIdentityGovernanceService()
                    .getConfiguration(getPropertyNames(), tenantDomain);
            boolean isEnabled = false;
            long suspensionDelay = 0;
            long[] notificationDelays = null;
            for (Property identityProperty : identityProperties) {

                if (identityProperty == null) {
                    continue;
                }

                if (NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED.equals(identityProperty.getName())) {
                    isEnabled = Boolean.parseBoolean(identityProperty.getValue());

                    if (!isEnabled) {
                        return;
                    }
                }

                if (NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY.
                        equals(identityProperty.getName())) {
                    try {
                        suspensionDelay = Long.parseLong(identityProperty.getValue());
                    } catch (NumberFormatException e) {
                        log.error("Error occurred while reading account suspension delay for tenant: " + tenantDomain,
                                e);
                    }
                }

                if (NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS.equals(identityProperty.getName())) {

                    if (identityProperty.getValue() != null) {
                        String[] parts = identityProperty.getValue().split(",");
                        notificationDelays = new long[parts.length];
                        for (int i = 0; i < parts.length; i++) {
                            try {
                                notificationDelays[i] = Long.parseLong(parts[i]);
                            } catch (NumberFormatException e) {
                                log.error("Error occurred while reading account suspension notification delays for "
                                        + "tenant: " + tenantDomain, e);
                            }
                        }
                    }
                }
            }

            if (log.isDebugEnabled()) {
                if (isEnabled) {
                    log.debug("Account suspension task is enabled for : " + tenantDomain);
                } else {
                    log.debug("Account suspension task is not enabled for : " + tenantDomain);
                }
            }

            if (!isEnabled) {
                return;
            }

            notifyUsers(tenantDomain, suspensionDelay, notificationDelays);

            lockAccounts(tenantDomain, suspensionDelay);

        } catch (IdentityGovernanceException e) {
            log.error("Error occurred while loading governance configuration for tenants", e);
        } catch (IdentityException e) {
            log.error("Unable to disable user accounts", e);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    /**
     * Notify users about account inactivity via Email.
     */
    private void notifyUsers(String tenantDomain, long suspensionDelay, long[] notificationDelays) {
        EmailUtil util = new EmailUtil();
        for (long delay : notificationDelays) {
            List<NotificationReceiver> receivers = null;
            try {
                receivers = NotificationReceiversRetrievalManager.getReceivers(delay, tenantDomain, suspensionDelay);
            } catch (AccountSuspensionNotificationException e) {
                log.error("Error occurred while retrieving notification receivers", e);
            }
            if (CollectionUtils.isNotEmpty(receivers)) {
                for (NotificationReceiver receiver : receivers) {
                    if (log.isDebugEnabled()) {
                        log.debug("Sending notification to: " + IdentityUtil.addDomainToName(receiver.getUsername(),
                                receiver.getUserStoreDomain()) + "@" + tenantDomain);
                    }
                    util.sendEmail(receiver);
                }
            }
        }
    }

    /**
     * Disable user accounts which exceeds max inactivity timeout.
     *
     * @throws IdentityException
     */
    private void lockAccounts(String tenantDomain, long suspensionDelay) throws IdentityException {
        List<NotificationReceiver> receivers = null;
        try {
            receivers = NotificationReceiversRetrievalManager.getReceivers(suspensionDelay, tenantDomain,
                    suspensionDelay);

        } catch (AccountSuspensionNotificationException e) {
            throw IdentityException.error("Error occurred while retrieving users for account disable", e);
        }
        if (receivers.size() > 0) {
            for (NotificationReceiver receiver : receivers) {
                if (log.isDebugEnabled()) {
                    log.debug("Locking idle account: " + IdentityUtil.addDomainToName(receiver.getUsername(),
                            receiver.getUserStoreDomain()) + "@" + tenantDomain);
                }
                RealmService realmService = NotificationTaskDataHolder.getInstance().getRealmService();
                int tenantId = IdentityTenantUtil.getTenantId(tenantDomain);

                UserRealm userRealm;
                try {
                    userRealm = (UserRealm) realmService.getTenantUserRealm(tenantId);
                } catch (UserStoreException e) {
                    throw new IdentityException("Failed retrieve the user realm for tenant: " + tenantDomain, e);
                }

                UserStoreManager userStoreManager;
                try {
                    userStoreManager = userRealm.getUserStoreManager();
                } catch (org.wso2.carbon.user.core.UserStoreException e) {
                    throw new IdentityException("Failed retrieve the user store manager for tenant: " + tenantDomain,
                            e);
                }

                Map<String, String> updatedClaims = new HashMap<>();
                updatedClaims.put(NotificationConstants.ACCOUNT_LOCKED_CLAIM, Boolean.TRUE.toString());
                updatedClaims.put(NotificationConstants.ACCOUNT_LOCKED_REASON_CLAIM,
                        IdentityMgtConstants.LockedReason.IDLE_ACCOUNT.toString());
                updatedClaims.put(NotificationConstants.PASSWORD_RESET_FAIL_ATTEMPTS_CLAIM, "0");
                try {
                    userStoreManager.setUserClaimValues(IdentityUtil.addDomainToName(receiver.getUsername(),
                            receiver.getUserStoreDomain()), updatedClaims, UserCoreConstants.DEFAULT_PROFILE);
                } catch (org.wso2.carbon.user.core.UserStoreException e) {
                    throw new IdentityException("Failed to update claim values for user: " + IdentityUtil
                            .addDomainToName(receiver.getUsername(), receiver.getUserStoreDomain()) + " in tenant: " +
                            tenantDomain);
                }

                User user = new User();
                user.setUserName(receiver.getUsername());
                user.setTenantDomain(tenantDomain);
                user.setUserStoreDomain(receiver.getUserStoreDomain());
                triggerNotification(user);
            }
        }
    }

    private String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS);
        return properties.toArray(new String[0]);
    }

    private void triggerNotification(User user) throws IdentityException {

        HashMap<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.USER_NAME, user.getUserName());
        properties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, user.getTenantDomain());
        properties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, user.getUserStoreDomain());

        Event identityMgtEvent = new Event(IdentityEventConstants.Event.POST_ACCOUNT_SUSPENSION, properties);
        try {
            NotificationTaskDataHolder.getInstance().getIdentityEventService().handleEvent(identityMgtEvent);
        } catch (IdentityEventException e) {
            throw new IdentityException("Failed to trigger an account suspension notification for user :  " +
                    user.toFullQualifiedUsername(), e);
        }
    }
}
