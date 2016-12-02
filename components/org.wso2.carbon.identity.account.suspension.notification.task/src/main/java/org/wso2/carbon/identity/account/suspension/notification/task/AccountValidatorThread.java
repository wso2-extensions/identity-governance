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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.internal.NotificationTaskDataHolder;
import org.wso2.carbon.identity.account.suspension.notification.task.util.EmailUtil;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationConstants;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.mgt.services.UserIdentityManagementAdminService;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.Tenant;
import org.wso2.carbon.utils.multitenancy.MultitenantConstants;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheduled task to send notifications
 */
public class AccountValidatorThread implements Runnable {

    private static final Log log = LogFactory.getLog(AccountValidatorThread.class);

    public AccountValidatorThread() {

    }

    @Override
    public void run() {

        RealmService realmService = NotificationTaskDataHolder.getInstance().getRealmService();

        Tenant tenants[] = new Tenant[0];

        try {
            tenants = (Tenant[]) realmService.getTenantManager().getAllTenants();
        } catch (UserStoreException e) {
            log.error("Error occurred while retrieving tenants", e);
        }

        handleTask(MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);

        for (Tenant tenant : tenants) {
            handleTask(tenant.getDomain());
        }

    }

    private void handleTask(String tenantDomain) {

        Property[] identityProperties;
        try {
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

            if (!isEnabled) {
                return;
            }

            notifyUsers(tenantDomain, suspensionDelay, notificationDelays);

            disableAccounts(tenantDomain, suspensionDelay);

        } catch (IdentityGovernanceException e) {
            log.error("Error occurred while loading governance configuration for tenants", e);
        } catch (IdentityException e) {
            log.error("Unable to disable user accounts", e);
        }
    }

    /**
     * Notify users about account inactivity via Email.
     */
    private void notifyUsers (String tenantDomain, long suspensionDelay, long[] notificationDelays) {
        EmailUtil util = new EmailUtil();
        for (long delay : notificationDelays) {
            List<NotificationReceiver> receivers = null;
            try {
                receivers = NotificationReceiversRetrievalManager.getReceivers(delay, tenantDomain, suspensionDelay);
            } catch (AccountSuspensionNotificationException e) {
                log.error("Error occurred while retrieving notification receivers", e);
                return;
            }
            if (!receivers.isEmpty()) {
                for (NotificationReceiver receiver : receivers) {
                    if (log.isDebugEnabled()) {
                        log.debug("Sending notification to: " + receiver.getUsername());
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
    private void disableAccounts(String tenantDomain, long suspensionDelay) throws IdentityException {
        List<NotificationReceiver> receivers = null;
        try {
            receivers = NotificationReceiversRetrievalManager.getReceivers(suspensionDelay, tenantDomain,
                    suspensionDelay);

        } catch (AccountSuspensionNotificationException e) {
            throw IdentityException.error("Error occurred while retrieving users for account disable", e);
        }
        if (receivers.size() > 0) {
            try {
                PrivilegedCarbonContext.startTenantFlow();
                PrivilegedCarbonContext privilegedCarbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
                privilegedCarbonContext.setTenantId(IdentityTenantUtil.getTenantId(tenantDomain));
                privilegedCarbonContext.setTenantDomain(tenantDomain);

                UserIdentityManagementAdminService userIdentityManagementAdminService =
                        new UserIdentityManagementAdminService();
                // Iterate through all the receivers and disable them
                for (NotificationReceiver receiver : receivers) {
                    if (log.isDebugEnabled()) {
                        log.debug("Disabling user " + receiver.getUsername());
                    }
                    userIdentityManagementAdminService.disableUserAccount(receiver.getUsername(), "email");
                }
            } finally {
                PrivilegedCarbonContext.endTenantFlow();
            }
        }
    }

    private String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ENABLED);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_ACCOUNT_DISABLE_DELAY);
        properties.add(NotificationConstants.SUSPENSION_NOTIFICATION_DELAYS);
        return properties.toArray(new String[properties.size()]);
    }
}
