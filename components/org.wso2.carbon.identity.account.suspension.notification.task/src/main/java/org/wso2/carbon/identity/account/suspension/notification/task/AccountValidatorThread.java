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
import org.wso2.carbon.identity.account.suspension.notification.task.bean.AccountValidatorThreadProperties;
import org.wso2.carbon.identity.account.suspension.notification.task.exception.AccountSuspensionNotificationException;
import org.wso2.carbon.identity.account.suspension.notification.task.util.EmailUtil;
import org.wso2.carbon.identity.account.suspension.notification.task.util.NotificationReceiver;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.mgt.services.UserIdentityManagementAdminService;

import java.util.List;

public class AccountValidatorThread implements Runnable {

    private static final Log log = LogFactory.getLog(AccountValidatorThread.class);

    private AccountValidatorThreadProperties accountValidatorThreadProperties = null;

    public AccountValidatorThread(AccountValidatorThreadProperties accountValidatorThreadProperties) {
        this.accountValidatorThreadProperties = accountValidatorThreadProperties;
    }

    public AccountValidatorThread() {
    }

    @Override public void run() {
        if (log.isDebugEnabled()) {
            log.debug("Executing the scheduled task...");
        }

        notifyUsers();

        try {
            disableAccounts();
        } catch (IdentityException e) {
            log.error("Unable to disable user accounts", e);
        }
    }

    /**
     * Notify users about account inactivity via Email.
     */
    private void notifyUsers() {
        EmailUtil util = new EmailUtil();
        for (long delay : accountValidatorThreadProperties.getNotificationDelays()) {
            List<NotificationReceiver> receivers = null;
            try {
                receivers = NotificationReceiversRetrievalManager.getReceivers(delay, accountValidatorThreadProperties);
            } catch (AccountSuspensionNotificationException e) {
                log.error("Error occurred while retrieving notification receivers", e);
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
    private void disableAccounts() throws IdentityException {
        List<NotificationReceiver> receivers = null;
        try {
            receivers = NotificationReceiversRetrievalManager
                    .getReceivers(accountValidatorThreadProperties.getDelayForSuspension(),
                            accountValidatorThreadProperties);
        } catch (AccountSuspensionNotificationException e) {
            throw IdentityException.error("Error occurred while retrieving users for account disable", e);
        }
        if (receivers.size() > 0) {
            try {
                PrivilegedCarbonContext.startTenantFlow();
                PrivilegedCarbonContext privilegedCarbonContext = PrivilegedCarbonContext.getThreadLocalCarbonContext();
                privilegedCarbonContext.setTenantId(accountValidatorThreadProperties.getTenantId());
                privilegedCarbonContext.setTenantDomain(accountValidatorThreadProperties.getTenantDomain());

                UserIdentityManagementAdminService userIdentityManagementAdminService = new UserIdentityManagementAdminService();
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
}
