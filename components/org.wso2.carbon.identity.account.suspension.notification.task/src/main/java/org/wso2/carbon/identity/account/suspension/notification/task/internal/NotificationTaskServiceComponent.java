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
package org.wso2.carbon.identity.account.suspension.notification.task.internal;

import org.apache.log4j.Logger;
import org.osgi.framework.BundleContext;
import org.osgi.service.component.ComponentContext;
import org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrievalFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.handler.AccountSuspensionNotificationHandler;
import org.wso2.carbon.identity.account.suspension.notification.task.jdbc.JDBCNotificationReceiversRetrievalFactory;
import org.wso2.carbon.identity.account.suspension.notification.task.ldap.LDAPNotificationReceiversRetrievalFactory;
import org.wso2.carbon.identity.event.handler.AbstractEventHandler;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;

/**
 * Notification scheduler. Check for users who requires a notification for relogin
 * and send a notification as configured.
 * @scr.component name="NotificationTaskServiceComponent"
 * immediate="true"
 * @scr.reference name="EventMgtService"
 * interface="org.wso2.carbon.identity.event.services.IdentityEventService" cardinality="1..1"
 * policy="dynamic" bind="setIdentityEventService" unbind="unsetIdentityEventService"
 * @scr.reference name="IdentityGovernanceService"
 * interface="org.wso2.carbon.identity.governance.IdentityGovernanceService" cardinality="1..1"
 * policy="dynamic" bind="setIdentityGovernanceService" unbind="unsetIdentityGovernanceService"
 * @scr.reference name="NotificationTaskServiceComponent"
 * interface="org.wso2.carbon.identity.account.suspension.notification.task.NotificationReceiversRetrievalFactory"
 * cardinality="0..n" policy="dynamic" bind="setNotificationReceiversRetrievalFactory"
 * unbind="unsetNotificationReceiversRetrievalFactory"
 * @scr.reference name="user.realmservice.default"
 * interface="org.wso2.carbon.user.core.service.RealmService"
 * cardinality="1..1" policy="dynamic" bind="setRealmService" unbind="unsetRealmService"
 */
public class NotificationTaskServiceComponent {
    /*

    * */
    private static final Logger log = Logger.getLogger(NotificationTaskServiceComponent.class);

    protected void activate(ComponentContext context) throws UserStoreException {

        BundleContext bundleContext = context.getBundleContext();
        NotificationTaskDataHolder.getInstance().setBundleContext(bundleContext);

        AccountSuspensionNotificationHandler handler = new AccountSuspensionNotificationHandler();
        context.getBundleContext().registerService(AbstractEventHandler.class.getName(), handler, null);

        LDAPNotificationReceiversRetrievalFactory ladLdapNotificationReceiversRetrievalFactory = new
                LDAPNotificationReceiversRetrievalFactory();
        bundleContext.registerService(NotificationReceiversRetrievalFactory.class.getName(),
                ladLdapNotificationReceiversRetrievalFactory, null);

        JDBCNotificationReceiversRetrievalFactory jdbcNotificationReceiversRetrievalFactory =
                new JDBCNotificationReceiversRetrievalFactory();
        bundleContext.registerService(NotificationReceiversRetrievalFactory.class.getName(),
                jdbcNotificationReceiversRetrievalFactory, null);

    }

    protected void deactivate(ComponentContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Notification bundle de-activated");
        }
    }

    protected void unsetIdentityEventService(IdentityEventService eventService) {
        NotificationTaskDataHolder.getInstance().setIdentityEventService(null);
    }

    protected void setIdentityEventService(IdentityEventService eventService) {
        NotificationTaskDataHolder.getInstance().setIdentityEventService(eventService);
    }

    protected void unsetIdentityGovernanceService(IdentityGovernanceService idpManager) {
        NotificationTaskDataHolder.getInstance().setIdentityGovernanceService(null);
    }

    protected void setIdentityGovernanceService(IdentityGovernanceService idpManager) {
        NotificationTaskDataHolder.getInstance().setIdentityGovernanceService(idpManager);
    }

    protected void setNotificationReceiversRetrievalFactory(
            NotificationReceiversRetrievalFactory notificationReceiversRetrievalFactory) {

        NotificationTaskDataHolder.getInstance().getNotificationReceiversRetrievalFactories()
                .put(notificationReceiversRetrievalFactory.getType(), notificationReceiversRetrievalFactory);
        if (log.isDebugEnabled()) {
            log.debug("Added notification retriever : " + notificationReceiversRetrievalFactory.getType());
        }

    }

    protected void unsetNotificationReceiversRetrievalFactory(
            NotificationReceiversRetrievalFactory notificationReceiversRetrievalFactory) {

        NotificationTaskDataHolder.getInstance().getNotificationReceiversRetrievalFactories()
                .remove(notificationReceiversRetrievalFactory.getType());

        if (log.isDebugEnabled()) {
            log.debug("Removed notification retriever : " + notificationReceiversRetrievalFactory.getType());
        }
    }

    protected void setRealmService(RealmService realmService) {

        NotificationTaskDataHolder.getInstance().setRealmService(realmService);
        if (log.isDebugEnabled()) {
            log.debug("RealmService is set in the User Store Count bundle");
        }
    }

    protected void unsetRealmService(RealmService realmService) {

        NotificationTaskDataHolder.getInstance().setRealmService(null);
        if (log.isDebugEnabled()) {
            log.debug("RealmService is unset in the Application Authentication Framework bundle");
        }
    }
}
