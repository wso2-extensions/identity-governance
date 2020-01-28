/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.governance.service.notification;

import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationTemplateManagerException;
import org.wso2.carbon.identity.governance.model.NotificationTemplate;

import java.util.List;

/**
 * Service interface for Notification Template Manager.
 */
public interface NotificationTemplateManager {

    /**
     * Return the notification template from the tenant registry which matches the given channel and template name.
     *
     * @param notificationChannel Notification Channel Name (Eg: SMS or EMAIL)
     * @param templateType        Display name of the template
     * @param locale              Locale
     * @param tenantDomain        Tenant Domain
     * @return Return {@link org.wso2.carbon.identity.governance.model.NotificationTemplate} object
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template
     */
    default NotificationTemplate getNotificationTemplate(String notificationChannel, String templateType, String locale,
                                                         String tenantDomain)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Add the notification template to the registry.
     *
     * @param notificationTemplate Notification template
     *                             {@link org.wso2.carbon.identity.governance.model.NotificationTemplate}
     * @param tenantDomain         Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while adding the notification template
     */
    default void addNotificationTemplate(NotificationTemplate notificationTemplate, String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Add a new notification template to the registry to the corresponding notification channel root directory.
     *
     * @param displayName         Notification template display name
     * @param notificationChannel Notification channel
     * @param tenantDomain        Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while adding the template to the registry
     */
    default void addNotificationTemplateType(String displayName, String notificationChannel, String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Add the default notification templates which matches the given notification channel to the respective tenants
     * registry.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL)
     * @param tenantDomain        Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while adding the default notification templates
     */
    default void addDefaultNotificationTemplates(String notificationChannel, String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Get the notification templates which matches the given notification template type.
     *
     * @param notificationChannel Notification channel type (Eg: EMAIL, SMS)
     * @return List of default notification templates
     */
    default List<NotificationTemplate> getDefaultNotificationTemplates(String notificationChannel) {

        return null;
    }
}
