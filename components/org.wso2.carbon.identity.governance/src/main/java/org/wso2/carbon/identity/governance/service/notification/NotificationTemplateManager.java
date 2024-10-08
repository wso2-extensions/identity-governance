/*
 * Copyright (c) 2019-2024, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
     * Add a new notification template to the system (db/ registry).
     *
     * @param notificationChannel Notification channel
     * @param displayName         Notification template display name
     * @param tenantDomain        Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while adding the template to the registry
     */
    default void addNotificationTemplateType(String notificationChannel, String displayName, String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Add a new notification template to the system (db/ registry).
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param displayName         Notification template display name.
     * @param tenantDomain        Tenant domain.
     * @param applicationUuid     Application UUID.
     * @throws NotificationTemplateManagerException If an error occurred while adding the template to the registry.
     */
    default void addNotificationTemplateType(String notificationChannel, String displayName,
                                             String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

    }

    /**
     * Get all available notification template types for the tenant.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param tenantDomain        Tenant domain.
     * @return List of notification template types.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template types.
     */
    default List<String> getAllNotificationTemplateTypes(String notificationChannel, String tenantDomain)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Delete a notification template type from the tenant registry.
     *
     * @param notificationChannel Notification channel.
     * @param templateDisplayName Display name of the template type.
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while deleting the notification template type.
     */
    default void deleteNotificationTemplateType(String notificationChannel, String templateDisplayName,
                                                String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Check whether the given notification template type exists in the system.
     *
     * @param notificationChannel     Notification channel.
     * @param templateTypeDisplayName Display name of the template type.
     * @param tenantDomain            Tenant Domain.
     * @throws NotificationTemplateManagerException If an error occurred while checking if template type exists.
     */
    default boolean isNotificationTemplateTypeExists(String notificationChannel, String templateTypeDisplayName,
                                                     String tenantDomain)
            throws NotificationTemplateManagerException {

        return false;
    }

    /**
     * Get all available notification template types for the tenant.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template types.
     */
    default List<NotificationTemplate> getAllNotificationTemplates(String notificationChannel, String tenantDomain)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Get all available system  notification templates of given type.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @return List of system notification templates.
     */
    default List<NotificationTemplate> getAllSystemNotificationTemplatesOfType(String notificationChannel,
                                             String templateDisplayName) throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Get all notification templates of the given type.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template.
     * @param tenantDomain        Tenant domain.
     * @return List of notification templates.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification templates.
     */
    default List<NotificationTemplate> getNotificationTemplatesOfType(String notificationChannel,
                                                                      String templateDisplayName, String tenantDomain)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Get all notification templates of the given type.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template.
     * @param tenantDomain        Tenant domain.
     * @param applicationUuid     Application UUID.
     * @return List of notification templates.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification templates.
     */
    default List<NotificationTemplate> getNotificationTemplatesOfType(String notificationChannel,
                                              String templateDisplayName, String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        return null;
    }

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
     * Return the notification template from the tenant registry which matches the given channel and template name.
     *
     * @param notificationChannel Notification Channel Name (Eg: SMS or EMAIL).
     * @param templateType        Display name of the template.
     * @param locale              Locale.
     * @param tenantDomain        Tenant Domain.
     * @param applicationUuid     Application UUID.
     * @return Return {@link org.wso2.carbon.identity.governance.model.NotificationTemplate} object.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template.
     */
    default NotificationTemplate getNotificationTemplate(String notificationChannel, String templateType, String locale,
                                                         String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Add the notification template.
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
     * Add the notification template.
     *
     * @param notificationTemplate  Notification template.
     * @param tenantDomain          Tenant domain.
     * @param applicationUuid       Application UUID.
     * @throws NotificationTemplateManagerException If an error occurred while adding the notification template.
     */
    default void addNotificationTemplate(NotificationTemplate notificationTemplate, String tenantDomain,
                                         String applicationUuid) throws NotificationTemplateManagerException {

    }

    /**
     * Update notification template.
     *
     * @param notificationTemplate Notification template
     *                             {@link org.wso2.carbon.identity.governance.model.NotificationTemplate}
     * @param tenantDomain         Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while updating the notification template
     */
    default void updateNotificationTemplate(NotificationTemplate notificationTemplate, String tenantDomain)
            throws NotificationTemplateManagerException {

    }

    /**
     * Update notification template of application.
     *
     * @param notificationTemplate  Notification template.
     * @param tenantDomain          Tenant domain.
     * @param applicationUuid       Application UUID.
     * @throws NotificationTemplateManagerException If an error occurred while updating the notification template.
     */
    default void updateNotificationTemplate(NotificationTemplate notificationTemplate, String tenantDomain,
                                         String applicationUuid) throws NotificationTemplateManagerException {

    }

    /**
     * Delete a notification template from the system.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template.
     * @param locale              Locale of the template.
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while deleting the notification template.
     */
    default void deleteNotificationTemplate(String notificationChannel, String templateDisplayName, String locale,
                                            String tenantDomain)
            throws NotificationTemplateManagerException {

    }


    /**
     * Delete an application notification template from the system.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template.
     * @param locale              Locale of the template.
     * @param tenantDomain        Tenant domain.
     * @param applicationUuid     Application UUID.
     * @throws NotificationTemplateManagerException If an error occurred while deleting the notification template.
     */
    default void deleteNotificationTemplate(String notificationChannel, String templateDisplayName, String locale,
                                            String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

    }


    /**
     * Delete all organization and application notification templates of a type from the system.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateType        Display name of the template.
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while deleting the notification template.
     */
    default void deleteCustomizedNotificationTemplates(String notificationChannel, String templateType,
                                                       String tenantDomain)
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
    @Deprecated
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

    /**
     * Return the default system notification template which matches the given channel and template name.
     *
     * @param notificationChannel Notification Channel Name (Eg: SMS or EMAIL)
     * @param templateType        Display name of the template
     * @param locale              Locale
     * @return Return {@link org.wso2.carbon.identity.governance.model.NotificationTemplate} object
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template
     */
    default NotificationTemplate getSystemNotificationTemplate(String notificationChannel, String templateType,
                                                                String locale)
            throws NotificationTemplateManagerException {

        return null;
    }

    /**
     * Check whether the given notification template exists in the system.
     *
     * @param notificationChannel     Notification channel.
     * @param templateDisplayName     Display name of the template.
     * @param tenantDomain            Tenant Domain.
     * @throws NotificationTemplateManagerException If an error occurred while checking if template type exists.
     */
    default boolean isNotificationTemplateExists(String notificationChannel, String templateDisplayName, String locale,
                                                 String tenantDomain)
            throws NotificationTemplateManagerException {

        return false;
    }

    /**
     * Check whether the given notification template exists in the system.
     *
     * @param notificationChannel     Notification channel.
     * @param templateDisplayName     Display name of the template.
     * @param tenantDomain            Tenant Domain.
     * @param applicationUuid         Application UUID.
     * @return True if the template exists, false otherwise.
     * @throws NotificationTemplateManagerException If an error occurred while checking if template type exists.
     */
    default boolean isNotificationTemplateExists(String notificationChannel, String templateDisplayName, String locale,
                                                 String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        return false;
    }
}
