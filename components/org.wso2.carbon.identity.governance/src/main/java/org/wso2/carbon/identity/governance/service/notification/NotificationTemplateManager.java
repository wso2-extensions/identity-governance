/*
 * Copyright (c) 2019-2024, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.governance.service.notification;

import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationTemplateManagerException;
import org.wso2.carbon.identity.governance.model.NotificationTemplate;

import java.util.List;

/**
 * Service interface for Notification Template Manager.
 */
public interface NotificationTemplateManager {


    /**
     * Add new notification template type to the system.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param displayName         Notification template display name
     * @param tenantDomain        Tenant domain
     * @throws NotificationTemplateManagerException If an error occurred while adding the template to the registry
     */
    default void addNotificationTemplateType(String notificationChannel, String displayName, String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Add new notification template type to the system.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param displayName         Notification template display name.
     * @param tenantDomain        Tenant domain.
     * @param applicationUuid     Application UUID.
     * @throws NotificationTemplateManagerException If an error occurred while adding the template to the registry.
     */
    @Deprecated
    default void addNotificationTemplateType(String notificationChannel, String displayName,
                                             String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Delete a notification template type from the tenant registry.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template type.
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while deleting the notification template type.
     */
    default void deleteNotificationTemplateType(String notificationChannel, String templateDisplayName,
                                                String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Check whether the given notification template type exists in the system.
     *
     * @param notificationChannel     Notification channel (Eg: SMS, EMAIL).
     * @param templateTypeDisplayName Display name of the template type.
     * @param tenantDomain            Tenant Domain.
     * @throws NotificationTemplateManagerException If an error occurred while checking if template type exists.
     */
    default boolean isNotificationTemplateTypeExists(String notificationChannel, String templateTypeDisplayName,
                                                     String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Resets the template type in the database by deleting of all it's user defined templates.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateType        Display name of the template.
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while resetting the notification template type.
     */
    default void resetNotificationTemplateType(String notificationChannel, String templateType, String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * @deprecated Use {@link #getAllNotificationTemplates(String, String, boolean)} instead.
     * <p>
     * Get all available notification template types in the tenant for a given notification channel.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param tenantDomain        Tenant domain.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification template types.
     */
    @Deprecated
    default List<NotificationTemplate> getAllNotificationTemplates(String notificationChannel, String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Retrieves all notification templates for a given notification channel and tenant domain.
     * <p>
     * This method fetches notification templates based on the specified notification channel
     * (e.g., email, SMS) and tenant domain. The behavior of template retrieval depends on the
     * {@code resolve} parameter:
     * <ul>
     *     <li><b>Resolved templates (resolve = true):</b> Retrieves templates resolved through the
     *     ancestor organization hierarchy, returning templates that are applicable across the tenant's
     *     organizational structure.</li>
     *     <li><b>Current organization's templates (resolve = false):</b> Retrieves templates strictly defined
     *     within the current organization, without considering organizational hierarchy.</li>
     * </ul>
     *
     * @param notificationChannel the type of notification channel for which templates are retrieved
     *                            (e.g., "EMAIL", "SMS").
     * @param tenantDomain        the tenant domain of the organization for which templates are retrieved.
     * @param resolve             a flag to indicate whether to retrieve resolved templates
     *                            ({@code true}) or templates specific to the current organization
     *                            ({@code false}).
     * @return a list of {@link NotificationTemplate} templates matching the specified criteria.
     * @throws NotificationTemplateManagerException if an error occurs while retrieving the templates.
     */
    default List<NotificationTemplate> getAllNotificationTemplates(String notificationChannel, String tenantDomain,
                                                                   boolean resolve)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException(
                "getAllNotificationTemplates method is not implemented in " + this.getClass());
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

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * @deprecated Use {@link #getNotificationTemplatesOfType(String, String, String, String, boolean)} instead.
     * <p>
     * Get all notification templates of the given type.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @param templateDisplayName Display name of the template.
     * @param tenantDomain        Tenant domain.
     * @param applicationUuid     Application UUID.
     * @return List of notification templates.
     * @throws NotificationTemplateManagerException If an error occurred while getting the notification templates.
     */
    @Deprecated
    default List<NotificationTemplate> getNotificationTemplatesOfType(String notificationChannel,
                                              String templateDisplayName, String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Retrieves notification templates of a specific type for a given notification channel and tenant domain.
     * <p>
     * This method fetches notification templates based on the specified notification channel
     * (e.g., email, SMS), template display name, tenant domain, and application UUID. The behavior
     * of template retrieval depends on the {@code resolve} parameter:
     * <ul>
     *     <li><b>Resolved templates (resolve = true):</b> Retrieves templates resolved through the
     *     ancestor organization hierarchy, ensuring that templates applicable across the organizational
     *     structure are returned.</li>
     *     <li><b>Current org's or app's templates (resolve = false):</b> Retrieves templates specific to the
     *     current organization or application, without considering the organization hierarchy.</li>
     * </ul>
     *
     * @param notificationChannel the type of notification channel for which templates are retrieved
     *                            (e.g., "EMAIL", "SMS").
     * @param templateDisplayName the display name of the notification template type (e.g., "Welcome Email").
     * @param tenantDomain        the tenant domain of the organization for which templates are retrieved.
     * @param applicationUuid     the UUID of the application associated with the notification templates.
     * @param resolve             a flag to indicate whether to retrieve resolved templates
     *                            ({@code true}) or templates specific to the current organization or application
     *                            ({@code false}).
     *
     * @return a list of {@link NotificationTemplate} templates matching the specified criteria.
     *
     * @throws NotificationTemplateManagerException if an error occurs while retrieving the templates.
     */
    default List<NotificationTemplate> getNotificationTemplatesOfType(String notificationChannel,
                                                                      String templateDisplayName, String tenantDomain,
                                                                      String applicationUuid, boolean resolve)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException(
                "getNotificationTemplatesOfType method is not implemented in " + this.getClass());
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

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * @deprecated Use {@link #getNotificationTemplate(String, String, String, String, String, boolean)} instead.
     * <p>
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
    @Deprecated
    default NotificationTemplate getNotificationTemplate(String notificationChannel, String templateType, String locale,
                                                         String tenantDomain, String applicationUuid)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Retrieves a specific notification template for a given notification channel, template type,
     * locale, tenant domain, and application UUID.
     * <p>
     * This method fetches a notification template based on the specified criteria. The behavior
     * depends on the {@code resolve} parameter:
     * <ul>
     *     <li><b>Resolved template (resolve = true):</b> Retrieves the template resolved through the
     *     ancestor organization hierarchy.</li>
     *     <li><b>Current org's or app's template  (resolve = false):</b> Retrieves the template specific to
     *     the current organization or application only.</li>
     * </ul>
     *
     * @param notificationChannel the notification channel (e.g., "EMAIL", "SMS").
     * @param templateType        the type of the notification template (e.g., "Welcome Email").
     * @param locale              the locale of the notification template (e.g., "en_US").
     * @param tenantDomain        the tenant domain of the organization from which the template is retrieved.
     * @param applicationUuid     the UUID of the application associated with the template.
     * @param resolve             a flag indicating whether to retrieve resolved template
     *                            ({@code true}) or template specific to the current organization or application
     *                            ({@code false}).
     * @return the {@link NotificationTemplate} template matching the specified criteria.
     * @throws NotificationTemplateManagerException if an error occurs while retrieving the template
     *                                              or if no template is found in the specified or default locale.
     */
    default NotificationTemplate getNotificationTemplate(String notificationChannel, String templateType, String locale,
                                                         String tenantDomain, String applicationUuid, boolean resolve)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException(
                "getNotificationTemplate method is not implemented in " + this.getClass());
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Get the notification templates which matches the given notification template type.
     *
     * @param notificationChannel Notification channel type (Eg: EMAIL, SMS)
     * @return List of default notification templates
     */
    default List<NotificationTemplate> getDefaultNotificationTemplates(String notificationChannel) {

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Check whether the given notification template exists in the system.
     *
     * @param notificationChannel   Notification channel.
     * @param templateDisplayName   Display name of the template.
     * @param tenantDomain          Tenant Domain.
     * @throws NotificationTemplateManagerException If an error occurred while checking if template type exists.
     */
    default boolean isNotificationTemplateExists(String notificationChannel, String templateDisplayName, String locale,
                                                 String tenantDomain)
            throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
    }

    /**
     * Get all available system notification templates of given type.
     *
     * @param notificationChannel Notification channel (Eg: SMS, EMAIL).
     * @return List of system notification templates.
     */
    default List<NotificationTemplate> getAllSystemNotificationTemplatesOfType(String notificationChannel,
                                                                               String templateDisplayName) throws NotificationTemplateManagerException {

        throw new UnsupportedOperationException("Not implemented yet");
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

        throw new UnsupportedOperationException("Not implemented yet");
    }
}
