package org.wso2.carbon.identity.governance.listener;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationTemplateManagerException;
import org.wso2.carbon.identity.governance.model.NotificationTemplate;
import org.wso2.carbon.identity.governance.service.notification.NotificationTemplateManager;

public class NotificationTemplateManagerTest {

    private NotificationTemplateManager notificationTemplateManager;
    private final String notificationChannel = "EMAIL";
    private final String displayName = "Sample Template";
    private final String tenantDomain = "carbon.super";
    private final String locale = "en_US";
    private final String applicationUuid = "app-1234";
    private final NotificationTemplate notificationTemplate = new NotificationTemplate();

    @BeforeTest
    public void setUp() {
        notificationTemplateManager = new NotificationTemplateManager() {
            // No implementation needed, using default methods which throw UnsupportedOperationException
        };
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testAddNotificationTemplateType() throws NotificationTemplateManagerException {

        notificationTemplateManager.addNotificationTemplateType(notificationChannel, displayName, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testAddNotificationTemplateTypeWithAppId() throws NotificationTemplateManagerException {

        notificationTemplateManager.addNotificationTemplateType(notificationChannel, displayName, tenantDomain,
                applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetAllNotificationTemplateTypes() throws NotificationTemplateManagerException {

        notificationTemplateManager.getAllNotificationTemplateTypes(notificationChannel, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testDeleteNotificationTemplateType() throws NotificationTemplateManagerException {

        notificationTemplateManager.deleteNotificationTemplateType(notificationChannel, displayName, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testIsNotificationTemplateTypeExists() throws NotificationTemplateManagerException {

        notificationTemplateManager.isNotificationTemplateTypeExists(notificationChannel, displayName, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testResetNotificationTemplateType() throws NotificationTemplateManagerException {

        notificationTemplateManager.resetNotificationTemplateType(notificationChannel, displayName, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetAllNotificationTemplates() throws NotificationTemplateManagerException {

        notificationTemplateManager.getAllNotificationTemplates(notificationChannel, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplatesOfType() throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplatesOfType(notificationChannel, displayName,
                tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplatesOfTypeWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplatesOfType(notificationChannel, displayName,
                tenantDomain, applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplate() throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplate(notificationChannel, displayName, locale, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplateWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplate(notificationChannel, displayName, locale, tenantDomain,
                applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testAddNotificationTemplate() throws NotificationTemplateManagerException {

        notificationTemplateManager.addNotificationTemplate(notificationTemplate, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testAddNotificationTemplateWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.addNotificationTemplate(notificationTemplate, tenantDomain, applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testUpdateNotificationTemplate() throws NotificationTemplateManagerException {

        notificationTemplateManager.updateNotificationTemplate(notificationTemplate, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testUpdateNotificationTemplateWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.updateNotificationTemplate(notificationTemplate, tenantDomain, applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testDeleteNotificationTemplate() throws NotificationTemplateManagerException {

        notificationTemplateManager.deleteNotificationTemplate(notificationChannel, displayName, locale,
                tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testDeleteNotificationTemplateWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.deleteNotificationTemplate(notificationChannel, displayName, locale,
                tenantDomain, applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testAddDefaultNotificationTemplates() throws NotificationTemplateManagerException {

        notificationTemplateManager.addDefaultNotificationTemplates(notificationChannel, tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetDefaultNotificationTemplates() {

        notificationTemplateManager.getDefaultNotificationTemplates(notificationChannel);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testIsNotificationTemplateExists() throws NotificationTemplateManagerException {

        notificationTemplateManager.isNotificationTemplateExists(notificationChannel, displayName, locale,
                tenantDomain);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testIsNotificationTemplateExistsWithApplicationId() throws NotificationTemplateManagerException {

        notificationTemplateManager.isNotificationTemplateExists(notificationChannel, displayName, locale,
                tenantDomain, applicationUuid);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
    public void testGetAllSystemNotificationTemplatesOfType() throws NotificationTemplateManagerException {

        notificationTemplateManager.getAllSystemNotificationTemplatesOfType(notificationChannel, displayName);
    }

    @Test(expectedExceptions = UnsupportedOperationException.class)
     public void testGetSystemNotificationTemplate() throws NotificationTemplateManagerException {

        notificationTemplateManager.getSystemNotificationTemplate(notificationChannel, displayName, locale);
    }
}
