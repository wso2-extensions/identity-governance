/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.governance.listener;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationTemplateManagerException;
import org.wso2.carbon.identity.governance.model.NotificationTemplate;
import org.wso2.carbon.identity.governance.service.notification.NotificationTemplateManager;

/**
 * Unit tests for NotificationTemplateManager.
 */
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

    @DataProvider(name = "resolveDataProvider")
    public Object[][] resolveDataProvider() {

        return new Object[][]{
                {true},
                {false}
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

    @Test(dataProvider = "resolveDataProvider", expectedExceptions = UnsupportedOperationException.class)
    public void testGetAllNotificationTemplatesWithResolve(boolean resolve)
            throws NotificationTemplateManagerException {

        notificationTemplateManager.getAllNotificationTemplates(notificationChannel, tenantDomain, resolve);
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

    @Test(dataProvider = "resolveDataProvider", expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplatesOfTypeWithApplicationIdWithResolve(boolean resolve)
            throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplatesOfType(notificationChannel, displayName,
                tenantDomain, applicationUuid, resolve);
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

    @Test(dataProvider = "resolveDataProvider", expectedExceptions = UnsupportedOperationException.class)
    public void testGetNotificationTemplateWithApplicationIdWithResolve(boolean resolve)
            throws NotificationTemplateManagerException {

        notificationTemplateManager.getNotificationTemplate(notificationChannel, displayName, locale, tenantDomain,
                applicationUuid, resolve);
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
