/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.listener;

import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.context.model.Flow;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionStep;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.api.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for {@link InvitedRegistrationCompletionListener}.
 */
public class InvitedRegistrationCompletionListenerTest {

    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolderStatic;
    private MockedStatic<Utils> mockedUtilsStatic;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryStatic;
    private MockedStatic<NotificationPasswordRecoveryManager> mockedRecoveryManagerStatic;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    private InvitedRegistrationCompletionListener listener;

    @BeforeMethod
    public void setUp() {

        mockedDataHolderStatic = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedUtilsStatic = mockStatic(Utils.class);
        mockedJDBCRecoveryStatic = mockStatic(JDBCRecoveryDataStore.class);
        mockedRecoveryManagerStatic = mockStatic(NotificationPasswordRecoveryManager.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);

        listener = new InvitedRegistrationCompletionListener();
    }

    @AfterMethod
    public void tearDown() {

        mockedDataHolderStatic.close();
        mockedUtilsStatic.close();
        mockedJDBCRecoveryStatic.close();
        mockedRecoveryManagerStatic.close();
        mockedIdentityTenantUtil.close();
    }

    @DataProvider
    public String[][] getNotificationChannel() {

        return new String[][]{
                {"EMAIL"},
                {"SMS"},
                {""}
        };
    }

    @Test(dataProvider = "getNotificationChannel")
    public void testDoPostExecuteSuccess(String channel) throws Exception {

        // Setup context.
        FlowExecutionStep step = mock(FlowExecutionStep.class);
        when(step.getFlowStatus()).thenReturn("COMPLETE");
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getFlowType()).thenReturn(Flow.Name.INVITED_USER_REGISTRATION.name());

        User user = new User();
        user.setUserName("john");
        user.setTenantDomain("carbon.super");
        user.setUserStoreDomain("PRIMARY");

        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("code123");
        when(context.getProperty(IdentityRecoveryConstants.USER)).thenReturn(user);
        when(context.getProperty(IdentityRecoveryConstants.NOTIFICATION_CHANNEL)).thenReturn(channel);
        when(context.getProperty(IdentityRecoveryConstants.RECOVERY_SCENARIO)).thenReturn("scenario");

        // Mock NotificationPasswordRecoveryManager
        NotificationPasswordRecoveryManager recoveryManager = mock(NotificationPasswordRecoveryManager.class);
        when(recoveryManager.getServerSupportedNotificationChannel(any())).thenReturn("EMAIL");
        when(recoveryManager.isAskPasswordEmailTemplateTypeExists(any())).thenReturn(true);
        mockedRecoveryManagerStatic.when(NotificationPasswordRecoveryManager::getInstance).thenReturn(recoveryManager);

        // Mock Utils
        mockedUtilsStatic.when(() -> Utils.getRecoveryConfigs(any(), any())).thenReturn("true");
        mockedUtilsStatic.when(() -> Utils.handleServerException((IdentityRecoveryConstants.ErrorMessages) any(),
                any(), any())).thenCallRealMethod();

        // Mock JDBCRecoveryDataStore and UserRecoveryData
        JDBCRecoveryDataStore jdbcStore = mock(JDBCRecoveryDataStore.class);
        UserRecoveryData recoveryData = mock(UserRecoveryData.class);
        when(recoveryData.getRecoveryFlowId()).thenReturn(null);
        when(recoveryData.getUser()).thenReturn(user);
        mockedJDBCRecoveryStatic.when(JDBCRecoveryDataStore::getInstance).thenReturn(jdbcStore);

        // Mock loadUserRecoveryData
        mockedUtilsStatic.when(() -> Utils.loadUserRecoveryData(any())).thenReturn(recoveryData);

        // Mock IdentityRecoveryServiceDataHolder and IdentityEventService
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        IdentityEventService eventService = mock(IdentityEventService.class);
        when(dataHolder.getIdentityEventService()).thenReturn(eventService);
        mockedDataHolderStatic.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);

        // Mock UserStoreManager
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        UserStoreManager userStoreManager = mock(UserStoreManager.class);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(any())).thenReturn(1);
        boolean result = listener.doPostExecute(step, context);

        assertTrue(result);
        verify(userStoreManager).setUserClaimValues(any(), any(), any());
        verify(eventService, atLeastOnce()).handleEvent(any());
        verify(jdbcStore).invalidate(user);
    }

    @Test
    public void testDoPostExecuteWhenUserNull() throws Exception {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        when(step.getFlowStatus()).thenReturn("COMPLETE");
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getFlowType()).thenReturn(Flow.Name.INVITED_USER_REGISTRATION.name());
        when(context.getProperty(IdentityRecoveryConstants.USER)).thenReturn(null);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn("code123");

        boolean result = listener.doPostExecute(step, context);
        assertFalse(result);
    }

    @Test
    public void testDoPostExecuteWhenConfirmationCodeNull() throws Exception {

        FlowExecutionStep step = mock(FlowExecutionStep.class);
        when(step.getFlowStatus()).thenReturn("COMPLETE");
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        when(context.getFlowType()).thenReturn(Flow.Name.INVITED_USER_REGISTRATION.name());
        User user = new User();
        user.setUserName("john");
        when(context.getProperty(IdentityRecoveryConstants.USER)).thenReturn(user);
        when(context.getProperty(IdentityRecoveryConstants.CONFIRMATION_CODE_INPUT)).thenReturn(null);

        boolean result = listener.doPostExecute(step, context);
        assertFalse(result);
    }

    @Test
    public void testIsEnabled() {

        assertTrue(listener.isEnabled());
    }
}
