/*
 * Copyright (c) 2024, WSO2 LLC. (https://www.wso2.org)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.wso2.carbon.identity.recovery.internal.service.impl.username;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.UsernameRecoverDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;

/**
 * Test class for UsernameRecoveryManagerImpl.
 */
public class UsernameRecoveryManagerImplTest {

    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String TRUE = "true";
    private static final String FALSE = "false";

    @Mock
    private UserAccountRecoveryManager mockUserAccountRecoveryManager;

    @Mock
    private UserRecoveryData mockUserRecoveryData;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    private UserRecoveryDataStore mockUserRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @InjectMocks
    private UsernameRecoveryManagerImpl usernameRecoveryManager;

    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<UserAccountRecoveryManager> mockedRecoveryManagerStatic;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<URLDecoder> mockURLDecoder;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;

    /**
     * Set up the test environment.
     */
    @BeforeMethod
    public void setUp() {

        openMocks(this);
        mockedUtils = mockStatic(Utils.class);
        mockedRecoveryManagerStatic = mockStatic(UserAccountRecoveryManager.class);
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockURLDecoder = mockStatic(URLDecoder.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
    }

    /**
     * Tear down the test environment.
     */
    @AfterMethod
    public void tearDown() {

        mockedUtils.close();
        mockedRecoveryManagerStatic.close();
        mockedJDBCRecoveryDataStore.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockURLDecoder.close();
        mockedIdentityUtil.close();
    }

    /**
     * Test to validate tenant domain.
     *
     * @throws IdentityRecoveryException if an error occurs during validation.
     */
    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void testTenantDomainValidation() throws IdentityRecoveryException {

        when(Utils.handleClientException(
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_EMPTY_TENANT_DOMAIN.getCode(),
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_EMPTY_TENANT_DOMAIN.getMessage(),
                null)).thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.initiate(null, null, null);
    }

    /**
     * Test to validate configurations.
     *
     * @throws IdentityRecoveryException if an error occurs during validation.
     */
    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void testConfigValidation() throws IdentityRecoveryException {

        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(FALSE);
        when(Utils.handleClientException(
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USERNAME_RECOVERY_NOT_ENABLED, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.initiate(null, TENANT_DOMAIN, null);
    }

    /**
     * Data provider for channel ID.
     *
     * @return Object array containing channel IDs.
     */
    @DataProvider
    public Object[][] channelIDProvider() {
        return new Object[][] {
                { null },
                { "0" }
        };
    }

    /**
     * Test to validate channel ID exception.
     *
     * @param channelId the channel ID to validate.
     * @throws IdentityRecoveryException if an error occurs during validation.
     */
    @Test(dataProvider = "channelIDProvider", expectedExceptions = IdentityRecoveryClientException.class)
    public void testChannelIDValidation(String channelId) throws IdentityRecoveryException {

        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        when(mockUserAccountRecoveryManager.getUserListByClaims(null, TENANT_DOMAIN)).thenReturn(null);
        when(Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.notify(null, channelId, TENANT_DOMAIN, properties);
    }

    /**
     * Test to invalidate recovery code.
     *
     * @throws IdentityRecoveryException if an error occurs during invalidation.
     */
    @Test(expectedExceptions = NullPointerException.class)
    public void testInvalidateRecoveryCode() throws IdentityRecoveryException {

        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
    }

    /**
     * Test to invalidate recovery code exception.
     *
     * @throws IdentityRecoveryException if an error occurs during invalidation.
     */
    @Test(expectedExceptions = NullPointerException.class)
    public void testInvalidateRecoveryCodeWithException() throws IdentityRecoveryException {

        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRecoveryFlowId()).thenReturn("FlowID");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
    }

    /**
     * Test to extract notification channel details exception.
     *
     * @throws IdentityRecoveryException if an error occurs during extraction.
     */
    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void testExtractChannelDetails() throws IdentityRecoveryException {

        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRemainingSetIds()).thenReturn("123");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        when(Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
    }

    /**
     * Test to notify user.
     *
     * @throws IdentityRecoveryException if an error occurs during notification.
     */
    @Test
    public void testNotifyUser() throws IdentityRecoveryException {

        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRemainingSetIds()).thenReturn("EXTERNAL,EXTERNAL");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        User mockUser = new User();
        mockUser.setUserName("KD123");
        mockUser.setTenantDomain(TENANT_DOMAIN);
        when(mockUserRecoveryData.getUser()).thenReturn(mockUser);
        when(Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_CHANNEL_ID, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        UsernameRecoverDTO code = usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
        assertEquals(code.getCode(), "UNR-02002");
    }

    /**
     * Test to notify user with exception.
     *
     * @throws IdentityRecoveryException if an error occurs during notification.
     */
    @Test
    public void testNotifyUserException() throws IdentityRecoveryException {

        mockIdentityEventService();
        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", FALSE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRemainingSetIds()).thenReturn("SMS,SMS");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        when(Utils.resolveEventName(anyString())).thenReturn("TRIGGER_SMS_NOTIFICATION_LOCAL");
        User mockUser = new User();
        mockUser.setUserName("KD123");
        mockUser.setTenantDomain(TENANT_DOMAIN);
        when(mockUserRecoveryData.getUser()).thenReturn(mockUser);
        UsernameRecoverDTO result = usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
        assertEquals(result.getCode(), "UNR-02001");
        assertEquals(result.getMessage(), "Username recovery information sent via user preferred notification channel.");
    }

    /**
     * Test to validate callback URL.
     *
     * @throws IdentityRecoveryException if an error occurs during validation.
     */
    @Test
    public void testCallbackURLValidation() throws IdentityRecoveryException {

        mockIdentityEventService();
        String callbackURL = "http://localhost:8080";
        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", TRUE);
        properties.put(IdentityRecoveryConstants.CALLBACK, callbackURL);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRemainingSetIds()).thenReturn("SMS,SMS");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        when(Utils.resolveEventName(anyString())).thenReturn("TRIGGER_SMS_NOTIFICATION_LOCAL");
        User mockUser = new User();
        mockUser.setUserName("KD123");
        mockUser.setTenantDomain(TENANT_DOMAIN);
        when(mockUserRecoveryData.getUser()).thenReturn(mockUser);
        when(Utils.handleClientException(
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_CALLBACK_URL_NOT_VALID, callbackURL))
                .thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
    }

    /**
     * Test to get callback URL with exception.
     *
     * @throws IdentityRecoveryException if an error occurs during retrieval.
     */
    @Test(expectedExceptions = NullPointerException.class)
    public void testCallbackURLDecoding() throws IdentityRecoveryException {

        mockIdentityEventService();
        String callbackURL = "http://localhost:8080";
        String recoveryCode = UUID.randomUUID().toString();
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", TRUE);
        properties.put(IdentityRecoveryConstants.CALLBACK, callbackURL);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserRecoveryData(recoveryCode, RecoverySteps.SEND_RECOVERY_INFORMATION))
                .thenReturn(mockUserRecoveryData);
        when(mockUserRecoveryData.getRemainingSetIds()).thenReturn("SMS,SMS");
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        when(Utils.resolveEventName(anyString())).thenReturn("TRIGGER_SMS_NOTIFICATION_LOCAL");
        mockURLDecoder.when(() -> URLDecoder.decode(anyString(), anyString())).thenThrow(new UnsupportedEncodingException());
        usernameRecoveryManager.notify(recoveryCode, "2", TENANT_DOMAIN, properties);
    }

    /**
     * Test to initiate recovery if username is null.
     *
     * @throws IdentityRecoveryException if an error occurs during initiation.
     */
    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void testInitiateRecoveryWithNullUsername() throws IdentityRecoveryException {

        org.wso2.carbon.user.core.common.User testUser = mock(org.wso2.carbon.user.core.common.User.class);
        testUser.setUserID("123");
        testUser.setTenantDomain(TENANT_DOMAIN);
        testUser.setUsername("testUser");
        ArrayList<org.wso2.carbon.user.core.common.User> userList = new ArrayList<>();
        userList.add(testUser);
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", TRUE);
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserListByClaims(null, TENANT_DOMAIN)).thenReturn(userList);
        when(IdentityUtil.getProperty(anyString())).thenReturn(TRUE);
        when(Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        usernameRecoveryManager.initiate(null, TENANT_DOMAIN, properties);
    }

    /**
     * Test to initiate recovery if username is null and return a valid result.
     *
     * @throws IdentityRecoveryException if an error occurs during initiation.
     */
    @Test
    public void testInitiateRecoveryValidUsername() throws IdentityRecoveryException {

        mockIdentityEventService();
        org.wso2.carbon.user.core.common.User testUser = mock(org.wso2.carbon.user.core.common.User.class);
        when(testUser.getDomainQualifiedUsername()).thenReturn("testUser");
        ArrayList<org.wso2.carbon.user.core.common.User> userList = new ArrayList<>();
        userList.add(testUser);
        Map<String, String> properties = new HashMap<>();
        properties.put("useLegacyAPI", TRUE);
        when(Utils.getRecoveryConfigs(anyString(), anyString())).thenReturn(TRUE);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockUserRecoveryDataStore);
        mockedRecoveryManagerStatic.when(UserAccountRecoveryManager::getInstance).thenReturn(mockUserAccountRecoveryManager);
        when(mockUserAccountRecoveryManager.getUserListByClaims(null, TENANT_DOMAIN)).thenReturn(userList);
        when(IdentityUtil.getProperty(anyString())).thenReturn(TRUE);
        when(Utils.isNotificationsInternallyManaged(TENANT_DOMAIN, properties)).thenReturn(true);
        when(Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND, null))
                .thenReturn(new IdentityRecoveryClientException(null));
        RecoveryInformationDTO result = usernameRecoveryManager.initiate(null, TENANT_DOMAIN, properties);
        assertEquals(result.getUsername(), "testUser");
    }

    /**
     * Mock the IdentityEventService.
     */
    private void mockIdentityEventService() {

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(
                identityRecoveryServiceDataHolder);
        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
    }
}
