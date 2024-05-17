/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.recovery.internal.service.impl;

import org.apache.commons.lang.StringUtils;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.base.MultitenantConstants;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.dto.NotificationChannelDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryChannelInfoDTO;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.model.Condition;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

/**
 * Class which contains the test cases for UserAccountRecoveryManager.
 */
public class UserAccountRecoveryManagerTest {

    @InjectMocks
    private UserAccountRecoveryManager userAccountRecoveryManager;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    RealmService realmService;

    @Mock
    ClaimMetadataManagementService claimMetadataManagementService;

    @Mock
    UserRealm userRealm;

    @Mock
    AbstractUserStoreManager abstractUserStoreManager;

    @Mock
    UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    IdentityEventService identityEventService;

    @Mock
    ClaimManager claimManager;

    /**
     * User claims map.
     */
    private HashMap<String, String> userClaims;
    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;

    @BeforeMethod
    public void setUp() {

        mockedJDBCRecoveryDataStore = Mockito.mockStatic(JDBCRecoveryDataStore.class);
        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedUtils = Mockito.mockStatic(Utils.class);
        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedIdentityRecoveryServiceDataHolder = Mockito.mockStatic(IdentityRecoveryServiceDataHolder.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedJDBCRecoveryDataStore.close();
        mockedIdentityUtil.close();
        mockedUtils.close();
        mockedIdentityTenantUtil.close();
        mockedIdentityRecoveryServiceDataHolder.close();
    }

    @BeforeTest
    private void setup() {

        MockitoAnnotations.openMocks(this);
        userAccountRecoveryManager = UserAccountRecoveryManager.getInstance();
        userClaims = buildUserClaimsMap();
    }

    /**
     * Test retrieve user recovery information.
     *
     * @throws Exception Error while getting user recovery information
     */
    @Test
    public void testRetrieveUserRecoveryInformation() throws Exception {

        // Test no matching users for a given set of claims.
        testNoMatchingUsersForGivenClaims();
        // Test notifications externally managed.
        testGetUserWithNotificationsExternallyManaged();
        // Test notifications internally managed.
        testGetUserWithNotificationsInternallyManaged();
    }

    /**
     * Test get user recovery when the notifications are internally managed.
     *
     * @throws Exception Error getting recovery information
     */
    private void testGetUserWithNotificationsInternallyManaged() throws Exception {

        mockUserstoreManager();
        mockRecoveryConfigs(true);
        mockJDBCRecoveryDataStore();
        mockBuildUser();
        // Test when the user is self-registered.
        testGetSelfSignUpUsers();
        // Test when the user is not self registered.
        testGetGeneralUsers();
    }

    /**
     * Test recovery data for self registered users with verified notification channels.
     *
     * @throws Exception Error while getting recovery information
     */
    private void testGetGeneralUsers() throws Exception {

        HashMap<String, String> userClaims = new HashMap<>(this.userClaims);

        // Changing the role to internal and remove channel verified information.
        userClaims.put(UserProfile.USER_ROLE.key, "INTERNAL");
        userClaims.remove(UserProfile.EMAIL_VERIFIED.key);
        userClaims.remove(UserProfile.PHONE_VERIFIED.key);
        when(abstractUserStoreManager
                .getUserClaimValues(anyString(), ArgumentMatchers.any(String[].class), anyString()))
                .thenReturn(userClaims);
        when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                anyInt(),anyInt(),isNull(), isNull())).thenReturn(getOneFilteredUser());
        RecoveryChannelInfoDTO recoveryChannelInfoDTO = userAccountRecoveryManager
                .retrieveUserRecoveryInformation(userClaims, StringUtils.EMPTY, RecoveryScenarios.USERNAME_RECOVERY,
                        null);
        assertNotNull(recoveryChannelInfoDTO, "Recovery Information for user : ");
        assertEquals(recoveryChannelInfoDTO.getUsername(), UserProfile.USERNAME.getValue(),
                "Notifications Externally managed scenario. Recovered username : ");
        assertNotNull(recoveryChannelInfoDTO.getRecoveryCode(),
                "Notifications Externally managed scenario. RecoveryCode : ");
        NotificationChannelDTO[] notificationChannelDTOS = recoveryChannelInfoDTO.getNotificationChannelDTOs();
        assertEquals(notificationChannelDTOS.length, 2,
                "Notifications Externally managed scenario. Available recovery channels");
        checkMaskedRecoveryValues(notificationChannelDTOS);
    }

    /**
     * Test recovery data for self registered users with verified notification channels.
     *
     * @throws Exception Error while getting recovery information
     */
    private void testGetSelfSignUpUsers() throws Exception {

        when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                anyInt(),anyInt(),isNull(), isNull())).thenReturn(getOneFilteredUser());
        when(abstractUserStoreManager
                .getUserClaimValues(anyString(), ArgumentMatchers.any(String[].class), isNull()))
                .thenReturn(userClaims);
        RecoveryChannelInfoDTO recoveryChannelInfoDTO = userAccountRecoveryManager
                .retrieveUserRecoveryInformation(userClaims, StringUtils.EMPTY, RecoveryScenarios.USERNAME_RECOVERY,
                        null);
        assertNotNull(recoveryChannelInfoDTO, "Recovery Information for user : ");
        assertEquals(recoveryChannelInfoDTO.getUsername(), UserProfile.USERNAME.getValue(),
                "Notifications Externally managed scenario. Recovered username : ");
        assertNotNull(recoveryChannelInfoDTO.getRecoveryCode(),
                "Notifications Externally managed scenario. RecoveryCode : ");
        NotificationChannelDTO[] notificationChannelDTOS = recoveryChannelInfoDTO.getNotificationChannelDTOs();
        assertEquals(notificationChannelDTOS.length, 2,
                "Notifications Externally managed scenario. Available recovery channels");
        checkMaskedRecoveryValues(notificationChannelDTOS);
    }

    /**
     * Check the length of masked notification channel information.
     *
     * @param notificationChannelDTOS NotificationChannelDTO list
     */
    private void checkMaskedRecoveryValues(NotificationChannelDTO[] notificationChannelDTOS) {

        for (NotificationChannelDTO notificationChannelDTO : notificationChannelDTOS) {
            if (notificationChannelDTO.getType().equals(NotificationChannels.EMAIL_CHANNEL.getChannelType())) {
                assertEquals(notificationChannelDTO.getValue().length(), UserProfile.EMAIL_ADDRESS.value.length());
            } else {
                assertEquals(notificationChannelDTO.getValue().length(), UserProfile.MOBILE.value.length());
            }
        }
    }

    /**
     * Test notifications externally managed scenario.
     *
     * @throws Exception Error while getting user recovery data
     */
    private void testGetUserWithNotificationsExternallyManaged() throws Exception {

        mockUserstoreManager();
        mockRecoveryConfigs(false);
        mockJDBCRecoveryDataStore();
        mockIdentityEventService();
        mockBuildUser();
        when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                anyInt(),anyInt(),isNull(), isNull())).thenReturn(getOneFilteredUser());
        when(claimManager.getAttributeName(anyString(),anyString())).
                thenReturn("http://wso2.org/claims/mockedClaim");
        RecoveryChannelInfoDTO recoveryChannelInfoDTO = userAccountRecoveryManager
                .retrieveUserRecoveryInformation(userClaims, StringUtils.EMPTY, RecoveryScenarios.USERNAME_RECOVERY,
                        null);
        assertEquals(recoveryChannelInfoDTO.getUsername(), UserProfile.USERNAME.getValue(),
                "Notifications Externally managed scenario. Recovered username : ");
        assertNotNull(recoveryChannelInfoDTO.getRecoveryCode(),
                "Notifications Externally managed scenario. RecoveryCode : ");
        NotificationChannelDTO[] notificationChannelDTOS = recoveryChannelInfoDTO.getNotificationChannelDTOs();
        assertEquals(notificationChannelDTOS.length, 1,
                "Notifications Externally managed scenario. Available recovery channels");
        assertEquals(notificationChannelDTOS[0].getType(), NotificationChannels.EXTERNAL_CHANNEL.getChannelType(),
                "Notification channel : ");
    }

    /**
     * No users matched for the given claims.
     *
     * @throws Exception Error while getting the user recovery information
     */
    private void testNoMatchingUsersForGivenClaims() throws Exception {

        try {
            mockUserstoreManager();
            mockClaimMetadataManagementService();
            mockedUtils.when(
                    () -> Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND,
                            null))
                    .thenReturn(IdentityException.error(IdentityRecoveryClientException.class,
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND.getCode(), ""));
            when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                    anyInt(),anyInt(),isNull(), isNull())).
                    thenReturn(new ArrayList<org.wso2.carbon.user.core.common.User>());
            userAccountRecoveryManager
                    .retrieveUserRecoveryInformation(userClaims, StringUtils.EMPTY, RecoveryScenarios.USERNAME_RECOVERY,
                            null);
        } catch (IdentityRecoveryException e) {
            assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_USER_FOUND.getCode(),
                    "No matching users for given set of claims : ");
        }
    }

    /**
     * Mock JDBCRecoveryDataStore to store user recovery data.
     *
     * @throws IdentityRecoveryException Error while mocking JDBCRecoveryDataStore
     */
    private void mockJDBCRecoveryDataStore() throws IdentityRecoveryException {

        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        doNothing().when(userRecoveryDataStore).invalidate(ArgumentMatchers.any(User.class));
        doNothing().when(userRecoveryDataStore).store(ArgumentMatchers.any(UserRecoveryData.class));
    }

    /**
     * Mock recovery configs.
     *
     * @param isNotificationInternallyManaged Whether the notifications internally managed
     * @throws Exception Error while mocking the configs
     */
    private void mockRecoveryConfigs(boolean isNotificationInternallyManaged) throws Exception {

        mockedIdentityUtil.when(() -> IdentityUtil.extractDomainFromName(anyString())).thenReturn("PRIMARY");
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn("PRIMARY");
        mockedUtils.when(() -> Utils.isAccountDisabled(ArgumentMatchers.any(User.class))).thenReturn(false);
        mockedUtils.when(() -> Utils.isAccountLocked(ArgumentMatchers.any(User.class))).thenReturn(false);
        mockedUtils.when(() -> Utils.isNotificationsInternallyManaged(anyString(), isNull()))
                .thenReturn(isNotificationInternallyManaged);
    }

    /**
     * Test get username by claims list.
     *
     * @throws Exception Error testing for testGetUsernameByClaims
     */
    @Test
    public void testGetUsernameByClaims() throws Exception {

        // Test no claims provided error.
        testNoClaimsProvidedToRetrieveMatchingUsers();
        // Test multiple users matching for the given set of claims.
        testMultipleUsersMatchingForGivenClaims();
        // Test no matching users for given set of claims.
        testNoMatchingUsers();
        // Test get matched user for given set of claims.
        testGetMatchedUser();
    }

    /**
     * Test get matched user for given claims.
     *
     * @throws Exception Error while getting the matched user
     */
    private void testGetMatchedUser() throws Exception {

        String testUsername1 = UserProfile.USERNAME.value;
        mockUserstoreManager();
        when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                anyInt(),anyInt(),isNull(), isNull())).thenReturn(getOneFilteredUser());
        String username = userAccountRecoveryManager
                .getUsernameByClaims(userClaims, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        assertEquals(username, testUsername1, "Test get matched users for given claims : ");
    }

    /**
     * Test no matching claims for given set of claims.
     *
     * @throws Exception Error getting the matching username
     */
    private void testNoMatchingUsers() throws Exception {

        mockUserstoreManager();
        when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                anyInt(),anyInt(),isNull(), isNull())).thenReturn(
                        new ArrayList<org.wso2.carbon.user.core.common.User>());
        String username = userAccountRecoveryManager
                .getUsernameByClaims(userClaims, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
        assertTrue(username.isEmpty(), "No matching users for given set of claims : ");
    }

    /**
     * Test no claims provided to retrieve a matching user for the given set of claims.
     */
    private void testNoClaimsProvidedToRetrieveMatchingUsers() {

        // Test no claims provided scenario.
        try {
            mockedUtils.when(() -> Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY))
                    .thenReturn(
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.
                                    getCode());
            mockedUtils.when(() -> Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getMessage(),
                    null)).thenReturn(IdentityException.error(IdentityRecoveryClientException.class,
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getCode(),
                    ""));
            String username = userAccountRecoveryManager
                    .getUsernameByClaims(new HashMap<String, String>(), MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            assertNull(username, "UserAccountRecoveryManager: No claims have provided to retrieve the user : ");
        } catch (IdentityRecoveryException e) {
            // Get error code with scenario.
            String errorCode = Utils.prependOperationScenarioToErrorCode(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_NO_FIELD_FOUND_FOR_USER_RECOVERY.getCode(),
                    IdentityRecoveryConstants.USER_ACCOUNT_RECOVERY);
            assertEquals(e.getErrorCode(), errorCode,
                    "UserAccountRecoveryManager: No claims have provided to retrieve the user : ");
        }
    }

    /**
     * Test multiple users matching for the given set of claims error.
     *
     * @throws Exception Error while checking for matched users.
     */
    private void testMultipleUsersMatchingForGivenClaims() throws Exception {

        mockUserstoreManager();
        try {
            mockedUtils.when(() -> Utils.handleClientException(
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_MATCHING_USERS, null))
                    .thenReturn(IdentityException.error(IdentityRecoveryClientException.class,
                            IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_MATCHING_USERS.getCode(),""));
            when(abstractUserStoreManager.getUserListWithID(any(Condition.class),anyString(),anyString(),
                    anyInt(),anyInt(),isNull(), isNull())).thenReturn(getFilteredUsers());
            when(claimManager.getAttributeName(anyString(),anyString())).
                    thenReturn("http://wso2.org/claims/mockedClaim");
            String username = userAccountRecoveryManager
                    .getUsernameByClaims(userClaims, MultitenantConstants.SUPER_TENANT_DOMAIN_NAME);
            assertNull(username, "UserAccountRecoveryManager: Exception should be thrown. Therefore, a "
                    + "value for an identified user cannot be returned : ");
        } catch (IdentityRecoveryException e) {
            assertEquals(e.getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_MULTIPLE_MATCHING_USERS.getCode(),
                    "Invalid error code for existing multiple users for given set of claims");
        }
    }


    /**
     * Get UserstoreManager by mocking IdentityRecoveryServiceDataHolder.
     *
     * @throws Exception Error while getting UserstoreManager
     */
    private void mockUserstoreManager() throws Exception {

        // Mock getTenantId.
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        // Get UserStoreManager.
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(
                identityRecoveryServiceDataHolder);
        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(ArgumentMatchers.anyInt())).thenReturn(userRealm);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
    }

    private void mockBuildUser() {

        User user = new User();
        user.setUserName(UserProfile.USERNAME.value);
        user.setUserStoreDomain("PRIMARY");
        when(Utils.buildUser(anyString(), anyString())).thenReturn(user);
    }

    private void mockIdentityEventService() {

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(
                identityRecoveryServiceDataHolder);
        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
    }

    private void mockClaimMetadataManagementService() {

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(
                identityRecoveryServiceDataHolder);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService())
                .thenReturn(claimMetadataManagementService);
    }

    /**
     * Get multiple filtered users
     *
     * @return Users
     */
    private List<org.wso2.carbon.user.core.common.User> getFilteredUsers() {

        List<org.wso2.carbon.user.core.common.User> users = new ArrayList<org.wso2.carbon.user.core.common.User>();
        org.wso2.carbon.user.core.common.User testUser1 = new org.wso2.carbon.user.core.common.User(UUID.randomUUID()
                .toString(), "sominda1", "sominda1");
        users.add(testUser1);

        org.wso2.carbon.user.core.common.User testUser2 = new org.wso2.carbon.user.core.common.User(UUID.randomUUID()
                .toString(), "sominda2", "sominda2");
        users.add(testUser2);

        return users;
    }

    /**
     * Get one filtered user
     *
     * @return Users
     */
    private List<org.wso2.carbon.user.core.common.User> getOneFilteredUser() {

        List<org.wso2.carbon.user.core.common.User> users = new ArrayList<org.wso2.carbon.user.core.common.User>();
        org.wso2.carbon.user.core.common.User testUser1 = new org.wso2.carbon.user.core.common.User(UUID.randomUUID()
                .toString(), "sominda1", "sominda1");
        users.add(testUser1);
        return users;
    }

    /**
     * Build user claims information map
     *
     * @return User claims
     */
    private HashMap<String, String> buildUserClaimsMap() {

        HashMap<String, String> userClaims = new HashMap<>();
        userClaims.put(UserProfile.EMAIL_ADDRESS.getKey(), UserProfile.EMAIL_ADDRESS.getValue());
        userClaims.put(UserProfile.MOBILE.getKey(), UserProfile.MOBILE.getValue());
        userClaims.put(UserProfile.PREFERRED_CHANNEL.getKey(), UserProfile.PREFERRED_CHANNEL.getValue());
        userClaims.put(UserProfile.EMAIL_VERIFIED.getKey(), UserProfile.EMAIL_VERIFIED.getValue());
        userClaims.put(UserProfile.PHONE_VERIFIED.getKey(), UserProfile.PHONE_VERIFIED.getValue());
        userClaims.put(UserProfile.USER_ROLE.getKey(), UserProfile.USER_ROLE.getValue());
        return userClaims;
    }

    /**
     * Enum contains the status codes and status messages for successful user self registration scenarios.
     */
    public enum UserProfile {

        USERNAME("Username", "sominda1"),
        EMAIL_ADDRESS(NotificationChannels.EMAIL_CHANNEL.getClaimUri(), "sominda@gmail.com"),
        MOBILE(NotificationChannels.SMS_CHANNEL.getClaimUri(), "1234567890"),
        PREFERRED_CHANNEL(IdentityRecoveryConstants.PREFERRED_CHANNEL_CLAIM, "EMAIL"),
        EMAIL_VERIFIED(NotificationChannels.EMAIL_CHANNEL.getVerifiedClaimUrl(), "TRUE"),
        PHONE_VERIFIED(NotificationChannels.SMS_CHANNEL.getVerifiedClaimUrl(), "TRUE"),
        USER_ROLE(IdentityRecoveryConstants.USER_ROLES_CLAIM, IdentityRecoveryConstants.SELF_SIGNUP_ROLE);

        private final String key;
        private final String value;

        UserProfile(String key, String value) {

            this.key = key;
            this.value = value;
        }

        /**
         * Get the key of the claim.
         *
         * @return Code
         */
        public String getKey() {

            return key;
        }

        /**
         * Get the value of the claim.
         *
         * @return Message
         */
        public String getValue() {

            return value;
        }
    }
}
