/*
 * Copyright (c) 2019, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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
package org.wso2.carbon.identity.governance.internal.service.impl.notification;

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
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.exceptions.notiification.NotificationChannelManagerException;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.HashMap;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Class contains test cases for DefaultNotificationChannelManager.
 */
public class DefaultNotificationChannelManagerTest {

    /**
     * DefaultNotificationChannelManager instance.
     */
    @InjectMocks
    private DefaultNotificationChannelManager defaultNotificationChannelManager;

    /**
     * Claims map with channel related attributes.
     */
    private HashMap<String, String> channelClaims;

    @Mock
    UserStoreManager userStoreManager;

    @Mock
    RealmService realmService;

    @Mock
    UserRealm userRealm;

    @Mock
    IdentityMgtServiceDataHolder identityMgtServiceDataHolder;

    private static final String SUCCESSFUL_CHANNEL_RESOLVE = "Successful channel resolve";
    private static final String ERROR_IN_CHANNEL_RESOLVE = "Error while resolving the channel";
    private static final String CHANNEL_RESOLVING_NOT_ENABLED = "Channel resolving not enabled";
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<IdentityMgtServiceDataHolder> mockedIdentityMgtServiceDataHolder;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() {

        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedIdentityMgtServiceDataHolder = Mockito.mockStatic(IdentityMgtServiceDataHolder.class);
        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedIdentityMgtServiceDataHolder.close();
        mockedIdentityTenantUtil.close();
    }

    /**
     * Initializing variables.
     */
    @BeforeTest
    public void setup() {

        MockitoAnnotations.openMocks(this);
        defaultNotificationChannelManager = new DefaultNotificationChannelManager();

        // Get the claims map with the corresponding channel claims and values.
        channelClaims = getDefaultClaimsMap(new String[] {
                NotificationChannels.EMAIL_CHANNEL.getClaimUri(), NotificationChannels.SMS_CHANNEL.getClaimUri()
        }, new String[] { "test@wso2.com", "1234567890" });
    }

    /**
     * Testing for supported notification channels
     */
    @Test
    public void testIsSupportedChannel() {

        // SMS notification channel.
        boolean isSupportedChannel1 = defaultNotificationChannelManager.isSupportedChannel("SMS");
        assertTrue(isSupportedChannel1);

        // EMAIL notification channel.
        boolean isSupportedChannel2 = defaultNotificationChannelManager.isSupportedChannel("EMAIL");
        assertTrue(isSupportedChannel2);

        // Unsupported channel notification channel.
        boolean isSupportedChannel3 = defaultNotificationChannelManager.isSupportedChannel("CALL");
        assertFalse(isSupportedChannel3);

        // Case sensitivity test.
        boolean isSupportedChannel4 = defaultNotificationChannelManager.isSupportedChannel("email");
        assertTrue(isSupportedChannel4);
    }

    /**
     * Test resolve notification channel for the user by resolving the channel claims in the request.
     *
     * @param channelClaims              Channel claims
     * @param defaultNotificationChannel Default notification channel
     * @param expectedChannel            Expected resolved channel
     * @param scenario                   Channel resolving scenario.
     * @param scenarioType               Channel resolving scenario type
     * @param expectedErrorCode          Expected error
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    @Test(dataProvider = "channelClaimsForChannelResolveUsingUserClaimsMap")
    public void testResolveCommunicationChannel(HashMap<String, String> channelClaims,
            String defaultNotificationChannel, String expectedChannel, String scenario, String scenarioType,
            String expectedErrorCode) throws NotificationChannelManagerException {

        // Test Meta data.
        String testUser = "testUser";
        String testTenantDomain = "testTenantDomain";
        String testUserstoreDomain = "testTenantDomain";

        // Convert the empty strings to null objects, since null objects cannot be passed as method arguments.
        if (StringUtils.isEmpty(expectedChannel)) {
            expectedChannel = null;
        }
        if (SUCCESSFUL_CHANNEL_RESOLVE.equals(scenarioType)) {

            // Mock configurations.
            mockSelfRegistrationConfigurations(defaultNotificationChannel, true);
            String resolvedChannel = defaultNotificationChannelManager
                    .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain, channelClaims);
            assertEquals(resolvedChannel, expectedChannel, scenario);
        } else if (ERROR_IN_CHANNEL_RESOLVE.equals(scenarioType)) {
            try {
                // Mock configurations.
                mockSelfRegistrationConfigurations(defaultNotificationChannel, true);
                String resolvedChannel = defaultNotificationChannelManager
                        .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain, channelClaims);
                assertEquals(resolvedChannel, expectedChannel, scenario);
            } catch (NotificationChannelManagerException e) {
                assertEquals(e.getErrorCode(), expectedErrorCode, scenario);
            }
        } else if (CHANNEL_RESOLVING_NOT_ENABLED.equals(scenarioType)) {

            // Mock configurations.
            mockSelfRegistrationConfigurations(defaultNotificationChannel, false);
            String resolvedChannel = defaultNotificationChannelManager
                    .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain, channelClaims);
            assertEquals(resolvedChannel, expectedChannel, scenario);
        }
    }

    /**
     * Test resolving notification channel using the user.
     *
     * @param channelClaims              Channel claims
     * @param defaultNotificationChannel Default notification channel
     * @param expectedChannel            Expected resolved channel
     * @param scenario                   Channel resolving scenario.
     * @param scenarioType               Channel resolving scenario type
     * @param expectedErrorCode          Expected error
     * @throws NotificationChannelManagerException Error while resolving the channel
     */
    @Test(dataProvider = "channelClaimsForChannelResolveUsingUser")
    public void testResolveCommunicationChannelWithUsername(HashMap<String, String> channelClaims,
            String defaultNotificationChannel, String expectedChannel, String scenario, String scenarioType,
            String expectedErrorCode) throws Exception {

        // Meta Data
        String testUser = "testUser";
        String testTenantDomain = "testTenantDomain";
        String testUserstoreDomain = "testTenantDomain";

        // Mock classes and configurations.
        mockUserstoreManager(channelClaims);
        mockSelfRegistrationConfigurations(defaultNotificationChannel,true);

        if (StringUtils.isEmpty(expectedChannel)) {
            expectedChannel = null;
        }
        if (SUCCESSFUL_CHANNEL_RESOLVE.equals(scenarioType)) {
            String resolvedChannel = defaultNotificationChannelManager
                    .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain);
            assertEquals(resolvedChannel, expectedChannel, scenario);

        } else if (ERROR_IN_CHANNEL_RESOLVE.equals(scenarioType)) {
            try {
                String resolvedChannel = defaultNotificationChannelManager
                        .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain);
                assertEquals(resolvedChannel, expectedChannel, scenario);
            } catch (NotificationChannelManagerException e) {
                assertEquals(e.getErrorCode(), expectedErrorCode, scenario);
            }
        } else if (CHANNEL_RESOLVING_NOT_ENABLED.equals(scenarioType)){
            mockSelfRegistrationConfigurations(defaultNotificationChannel, false);
            String resolvedChannel = defaultNotificationChannelManager
                    .resolveCommunicationChannel(testUser, testTenantDomain, testUserstoreDomain);
            assertEquals(resolvedChannel, expectedChannel, scenario);
        }
    }

    /**
     * Mock self registration configurations.
     *
     * @param defaultNotificationChannel Default notification channel
     * @param enableResolving            Enable channel resolving
     */
    private void mockSelfRegistrationConfigurations(String defaultNotificationChannel, boolean enableResolving) {

        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(
                IdentityMgtConstants.NotificationChannelConstants.DEFAULT_NOTIFICATION_CHANNEL))
                .thenReturn(defaultNotificationChannel);
        mockedIdentityUtil.when(
                () -> IdentityUtil.getProperty(IdentityMgtConstants.PropertyConfig.RESOLVE_NOTIFICATION_CHANNELS))
                .thenReturn(Boolean.toString(enableResolving));
    }

    /**
     * Mock userstore manager to get user claims.
     *
     * @param claimsMap User claims
     * @throws Exception Error while mocking Userstoremanager
     */
    private void mockUserstoreManager(HashMap<String, String> claimsMap) throws Exception {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(ArgumentMatchers.anyString()))
                .thenReturn(-1234);
        mockedIdentityMgtServiceDataHolder.when(
                (MockedStatic.Verification) IdentityMgtServiceDataHolder.getInstance())
                .thenReturn(identityMgtServiceDataHolder);
        Mockito.when(identityMgtServiceDataHolder.getRealmService()).thenReturn(realmService);
        Mockito.when(realmService.getTenantUserRealm(ArgumentMatchers.anyInt())).thenReturn(userRealm);
        Mockito.when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        Mockito.when(
                userStoreManager.getUserClaimValues(ArgumentMatchers.anyString(), ArgumentMatchers.any(String[].class),
                        ArgumentMatchers.isNull())).thenReturn(claimsMap);
    }

    /**
     * Method to create a claims map.
     * NOTE: the length of both arguments needs to be the same.
     *
     * @param keys   Key set
     * @param values Values
     * @return Map of claims
     */
    private HashMap<String, String> getDefaultClaimsMap(String[] keys, String[] values) {

        HashMap<String, String> channelClaims = new HashMap<>();
        for (int counter = 0; counter < keys.length; counter++) {
            channelClaims.put(keys[counter], values[counter]);
        }
        return channelClaims;
    }

    /**
     * Contains user data related to the channels to resolve the notification channel using user claims in the request.
     *
     * @return Object[][]
     */
    @DataProvider(name = "channelClaimsForChannelResolveUsingUserClaimsMap")
    private Object[][] buildChannelClaimSet1() {

        // Preferred Channel EMAIL with email claim values.
        HashMap<String, String> channelClaimsMap1 = new HashMap<>(channelClaims);
        channelClaimsMap1.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.EMAIL_CHANNEL.getChannelType());
        String defaultChannel1 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel1 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message1 = "SCENARIO: User specified the Preferred Channel as EMAIL and has email claim values : ";

        // Preferred Channel SMS with SMS claim values.
        HashMap<String, String> channelClaimsMap2 = new HashMap<>(channelClaims);
        channelClaimsMap2.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.SMS_CHANNEL.getChannelType());
        String defaultChannel2 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel2 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message2 = "SCENARIO: User specified the Preferred Channel as SMS and has mobile claim values : ";

        // User has not specified a preferred channel, but have values for email claim.
        HashMap<String, String> channelClaimsMap3 = new HashMap<>(channelClaims);
        channelClaimsMap3.remove(NotificationChannels.SMS_CHANNEL.getClaimUri());
        String defaultChannel3 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel3 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message3 = "SCENARIO: User has no preferred channel, has email claims only : ";

        // User has not specified a preferred channel, but have values for mobile claim.
        HashMap<String, String> channelClaimsMap4 = new HashMap<>(channelClaims);
        channelClaimsMap4.remove(NotificationChannels.EMAIL_CHANNEL.getClaimUri());
        String defaultChannel4 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel4 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message4 = "SCENARIO: User has no preferred channel, has mobile claims only : ";

        // User has not specified a preferred channel, but provided both email and mobile values.
        // Default notification channel in EMAIL.
        HashMap<String, String> channelClaimsMap5 = new HashMap<>(channelClaims);
        String defaultChannel5 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel5 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message5 = "SCENARIO: User has both email and mobile claims. Default channel EMAIL : ";

        // User has not specified a preferred channel, but provided both email and mobile values.
        // Default notification channel in SMS.
        HashMap<String, String> channelClaimsMap6 = new HashMap<>(channelClaims);
        String defaultChannel6 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String expectedChannel6 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message6 = "SCENARIO: User has both email and mobile claims. Default channel SMS : ";

        // User has not specified a preferred channel or notification channels.
        HashMap<String, String> channelClaimsMap7 = new HashMap<>();
        String defaultChannel7 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message7 = "SCENARIO: User has not specified any notification channels : ";

        /* ERROR SCENARIOS */
        // No claim values for the preferred channel.
        HashMap<String, String> channelClaimsMap8 = new HashMap<>();
        channelClaimsMap8.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.EMAIL_CHANNEL.getChannelType());
        String defaultChannel8 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message8 = "SCENARIO: User specified the Preferred Channel as EMAIL, but no email claim values : ";
        String expectedErrorCode8 = IdentityMgtConstants.ErrorMessages.
                ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL.getCode();

        // Invalid notification channel as the preferred notification channel.
        HashMap<String, String> channelClaimsMap9 = new HashMap<>(channelClaims);
        channelClaimsMap9.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM, "new notification channel");
        String defaultChannel9 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message9 = "SCENARIO: User specified an invalid notification channel type : ";
        String expectedErrorCode9 = IdentityMgtConstants.ErrorMessages.
                ERROR_CODE_UNSUPPORTED_PREFERRED_CHANNEL.getCode();

        /* CHANNEL RESOLVING CONFIG NOT ENABLED */
        // Default notification channel in EMAIL.
        HashMap<String, String> channelClaimsMap10 = new HashMap<>(channelClaims);
        String defaultChannel10 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel10 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message10 = "SCENARIO: Configs not enabled. Default Channel " + defaultChannel10 + " : ";

        // Default notification channel in SMS.
        HashMap<String, String> channelClaimsMap11 = new HashMap<>(channelClaims);
        String defaultChannel11 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String expectedChannel11 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message11 = "SCENARIO: Configs not enabled. Default Channel " + defaultChannel11 + " : ";

        return new Object[][] {
                { channelClaimsMap1, defaultChannel1, expectedChannel1, message1, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap2, defaultChannel2, expectedChannel2, message2, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap3, defaultChannel3, expectedChannel3, message3, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap4, defaultChannel4, expectedChannel4, message4, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap5, defaultChannel5, expectedChannel5, message5, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap6, defaultChannel6, expectedChannel6, message6, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap7, defaultChannel7, StringUtils.EMPTY, message7, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap8, defaultChannel8, StringUtils.EMPTY, message8, ERROR_IN_CHANNEL_RESOLVE,
                  expectedErrorCode8 },
                { channelClaimsMap9, defaultChannel9, StringUtils.EMPTY, message9, ERROR_IN_CHANNEL_RESOLVE,
                  expectedErrorCode9 },
                { channelClaimsMap10, defaultChannel10, expectedChannel10, message10, CHANNEL_RESOLVING_NOT_ENABLED,
                  null },
                { channelClaimsMap11, defaultChannel11, expectedChannel11, message11, CHANNEL_RESOLVING_NOT_ENABLED,
                  null }
        };
    }

    /**
     * Contains user data related to the channels to resolve the notification channel using username in the request.
     *
     * @return Object[][]
     */
    @DataProvider(name = "channelClaimsForChannelResolveUsingUser")
    private Object[][] buildChannelClaimSet2() {

        // Preferred Channel EMAIL with email claim values.
        HashMap<String, String> channelClaimsMap1 = new HashMap<>(channelClaims);
        channelClaimsMap1.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.EMAIL_CHANNEL.getChannelType());
        String defaultChannel1 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel1 = defaultChannel1;
        String message1 = "SCENARIO: User specified the Preferred Channel as EMAIL and has email claim values : ";

        // Preferred Channel SMS with SMS claim values.
        HashMap<String, String> channelClaimsMap2 = new HashMap<>(channelClaims);
        channelClaimsMap2.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.SMS_CHANNEL.getChannelType());
        String defaultChannel2 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel2 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message2 = "SCENARIO: User specified the Preferred Channel as SMS and has mobile claim values : ";

        // User has not specified a preferred channel, but have values for email claim.
        HashMap<String, String> channelClaimsMap3 = new HashMap<>(channelClaims);
        channelClaimsMap3.remove(NotificationChannels.SMS_CHANNEL.getClaimUri());
        String defaultChannel3 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel3 = defaultChannel3;
        String message3 = "SCENARIO: User has no preferred channel, has email claims only : ";

        // User has not specified a preferred channel, but have values for mobile claim.
        HashMap<String, String> channelClaimsMap4 = new HashMap<>(channelClaims);
        channelClaimsMap4.remove(NotificationChannels.EMAIL_CHANNEL.getClaimUri());
        String defaultChannel4 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel4 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String message4 = "SCENARIO: User has no preferred channel, has mobile claims only : ";

        // User has not specified a preferred channel, but provided both email and mobile values.
        // Default notification channel in EMAIL.
        HashMap<String, String> channelClaimsMap5 = new HashMap<>(channelClaims);
        String defaultChannel5 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel5 = defaultChannel5 ;
        String message5 = "SCENARIO: User has both email and mobile claims. Default channel EMAIL : ";

        // User has not specified a preferred channel, but provided both email and mobile values.
        // Default notification channel in SMS.
        HashMap<String, String> channelClaimsMap6 = new HashMap<>(channelClaims);
        String defaultChannel6 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String expectedChannel6 = defaultChannel6;
        String message6 = "SCENARIO: User has both email and mobile claims. Default channel SMS : ";

        /* ERROR SCENARIOS */
        // Preferred channel as EMAIL but no claims for email channel.
        HashMap<String, String> channelClaimsMap7 = new HashMap<>(channelClaims);
        channelClaimsMap7.remove(NotificationChannels.EMAIL_CHANNEL.getClaimUri());
        channelClaimsMap7.put(IdentityMgtConstants.Claim.PREFERED_CHANNEL_CLAIM,
                NotificationChannels.EMAIL_CHANNEL.getChannelType());
        String defaultChannel7 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message7 = "SCENARIO: User specified the Preferred Channel as EMAIL, but no email claim values : ";
        String expectedErrorCode7 =
                IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_CLAIM_MATCHED_FOR_PREFERRED_CHANNEL.getCode();

        // User has no channel claims.
        HashMap<String, String> channelClaimsMap8 = new HashMap<>();
        String defaultChannel8 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String message8 = "SCENARIO: User no channel claims : ";
        String expectedErrorCode8 =
                IdentityMgtConstants.ErrorMessages.ERROR_CODE_NO_NOTIFICATION_CHANNELS.getCode();

        /* CHANNEL RESOLVING CONFIG NOT ENABLED */
        // Default notification channel in EMAIL.
        HashMap<String, String> channelClaimsMap9 = new HashMap<>(channelClaims);
        String defaultChannel9 = NotificationChannels.EMAIL_CHANNEL.getChannelType();
        String expectedChannel9 = defaultChannel9;
        String message9 = "SCENARIO: Configs not enabled. Default Channel " + defaultChannel9 + " : ";

        // Default notification channel in SMS.
        HashMap<String, String> channelClaimsMap10 = new HashMap<>(channelClaims);
        String defaultChannel10 = NotificationChannels.SMS_CHANNEL.getChannelType();
        String expectedChannel10 = defaultChannel10;
        String message10 = "SCENARIO: Configs not enabled. Default Channel " + defaultChannel10 + " : ";

        return new Object[][] {
                { channelClaimsMap1, defaultChannel1, expectedChannel1, message1, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap2, defaultChannel2, expectedChannel2, message2, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap3, defaultChannel3, expectedChannel3, message3, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap4, defaultChannel4, expectedChannel4, message4, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap5, defaultChannel5, expectedChannel5, message5, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap6, defaultChannel6, expectedChannel6, message6, SUCCESSFUL_CHANNEL_RESOLVE, null },
                { channelClaimsMap7, defaultChannel7, StringUtils.EMPTY, message7, ERROR_IN_CHANNEL_RESOLVE,
                  expectedErrorCode7 },
                { channelClaimsMap8, defaultChannel8, StringUtils.EMPTY, message8, ERROR_IN_CHANNEL_RESOLVE,
                  expectedErrorCode8 },
                { channelClaimsMap9, defaultChannel9, expectedChannel9, message9, CHANNEL_RESOLVING_NOT_ENABLED,
                  null },
                { channelClaimsMap10, defaultChannel10, expectedChannel10, message10, CHANNEL_RESOLVING_NOT_ENABLED,
                  null }
        };
    }
}
