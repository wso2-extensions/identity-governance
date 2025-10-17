/*
 * Copyright (c) 2024-2025, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.lang.StringUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.bean.context.MessageContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class UserEmailVerificationHandlerTest {

    @InjectMocks
    private UserEmailVerificationHandler userEmailVerificationHandler;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private IdentityRecoveryServiceDataHolder serviceDataHolder;

    @Mock
    private MessageContext messageContext;

    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;
    private MockedStatic<IdentityUtil> mockedIdentityUtils;
    private MockedStatic<FlowMgtConfigUtils> mockedFlowMgtUtils;

    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final String TEST_USER_STORE_DOMAIN = "TESTING";
    private static final String TEST_USERNAME = "testuser";
    private static final String EXISTING_EMAIL_1 = "old1@abc.com";
    private static final String EXISTING_EMAIL_2 = "old2@abc.com";
    private static final String NEW_EMAIL = "new@abc.com";

    @AfterMethod
    public void close() {

        mockedJDBCRecoveryDataStore.close();
        mockedUtils.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedFrameworkUtils.close();
        mockedIdentityUtils.close();
        mockedFlowMgtUtils.close();
    }

    @BeforeMethod
    public void setUp() throws NoSuchFieldException, IllegalAccessException {

        MockitoAnnotations.openMocks(this);
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedUtils = mockStatic(Utils.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedIdentityUtils = mockStatic(IdentityUtil.class);
        mockedFlowMgtUtils = mockStatic(FlowMgtConfigUtils.class);
        FlowConfigDTO mockFlowConfig = mock(FlowConfigDTO.class);
        when(mockFlowConfig.getIsEnabled()).thenReturn(true);

        userEmailVerificationHandler = new UserEmailVerificationHandler();

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");
        mockedIdentityUtils.when(() -> IdentityUtil.addDomainToName(eq(TEST_USERNAME), anyString()))
                .thenReturn(String.format("%s/%s", TEST_USERNAME, TEST_USER_STORE_DOMAIN));
        mockedFlowMgtUtils.when(() -> FlowMgtConfigUtils.getFlowConfig(anyString(), anyString()))
                .thenReturn(mockFlowConfig);


        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(eq(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))).thenReturn(TEST_USER_STORE_DOMAIN);
    }

    @Test
    public void getNames() {

        Assert.assertEquals(userEmailVerificationHandler.getName(), "userEmailVerification");
        Assert.assertEquals(userEmailVerificationHandler.getFriendlyName(), "User Email Verification");
    }

    @Test(description = "Verification - Disabled, Multi attribute - Disabled")
    public void testHandleEventPreSetUserClaimsVerificationDisabledMultiDisabled()
            throws IdentityEventException, UserStoreException {

        /*
         Notification on email update is enabled.
         Expected: Notification event should be triggered, pending email claim should be set to empty string.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockUtilMethods(false, false, false,
                true);
        mockPrimaryEmail(EXISTING_EMAIL_1);
        mockPendingVerificationEmail(EXISTING_EMAIL_2);

        userEmailVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        // Case 2: Throw error when triggering event.
        doThrow(new IdentityEventException("error")).when(identityEventService).handleEvent(any());
        try {
            userEmailVerificationHandler.handleEvent(event);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        }

        // Reset.
        doNothing().when(identityEventService).handleEvent(any());

        // Case 2: Throw UserStoreException when getting the primary email.
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);
        mockUtilMethods(false, false, false,
                true);
        mockPendingVerificationEmail(EXISTING_EMAIL_2);
        when(userStoreManager.getUserClaimValue(anyString(), eq(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM),
                any())).thenThrow(new UserStoreException("error"));

        try {
            userEmailVerificationHandler.handleEvent(event2);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        }
    }

    @Test(description = "Verification - Disabled, Multi attribute - Enabled")
    public void testHandleEventPreSetUserClaimsVerificationDisabledMultiEnabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         New Email is not in the existing email address list.
         Expected: New email should be added to the new email addresses claim.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockUtilMethods(false, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, EXISTING_EMAIL_2));
        mockExistingEmailAddressesList(existingEmails);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM), NEW_EMAIL);
        // Multiple email addresses related claims should only be updated when they are present in the request.
        Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));

        // Case 2 : Send email addresses claim with event.
        String emailsClaim = String.format("%s,%s", EXISTING_EMAIL_1, EXISTING_EMAIL_2);
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, emailsClaim, NEW_EMAIL);

        mockUtilMethods(false, true, false,
                false);

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM), NEW_EMAIL);
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM), emailsClaim);
    }

    @Test(description = "Verification - Enabled, Multi attribute - Disabled")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiDisabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);
        mockUtilMethods(true, false, false,
                false);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaimsC1 = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaimsC1.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), NEW_EMAIL);

        // Case 2: Send SELF_SIGNUP_ROLE with the event.
        mockPendingVerificationEmail(EXISTING_EMAIL_1);
        String[] roleList = new String[]{IdentityRecoveryConstants.SELF_SIGNUP_ROLE};
        Map<String, Object> additionalEventProperties = new HashMap<>();
        additionalEventProperties.put(IdentityEventConstants.EventProperty.ROLE_LIST, roleList);
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL, additionalEventProperties, null);

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaimsC2 = getUserClaimsFromEvent(event2);
        Assert.assertFalse(userClaimsC2.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM));

        // Case 3: Try to change the primary email value with existing verified primary email value.
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);
        mockPrimaryEmail(NEW_EMAIL);
        mockPrimaryEmailVerificationStatus(true);

        userEmailVerificationHandler.handleEvent(event3);
        Map<String, String> userClaimsC3 = getUserClaimsFromEvent(event3);
        Assert.assertEquals(userClaimsC3.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        // Case 4: Try to change the primary email value with existing unverified primary email value.
        Event event4 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);
        mockPrimaryEmail(NEW_EMAIL);
        mockPrimaryEmailVerificationStatus(false);

        userEmailVerificationHandler.handleEvent(event4);
        Map<String, String> userClaimsC4 = getUserClaimsFromEvent(event4);
        Assert.assertEquals(userClaimsC4.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM),
                NEW_EMAIL);
    }

    @Test(description = "Verification - Enabled, Multi attribute - Disabled, User verify - Enabled")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiDisabledUserVerifyEnabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.TRUE,
                null, null, NEW_EMAIL);
        mockUtilMethods(true, false, true,
                false);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaimsC1 = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaimsC1.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), NEW_EMAIL);

        // Case 2: verifyEmail claim is false.
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);
        mockUtilMethods(true, false, true,
                false);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaimsC2 = getUserClaimsFromEvent(event2);
        Assert.assertEquals(userClaimsC2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Change primary email which is not in the " +
            "verified email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC1()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         Try to change the primary email, new Email is not in the existing verified email address list.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, EXISTING_EMAIL_2));
        mockExistingEmailAddressesList(existingEmails);

        List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), NEW_EMAIL);
        Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
        Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Change primary email which is already " +
            "in the verified email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC2() throws IdentityEventException {

            /*
            Try to change the primary email, new Email is in the existing verified email address list.
            Expected: Thread local should be set to skip sending email verification on update.
            */
            Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                    null, null, NEW_EMAIL);

            mockUtilMethods(true, true, false,
                    false);
            List<String> existingEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, NEW_EMAIL));
            mockExistingEmailAddressesList(existingEmails);

            List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, NEW_EMAIL));
            mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

            userEmailVerificationHandler.handleEvent(event);
            mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(
                    eq(IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates
                            .SKIP_ON_ALREADY_VERIFIED_EMAIL_ADDRESSES.toString())));
            Map<String, String> userClaims = getUserClaimsFromEvent(event);
            Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM));
            Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM));
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Update verified list with new email")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC3()
            throws IdentityEventException, UserStoreException {

        /*
        Case 1.1: Try to update the verified email list with a new email when primary email is verified
        and NOT in the verified email list.
        Expected: New email should go to pending. Primary email should be added to verified list.
        */
        String newVerifiedEmails1 = String.format("%s,%s", EXISTING_EMAIL_1, NEW_EMAIL);
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails1, null, null);

        mockUtilMethods(true, true, false, false);
        List<String> existingEmails1 = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingEmailAddressesList(existingEmails1);

        // Primary email is verified but NOT in the verified list
        List<String> existingVerifiedEmails1 = new ArrayList<>();
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails1);
        mockPrimaryEmail(EXISTING_EMAIL_1);
        mockPrimaryEmailVerificationStatus(true);

        userEmailVerificationHandler.handleEvent(event1);
        Map<String, String> userClaims1 = getUserClaimsFromEvent(event1);

        // New email should be in pending
        Assert.assertEquals(userClaims1.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), NEW_EMAIL);

        // Primary email should be added to verified list since it's verified
        String updatedVerifiedEmails1 = userClaims1.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        Assert.assertTrue(StringUtils.contains(updatedVerifiedEmails1, EXISTING_EMAIL_1));
        Assert.assertFalse(StringUtils.contains(updatedVerifiedEmails1, NEW_EMAIL));

        /*
        Case 1.2: Try to update the verified email list with a new email when primary email is verified
        and ALREADY in the verified email list.
        Expected: New email should go to pending. Verified list should remain unchanged (primary already there).
        */
        String newVerifiedEmails2 = String.format("%s,%s", EXISTING_EMAIL_1, NEW_EMAIL);
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails2, null, null);

        mockUtilMethods(true, true, false, false);
        List<String> existingEmails2 = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingEmailAddressesList(existingEmails2);

        // Primary email is verified and ALREADY in the verified list
        List<String> existingVerifiedEmails2 = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails2);
        mockPrimaryEmail(EXISTING_EMAIL_1);
        mockPrimaryEmailVerificationStatus(true);

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);

        // New email should be in pending
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), NEW_EMAIL);

        // Verified list should still contain primary, but not the new email
        String updatedVerifiedEmails2 = userClaims2.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        Assert.assertTrue(StringUtils.contains(updatedVerifiedEmails2, EXISTING_EMAIL_1));
        Assert.assertFalse(StringUtils.contains(updatedVerifiedEmails2, NEW_EMAIL));

        /*
        Case 2: Update verified email list with a NEW email set as primary.
        Expected: Email should be added to the updated verified email list only if primary email is verified.
        */
        String newVerifiedEmails3 = String.format("%s,%s", EXISTING_EMAIL_1, NEW_EMAIL);

        mockUtilMethods(true, true, false, false);
        List<String> existingEmails3 = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingEmailAddressesList(existingEmails3);

        List<String> existingVerifiedEmails3 = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails3);

        mockPrimaryEmail(NEW_EMAIL);

        // Case 2.1: Test when primary email is already verified.
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails3, null, null);
        mockPrimaryEmailVerificationStatus(true);

        userEmailVerificationHandler.handleEvent(event3);
        Map<String, String> userClaims3 = getUserClaimsFromEvent(event3);
        String updatedVerifiedEmails3 = userClaims3.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        Assert.assertTrue(StringUtils.contains(updatedVerifiedEmails3, NEW_EMAIL));

        // Case 2.2: Test when primary email is not verified.
        Event event4 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails3, null, null);
        mockPrimaryEmailVerificationStatus(false);

        userEmailVerificationHandler.handleEvent(event4);
        Map<String, String> userClaims4 = getUserClaimsFromEvent(event4);
        String updatedVerifiedEmails4 = userClaims4.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        Assert.assertFalse(StringUtils.contains(updatedVerifiedEmails4, NEW_EMAIL));

    /*
     Case 3: Add multiple new emails to verified emails list.
     Expected: Error should be thrown.
     */
        String newVerifiedEmails5 = String.format("%s,%s", EXISTING_EMAIL_1, NEW_EMAIL);
        Event event5 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails5, null, null);

        mockUtilMethods(true, true, false, false);
        mockExistingEmailAddressesList(new ArrayList<>());
        mockExistingVerifiedEmailAddressesList(new ArrayList<>());

        try {
            userEmailVerificationHandler.handleEvent(event5);
            Assert.fail("Expected IdentityEventClientException to be thrown");
        } catch (IdentityEventClientException e) {
            Assert.assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_VERIFY_MULTIPLE_EMAILS.getCode());
        }
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Remove email from email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC4() throws IdentityEventException {

        /*
         Remove an email from the existing emails list.
         Expected: Removed email should be removed from the verified email list as well.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, EXISTING_EMAIL_1, null);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, EXISTING_EMAIL_2));
        mockExistingEmailAddressesList(existingEmails);

        List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(EXISTING_EMAIL_1, EXISTING_EMAIL_2));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM), EXISTING_EMAIL_1);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM), EXISTING_EMAIL_1);
    }

    @Test
    public void testHandleEventThreadLocalValues() throws IdentityEventException, UserStoreException {

        mockUtilMethods(true, false, false,
                false);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        // Case 1: Thread local value = SKIP_ON_CONFIRM.
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockedUtils.when(Utils::getThreadLocalToSkipSendingEmailVerificationOnUpdate).thenReturn(
                IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_CONFIRM
                        .toString());

        userEmailVerificationHandler.handleEvent(event1);
        Map<String, String> userClaims1 = getUserClaimsFromEvent(event1);
        Assert.assertFalse(userClaims1.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));

        // Case 2: Thread local value = SKIP_ON_CONFIRM.
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockedUtils.when(Utils::getThreadLocalToSkipSendingEmailVerificationOnUpdate).thenReturn(
                IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates.SKIP_ON_EMAIL_OTP_FLOW
                .toString());

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        Assert.assertFalse(userClaims2.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));

        // Case 2: Thread local value = random value.
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_EMAIL);

        mockedUtils.when(Utils::getThreadLocalToSkipSendingEmailVerificationOnUpdate).thenReturn("test");

        userEmailVerificationHandler.handleEvent(event3);
        Map<String, String> userClaims3 = getUserClaimsFromEvent(event3);
        Assert.assertFalse(userClaims3.containsKey(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM));

    }

    @Test
    public void testHandleEventPostSetUserClaims()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);
        mockUtilMethods(true, true, false,
                true);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        userEmailVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());

        // Case 2: Error is thrown when retrieving verification pending email.
        Event event1 = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);
        mockUtilMethods(true, true, false,
                true);
        when(userStoreManager.getUserClaimValues(TEST_USERNAME, new String[]{
                IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM}, null))
                .thenThrow(new UserStoreException());
        try {
            userEmailVerificationHandler.handleEvent(event1);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        }
    }

    @Test
    public void testHandleEventPostSetUserClaimsRecoveryScenarios()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);
        mockUtilMethods(true, true, false,
                true);
        mockPendingVerificationEmail(EXISTING_EMAIL_1);

        // Case 1: Change primary email address.
        mockedUtils.when(Utils::getThreadLocalIsOnlyVerifiedEmailAddressesUpdated).thenReturn(false);
        userEmailVerificationHandler.handleEvent(event);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        Assert.assertEquals(RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE,
                capturedRecoveryData.getRecoveryScenario());
        Assert.assertEquals(RecoverySteps.VERIFY_EMAIL, capturedRecoveryData.getRecoveryStep());

        reset(userRecoveryDataStore);

        // Case 2: Change verified list.
        Event event2 = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);
        mockedUtils.when(Utils::getThreadLocalIsOnlyVerifiedEmailAddressesUpdated).thenReturn(true);
        userEmailVerificationHandler.handleEvent(event2);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor2 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor2.capture());
        UserRecoveryData capturedRecoveryData2 = recoveryDataCaptor2.getValue();
        Assert.assertEquals(RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                capturedRecoveryData2.getRecoveryScenario());
        Assert.assertEquals(RecoverySteps.VERIFY_EMAIL, capturedRecoveryData2.getRecoveryStep());
    }

    @Test
    public void testHandleEventPreAddUserVerifyEmailClaim() throws IdentityEventException {

        /*
         Case 1: Enable verifyEmail claim and not provide the email address claim value.
         */
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_ADD_USER, IdentityRecoveryConstants.TRUE,
                null, null, null);

        try {
            userEmailVerificationHandler.handleEvent(event1);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventClientException);
            Assert.assertEquals(((IdentityEventClientException) e).getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_VERIFICATION_EMAIL_NOT_FOUND.getCode());
        }

        /*
         Case 2: Provide the email address claim value.
         */
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_ADD_USER, IdentityRecoveryConstants.TRUE,
                null, null, NEW_EMAIL);

        userEmailVerificationHandler.handleEvent(event2);
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.PRE_VERIFY_EMAIL_CLAIM),
                any()));
    }

    @Test
    public void testHandleEventPostAddUserVerifyEmailClaim() throws IdentityEventException {

        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION, true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                true);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig
                        .EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,true);

        mockedUtils.when(() -> Utils.isAccountStateClaimExisting(anyString())).thenReturn(true);

        Claim claim = new Claim();
        claim.setClaimUri(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        claim.setValue(Boolean.TRUE.toString());
        mockedUtils.when(Utils::getEmailVerifyTemporaryClaim).thenReturn(claim);

        Event event = createEvent(IdentityEventConstants.Event.POST_ADD_USER, IdentityRecoveryConstants.TRUE,
                null, null, null);

        userEmailVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());
        mockedUtils.verify(() -> Utils.publishRecoveryEvent(any(),
                eq(IdentityEventConstants.Event.POST_VERIFY_EMAIL_CLAIM),
                any()));
    }

    @Test
    public void testGetPriority() {

        Assert.assertEquals(userEmailVerificationHandler.getPriority(messageContext), 65);
    }

    @Test
    public void testGetRecoveryData() throws IdentityEventException, IdentityRecoveryException {

        User user = new User();
        UserRecoveryData userRecoveryData = mock(UserRecoveryData.class);
        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(user)).thenReturn(userRecoveryData);

        UserRecoveryData response = userEmailVerificationHandler.getRecoveryData(user);
        Assert.assertEquals(response, userRecoveryData);
    }

    @DataProvider(name = "claimDeletionData")
    public Object[][] getClaimDeletionData() {

        return new Object[][]{
                // claimURI, shouldCallSetUserClaimValues
                {IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, true},
                {"some.other.claim", false}
        };
    }

    @Test(description = "Test handling of claim deletion events - verifies that 'emailVerified' claim is cleared" +
            "when email claim is deleted, and no action for other claims.", dataProvider = "claimDeletionData")
    public void testHandleEventPreDeleteUserClaim(String claimURI, boolean shouldCallSetUserClaimValues)
            throws IdentityEventException, UserStoreException {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        eventProperties.put(IdentityEventConstants.EventProperty.CLAIM_URI, claimURI);
        Event event = new Event(IdentityEventConstants.Event.PRE_DELETE_USER_CLAIM, eventProperties);

        userEmailVerificationHandler.handleEvent(event);

        if (shouldCallSetUserClaimValues) {
            // Verify that setUserClaimValues was called with empty string for emailVerified claim.
            Map<String, String> expectedClaims = new HashMap<>();
            expectedClaims.put(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM, StringUtils.EMPTY);
            verify(userStoreManager).setUserClaimValues(eq(TEST_USERNAME), eq(expectedClaims), isNull());
        } else {
            // Verify that setUserClaimValues was not called for other claims.
            verify(userStoreManager, never()).setUserClaimValues(anyString(), any(), any());
        }
    }

    @DataProvider(name = "multiAttributeEnabledData")
    public Object[][] multiAttributeEnabledData() {

        return new Object[][]{
                {false},
                {true}
        };
    }

    @Test(description = "Test handling of primary email deletion — primary email set to EMPTY should clear" +
            "emailVerified claim", dataProvider = "multiAttributeEnabledData")
    public void testHandleEventPreSetUserClaimsPrimaryEmailDeletionClearsVerification(boolean multiAttributeEnabled)
            throws IdentityEventException {

        mockUtilMethods(true, multiAttributeEnabled, false, false);

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, StringUtils.EMPTY);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);

        Event event = new Event(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, eventProperties);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertTrue(userClaims.containsKey(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM),
                "'emailVerified' claim not found in user claims map");
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM), StringUtils.EMPTY,
                "'emailVerified' claim should be cleared (empty string) when primary email is set to empty string.");
    }

    private void mockExistingEmailAddressesList(List<String> existingEmails) {

        mockedUtils.when(() -> Utils.getMultiValuedClaim(any(), any(),
                        eq(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM)))
                .thenReturn(existingEmails);
    }

    private void mockExistingVerifiedEmailAddressesList(List<String> existingVerifiedEmails) {

        mockedUtils.when(() -> Utils.getMultiValuedClaim(any(), any(),
                        eq(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM)))
                .thenReturn(existingVerifiedEmails);
    }

    private void mockPrimaryEmail(String primaryEmail) throws UserStoreException {

        when(userStoreManager.getUserClaimValue(anyString(), eq(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM),
                any())).thenReturn(primaryEmail);
    }

    private void mockPendingVerificationEmail(String pendingEmail) throws UserStoreException {

        Map<String, String> pendingEmailClaim = new HashMap<>();
        pendingEmailClaim.put(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM, pendingEmail);
        when(userStoreManager.getUserClaimValues(anyString(),
                eq(new String[]{IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM}),
                any())).thenReturn(pendingEmailClaim);
    }

    private void mockPrimaryEmailVerificationStatus(boolean isVerified) {
        
        mockedUtils.when(() -> Utils.getUserClaim(any(), any(), eq(IdentityRecoveryConstants.EMAIL_VERIFIED_CLAIM)))
                .thenReturn(String.valueOf(isVerified));
    }

    private void mockUtilMethods(boolean emailVerificationEnabled, boolean multiAttributeEnabled,
                                 boolean userVerifyClaimEnabled, boolean notificationOnEmailUpdate) {

        mockedUtils.when(() -> Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString(), anyString()))
                .thenReturn(multiAttributeEnabled);
        mockedUtils.when(Utils::isUseVerifyClaimEnabled).thenReturn(userVerifyClaimEnabled);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                emailVerificationEnabled);
        mockGetConnectorConfig(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                notificationOnEmailUpdate);
    }

    private void mockGetConnectorConfig(String connectorConfig, boolean value) {

        mockedUtils.when(() -> Utils.getConnectorConfig(eq(connectorConfig), anyString()))
                .thenReturn(String.valueOf(value));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> getUserClaimsFromEvent(Event event2) {

        Map<String, Object> eventProperties = event2.getEventProperties();
        return (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS);
    }

    private Event createEvent(String eventType, String verifyEmailClaim, String verifiedEmailsClaim,
                           String emailsClaim, String primaryEmail) {

        return createEvent(eventType, verifyEmailClaim, verifiedEmailsClaim, emailsClaim, primaryEmail, null, null);
    }

    private Event createEvent(String eventType, String verifyEmailClaim, String verifiedEmailsClaim,
                              String emailsClaim, String primaryEmail, Map<String, Object> additionalEventProperties,
                              Map<String, String> additionalClaims) {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        if (additionalEventProperties != null) {
            eventProperties.putAll(additionalEventProperties);
        }

        Map<String, String> claims = new HashMap<>();
        if (primaryEmail != null && !primaryEmail.isEmpty()) {
            claims.put(IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM, primaryEmail);
        }
        if (verifyEmailClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM, verifyEmailClaim);
        }
        if (emailsClaim != null) {
            claims.put(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM, emailsClaim);
        }
        if (verifiedEmailsClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM, verifiedEmailsClaim);
        }

        if (additionalClaims != null) {
            claims.putAll(additionalClaims);
        }

        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        return new Event(eventType, eventProperties);
    }
}
