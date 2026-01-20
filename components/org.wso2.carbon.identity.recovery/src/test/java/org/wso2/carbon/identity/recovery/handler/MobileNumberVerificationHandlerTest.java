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
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.testng.annotations.Test;
import org.testng.Assert;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
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
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.api.UserStoreException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit test cases for MobileNumberVerificationHandler.
 */
public class MobileNumberVerificationHandlerTest {

    @InjectMocks
    private MobileNumberVerificationHandler mobileNumberVerificationHandler;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private RealmConfiguration realmConfiguration;

    @Mock
    private ClaimManager claimManager;

    @Mock
    private RealmService realmService;

    @Mock
    private UserRealm userRealm;

    @Mock
    private TenantManager tenantManager;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private IdentityEventService identityEventService;

    @Mock
    private IdentityRecoveryServiceDataHolder serviceDataHolder;

    private MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;

    private static final String TEST_USERNAME = "testuser";
    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final int TEST_TENANT_ID = 5;
    private static final String TEST_USER_STORE_DOMAIN = "TESTING";
    private static final String EXISTING_NUMBER_1 = "0777777777";
    private static final String EXISTING_NUMBER_2 = "0711111111";
    private static final String NEW_MOBILE_NUMBER = "0722222222";

    @BeforeMethod
    public void setUpMethod() throws UserStoreException {

        MockitoAnnotations.openMocks(this);
        mobileNumberVerificationHandler = new MobileNumberVerificationHandler();
        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedUtils = mockStatic(Utils.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);

        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        mockedFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");
        mockedUtils.when(() -> Utils.resolveEventName(NotificationChannels.SMS_CHANNEL.getChannelType())).thenReturn(
                "TRIGGER_SMS_NOTIFICATION_LOCAL");

        when(serviceDataHolder.getRealmService()).thenReturn(realmService);
        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(TEST_TENANT_DOMAIN)).thenReturn(TEST_TENANT_ID);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn(TEST_USER_STORE_DOMAIN);
    }

    @AfterMethod
    public void tearDown() {

        mockedJDBCRecoveryDataStore.close();
        mockedUtils.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedFrameworkUtils.close();
        Utils.unsetThreadLocalIsOnlyVerifiedEmailAddressesUpdated();
        Utils.unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated();
        Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
        Utils.unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated();
    }

    @Test
    public void testGetNames() {

        Assert.assertEquals(mobileNumberVerificationHandler.getName(), "userMobileVerification");
        Assert.assertEquals(mobileNumberVerificationHandler.getFriendlyName(), "User Mobile Number Verification");
    }

    @Test(description = "Verification disabled, Multi-attribute disabled, Change primary mobile")
    public void testHandleEventVerificationDisabledMultiAttributeDisabled()
            throws UserStoreException, IdentityEventException, IdentityRecoveryException {

        Event event =
                createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, null,
                        null, null, NEW_MOBILE_NUMBER);
        mockVerificationPendingMobileNumber();
        mockUtilMethods(false, false, false);

        mobileNumberVerificationHandler.handleEvent(event);

        // Expectation: Any pending mobile verification should be invalidated.
        verify(userRecoveryDataStore).invalidate(any(), eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE),
                eq(RecoverySteps.VERIFY_MOBILE_NUMBER));
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        reset(userRecoveryDataStore);
        // Case 2: User claims null.
        Event event2 =
                createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, null,
                        null, null, null);
        mobileNumberVerificationHandler.handleEvent(event2);
        verify(userRecoveryDataStore, never()).invalidate(any(), eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE), any());
    }

    @Test(description = "Verification disabled, Multi-attribute enabled, Change primary mobile")
    public void testHandleEventVerificationDisabledMultiAttributeEnabled()
            throws UserStoreException, IdentityEventException, IdentityRecoveryException {

        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, null,
                null, null, NEW_MOBILE_NUMBER);
        mockVerificationPendingMobileNumber();
        mockUtilMethods(false, true, false);

        // New primary mobile number is not included in existing all mobile numbers list.
        List<String> allMobileNumbers = new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1, EXISTING_NUMBER_2));
        mockExistingNumbersList(allMobileNumbers);

        // Expectation: New mobile number should be added to the mobile number claim.
        mobileNumberVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertTrue(StringUtils.contains(
                userClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), NEW_MOBILE_NUMBER));

        // Case 2: Updated verified mobile numbers list.
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, null,
                NEW_MOBILE_NUMBER, null, null);
        mockVerificationPendingMobileNumber();

        // Expectation: Verified mobile number claim should be removed from user claims.
        mobileNumberVerificationHandler.handleEvent(event2);
        verify(userRecoveryDataStore, times(2)).invalidate(any(),
                eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE),
                eq(RecoverySteps.VERIFY_MOBILE_NUMBER));
        Map<String, String> userClaims3 = getUserClaimsFromEvent(event2);
        Assert.assertFalse(userClaims3.containsKey(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM));
    }

    @Test(description = "PRE_SET_USER_CLAIMS: Verification enabled, Multi-attribute disabled, Claims null")
    public void testHandleEventVerificationEnabledMultiAttributeDisabledThreadLocal() throws Exception {

        /*
         Case 1: Claims null, skipSendingSmsOtpVerificationOnUpdate set to skip.
         */
        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, false, false);
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, null);

        mobileNumberVerificationHandler.handleEvent(event1);
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                eq(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_INAPPLICABLE_CLAIMS.toString())));

        /*
         Case 2: skipSendingSmsOtpVerificationOnUpdate set to skip.
         */
        // Case 2.1: skipSendingSmsOtpVerificationOnUpdate set to SKIP_ON_CONFIRM.
        mockVerificationPendingMobileNumber();
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_CONFIRM.toString());

        mobileNumberVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        // Case 2.2: skipSendingSmsOtpVerificationOnUpdate set to SKIP_ON_SMS_OTP_FLOW.
        mockVerificationPendingMobileNumber();
        Event event2_1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_SMS_OTP_FLOW.toString());

        mobileNumberVerificationHandler.handleEvent(event2_1);
        Map<String, String> userClaims2_1 = getUserClaimsFromEvent(event2_1);
        Assert.assertEquals(userClaims2_1.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        /*
         Case 3: skipSendingSmsOtpVerificationOnUpdate set to some value.
         */
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn("test");

        mobileNumberVerificationHandler.handleEvent(event3);
        mockedUtils.verify(Utils::unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate);
    }

    @Test(description = "PRE_SET_USER_CLAIMS: Verification enabled, Multi-attribute disabled, Change primary mobile")
    public void testHandleEventVerificationEnabledMultiAttributeDisabled() throws Exception {

        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, false, false);
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);

        mobileNumberVerificationHandler.handleEvent(event);

        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                NEW_MOBILE_NUMBER);

        /*
         Case 2: New mobile number is same as existing verified primary mobile number.
         */
        mockExistingPrimaryMobileNumber(NEW_MOBILE_NUMBER);
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockPrimaryMobileVerificationStatus(true);

        mobileNumberVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);

        /*
         Case 3: New mobile number is same as existing unverified primary mobile number.
         */
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockPrimaryMobileVerificationStatus(false);

        mobileNumberVerificationHandler.handleEvent(event3);
        Map<String, String> userClaims3 = getUserClaimsFromEvent(event3);
        Assert.assertEquals(userClaims3.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                NEW_MOBILE_NUMBER);

        /*
         Case 4: Enable userVerify and send verifyMobileClaim as false.
         */
        mockExistingPrimaryMobileNumber(EXISTING_NUMBER_1);
        mockedUtils.when(Utils::isUseVerifyClaimEnabled).thenReturn(true);
        Event event4 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, NEW_MOBILE_NUMBER);

        mobileNumberVerificationHandler.handleEvent(event4);
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_INAPPLICABLE_CLAIMS.toString()), atLeastOnce());

        /*
         Case 5: Mobile number claim is null.
         */
        Event event5 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);

        mobileNumberVerificationHandler.handleEvent(event5);
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_INAPPLICABLE_CLAIMS.toString()), atLeastOnce());

        /*
         Case 6: Throw error when retrieving existing mobile number.
         */
        when(userStoreManager.getUserClaimValue(anyString(),
                eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), isNull()))
                .thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            mobileNumberVerificationHandler.handleEvent(event);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        }
    }

    @Test(description = "Verification enabled, Multi-attribute enabled, Update primary mobile not in verified list")
    public void testUpdatePrimaryMobileNotInVerifiedList() throws Exception {

        mockUtilMethods(true, true, false);
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, null, NEW_MOBILE_NUMBER);
        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockVerificationPendingMobileNumber();

        mobileNumberVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                NEW_MOBILE_NUMBER);
        // Multiple mobile numbers related claims should only be updated when they are present in the request.
        Assert.assertFalse(userClaims.containsKey(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM));

        // Case 2: Send mobileNumbers claim in the request.
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, null,
                null, EXISTING_NUMBER_1, NEW_MOBILE_NUMBER);
        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockVerificationPendingMobileNumber();

        mobileNumberVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        Assert.assertEquals(userClaims2.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                NEW_MOBILE_NUMBER);
        Assert.assertTrue(userClaims2.containsKey(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM));
    }

    @Test(description = "Verification enabled, Multi-attribute enabled, Update primary mobile in verified list")
    public void testUpdatePrimaryMobileInVerifiedList() throws Exception {

        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, true, false);
        String newVerifiedMobileNumbers = EXISTING_NUMBER_1 + "," + NEW_MOBILE_NUMBER;
        String newMobileNumbers = EXISTING_NUMBER_1 + "," + NEW_MOBILE_NUMBER;
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS,
                IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers, newMobileNumbers, NEW_MOBILE_NUMBER);
        mockExistingVerifiedNumbersList(Arrays.asList(EXISTING_NUMBER_1, NEW_MOBILE_NUMBER));


        mobileNumberVerificationHandler.handleEvent(event);

        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                eq(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_ALREADY_VERIFIED_MOBILE_NUMBERS.toString())));
    }

    @Test(description = "Verification enabled, Multi-attribute enabled, Add new mobile to verified list")
    public void testAddNewMobileToVerifiedList() throws Exception {

        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, true, false);
        String newVerifiedMobileNumbers = EXISTING_NUMBER_1 + "," + NEW_MOBILE_NUMBER;
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS,
                IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers, null, null);
        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));

        mobileNumberVerificationHandler.handleEvent(event);

        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                NEW_MOBILE_NUMBER);

        // Case 2: Add multiple numbers to new verified list at once.
        String newVerifiedMobileNumbers2 = EXISTING_NUMBER_1 + "," + NEW_MOBILE_NUMBER;
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS,
                IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers2, null, null);
        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList()));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));

        try {
            mobileNumberVerificationHandler.handleEvent(event2);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventClientException);
        }

        // Case 3: Added new number is existing verified primary mobile number.
        String newVerifiedMobileNumbers3 = EXISTING_NUMBER_1 + "," + NEW_MOBILE_NUMBER;
        Event event3 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS,
                IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers3, null, null);

        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingPrimaryMobileNumber(NEW_MOBILE_NUMBER);
        mockPrimaryMobileVerificationStatus(true);

        mobileNumberVerificationHandler.handleEvent(event3);
        Map<String, String> userClaims3 = getUserClaimsFromEvent(event3);
        Assert.assertTrue(
                StringUtils.contains(userClaims3.get(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM),
                        NEW_MOBILE_NUMBER));
        Assert.assertTrue(StringUtils.contains(userClaims3.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                NEW_MOBILE_NUMBER));

        // Case 4: Added new number is existing unverified primary mobile number.
        Event event4 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS,
                IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers3, null, null);
        mockExistingVerifiedNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingNumbersList(new ArrayList<>(Arrays.asList(EXISTING_NUMBER_1)));
        mockExistingPrimaryMobileNumber(NEW_MOBILE_NUMBER);
        mockPrimaryMobileVerificationStatus(false);

        mobileNumberVerificationHandler.handleEvent(event4);
        Map<String, String> userClaims4 = getUserClaimsFromEvent(event4);
        Assert.assertTrue(
                StringUtils.contains(userClaims4.get(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM),
                        NEW_MOBILE_NUMBER));
        Assert.assertFalse(
                StringUtils.contains(userClaims4.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                        NEW_MOBILE_NUMBER));
        Assert.assertTrue(
                StringUtils.contains(userClaims4.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                        NEW_MOBILE_NUMBER));
    }

    @Test(description = "POST_SET_USER_CLAIMS: Verification enabled, Multi-attribute enabled")
    public void testHandleEventPostSet() throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, null,
                null, null, null);
        mockUtilMethods(true, true, false);

        MockedStatic<RecoveryScenarios> mockedStaticRecoveryScenarios = mockStatic(RecoveryScenarios.class);
        mockedStaticRecoveryScenarios.when(() ->
                        RecoveryScenarios.getRecoveryScenario(
                                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString()))
                .thenReturn(RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE);

        /*
         Case 1: skipSendingSmsOtpVerificationOnUpdate set to skip.
         Expected: Invalidation should not be triggered.
         */
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_EXISTING_MOBILE_NUM.toString());

        mobileNumberVerificationHandler.handleEvent(event);
        verify(identityEventService, never()).handleEvent(any());

        /*
         Case 2: skipSendingSmsOtpVerificationOnUpdate set to null.
         Expected: Notification event should be triggered.
         */
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(null);
        mockVerificationPendingMobileNumber();
        mobileNumberVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());

        // Case 3: Handle exception thrown from userStoreManager.getUserClaimValues.
        when(userStoreManager.getUserClaimValues(eq(TEST_USERNAME),
                eq(new String[]{IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM}), isNull()))
                .thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            mobileNumberVerificationHandler.handleEvent(event);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventException);
        } finally {
            mockedStaticRecoveryScenarios.close();
        }
    }

    @Test
    public void testHandleEventPostSetRecoveryScenarios()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, null,
                null, null, null);
        mockUtilMethods(true, true, false);

        MockedStatic<RecoveryScenarios> mockedStaticRecoveryScenarios = mockStatic(RecoveryScenarios.class);
        mockedStaticRecoveryScenarios.when(() ->
                        RecoveryScenarios.getRecoveryScenario(
                                RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE.toString()))
                .thenReturn(RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE);
        mockVerificationPendingMobileNumber();

        // Case 1: Update the primary mobile number.
        mockedUtils.when(Utils::getThreadLocalIsOnlyVerifiedMobileNumbersUpdated).thenReturn(false);
        mobileNumberVerificationHandler.handleEvent(event);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor.capture());
        UserRecoveryData capturedRecoveryData = recoveryDataCaptor.getValue();
        Assert.assertEquals(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                capturedRecoveryData.getRecoveryScenario());
        Assert.assertEquals(RecoverySteps.VERIFY_MOBILE_NUMBER, capturedRecoveryData.getRecoveryStep());

        reset(userRecoveryDataStore);
        // Case 2: Update the verified list.
        Event event2 = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, null,
                null, null, null);
        mockedUtils.when(Utils::getThreadLocalIsOnlyVerifiedMobileNumbersUpdated).thenReturn(true);
        mobileNumberVerificationHandler.handleEvent(event2);

        ArgumentCaptor<UserRecoveryData> recoveryDataCaptor2 = ArgumentCaptor.forClass(UserRecoveryData.class);
        verify(userRecoveryDataStore).store(recoveryDataCaptor2.capture());
        UserRecoveryData capturedRecoveryData2 = recoveryDataCaptor2.getValue();
        Assert.assertEquals(RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                capturedRecoveryData2.getRecoveryScenario());
        Assert.assertEquals(RecoverySteps.VERIFY_MOBILE_NUMBER, capturedRecoveryData2.getRecoveryStep());
    }

    @DataProvider(name = "claimDeletionData")
    public Object[][] getClaimDeletionData() {

        return new Object[][]{
                // claimURI, shouldCallSetUserClaimValues
                {IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, true},
                {"some.other.claim", false}
        };
    }

    @Test(description = "Test handling of claim deletion events - verifies that 'phoneVerified' claim is cleared when " +
            "mobile claim is deleted, and no action for other claims.", dataProvider = "claimDeletionData")
    public void testHandleEventPreDeleteUserClaim(String claimURI, boolean shouldCallSetUserClaimValues)
            throws IdentityEventException, org.wso2.carbon.user.core.UserStoreException {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        eventProperties.put(IdentityEventConstants.EventProperty.CLAIM_URI, claimURI);
        Event event = new Event(IdentityEventConstants.Event.PRE_DELETE_USER_CLAIM, eventProperties);

        mobileNumberVerificationHandler.handleEvent(event);

        if (shouldCallSetUserClaimValues) {
            // Verify that setUserClaimValues was called with empty string for phoneVerified claim.
            Map<String, String> expectedClaims = new HashMap<>();
            expectedClaims.put(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM, StringUtils.EMPTY);
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

    @Test(description = "Test handling of primary mobile deletion — primary mobile set to EMPTY should clear" +
            "phoneVerified claim.", dataProvider = "multiAttributeEnabledData")
    public void testHandleEventPreSetUserClaimsPrimaryMobileDeletionClearsVerification(boolean multiAttributeEnabled)
            throws IdentityEventException {

        mockUtilMethods(true, multiAttributeEnabled, false);

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        Map<String, String> claims = new HashMap<>();
        claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, StringUtils.EMPTY);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);

        Event event = new Event(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, eventProperties);

        mobileNumberVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertTrue(userClaims.containsKey(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM),
                "'phoneVerified' claim not found in user claims map.");
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM), StringUtils.EMPTY,
                "'phoneVerified' claim should be cleared (empty string) when primary mobile is set to empty string.");
    }

    private void mockExistingPrimaryMobileNumber(String mobileNumber) throws UserStoreException {

        when(userStoreManager.getUserClaimValue(anyString(),
                eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), isNull())).thenReturn(mobileNumber);
    }

    private void mockExistingNumbersList(List<String> existingAllMobileNumbers) {

        mockedUtils.when(() -> Utils.getMultiValuedClaim(any(), any(),
                        eq(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM)))
                .thenReturn(existingAllMobileNumbers);
    }

    private void mockPrimaryMobileVerificationStatus(boolean isVerified) {

        mockedUtils.when(() -> Utils.getUserClaim(any(), any(), eq(IdentityRecoveryConstants.MOBILE_VERIFIED_CLAIM)))
                .thenReturn(String.valueOf(isVerified));
    }

    private void mockExistingVerifiedNumbersList(List<String> exisitingVerifiedNumbersList) {

        mockedUtils.when(() -> Utils.getMultiValuedClaim(any(), any(),
                        eq(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM)))
                .thenReturn(exisitingVerifiedNumbersList);
    }


    @SuppressWarnings("unchecked")
    private static Map<String, String> getUserClaimsFromEvent(Event event2) {

        Map<String, Object> eventProperties = event2.getEventProperties();
        return (Map<String, String>) eventProperties.get(IdentityEventConstants.EventProperty.USER_CLAIMS);
    }

    private void mockUtilMethods(boolean mobileVerificationEnabled, boolean multiAttributeEnabled,
                                 boolean useVerifyClaimEnabled) {

        mockedUtils.when(() -> Utils.isMultiMobileNumbersPerUserEnabled(anyString(), anyString()))
                .thenReturn(multiAttributeEnabled);
        mockedUtils.when(Utils::isUseVerifyClaimEnabled).thenReturn(useVerifyClaimEnabled);
        mockedUtils.when(() -> Utils.getConnectorConfig(
                        eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE),
                        anyString()))
                .thenReturn(String.valueOf(mobileVerificationEnabled));
    }

    private void mockVerificationPendingMobileNumber() throws UserStoreException {

        // Verification pending mobile number claim config.
        Claim pendingMobileNumberClaim = new Claim();
        pendingMobileNumberClaim.setClaimUri(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM);
        when(claimManager.getClaim(eq(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM)))
                .thenReturn(pendingMobileNumberClaim);

        Map<String, String> pendingMobileNumberClaimMap = new HashMap<>();
        pendingMobileNumberClaimMap.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, EXISTING_NUMBER_2);
        when(userStoreManager.getUserClaimValues(eq(TEST_USERNAME),
                eq(new String[]{IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM}), isNull()))
                .thenReturn(pendingMobileNumberClaimMap);
    }

    private Event createEvent(String eventType, String verifyMobileClaim, String verifiedMobileNumbersClaim,
                           String mobileNumbersClaim, String mobileNumber) {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);

        Map<String, String> claims = new HashMap<>();
        if (mobileNumber != null && !mobileNumber.isEmpty()) {
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM, mobileNumber);
        }
        if (verifyMobileClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFY_MOBILE_CLAIM, verifyMobileClaim);
        }
        if (mobileNumbersClaim != null) {
            claims.put(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM, mobileNumbersClaim);
        }
        if (verifiedMobileNumbersClaim != null) {
            claims.put(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, verifiedMobileNumbersClaim);
        }
        eventProperties.put(IdentityEventConstants.EventProperty.USER_CLAIMS, claims);
        return new Event(eventType, eventProperties);
    }
}
