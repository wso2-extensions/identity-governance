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

package org.wso2.carbon.identity.recovery.handler;

import org.apache.commons.lang.StringUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.testng.annotations.Test;
import org.testng.Assert;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
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
import java.util.Collections;
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
    private JDBCRecoveryDataStore jdbcRecoveryDataStore;

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

    private static final String username = "testuser";
    private static final String tenantDomain = "test.com";
    private static final int tenantId = 5;
    private static final String userStoreDomain = "TESTING";
    private static final String existingNumber1 = "0777777777";
    private static final String existingNumber2 = "0711111111";
    private static final String newMobileNumber = "0722222222";

    @BeforeMethod
    public void setUp() throws UserStoreException {

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

        when(serviceDataHolder.getRealmService()).thenReturn(realmService);
        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getTenantId(tenantDomain)).thenReturn(tenantId);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))
                .thenReturn(userStoreDomain);
    }

    @AfterMethod
    public void tearDown() {

        mockedJDBCRecoveryDataStore.close();
        mockedUtils.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedFrameworkUtils.close();
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
                getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, IdentityRecoveryConstants.FALSE,
                        existingNumber1);

        mockVerificationPendingMobileNumber();
        mockUtilMethods(false, false, false);

        mobileNumberVerificationHandler.handleEvent(event);
        verify(userRecoveryDataStore).invalidate(any(), eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE),
                eq(RecoverySteps.VERIFY_MOBILE_NUMBER));
    }

    @Test(description = "Verification disabled, Multi-attribute enabled, Change primary mobile")
    public void testHandleEventVerificationDisabledMultiAttributeEnabled()
            throws UserStoreException, IdentityEventException, IdentityRecoveryException {

        Event event = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIM, IdentityRecoveryConstants.FALSE,
                existingNumber1);

        mockVerificationPendingMobileNumber();
        mockUtilMethods(false, true, false);

        // Mobile number is not included in all mobile numbers list.
        List<String> allMobileNumbers = Arrays.asList(existingNumber1, existingNumber2);
        mockExistingNumbersList(allMobileNumbers);

        try {
            mobileNumberVerificationHandler.handleEvent(event);
        } catch (IdentityEventClientException e) {
            Assert.assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_PRIMARY_MOBILE_NUMBER_SHOULD_BE_INCLUDED_IN_MOBILE_NUMBERS_LIST.getCode());
        }
    }

    @Test(description = "PRE_SET_USER_CLAIMS: Verification enabled, Multi-attribute disabled, Change primary mobile")
    public void testHandleEventVerificationEnabledMultiAttributeDisabledPreSet()
            throws UserStoreException, IdentityEventException, IdentityRecoveryException {

        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, false, false);

        // Case 1: Claims null.
        Event event1 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS);
        mobileNumberVerificationHandler.handleEvent(event1);
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                eq(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_INAPPLICABLE_CLAIMS.toString())));

        /*
         * Case 2: Thread local set to skip.
         * Expected: Invalidation should be triggered.
         */
        Event event2 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                existingNumber1);
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_CONFIRM.toString());
        mockVerificationPendingMobileNumber();

        mobileNumberVerificationHandler.handleEvent(event2);
        verify(userRecoveryDataStore, atLeastOnce()).invalidate(any(), eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE),
                eq(RecoverySteps.VERIFY_MOBILE_NUMBER));

        /*
         * Case 3: Thread local set to some value.
         * Expected: Invalidation should be triggered.
         */
        Event event3 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                existingNumber1);
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn("test");

        mobileNumberVerificationHandler.handleEvent(event3);
        mockedUtils.verify(Utils::unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate);

        /*
         * Case 4: Claims not null, new mobile number is sent to be verified.
         * Expected: New mobile number should be added to the mobileNumber.pendingValue claim.
         */
        String newVerifiedMobileNumbersList = existingNumber1 + "," + newMobileNumber;
        String newMobileNumbersList = existingNumber1 + "," + existingNumber2;
        Event event4 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbersList, newMobileNumbersList);

        // Mock existing verified mobile numbers.
        List<String> exisitingVerifiedNumbersList = Arrays.asList(existingNumber1, existingNumber2);
        mockedUtils.when(() -> Utils.getExistingClaimValue(any(), any(),
                        eq(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM)))
                .thenReturn(exisitingVerifiedNumbersList);

        List<String> existingAllMobileNumbers = Collections.singletonList(existingNumber1);
        mockExistingNumbersList(existingAllMobileNumbers);

        mobileNumberVerificationHandler.handleEvent(event4);
        Map<String, String> userClaimsEvent2 = getUserClaimsFromEvent(event4);
        Assert.assertEquals(userClaimsEvent2.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                newMobileNumber);

        /*
         * Case 5: Claims not null, new mobile number is same as the existing mobile number.
         * Expected: Pending mobile verification should be invalidated.
         */
        mockExistingPrimaryMobileNumber(newMobileNumber);
        Event event5 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbersList, newMobileNumbersList);

        mobileNumberVerificationHandler.handleEvent(event5);
        Map<String, String> userClaimsEvent3 = getUserClaimsFromEvent(event5);

        verify(userRecoveryDataStore, atLeastOnce()).invalidate(any(),
                eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE), eq(RecoverySteps.VERIFY_MOBILE_NUMBER));
        Assert.assertEquals(userClaimsEvent3.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                "");
    }

    @Test(description = "PRE_SET_USER_CLAIMS: Verification enabled, Multi-attribute enabled, Change primary mobile")
    public void testHandleEventVerificationEnabledMultiAttributeEnabledPreSet()
            throws UserStoreException, IdentityEventException, IdentityRecoveryException {

        mockVerificationPendingMobileNumber();
        mockUtilMethods(true, true, false);

        /*
         * Case 1: Try to update primary mobile number which is not in the verified mobile numbers list.
         * Expected: IdentityEventClientException
         */
        String newVerifiedMobileNumbers = existingNumber1 + "," + newMobileNumber;
        String newMobileNumbers = existingNumber1 + "," + existingNumber2;
        Event event1 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers, newMobileNumbers);

        List<String> exisitingVerifiedNumbersList = new ArrayList<>(Arrays.asList(existingNumber1));
        mockExistingVerifiedNumbersList(exisitingVerifiedNumbersList);

        List<String> existingAllMobileNumbers = new ArrayList<>(Arrays.asList(existingNumber1, newMobileNumber));
        mockExistingNumbersList(existingAllMobileNumbers);

        try {
            mobileNumberVerificationHandler.handleEvent(event1);
        } catch (IdentityEventClientException e) {
            Assert.assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.
                    ERROR_CODE_PRIMARY_MOBILE_NUMBER_SHOULD_BE_INCLUDED_IN_VERIFIED_MOBILES_LIST.getCode());
        }

        /*
         * Case 2: Try to update primary mobile number which is in the verified mobile numbers list.
         * Expected: Thread local should be set to skip sending SMS OTP verification.
         */
        List<String> existingVerifiedNumbersList1 = new ArrayList<>(Arrays.asList(existingNumber1, newMobileNumber));
        mockExistingVerifiedNumbersList(existingVerifiedNumbersList1);

        mobileNumberVerificationHandler.handleEvent(event1);
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                eq(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_INAPPLICABLE_CLAIMS.toString())));

        /*
         * Case 3: Try to update verified numbers list with a new mobile number, which is not in the all mobile numbers.
         */
        String newVerifiedMobileNumbers3 = existingNumber1 + "," + newMobileNumber;
        Event event3 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers3, null, null);

        List<String> existingVerifiedNumbersList3 = new ArrayList<>(Arrays.asList(existingNumber1));
        mockExistingVerifiedNumbersList(existingVerifiedNumbersList3);

        List<String> existingAllMobileNumbers3 = new ArrayList<>(Arrays.asList(existingNumber1));
        mockExistingNumbersList(existingAllMobileNumbers3);

        mobileNumberVerificationHandler.handleEvent(event3);
        Map<String, String> userClaimsCase3 = getUserClaimsFromEvent(event3);
        Assert.assertEquals(userClaimsCase3.get(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM),
                newMobileNumber);

        /*
         * Case 4: Try to update verified numbers list with the new mobile number, which is not in the all
         * mobile numbers.
         * Set the new mobile number as existing primary mobile number.
         * Expected: Verification skip thread local should be set.
         * Expected: The new mobile number should be added to the mobileNumbers claim.
         */
        Event event4 = getEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedMobileNumbers3, null, null);

        mockExistingPrimaryMobileNumber(newMobileNumber);
        mobileNumberVerificationHandler.handleEvent(event4);
        Map<String, String> userClaimsCase4 = getUserClaimsFromEvent(event4);
        Assert.assertTrue(StringUtils.contains(userClaimsCase4.get(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM),
                newMobileNumber));
        Assert.assertTrue(StringUtils.contains(
                userClaimsCase4.get(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM),
                newMobileNumber));
        mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(
                eq(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_EXISTING_MOBILE_NUM.toString())));

    }

    @Test(description = "POST_SET_USER_CLAIMS: Verification enabled, Multi-attribute enabled")
    public void testHandleEventPostSet() throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = getEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS);
        mockUtilMethods(true, true, false);

        /*
         Case 1: skipSendingSmsOtpVerificationOnUpdate set to skip.
         Expected: Invalidation should not be triggered.
         */
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(IdentityRecoveryConstants.SkipMobileNumberVerificationOnUpdateStates
                        .SKIP_ON_EXISTING_MOBILE_NUM.toString());
        mobileNumberVerificationHandler.handleEvent(event);
        verify(userRecoveryDataStore, never()).invalidate(any(), eq(RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE),
                eq(RecoverySteps.VERIFY_MOBILE_NUMBER));

        /*
         Case 2: skipSendingSmsOtpVerificationOnUpdate set to null.
         Expected: Notification event should be triggered.
         */
        mockedUtils.when(Utils::getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate)
                .thenReturn(null);
        mockVerificationPendingMobileNumber();
        mobileNumberVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());
    }

    private void mockExistingPrimaryMobileNumber(String mobileNumber) throws UserStoreException {

        when(userStoreManager.getUserClaimValue(anyString(),
                eq(IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM), isNull())).thenReturn(mobileNumber);
    }

    private void mockExistingNumbersList(List<String> existingAllMobileNumbers) {

        mockedUtils.when(() -> Utils.getExistingClaimValue(any(), any(),
                        eq(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM)))
                .thenReturn(existingAllMobileNumbers);
    }

    private void mockExistingVerifiedNumbersList(List<String> exisitingVerifiedNumbersList) {

        mockedUtils.when(() -> Utils.getExistingClaimValue(any(), any(),
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

        mockedUtils.when(() ->
                Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(anyString())).thenReturn(multiAttributeEnabled);
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
        pendingMobileNumberClaimMap.put(IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM, existingNumber2);
        when(userStoreManager.getUserClaimValues(eq(username),
                eq(new String[]{IdentityRecoveryConstants.MOBILE_NUMBER_PENDING_VALUE_CLAIM}), isNull()))
                .thenReturn(pendingMobileNumberClaimMap);
    }

    private Event getEvent(String eventType) {

        return getEvent(eventType, null, null, null,
                null);
    }

    private Event getEvent(String eventType, String verifyMobileClaim, String verifiedMobileNumbersClaim) {

        String mobileNumberClaim = existingNumber1 + "," + existingNumber2;
        return getEvent(eventType, verifyMobileClaim, verifiedMobileNumbersClaim, mobileNumberClaim , newMobileNumber);
    }

    private Event getEvent(String eventType, String verifyMobileClaim, String verifiedMobileNumbersClaim,
                           String mobileNumbersClaim) {

        return getEvent(eventType, verifyMobileClaim, verifiedMobileNumbersClaim, mobileNumbersClaim, newMobileNumber);
    }

    private Event getEvent(String eventType, String verifyMobileClaim, String verifiedMobileNumbersClaim,
                           String mobileNumbersClaim, String mobileNumber) {

        Map<String, Object> eventProperties = getEventProperties();
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

    private Map<String, Object> getEventProperties() {

        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, username);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        return eventProperties;
    }
}
