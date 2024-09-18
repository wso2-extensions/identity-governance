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
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.event.IdentityEventClientException;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.user.api.ClaimManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.config.RealmConfiguration;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
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

    private static final String tenantDomain = "test.com";
    private static final String userStoreDomainName = "TESTING";
    private static final String username = "testuser";
    private static final String existingEmail1 = "old1@abc.com";
    private static final String existingEmail2 = "old2@abc.com";
    private static final String newEmail = "new@abc.com";

    @BeforeClass
    public void init() {

        mockedJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedUtils = mockStatic(Utils.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);
    }

    @AfterClass
    public void close() {

        mockedJDBCRecoveryDataStore.close();
        mockedUtils.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedFrameworkUtils.close();
    }

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        userEmailVerificationHandler = new UserEmailVerificationHandler();

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(serviceDataHolder);
        mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
        mockedFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");

        when(serviceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(realmConfiguration.getUserStoreProperty(eq(
                UserCoreConstants.RealmConfig.PROPERTY_DOMAIN_NAME))).thenReturn(userStoreDomainName);
    }

    @Test
    public void getNames() {

        Assert.assertEquals(userEmailVerificationHandler.getName(), "userEmailVerification");
        Assert.assertEquals(userEmailVerificationHandler.getFriendlyName(), "User Email Verification");
    }

    @Test(description = "Verification - Disabled, Multi attribute - Disabled")
    public void testHandleEventPreSetUserClaimsVerificationDisabledMultiDisabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         Notification on email update is enabled.
         Expected: Notification event should be triggered, pending email claim should be set to empty string.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, newEmail);

        mockUtilMethods(false, false, false,
                true);
        mockPrimaryEmail(existingEmail2);
        mockPendingVerificationEmail(existingEmail1);

        userEmailVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM),
                StringUtils.EMPTY);
    }

    @Test(description = "Verification - Disabled, Multi attribute - Enabled")
    public void testHandleEventPreSetUserClaimsVerificationDisabledMultiEnabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         New Email is not in the existing email address list.
         Expected: New email should be added to the new email addresses claim.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, newEmail);

        mockUtilMethods(false, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(existingEmail1, existingEmail2));
        mockExistingEmailAddressesList(existingEmails);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertTrue(StringUtils.contains(
                userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM), newEmail));
    }

    @Test(description = "Verification - Enabled, Multi attribute - Disabled")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiDisabled()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         New Email is not in the existing email address list.
         Expected: IdentityEventClientException should be thrown.
         */
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, newEmail);
        mockUtilMethods(true, false, false,
                false);

        userEmailVerificationHandler.handleEvent(event1);
        Map<String, String> userClaimsC1 = getUserClaimsFromEvent(event1);
        Assert.assertEquals(userClaimsC1.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), newEmail);
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Change primary email which is not in the " +
            "verified email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC1()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        /*
         Try to change the primary email, new Email is not in the existing verified email address list.
         Expected: IdentityEventClientException should be thrown.
         */
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, newEmail);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(existingEmail1, existingEmail2));
        mockExistingEmailAddressesList(existingEmails);

        List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(existingEmail1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

        try {
            userEmailVerificationHandler.handleEvent(event1);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof IdentityEventClientException);
        }
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Change primary email which is already " +
            "in the verified email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC2() throws IdentityEventException {

            /*
            Try to change the primary email, new Email is in the existing verified email address list.
            Expected: Thread local should be set to skip sending email verification on update.
            */
            Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                    null, null, newEmail);

            mockUtilMethods(true, true, false,
                    false);
            List<String> existingEmails = new ArrayList<>(Arrays.asList(existingEmail1, newEmail));
            mockExistingEmailAddressesList(existingEmails);

            List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(existingEmail1, newEmail));
            mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

            userEmailVerificationHandler.handleEvent(event);
            mockedUtils.verify(() -> Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(
                    eq(IdentityRecoveryConstants.SkipEmailVerificationOnUpdateStates
                            .SKIP_ON_INAPPLICABLE_CLAIMS.toString())));
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Update verified list with new email")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC3()
            throws IdentityEventException, UserStoreException {

        /*
        Case 1:Try to update the verified email list with a new email.
        Expected: IdentityEventClientException should be thrown.
        */
        String newVerifiedEmails = String.format("%s,%s", existingEmail1, newEmail);
        Event event1 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails, null, null);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(existingEmail1));
        mockExistingEmailAddressesList(existingEmails);

        List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(existingEmail1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

        userEmailVerificationHandler.handleEvent(event1);
        Map<String, String> userClaims1 = getUserClaimsFromEvent(event1);
        Assert.assertEquals(userClaims1.get(IdentityRecoveryConstants.EMAIL_ADDRESS_PENDING_VALUE_CLAIM), newEmail);

        /*
         Case 2: Update verified email list with the existing primary email which is not in the verified email list.
         Expected: Email should be added to the updated verified email list.
         */
        String newVerifiedEmails2 = String.format("%s,%s", existingEmail1, newEmail);
        Event event2 = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                newVerifiedEmails2, null, null);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails2 = new ArrayList<>(Arrays.asList(existingEmail1));
        mockExistingEmailAddressesList(existingEmails2);

        List<String> existingVerifiedEmails2 = new ArrayList<>(Arrays.asList(existingEmail1));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails2);

        mockPrimaryEmail(newEmail);

        userEmailVerificationHandler.handleEvent(event2);
        Map<String, String> userClaims2 = getUserClaimsFromEvent(event2);
        String updatedVerifiedEmails = userClaims2.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        Assert.assertTrue(StringUtils.contains(updatedVerifiedEmails, newEmail));
    }

    @Test(description = "Verification - Enabled, Multi attribute - Enabled, Remove email from email list")
    public void testHandleEventPreSetUserClaimsVerificationEnabledMultiEnabledC4() throws IdentityEventException {

        /*
         Remove an email from the existing emails list.
         Expected: Removed email should be removed from the verified email list as well.
         */
        Event event = createEvent(IdentityEventConstants.Event.PRE_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, existingEmail1, null);

        mockUtilMethods(true, true, false,
                false);
        List<String> existingEmails = new ArrayList<>(Arrays.asList(existingEmail1, existingEmail2));
        mockExistingEmailAddressesList(existingEmails);

        List<String> existingVerifiedEmails = new ArrayList<>(Arrays.asList(existingEmail1, existingEmail2));
        mockExistingVerifiedEmailAddressesList(existingVerifiedEmails);

        userEmailVerificationHandler.handleEvent(event);
        Map<String, String> userClaims = getUserClaimsFromEvent(event);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM), existingEmail1);
        Assert.assertEquals(userClaims.get(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM), existingEmail1);
    }

    @Test
    public void testHandleEventPostSetUserClaims()
            throws IdentityEventException, IdentityRecoveryException, UserStoreException {

        Event event = createEvent(IdentityEventConstants.Event.POST_SET_USER_CLAIMS, IdentityRecoveryConstants.FALSE,
                null, null, null);
        mockUtilMethods(true, true, false,
                true);
        mockPendingVerificationEmail(existingEmail1);

        userEmailVerificationHandler.handleEvent(event);
        verify(identityEventService).handleEvent(any());
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

    private void mockUtilMethods(boolean emailVerificationEnabled, boolean multiAttributeEnabled,
                                 boolean userVerifyClaimEnabled, boolean notificationOnEmailUpdate) {

        mockedUtils.when(
                Utils::isMultiEmailsAndMobileNumbersPerUserEnabled).thenReturn(multiAttributeEnabled);
        mockedUtils.when(Utils::isUseVerifyClaimEnabled).thenReturn(userVerifyClaimEnabled);
        mockedUtils.when(() -> Utils.getConnectorConfig(
                        eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE),
                        anyString()))
                .thenReturn(String.valueOf(emailVerificationEnabled));
        mockedUtils.when(() -> Utils.getConnectorConfig(
                        eq(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE),
                        anyString()))
                .thenReturn(String.valueOf(notificationOnEmailUpdate));
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
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, username);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
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
