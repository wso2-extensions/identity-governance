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

package org.wso2.carbon.identity.recovery.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang.StringUtils;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationFailureReason;
import org.wso2.carbon.identity.auth.attribute.handler.model.ValidationResult;
import org.wso2.carbon.identity.claim.metadata.mgt.ClaimMetadataManagementService;
import org.wso2.carbon.identity.claim.metadata.mgt.exception.ClaimMetadataException;
import org.wso2.carbon.identity.claim.metadata.mgt.model.LocalClaim;
import org.wso2.carbon.identity.claim.metadata.mgt.util.ClaimConstants;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.handler.event.account.lock.exception.AccountLockServiceException;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationClientException;
import org.wso2.carbon.identity.recovery.exception.SelfRegistrationException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.user.functionality.mgt.UserFunctionalityMgtConstants;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.claim.ClaimManager;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.constants.UserCoreErrorConstants;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.user.core.tenant.TenantManager;
import org.wso2.carbon.user.core.util.UserCoreUtil;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;
import org.wso2.carbon.identity.governance.service.notification.NotificationChannels;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.security.NoSuchAlgorithmException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;
import static junit.framework.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;
import static org.wso2.carbon.identity.auth.attribute.handler.AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND;

public class UtilsTest {

    @Mock
    private UserStoreManager userStoreManager;
    @Mock
    private UserRealm userRealm;
    @Mock
    private RealmService realmService;
    @Mock
    private RealmConfiguration realmConfiguration;
    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;
    @Mock
    private IdentityGovernanceService identityGovernanceService;
    @Mock
    private AccountLockService accountLockService;
    @Mock
    private TenantManager tenantManager;
    @Mock
    private ClaimManager claimManager;
    @Mock
    private AbstractUserStoreManager abstractUserStoreManager;
    @Mock
    private IdentityEventService identityEventService;
    @Mock
    private ClaimMetadataManagementService claimMetadataManagementService;

    private static MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;
    private static MockedStatic<UserStoreManager> mockedStaticUserStoreManager;
    private static MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private static MockedStatic<IdentityUtil> mockedStaticIdentityUtil;
    private static MockedStatic<FrameworkUtils> mockedStaticFrameworkUtils;
    private static MockedStatic<MultitenantUtils> mockedStaticMultiTenantUtils;
    private static MockedStatic<UserCoreUtil> mockedStaticUserCoreUtil;
    private static MockedStatic<JDBCRecoveryDataStore> mockedStaticJDBC;

    private static final String TENANT_DOMAIN = "test.com";
    private static final int TENANT_ID = 123;
    private static final String USER_NAME = "testUser";
    private static final String USER_STORE_DOMAIN = "TEST";
    private static final String TRUE_STRING = "TRUE";
    private static final String FALSE_STRING = "FALSE";
    private static final String PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL =
            "Recovery.Notification.Password.OTP.SendOTPInEmail";
    private static final String PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE =
            "Recovery.Notification.Password.OTP.SendOnlyOTPAsConfirmationCode";

    @BeforeClass
    public static void beforeClass() {

        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedStaticUserStoreManager = mockStatic(UserStoreManager.class);
        mockedIdentityRecoveryServiceDataHolder = Mockito.mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedStaticIdentityUtil = mockStatic(IdentityUtil.class);
        mockedStaticFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedStaticMultiTenantUtils = mockStatic(MultitenantUtils.class);
        mockedStaticUserCoreUtil = mockStatic(UserCoreUtil.class);
        mockedStaticJDBC = mockStatic(JDBCRecoveryDataStore.class);
    }

    @AfterClass
    public static void afterClass() {

        mockedStaticIdentityTenantUtil.close();
        mockedStaticUserStoreManager.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedStaticIdentityUtil.close();
        mockedStaticFrameworkUtils.close();
        mockedStaticMultiTenantUtils.close();
        mockedStaticUserCoreUtil.close();
        mockedStaticJDBC.close();
    }

    @BeforeMethod
    public void setUp() throws org.wso2.carbon.user.api.UserStoreException, IdentityGovernanceException {

        MockitoAnnotations.openMocks(this);

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(identityRecoveryServiceDataHolder);
        mockedStaticIdentityUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedStaticIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn("PRIMARY");
        mockedStaticIdentityUtil.when(() -> IdentityUtil.addDomainToName(USER_NAME, USER_STORE_DOMAIN))
                .thenReturn(USER_STORE_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USER_NAME);
        mockedStaticFrameworkUtils.when(FrameworkUtils::getMultiAttributeSeparator).thenReturn(",");
        mockedStaticMultiTenantUtils.when(() ->
                MultitenantUtils.getTenantAwareUsername(USER_NAME)).thenReturn(USER_NAME);

        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(realmService);
        when(identityRecoveryServiceDataHolder.getIdentityGovernanceService()).thenReturn(identityGovernanceService);
        when(identityGovernanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN))).thenAnswer(invocation -> {
            String[] keys = invocation.getArgument(0);
            Property property = new Property();
            property.setName(keys[0]);
            property.setValue(FALSE_STRING);
            return new Property[]{property};
        });
        when(identityRecoveryServiceDataHolder.getAccountLockService()).thenReturn(accountLockService);
        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService()).thenReturn(
                claimMetadataManagementService);

        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(realmService.getBootstrapRealm()).thenReturn(userRealm);
        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userRealm.getClaimManager()).thenReturn(claimManager);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfiguration);
        when(tenantManager.getTenantId(eq(TENANT_DOMAIN))).thenReturn(TENANT_ID);
    }

    @Test(expectedExceptions = IdentityRecoveryClientException.class)
    public void testCheckPasswordPatternViolationForInvalidDomain() throws Exception {

        User user = getUser();
        when(userStoreManager.getSecondaryUserStoreManager(USER_STORE_DOMAIN)).thenReturn(null);

        try {
            Utils.checkPasswordPatternViolation(new UserStoreException("Invalid Domain Name"), user);
        } catch (IdentityRecoveryClientException e) {
            assertEquals(e.getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_DOMAIN_VIOLATED.getCode());
            assertEquals(e.getMessage(), "Invalid domain " + user.getUserStoreDomain() + " provided.");
            throw e;
        }
    }

    @Test
    public void testGetArbitraryProperties() {

        org.wso2.carbon.identity.recovery.model.Property property =
                new org.wso2.carbon.identity.recovery.model.Property("key", "value");

        org.wso2.carbon.identity.recovery.model.Property[] properties =
                new org.wso2.carbon.identity.recovery.model.Property[]{property};
        Utils.setArbitraryProperties(properties);

        org.wso2.carbon.identity.recovery.model.Property[] result = Utils.getArbitraryProperties();

        assertNotNull(result);
        assertEquals(result.length, 1);
        assertEquals(result[0].getKey(), "key");
        assertEquals(result[0].getValue(), "value");

        Utils.clearArbitraryProperties();
        org.wso2.carbon.identity.recovery.model.Property[] result1 = Utils.getArbitraryProperties();
        assertNull(result1);
    }

    @Test
    public void testGetEmailVerifyTemporaryClaim() {

        Claim claim = new Claim();
        claim.setClaimUri(IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);
        Utils.setEmailVerifyTemporaryClaim(claim);

        Claim result = Utils.getEmailVerifyTemporaryClaim();
        assertNotNull(result);
        assertEquals(result.getClaimUri(), IdentityRecoveryConstants.VERIFY_EMAIL_CLIAM);

        Utils.clearEmailVerifyTemporaryClaim();
        Claim result1 = Utils.getEmailVerifyTemporaryClaim();
        assertNull(result1);
    }

    @Test
    public void testThreadLocalToSkipSendingEmailVerificationOnUpdate() {

        String threadLocalValue = "test-value";
        Utils.setThreadLocalToSkipSendingEmailVerificationOnUpdate(threadLocalValue);

        String result = Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate();
        assertEquals(result, threadLocalValue);

        Utils.unsetThreadLocalToSkipSendingEmailVerificationOnUpdate();
        String result1 = Utils.getThreadLocalToSkipSendingEmailVerificationOnUpdate();
        assertNull(result1);
    }

    @Test
    public void ThreadLocalToSkipSendingSmsOtpVerificationOnUpdate() {

        String threadLocalValue = "test-value";
        Utils.setThreadLocalToSkipSendingSmsOtpVerificationOnUpdate(threadLocalValue);

        String result = Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        assertEquals(result, threadLocalValue);

        Utils.unsetThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        String result1 = Utils.getThreadLocalToSkipSendingSmsOtpVerificationOnUpdate();
        assertNull(result1);
    }

    @Test
    public void testIsOnlyVerifiedMobileNumbersUpdatedThreadLocal() {

        // Initially should be false.
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedMobileNumbersUpdated());

        // Set to true.
        Utils.setThreadLocalIsOnlyVerifiedMobileNumbersUpdated(true);
        assertTrue(Utils.getThreadLocalIsOnlyVerifiedMobileNumbersUpdated());

        // Set to false.
        Utils.setThreadLocalIsOnlyVerifiedMobileNumbersUpdated(false);
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedMobileNumbersUpdated());

        // Unset.
        Utils.unsetThreadLocalIsOnlyVerifiedMobileNumbersUpdated();
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedMobileNumbersUpdated());
    }

    @Test
    public void testIsOnlyVerifiedEmailAddressesUpdatedThreadLocal() {

        // Initially should be false.
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedEmailAddressesUpdated());

        // Set to true.
        Utils.setThreadLocalIsOnlyVerifiedEmailAddressesUpdated(true);
        assertTrue(Utils.getThreadLocalIsOnlyVerifiedEmailAddressesUpdated());

        // Set to false.
        Utils.setThreadLocalIsOnlyVerifiedEmailAddressesUpdated(false);
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedEmailAddressesUpdated());

        // Unset.
        Utils.unsetThreadLocalIsOnlyVerifiedEmailAddressesUpdated();
        assertFalse(Utils.getThreadLocalIsOnlyVerifiedEmailAddressesUpdated());
    }

    @Test
    public void testGetClaimFromUserStoreManager() throws Exception {

        User user = getUser();
        Map<String, String> claimMap = new HashMap<>();
        claimMap.put("testClaim", "testValue");
        when(userStoreManager.getUserClaimValues(any(), any(), anyString()))
                .thenReturn(claimMap);

        String result = Utils.getClaimFromUserStoreManager(user, "testClaim");
        assertEquals("testValue", result);
    }

    @Test
    public void testRemoveClaimFromUserStoreManager() throws Exception {

        User user = getUser();
        String[] claims = new String[]{"testClaim"};
        Utils.removeClaimFromUserStoreManager(user, claims);
        String userStoreQualifiedUsername = USER_STORE_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USER_NAME;

        verify(userStoreManager).deleteUserClaimValues(eq(userStoreQualifiedUsername), eq(claims), anyString());
    }

    @Test
    public void testHandleServerException() throws IdentityRecoveryServerException {

        Exception exception =
                Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE, "data");
        assertEquals(exception.getClass(), IdentityRecoveryServerException.class);
        assertEquals(((IdentityRecoveryServerException) exception).getErrorCode(),
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode());

        Exception exception1 =
                Utils.handleServerException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE,
                "data", new Exception("test"));
        assertEquals(exception1.getClass(), IdentityRecoveryServerException.class);
        assertEquals(((IdentityRecoveryServerException) exception1).getErrorCode(),
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode());
        assertEquals(exception1.getCause().getMessage(), "test");

        Exception exception2 =
                Utils.handleServerException("code2", "message2%s", "data2");
        assertEquals(exception2.getClass(), IdentityRecoveryServerException.class);
        assertEquals(((IdentityRecoveryServerException) exception2).getErrorCode(), "code2");
        assertEquals(exception2.getMessage(), String.format("message2%s", "data2"));
    }

    @Test
    public void testHandleClientException() throws IdentityRecoveryClientException {

        Exception exception1 =
                Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE, "data1");
        assertEquals(exception1.getClass(), IdentityRecoveryClientException.class);
        assertEquals(((IdentityRecoveryClientException) exception1).getErrorCode(),
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode());
        assertEquals(exception1.getMessage(),
                String.format(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getMessage(), "data1"));

        Exception exception2 =
                Utils.handleClientException(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE,
                        "data", new Exception("test"));
        assertEquals(exception2.getClass(), IdentityRecoveryClientException.class);
        assertEquals(((IdentityRecoveryClientException) exception2).getErrorCode(),
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE.getCode());
        assertEquals(exception2.getCause().getMessage(), "test");

        Exception exception3 =
                Utils.handleClientException("code2", "message2%s", "data2");
        assertEquals(exception3.getClass(), IdentityRecoveryClientException.class);
        assertEquals(((IdentityRecoveryClientException) exception3).getErrorCode(), "code2");
        assertEquals(exception3.getMessage(), String.format("message2%s", "data2"));
    }

    @Test
    public void testHandleFunctionalityLockMgtServerException() {

        IdentityRecoveryConstants.ErrorMessages error = IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_EXPIRED_CODE;
        String userId = "testUser";
        String functionalityIdentifier = "PASSWORD_RECOVERY";
        boolean isDetailedErrorMessagesEnabled = true;

        try {
            Utils.handleFunctionalityLockMgtServerException(error, userId, TENANT_ID, functionalityIdentifier,
                    isDetailedErrorMessagesEnabled);
        } catch (IdentityRecoveryServerException e) {
            String expectedErrorCode = IdentityRecoveryConstants.PASSWORD_RECOVERY_SCENARIO + "-" + error.getCode();
            assertEquals(e.getErrorCode(), expectedErrorCode);
            String expectedErrorMessage = error.getMessage() +
                    String.format("functionality: %s \nuserId: %s \ntenantId: %d.", functionalityIdentifier, userId,
                            TENANT_ID);
            assertEquals(e.getMessage(), expectedErrorMessage);
            assertNull(e.getCause());
        }

        // Test with detailed error messages disabled.
        isDetailedErrorMessagesEnabled = false;
        try {
            Utils.handleFunctionalityLockMgtServerException(error, userId, TENANT_ID,
                    functionalityIdentifier, isDetailedErrorMessagesEnabled);
        } catch (IdentityRecoveryServerException e) {
            // Verify that the error message doesn't contain the detailed information.
            assertFalse(e.getMessage().contains("functionality:"));
            assertFalse(e.getMessage().contains("userId:"));
            assertFalse(e.getMessage().contains("tenantId:"));
        }
    }

    @Test
    public void testDoHash() throws org.wso2.carbon.user.api.UserStoreException {

        String value = "testValue";
        String expectedHash = "expected_hash";

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.hashCode(value)).thenReturn(expectedHash);
            mockedUtils.when(() -> Utils.doHash(value)).thenCallRealMethod();

            String result = Utils.doHash(value);
            assertEquals(result, expectedHash);
        }

        try (MockedStatic<Utils> mockedUtils = mockStatic(Utils.class)) {
            mockedUtils.when(() -> Utils.hashCode(value)).thenThrow(new NoSuchAlgorithmException("Test exception"));
            mockedUtils.when(() -> Utils.doHash(value)).thenCallRealMethod();

            Utils.doHash(value);
        } catch (Exception e) {
            assertTrue(e instanceof org.wso2.carbon.user.api.UserStoreException);
        }
    }

    @Test
    public void testSetClaimInUserStoreManager() throws org.wso2.carbon.user.api.UserStoreException {

        String claim = "testClaim";
        String value = "testValue";

        Map<String, String> existingValues = new HashMap<>();
        existingValues.put(claim, "oldValue");
        when(userStoreManager.getUserClaimValues(anyString(), any(String[].class), anyString()))
                .thenReturn(existingValues);

        Utils.setClaimInUserStoreManager(getUser(), claim, value);

        verify(userStoreManager).setUserClaimValues(anyString(),
                argThat(map -> map.containsKey(claim) && map.get(claim).equals(value)), anyString());
    }

    @Test
    public void testGetClaimListOfUser() throws IdentityRecoveryClientException, IdentityRecoveryServerException,
            UserStoreException {

        String[] claimsList = {"claim1", "claim2"};
        Map<String, String> expectedClaims = new HashMap<>();
        expectedClaims.put("claim1", "value1");
        expectedClaims.put("claim2", "value2");

        when(userStoreManager.getUserClaimValues(anyString(), eq(claimsList), anyString()))
                .thenReturn(expectedClaims);

        Map<String, String> result = Utils.getClaimListOfUser(getUser(), claimsList);
        assertEquals(result, expectedClaims);

        // Case 2: Throw UserStoreException.
        when(userStoreManager.getUserClaimValues(anyString(), eq(claimsList), anyString()))
                .thenThrow(new UserStoreException());
        try {
            Utils.getClaimListOfUser(getUser(), claimsList);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testSetClaimsListOfUser() throws Exception {

        User user = getUser();
        Map<String, String> claims = new HashMap<>();
        claims.put("http://wso2.org/claims/givenname", "John");
        claims.put("http://wso2.org/claims/emailaddress", "john@example.com");

        Utils.setClaimsListOfUser(user, claims);

        String userStoreDomainQualifiedUsername = getUserStoreQualifiedUsername(USER_NAME, USER_STORE_DOMAIN);
        verify(userStoreManager).setUserClaimValues(eq(userStoreDomainQualifiedUsername), eq(claims), anyString());

        // Case 2: Throw UserStoreException.
        try {
            doThrow(new UserStoreException("Test exception"))
                    .when(userStoreManager).setUserClaimValues(anyString(), anyMap(), anyString());
            Utils.setClaimsListOfUser(user, claims);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testGetRecoveryConfigs() throws Exception {

        String key = "recovery.key";
        String expectedValue = "test_value";
        Property property = new Property();
        property.setName(key);
        property.setValue(expectedValue);
        Property[] properties = new Property[]{property};

        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenReturn(properties);

        String result = Utils.getRecoveryConfigs(key, TENANT_DOMAIN);
        assertEquals(result, expectedValue);

        // Case 2: Throw IdentityGovernanceException.
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenThrow(new IdentityGovernanceException("Test exception"));
        try {
            Utils.getRecoveryConfigs(key, TENANT_DOMAIN);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }

        // Case 3: Return empty connectorConfigs.
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenReturn(new Property[0]);
        try {
            Utils.getRecoveryConfigs(key, TENANT_DOMAIN);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testGetSignUpConfigs_Success() throws Exception {

        String key = "recovery.key";
        String expectedValue = "test_value";
        Property property = new Property();
        property.setName(key);
        property.setValue(expectedValue);
        Property[] properties = new Property[]{property};
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenReturn(properties);

        String result = Utils.getSignUpConfigs(key, TENANT_DOMAIN);
        assertEquals(result, expectedValue);

        // Case 2: Throw IdentityGovernanceException.
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenThrow(new IdentityGovernanceException("Test exception"));
        try {
            Utils.getSignUpConfigs(key, TENANT_DOMAIN);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testGetConnectorConfig() throws Exception {

        String key = "recovery.key";
        String expectedValue = "test_value";
        Property property = new Property();
        property.setName(key);
        property.setValue(expectedValue);
        Property[] properties = new Property[]{property};
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenReturn(properties);

        String result = Utils.getConnectorConfig(key, TENANT_DOMAIN);
        assertEquals(result, expectedValue);

        // Case 2: Throw IdentityGovernanceException.
        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenThrow(new IdentityGovernanceException("Test exception"));
        try {
            Utils.getSignUpConfigs(key, TENANT_DOMAIN);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testGetChallengeSetDirFromUri() {

        String uri1 = "http://wso2.org/claims/challengeQuestion1";
        String uri2 = "challengeQuestion1";
        String uri3 = null;

        assertEquals(Utils.getChallengeSetDirFromUri(uri1), "challengeQuestion1");
        assertEquals(Utils.getChallengeSetDirFromUri(uri2), "challengeQuestion1");
        assertNull(Utils.getChallengeSetDirFromUri(uri3));
    }

    @Test
    public void testIsAccountLocked() throws Exception {

        User user = getUser();
        when(accountLockService.isAccountLocked(eq(USER_NAME), eq(TENANT_DOMAIN), eq(USER_STORE_DOMAIN)))
                .thenReturn(true);
        assertTrue(Utils.isAccountLocked(user));

        // Case 2: Throws AccountLockServiceException.
        when(accountLockService.isAccountLocked(anyString(), anyString(), anyString()))
                .thenThrow(new AccountLockServiceException("Test exception"));
        try {
            Utils.isAccountLocked(user);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
    }

    @Test
    public void testIsAccountDisabled() throws Exception {

        User user = getUser();
        Map<String, String> claimValues = new HashMap<>();
        claimValues.put(IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM, Boolean.TRUE.toString());
        when(userStoreManager.getUserClaimValues(eq(getUserStoreQualifiedUsername(USER_NAME, USER_STORE_DOMAIN)),
                eq(new String[]{IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM}), anyString()))
                .thenReturn(claimValues);

        assertTrue(Utils.isAccountDisabled(user));

        // Case 2: Throws error while loading realm service.
        when(realmService.getTenantUserRealm(anyInt())).thenThrow(
                new UserStoreException("Test exception"));

        try {
            Utils.isAccountDisabled(user);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE.getCode());
        }

        // Case 3: Throws error while loading user store manager.
        doReturn(userRealm).when(realmService).getTenantUserRealm(anyInt());
        when(userRealm.getUserStoreManager()).thenThrow(
                new  UserStoreException("Test exception"));

        try {
            Utils.isAccountDisabled(user);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_STORE_MANAGER.getCode());
        }

        // Case 4: Throws error while getting user claim values.
        doReturn(userRealm).when(realmService).getTenantUserRealm(anyInt());
        doReturn(userStoreManager).when(userRealm).getUserStoreManager();
        when(userStoreManager.getUserClaimValues(eq(getUserStoreQualifiedUsername(USER_NAME, USER_STORE_DOMAIN)),
                eq(new String[]{IdentityRecoveryConstants.ACCOUNT_DISABLED_CLAIM}), anyString()))
                .thenThrow(new UserStoreException("Test exception"));

        try {
            Utils.isAccountDisabled(user);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_LOAD_USER_CLAIMS.getCode());
        }
    }

    @Test
    public void testCreateUser() {

        User user = Utils.createUser(USER_NAME, TENANT_DOMAIN);
        assertEquals(user.getUserName(), USER_NAME);
        assertEquals(user.getTenantDomain(), TENANT_DOMAIN);
    }

    @Test
    public void testValidateCallbackURL() throws IdentityEventException, IdentityGovernanceException {

        String callbackURL = "https://example.com/callback";
        String tenantDomain = "example.com";
        String callbackRegexType = "RECOVERY_CALLBACK_REGEX";

        String expectedValue = "https://.*\\.com/.*";
        Property property = new Property();
        property.setName(callbackRegexType);
        property.setValue(expectedValue);
        Property[] properties = new Property[]{property};
        when(identityGovernanceService.getConfiguration(new String[]{callbackRegexType,}, tenantDomain))
                .thenReturn(properties);

        boolean result = Utils.validateCallbackURL(callbackURL, tenantDomain, callbackRegexType);
        assertTrue(result);

        result = Utils.validateCallbackURL("http://malicious.com", tenantDomain, callbackRegexType);
        assertFalse(result);
    }

    @Test
    public void testGetCallbackURLFromRegistration() throws MalformedURLException, UnsupportedEncodingException {

        org.wso2.carbon.identity.recovery.model.Property[] properties =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                        new org.wso2.carbon.identity.recovery.model.Property(IdentityRecoveryConstants.CALLBACK,
                                "https://example.com/callback?param=value"),
                        new org.wso2.carbon.identity.recovery.model.Property("other", "value")
                };

        String result = Utils.getCallbackURLFromRegistration(properties);
        assertEquals(result, "https://example.com/callback");
    }

    @Test
    public void testGetCallbackURL() throws UnsupportedEncodingException, URISyntaxException {

        org.wso2.carbon.identity.recovery.model.Property[] properties =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property(IdentityRecoveryConstants.CALLBACK,
                        "https://example.com/callback?param=value"),
                new org.wso2.carbon.identity.recovery.model.Property("other", "value")
        };

        String result = Utils.getCallbackURL(properties);
        assertEquals(result, "https://example.com/callback");
    }

    @Test
    public void testIsAccessUrlAvailable() {

        org.wso2.carbon.identity.recovery.model.Property[] propertiesTrue =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property(IdentityRecoveryConstants.IS_ACCESS_URL_AVAILABLE,
                        "true")
        };
        assertTrue(Utils.isAccessUrlAvailable(propertiesTrue));

        // Case 2: When IS_ACCESS_URL_AVAILABLE property is not present.
        org.wso2.carbon.identity.recovery.model.Property[] propertiesNoAccessUrl =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property("someOtherKey", "someValue")
        };
        assertFalse(Utils.isAccessUrlAvailable(propertiesNoAccessUrl));

        // Case 3: Null properties.
        assertFalse(Utils.isAccessUrlAvailable(null));
    }

    @Test
    public void testIsLiteSignUp() {

        org.wso2.carbon.identity.recovery.model.Property[] propertiesTrue =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property(IdentityRecoveryConstants.IS_LITE_SIGN_UP, "true")
        };
        assertTrue(Utils.isLiteSignUp(propertiesTrue));

        // Case 2: Test when property is not present.
        org.wso2.carbon.identity.recovery.model.Property[] propertiesNoLiteSignUp =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property("someOtherKey", "someValue")
        };
        assertFalse(Utils.isLiteSignUp(propertiesNoLiteSignUp));

        // Case 3: Test with null properties.
        assertFalse(Utils.isLiteSignUp(null));
    }

    @Test
    public void testIsUserPortalURL() {

        org.wso2.carbon.identity.recovery.model.Property[] propertiesTrue =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property(IdentityRecoveryConstants.IS_USER_PORTAL_URL,
                        "true")
        };
        assertTrue(Utils.isUserPortalURL(propertiesTrue));

        // Case 2: Test when property is not present.
        org.wso2.carbon.identity.recovery.model.Property[] propertiesNoUserPortalURL =
                new org.wso2.carbon.identity.recovery.model.Property[]{
                new org.wso2.carbon.identity.recovery.model.Property("someOtherKey", "someValue")
        };
        assertFalse(Utils.isUserPortalURL(propertiesNoUserPortalURL));

        // Case 3: Test with null properties.
        assertFalse(Utils.isUserPortalURL(null));
    }

    @Test
    public void testCheckPasswordPatternViolation() throws Exception {

        String PROPERTY_PASSWORD_ERROR_MSG = "PasswordJavaRegExViolationErrorMsg";
        String errorCode = UserCoreErrorConstants.ErrorMessages
                .ERROR_CODE_ERROR_DURING_PRE_UPDATE_CREDENTIAL_BY_ADMIN.getCode();
        String errorMessage = "TEST_CUSTOM_PASSWORD_ERROR_MSG";
        User user = getUser();
        user.setUserStoreDomain(UserCoreConstants.PRIMARY_DEFAULT_DOMAIN_NAME);

        when(realmConfiguration.getUserStoreProperty(PROPERTY_PASSWORD_ERROR_MSG)).thenReturn(errorMessage);
        UserStoreException exceptionWithViolation =
                new UserStoreException(String.format("%s: %s", errorCode, errorMessage));

        try {
            Utils.checkPasswordPatternViolation(exceptionWithViolation, user);
            fail("Expected IdentityRecoveryClientException was not thrown");
        } catch (IdentityRecoveryClientException e) {
            assertEquals(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(),
                    e.getErrorCode());
        }

        // Case 2 - Password error message property is not set.
        when(realmConfiguration.getUserStoreProperty(PROPERTY_PASSWORD_ERROR_MSG)).thenReturn("");
        UserStoreException exceptionWithInvalidPasswordCode = new UserStoreException(
                UserCoreErrorConstants.ErrorMessages.ERROR_CODE_INVALID_PASSWORD.getCode());
        when(realmConfiguration.getUserStoreProperty(UserCoreConstants.RealmConfig.PROPERTY_JAVA_REG_EX))
                .thenReturn("^[a-zA-Z0-9]{5,10}$");
        try {
            Utils.checkPasswordPatternViolation(exceptionWithInvalidPasswordCode, user);
            fail("Expected IdentityRecoveryClientException was not thrown");
        } catch (IdentityRecoveryClientException e) {
            assertEquals(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_POLICY_VIOLATION.getCode(), e.getErrorCode());
            assertTrue(e.getMessage().contains("^[a-zA-Z0-9]{5,10}$"));
        }
    }

    @Test
    public void testIsAccountStateClaimExisting() throws Exception {

        Claim mockClaim = mock(Claim.class);
        when(claimManager.getClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI)).thenReturn(mockClaim);
        boolean result = Utils.isAccountStateClaimExisting(TENANT_DOMAIN);
        assertTrue(result);

        // Case 2: Throws UserStoreException.
        when(claimManager.getClaim(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI))
                .thenThrow(new UserStoreException("Test exception"));
        try {
            Utils.isAccountStateClaimExisting(TENANT_DOMAIN);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityEventException);
        }
    }

    @Test
    public void testPrependOperationScenarioToErrorCode() {

        String scenario = "USR";
        String errorCode = "20045";

        // Test with valid scenario and error code.
        String result = Utils.prependOperationScenarioToErrorCode(errorCode, scenario);
        assertEquals(result, "USR-20045");

        // Test with empty error code.
        result = Utils.prependOperationScenarioToErrorCode("", scenario);
        assertEquals(result, "");

        // Test with scenario already in error code.
        result = Utils.prependOperationScenarioToErrorCode("USR-20045", scenario);
        assertEquals(result, "USR-20045");
    }

    @Test
    public void testIsNotificationsInternallyManaged() throws Exception {

        Property[] properties = new Property[1];
        properties[0] = new Property();
        properties[0].setName(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        properties[0].setValue(Boolean.TRUE.toString());
        when(identityGovernanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                .thenReturn(properties);

        // Case 1: Empty properties.
        assertTrue(Utils.isNotificationsInternallyManaged(TENANT_DOMAIN, new HashMap<>()));

        // Case 2: Properties with MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY set to true.
        Map<String, String> props = new HashMap<>();
        props.put(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY, "true");
        assertTrue(Utils.isNotificationsInternallyManaged(TENANT_DOMAIN, props));

        // Case 4: Properties without MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY.
        props.remove(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY);
        assertTrue(Utils.isNotificationsInternallyManaged(TENANT_DOMAIN, props));

        // Case 5: Invalid boolean value.
        props.put(IdentityRecoveryConstants.MANAGE_NOTIFICATIONS_INTERNALLY_PROPERTY_KEY, "invalid");
        assertFalse(Utils.isNotificationsInternallyManaged(TENANT_DOMAIN, props));
    }

    @Test
    public void testResolveEventName() {

        // Case 1: SMS channel.
        String smsChannel = NotificationChannels.SMS_CHANNEL.getChannelType();
        String expectedSmsEventName = IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_PREFIX + smsChannel
                + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX
                + IdentityRecoveryConstants.NOTIFICATION_EVENTNAME_SUFFIX_LOCAL;
        assertEquals(Utils.resolveEventName(smsChannel), expectedSmsEventName);

        // Case 2: Other channels.
        String otherChannel = "EMAIL";
        assertEquals(Utils.resolveEventName(otherChannel), IdentityEventConstants.Event.TRIGGER_NOTIFICATION);
    }

    @Test
    public void testValidateEmailUsernameValidEmail() throws IdentityRecoveryClientException {

        mockedStaticIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(true);
        User user = new User();
        user.setUserName("test@example.com");
        user.setTenantDomain("carbon.super");

        Utils.validateEmailUsername(user);

        // Case 2: Invalid email username.
        User user2 = new User();
        user2.setUserName("testuser");
        user2.setTenantDomain(TENANT_DOMAIN);

        try {
            Utils.validateEmailUsername(user);
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @Test
    public void testBuildUser() {

        mockedStaticUserCoreUtil.when(() -> UserCoreUtil.removeDomainFromName(USER_NAME)).thenReturn(USER_NAME);
        User user = Utils.buildUser(USER_NAME, TENANT_DOMAIN);
        assertEquals(user.getUserName(), USER_NAME);
        assertEquals(user.getTenantDomain(), TENANT_DOMAIN);
    }

    @Test
    public void testIsPerUserFunctionalityLockingEnabled() {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                UserFunctionalityMgtConstants.ENABLE_PER_USER_FUNCTIONALITY_LOCKING)).thenReturn("true");
        assertTrue(Utils.isPerUserFunctionalityLockingEnabled());
    }

    @Test
    public void testIsPasswordRecoveryEmailOtpEnabled()
            throws IdentityRecoveryServerException, IdentityGovernanceException {

        Property property = new Property();
        property.setName(PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL);
        property.setValue(TRUE_STRING);
        Property[] properties = new Property[]{property};

        when(identityGovernanceService.
                getConfiguration(new String[]{PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL}, TENANT_DOMAIN)).
                thenReturn(properties);

        assertTrue(Utils.isPasswordRecoveryEmailOtpEnabled(TENANT_DOMAIN));
    }

    @Test
    public void testSkipConcatForOTPBasedEmailRecovery() throws IdentityGovernanceException {

        // case 1: False in server config.
        mockedStaticIdentityUtil.when(() -> IdentityUtil.
                        getProperty(PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE)).
                thenReturn(FALSE_STRING);
        assertFalse(Utils.skipConcatForOTPBasedEmailRecovery(TENANT_DOMAIN));

        // case 2: True in server config but email OTP is disabled.
        mockedStaticIdentityUtil.when(() -> IdentityUtil.
                        getProperty(PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE)).
                thenReturn(TRUE_STRING);
        Property property = new Property();
        property.setName(PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL);
        property.setValue(FALSE_STRING);
        Property[] properties = new Property[]{property};

        when(identityGovernanceService.
                getConfiguration(new String[]{PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL}, TENANT_DOMAIN)).
                thenReturn(properties);

        assertFalse(Utils.skipConcatForOTPBasedEmailRecovery(TENANT_DOMAIN));

        // Case 3: True in server config and email OTP is enabled.
        property.setValue(TRUE_STRING);
        properties = new Property[]{property};

        when(identityGovernanceService.
                getConfiguration(new String[]{PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL}, TENANT_DOMAIN)).
                thenReturn(properties);
        assertTrue(Utils.skipConcatForOTPBasedEmailRecovery(TENANT_DOMAIN));
    }

    @Test
    public void testIsDetailedErrorResponseEnabled() {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                IdentityRecoveryConstants.ENABLE_DETAILED_ERROR_RESPONSE)).thenReturn("true");
        assertTrue(Utils.isDetailedErrorResponseEnabled());
    }

    @Test
    public void testGetUserId() throws Exception {

        String expectedUserId = "12345-67890-abcde-fghij";
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.getUserIDFromUserName(USER_NAME)).thenReturn(expectedUserId);

        String userId = Utils.getUserId(USER_NAME, TENANT_ID);
        assertEquals(userId, expectedUserId);

        // Case 2: RealmService is null.
        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(null);
        try {
            Utils.getUserId(USER_NAME, TENANT_ID);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE.getCode());
        }

        // Reset RealmService mock.
        when(identityRecoveryServiceDataHolder.getRealmService()).thenReturn(realmService);

        // Case 4: UserStoreException when getting UserStoreManager.
        when(realmService.getTenantUserRealm(TENANT_ID)).thenThrow(new UserStoreException("Test exception"));
        try {
            Utils.getUserId(USER_NAME, TENANT_ID);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages
                    .ERROR_CODE_FAILED_TO_LOAD_REALM_SERVICE.getCode());
        }

        // Reset UserStoreManager mock.
        doReturn(userRealm).when(realmService).getTenantUserRealm(TENANT_ID);
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);

        // Case 5: UserStoreException when getting user ID
        when(abstractUserStoreManager.getUserIDFromUserName(USER_NAME)).thenThrow(
                new UserStoreException("Test exception"));
        try {
            Utils.getUserId(USER_NAME, TENANT_ID);
            fail("Expected IdentityRecoveryServerException was not thrown");
        } catch (IdentityRecoveryServerException e) {
            assertEquals(e.getErrorCode(), IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FAILED_TO_UPDATE_USER_CLAIMS.getCode());
        }
    }

    @Test
    public void testIsSkipRecoveryWithChallengeQuestionsForInsufficientAnswersEnabled() {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                IdentityRecoveryConstants.RECOVERY_QUESTION_PASSWORD_SKIP_ON_INSUFFICIENT_ANSWERS))
                .thenReturn("true");
        assertTrue(Utils.isSkipRecoveryWithChallengeQuestionsForInsufficientAnswersEnabled());
    }

    @Test
    public void testIsUseVerifyClaimEnabled() {

        mockedStaticIdentityUtil.when(() ->IdentityUtil.getProperty
                (IdentityRecoveryConstants.ConnectorConfig.USE_VERIFY_CLAIM_ON_UPDATE)).thenReturn("true");
        assertTrue(Utils.isUseVerifyClaimEnabled());
    }

    @Test
    public void testPublishRecoveryEvent() throws IdentityEventException {

        Map<String, Object> map = new HashMap<>();
        String eventName = "testEvent";
        String confirmationCode = "123456";

        Utils.publishRecoveryEvent(map, eventName, confirmationCode);
        verify(identityEventService).handleEvent(any());
    }

    @Test
    public void testGetAccountState() throws UserStoreException {

        String expectedAccountState = "testValue";
        User user = getUser();

        // Case 1: Existing user.
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.isExistingUser(user.getUserName())).thenReturn(true);

        Map<String, String> claimMap = new HashMap<>();
        claimMap.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI, expectedAccountState);
        when(abstractUserStoreManager.getUserClaimValues(user.getUserName(),
                new String[]{IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI}, "default"))
                .thenReturn(claimMap);
        String accountState = Utils.getAccountState(user);
        assertEquals(accountState, expectedAccountState);

        // Case 2: User doesn't exist in primary user store, secondary user store null.
        when(abstractUserStoreManager.isExistingUser(user.getUserName())).thenReturn(false);
        when(abstractUserStoreManager.getSecondaryUserStoreManager()).thenReturn(null);

        accountState = Utils.getAccountState(user);
        assertEquals(accountState, StringUtils.EMPTY);
    }

    @Test
    public void testGetAccountStateForUserNameWithoutUserDomain() throws UserStoreException {

        User user = getUser();
        String expectedAccountState = "testValue";
        String userStoreDomainQualifiedUsername = USER_STORE_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USER_NAME;
        mockedStaticUserCoreUtil.when(() -> UserCoreUtil.addDomainToName(USER_NAME, USER_STORE_DOMAIN))
                .thenReturn(userStoreDomainQualifiedUsername);

        // Case 1: Existing user.
        when(userRealm.getUserStoreManager()).thenReturn(abstractUserStoreManager);
        when(abstractUserStoreManager.isExistingUser(user.getUserName())).thenReturn(true);

        Map<String, String> claimMap = new HashMap<>();
        claimMap.put(IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI, expectedAccountState);
        when(abstractUserStoreManager.getUserClaimValues(userStoreDomainQualifiedUsername,
                new String[]{IdentityRecoveryConstants.ACCOUNT_STATE_CLAIM_URI}, "default"))
                .thenReturn(claimMap);

        String accountState = Utils.getAccountStateForUserNameWithoutUserDomain(user);
        assertEquals(accountState, expectedAccountState);
    }

    @Test
    public void testGenerateRandomPassword() {

        int passwordLength = 10;
        char[] result = Utils.generateRandomPassword(passwordLength);
        assertEquals(result.length, passwordLength);
    }

    @Test
    public void testReIssueExistingConfirmationCodeNotificationBasedPasswordRecovery() {

        int recoveryConfirmationTolerancePeriod = 10;
        int recoveryCodeExpiryTime = 20;

        UserRecoveryData recoveryData = new UserRecoveryData(getUser(), "12345",
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD);

        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.MINUTE, -5); 
        Date creationTime = calendar.getTime();
        Timestamp timestamp = new Timestamp(creationTime.getTime());

        recoveryData.setTimeCreated(timestamp);

        // Case 1: With RECOVERY_CONFIRMATION_CODE_DEFAULT_TOLERANCE, ConfirmationTolerancePeriod = 10.
        boolean result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 2: With RECOVERY_CODE_DEFAULT_EXPIRY_TIME, ConfirmationTolerancePeriod = 10.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(recoveryConfirmationTolerancePeriod));
        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 3: RecoveryCodeExpiryTime = 20, ConfirmationTolerancePeriod = 10.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CODE_EXPIRY_TIME,
                String.valueOf(recoveryCodeExpiryTime));
        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertTrue(result);

        // Case 4: Invalid value for RECOVERY_CODE_EXPIRY_TIME.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.ConnectorConfig.RECOVERY_CODE_EXPIRY_TIME,
                "invalid");
        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 5: Invalid value for RECOVERY_CONFIRMATION_CODE_TOLERANCE_PERIOD.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.RECOVERY_CONFIRMATION_CODE_TOLERANCE_PERIOD, "invalid");
        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);
    }

    @Test
    public void testReIssueExistingConfirmationCodeSelfSignupEmail() throws IdentityGovernanceException {

        int selfRegistrationCodeTolerance = 10;
        int selfRegistrationCodeExpiryTime = 20;

        UserRecoveryData recoveryData = new UserRecoveryData(getUser(), "12345",
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.UPDATE_PASSWORD);

        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.MINUTE, -5); 
        Date creationTime = calendar.getTime();
        Timestamp timestamp = new Timestamp(creationTime.getTime());

        recoveryData.setTimeCreated(timestamp);

        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(selfRegistrationCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, String.valueOf(selfRegistrationCodeExpiryTime));

        boolean result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertTrue(result);

        // Case 2: Invalid value for SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                "invalid");

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 3: Invalid value for SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(selfRegistrationCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, "invalid");

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 4: SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME less than tolerance period.
        selfRegistrationCodeTolerance = 10;
        selfRegistrationCodeExpiryTime = 5;
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(selfRegistrationCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, String.valueOf(selfRegistrationCodeExpiryTime));

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);
    }

    @Test
    public void testReIssueExistingConfirmationCodeSelfSignupSMS() throws IdentityGovernanceException {

        int selfRegistrationCodeTolerance = 10;
        int selfRegistrationCodeExpiryTime = 20;

        UserRecoveryData recoveryData = new UserRecoveryData(getUser(), "12345",
                RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.UPDATE_PASSWORD);

        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.MINUTE, -5);
        Date creationTime = calendar.getTime();
        Timestamp timestamp = new Timestamp(creationTime.getTime());

        recoveryData.setTimeCreated(timestamp);

        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_SMS_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(selfRegistrationCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME,
                String.valueOf(selfRegistrationCodeExpiryTime));

        boolean result = Utils.reIssueExistingConfirmationCode(recoveryData, "SMS");
        assertTrue(result);

        // Other notification channel.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.SELF_SIGN_UP_EMAIL_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(5));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, String.valueOf(selfRegistrationCodeExpiryTime));

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "OTHER");
        assertFalse(result);
    }

    @Test
    public void testReIssueExistingConfirmationCodeAskPassword() throws IdentityGovernanceException {

        int askPasswordCodeTolerance = 10;
        int askPasswordCodeExpiryTime = 20;

        UserRecoveryData recoveryData = new UserRecoveryData(getUser(), "12345",
                RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD);

        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.MINUTE, -5);
        Date creationTime = calendar.getTime();
        Timestamp timestamp = new Timestamp(creationTime.getTime());

        recoveryData.setTimeCreated(timestamp);

        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.ASK_PASSWORD_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(askPasswordCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                        .ASK_PASSWORD_EXPIRY_TIME,
                String.valueOf(askPasswordCodeExpiryTime));

        boolean result = Utils.reIssueExistingConfirmationCode(recoveryData, "SMS");
        assertTrue(result);

        // Case 3: Invalid value for SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME.
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.ASK_PASSWORD_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(askPasswordCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .ASK_PASSWORD_EXPIRY_TIME, "invalid");

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);

        // Case 4: Code expiry time is less than tolerance period.
        askPasswordCodeExpiryTime = 5;
        mockIdentityUtilsGetProperty(IdentityRecoveryConstants.ASK_PASSWORD_CONFIRMATION_CODE_TOLERANCE_PERIOD,
                String.valueOf(askPasswordCodeTolerance));
        mockGetRecoveryConfig(IdentityRecoveryConstants.ConnectorConfig
                .ASK_PASSWORD_EXPIRY_TIME, String.valueOf(askPasswordCodeExpiryTime));

        result = Utils.reIssueExistingConfirmationCode(recoveryData, "EMAIL");
        assertFalse(result);
    }

    @Test
    public void testHandleAttributeValidationFailureWithValidationResult() {

        // Case 1: Validation result is null.
        ValidationResult validationResult = null;
        try {
            Utils.handleAttributeValidationFailure(validationResult);
        } catch (Exception e) {
            assertTrue(e instanceof SelfRegistrationClientException);
            assertEquals(((SelfRegistrationClientException) e).getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED_ERROR_VALIDATING_ATTRIBUTES
                            .getCode());
        }

        // Case 2: Validation result is not null.
        ValidationFailureReason validationFailureReason = new ValidationFailureReason();
        validationFailureReason.setErrorCode("test-code");
        validationFailureReason.setReason("test-reason");
        validationFailureReason.setAuthAttribute("test-auth-attribute");

        validationResult = new ValidationResult();
        validationResult.setValidationFailureReasons(new ArrayList<>(Arrays.asList(validationFailureReason)));

        try {
            Utils.handleAttributeValidationFailure(validationResult);
        } catch (Exception e) {
            assertTrue(e instanceof SelfRegistrationClientException);
            assertEquals(((SelfRegistrationClientException) e).getErrorCode(),
                    IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_INVALID_USER_ATTRIBUTES_FOR_REGISTRATION
                            .getCode());
        }
    }

    @Test
    public void testHandleAttributeValidationFailure() {

        AuthAttributeHandlerException exception =
                new AuthAttributeHandlerClientException(ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getCode(),
                        ERROR_CODE_AUTH_ATTRIBUTE_HANDLER_NOT_FOUND.getMessage());
        try {
            Utils.handleAttributeValidationFailure(exception);
        } catch (Exception e) {
            assertTrue(e instanceof SelfRegistrationClientException);
        }

        // Case 2: AuthAttributeHandlerException exception.
        AuthAttributeHandlerException exception1 =
                new AuthAttributeHandlerException("test-code", "test-message");
        try {
            Utils.handleAttributeValidationFailure(exception1);
        } catch (Exception e) {
            assertTrue(e instanceof SelfRegistrationException);
        }
    }

    @Test
    public void testGetMultiValuedClaim() throws IdentityEventException, org.wso2.carbon.user.core.UserStoreException {

        User user = getUser();
        String claimValue = "value1,value2,value3";
        List<String> expectedClaimList = Arrays.asList("value1", "value2", "value3");
        when(userStoreManager.getUserClaimValue(any(), anyString(), any()))
                .thenReturn(claimValue);

        List<String> result = Utils.getMultiValuedClaim(userStoreManager, user, "testClaim");
        assertEquals(expectedClaimList, result);

        // Case 2: Throw user store exception when retrieving user claim value.
        when(userStoreManager.getUserClaimValue(any(), anyString(), any()))
                .thenThrow(new org.wso2.carbon.user.core.UserStoreException());
        try {
            Utils.getMultiValuedClaim(userStoreManager, user, "testClaim");
        } catch (Exception e) {
            assertTrue(e instanceof IdentityEventException);
        }
    }

    @Test
    public void testIsMultiEmailsAndMobileNumbersPerUserEnabled() throws Exception {

        // Mock ClaimMetadataManagementService
        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService())
                .thenReturn(claimMetadataManagementService);

        // Case 1: When support_multi_emails_and_mobile_numbers_per_user config is false.
        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("false");

        boolean isEnabled = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        boolean isEmailsEnabled = Utils.isMultiEmailAddressesPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        boolean isMobilesEnabled = Utils.isMultiMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(isEnabled);
        assertFalse(isEmailsEnabled);
        assertFalse(isMobilesEnabled);

        // Case 1.1: When mobile verification on update is disabled, verified list claim is optional.
        Map<String, String> mobileClaimProps = new HashMap<>();
        mobileClaimProps.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());
        mobileClaimProps.put(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM, "SUPPORTED");
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN))
                .thenReturn(returnMobileRelatedLocalClaims(mobileClaimProps));

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("true");
        when(identityGovernanceService.getConfiguration(eq(new String[]{IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE}), eq(TENANT_DOMAIN))).thenReturn(new Property[]{
                        buildGovernanceProperty(IdentityRecoveryConstants.ConnectorConfig
                                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, FALSE_STRING)});

        boolean mobilesEnabledWhenVerificationDisabled =
                Utils.isMultiMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(mobilesEnabledWhenVerificationDisabled);

        // Case 1.2: When mobile verification on update is enabled, verified list claim becomes mandatory.
        when(identityGovernanceService.getConfiguration(eq(new String[]{IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE}), eq(TENANT_DOMAIN))).thenReturn(new Property[]{
                        buildGovernanceProperty(IdentityRecoveryConstants.ConnectorConfig
                                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, TRUE_STRING)});

        boolean mobilesEnabledWhenVerificationEnabled =
                Utils.isMultiMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(mobilesEnabledWhenVerificationEnabled);

        // Case 2: When support_multi_emails_and_mobile_numbers_per_user config is true.
        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("true");

        Map<String, String> claimProperties2 = new HashMap<>();
        claimProperties2.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN)).thenReturn(
                returnMultiEmailAndMobileRelatedLocalClaims(claimProperties2));

        isEnabled = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        isEmailsEnabled = Utils.isMultiEmailAddressesPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        isMobilesEnabled = Utils.isMultiMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(isEnabled);
        assertTrue(isEmailsEnabled);
        assertTrue(isMobilesEnabled);

        // Case 3: When support by default is disabled for feature related claims.
        Map<String, String> claimProperties3 = new HashMap<>();
        claimProperties3.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.FALSE.toString());
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN)).thenReturn(
                returnMultiEmailAndMobileRelatedLocalClaims(claimProperties3));

        isEnabled = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        boolean emailsEnabled = Utils.isMultiEmailAddressesPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        boolean mobilesEnabled = Utils.isMultiMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(isEnabled);
        assertFalse(emailsEnabled);
        assertFalse(mobilesEnabled);

        // Case 4: When user store domain is excluded for feature related claims.
        Map<String, String> claimProperties4 = new HashMap<>();
        claimProperties4.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());
        claimProperties4.put(ClaimConstants.EXCLUDED_USER_STORES_PROPERTY, USER_STORE_DOMAIN);
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN)).thenReturn(
                returnMultiEmailAndMobileRelatedLocalClaims(claimProperties4));

        isEnabled = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(isEnabled);

        // Case 5: When ClaimMetadataException is thrown.
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN))
                .thenThrow(new ClaimMetadataException("Test exception"));

        isEnabled = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(isEnabled);
    }

    @Test
    public void testIsMultiEmailsEnabledWhenVerificationDisabled() throws Exception {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("true");

        Property property = new Property();
        property.setName(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        property.setValue(FALSE_STRING);
        when(identityGovernanceService.getConfiguration(eq(new String[]{IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_EMAIL_VERIFICATION_ON_UPDATE}), eq(TENANT_DOMAIN))).thenReturn(new Property[]{property});

        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService()).thenReturn(
                claimMetadataManagementService);

        Map<String, String> claimProps = new HashMap<>();
        claimProps.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());
        LocalClaim emailAddressesClaim = new LocalClaim(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
        emailAddressesClaim.setClaimProperties(claimProps);
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN)).thenReturn(
                Collections.singletonList(emailAddressesClaim));

        boolean emailsEnabled = Utils.isMultiEmailAddressesPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertTrue(emailsEnabled);
    }

    @Test
    public void testIsMultiEmailsDisabledWhenVerificationEnabledAndClaimMissing() throws Exception {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("true");

        Property property = new Property();
        property.setName(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        property.setValue(TRUE_STRING);
        when(identityGovernanceService.getConfiguration(eq(new String[]{IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_EMAIL_VERIFICATION_ON_UPDATE}), eq(TENANT_DOMAIN))).thenReturn(new Property[]{property});

        ClaimMetadataManagementService claimMetadataManagementService = mock(ClaimMetadataManagementService.class);
        when(identityRecoveryServiceDataHolder.getClaimMetadataManagementService()).thenReturn(
                claimMetadataManagementService);

        Map<String, String> claimProps = new HashMap<>();
        claimProps.put(ClaimConstants.SUPPORTED_BY_DEFAULT_PROPERTY, Boolean.TRUE.toString());
        LocalClaim emailAddressesClaim = new LocalClaim(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
        emailAddressesClaim.setClaimProperties(claimProps);
        when(claimMetadataManagementService.getLocalClaims(TENANT_DOMAIN)).thenReturn(
                Collections.singletonList(emailAddressesClaim));

        boolean emailsEnabled = Utils.isMultiEmailAddressesPerUserEnabled(TENANT_DOMAIN, USER_STORE_DOMAIN);
        assertFalse(emailsEnabled);
    }

    @Test(description = "Test getUserClaim() returns the stored claim value if it exists in the user store.")
    public void testGetUserClaimReturnsValue() throws Exception {

        User user = getUser();
        String claimUri = "http://wso2.org/claims/emailaddress";
        String claimValue = "john@example.com";

        Map<String, String> claims = new HashMap<>();
        claims.put(claimUri, claimValue);

        when(userStoreManager.getUserClaimValues(eq(USER_NAME), eq(new String[]{claimUri}), any()))
                .thenReturn(claims);

        String result = Utils.getUserClaim(userStoreManager, user, claimUri);
        assertEquals(result, claimValue);
    }

    @Test(description = "Test getUserClaim() returns null when the requested claim is absent from the user store.")
    public void testGetUserClaimEmptyMapReturnsNull() throws Exception {

        User user = getUser();
        String claimUri = "http://wso2.org/claims/emailaddress";

        when(userStoreManager.getUserClaimValues(eq(USER_NAME), eq(new String[]{claimUri}), any()))
                .thenReturn(emptyMap());

        String result = Utils.getUserClaim(userStoreManager, user, claimUri);
        assertNull(result);
    }

    @Test(description = "Test getUserClaim() throws IdentityEventException when the user‑store lookup fails",
            expectedExceptions = IdentityEventException.class)
    public void testGetUserClaimUserStoreExceptionPropagates() throws Exception {

        User user = getUser();
        String claimUri = "http://wso2.org/claims/emailaddress";

        when(userStoreManager.getUserClaimValues(anyString(), any(), any()))
                .thenThrow(new org.wso2.carbon.user.core.UserStoreException("DB down"));

        Utils.getUserClaim(userStoreManager, user, claimUri);
    }

    @Test
    public void testLoadUserRecoveryDataSuccess() throws Exception {

        String code = "testCode";
        String hashedCode = Utils.hashCode(code);
        UserRecoveryData expectedData = mock(UserRecoveryData.class);
        UserRecoveryDataStore mockStore = mock(UserRecoveryDataStore.class);
        mockedStaticJDBC.when(JDBCRecoveryDataStore::getInstance).thenReturn(mockStore);

        // Define correct behavior explicitly.
        when(mockStore.load(hashedCode)).thenReturn(expectedData);

        UserRecoveryData result = Utils.loadUserRecoveryData(code);

        assertEquals(result, expectedData);
        verify(mockStore).load(hashedCode);
    }

    @Test
    public void testResolveUserFromContext_withUserInstance() {
        FlowExecutionContext context = Mockito.mock(FlowExecutionContext.class);
        User user = new User();
        user.setUserName("testuser");
        user.setTenantDomain("carbon.super");
        Mockito.when(context.getProperty("user")).thenReturn(user);
        User result = Utils.resolveUserFromContext(context);
        assertNotNull(result);
        assertEquals("testuser", result.getUserName());
        assertEquals("carbon.super", result.getTenantDomain());
    }

    @Test
    public void testResolveUserFromContext_withJsonString() throws Exception {
        FlowExecutionContext context = Mockito.mock(FlowExecutionContext.class);
        User user = new User();
        user.setUserName("jsonuser");
        user.setTenantDomain("example.com");
        ObjectMapper mapper = new ObjectMapper();
        String json = mapper.writeValueAsString(user);
        Mockito.when(context.getProperty("user")).thenReturn(json);
        User result = Utils.resolveUserFromContext(context);
        assertNotNull(result);
        assertEquals("jsonuser", result.getUserName());
        assertEquals("example.com", result.getTenantDomain());
    }

    @Test
    public void testResolveUserFromContext_withLinkedHashMap() {
        FlowExecutionContext context = Mockito.mock(FlowExecutionContext.class);
        context.setProperty("USER", "user");
        LinkedHashMap<String, Object> map = new LinkedHashMap<>();
        map.put("userName", "mapuser");
        map.put("tenantDomain", "tenant.org");
        Mockito.when(context.getProperty("user")).thenReturn(map);
        User result = Utils.resolveUserFromContext(context);
        assertNotNull(result);
        assertEquals("mapuser", result.getUserName());
        assertEquals("tenant.org", result.getTenantDomain());
    }

    @Test
    public void testResolveUserFromContext_withInvalidString() {
        FlowExecutionContext context = Mockito.mock(FlowExecutionContext.class);
        Mockito.when(context.getProperty("USER")).thenReturn("not a json");
        User result = Utils.resolveUserFromContext(context);
        assertNull(result);
    }

    @Test
    public void testResolveUserFromContext_withNull() {
        FlowExecutionContext context = Mockito.mock(FlowExecutionContext.class);
        Mockito.when(context.getProperty("USER")).thenReturn(null);
        User result = Utils.resolveUserFromContext(context);
        assertNull(result);
    }


    private static Property buildGovernanceProperty(String name, String value) {

        Property property = new Property();
        property.setName(name);
        property.setValue(value);
        return property;
    }


    private static List<LocalClaim> returnMobileRelatedLocalClaims(Map<String, String> claimProperties) {

        LocalClaim mobileNumbersClaim = new LocalClaim(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
        mobileNumbersClaim.setClaimProperties(claimProperties);
        return Collections.singletonList(mobileNumbersClaim);
    }

    private static List<LocalClaim> returnMultiEmailAndMobileRelatedLocalClaims(Map<String, String> claimProperties) {

        List<LocalClaim> localClaims = new ArrayList<>();

        LocalClaim mobileNumbersClaim = new LocalClaim(IdentityRecoveryConstants.MOBILE_NUMBERS_CLAIM);
        mobileNumbersClaim.setClaimProperties(claimProperties);

        LocalClaim verifiedMobileNumbersClaim = new LocalClaim(IdentityRecoveryConstants.VERIFIED_MOBILE_NUMBERS_CLAIM);
        verifiedMobileNumbersClaim.setClaimProperties(claimProperties);

        LocalClaim emailAddressesClaim = new LocalClaim(IdentityRecoveryConstants.EMAIL_ADDRESSES_CLAIM);
        emailAddressesClaim.setClaimProperties(claimProperties);

        LocalClaim verifiedEmailAddressesClaim =
                new LocalClaim(IdentityRecoveryConstants.VERIFIED_EMAIL_ADDRESSES_CLAIM);
        verifiedEmailAddressesClaim.setClaimProperties(claimProperties);

        localClaims.add(verifiedMobileNumbersClaim);
        localClaims.add(mobileNumbersClaim);
        localClaims.add(emailAddressesClaim);
        localClaims.add(verifiedEmailAddressesClaim);

        return localClaims;
    };

    private static User getUser() {

        User user = new User();
        user.setUserName(USER_NAME);
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain(USER_STORE_DOMAIN);
        return user;
    }

    private static void mockIdentityUtilsGetProperty(String key, String value) {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(key)).thenReturn(value);
    }

    private void mockGetRecoveryConfig(String key, String value) throws IdentityGovernanceException {

        Property property = new Property();
        property.setName(key);
        property.setValue(value);
        Property[] properties = new Property[]{property};

        when(identityGovernanceService.getConfiguration(eq(new String[]{key}), eq(TENANT_DOMAIN)))
                .thenReturn(properties);
    }

    private static String getUserStoreQualifiedUsername(String username, String userStoreDomainName) {

        return userStoreDomainName +  UserCoreConstants.DOMAIN_SEPARATOR + username;
    }
}
