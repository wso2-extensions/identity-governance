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

package org.wso2.carbon.identity.recovery.util;

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
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.handler.event.account.lock.exception.AccountLockServiceException;
import org.wso2.carbon.identity.handler.event.account.lock.service.AccountLockService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static junit.framework.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

public class UtilsTest {

    @Mock
    private UserStoreManager userStoreManager;
    @Mock
    private UserRealm userRealm;
    @Mock
    private RealmService realmService;
    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;
    @Mock
    private IdentityGovernanceService identityGovernanceService;
    @Mock
    private AccountLockService accountLockService;

    private static MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;
    private static MockedStatic<UserStoreManager> mockedStaticUserStoreManager;
    private static MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private static MockedStatic<IdentityUtil> mockedStaticIdentityUtil;
    private static MockedStatic<FrameworkUtils> mockedStaticFrameworkUtils;
    private static MockedStatic<MultitenantUtils> mockedStaticMultiTenantUtils;

    private static final String TENANT_DOMAIN = "test.com";
    private static final int TENANT_ID = 123;
    private static final String USER_NAME = "testUser";
    private static final String USER_STORE_DOMAIN = "TEST";

    @BeforeClass
    public static void beforeClass() {

        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedStaticUserStoreManager = mockStatic(UserStoreManager.class);
        mockedIdentityRecoveryServiceDataHolder = Mockito.mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedStaticIdentityUtil = mockStatic(IdentityUtil.class);
        mockedStaticFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedStaticMultiTenantUtils = mockStatic(MultitenantUtils.class);
    }

    @AfterClass
    public static void afterClass() {

        mockedStaticIdentityTenantUtil.close();
        mockedStaticUserStoreManager.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedStaticIdentityUtil.close();
        mockedStaticFrameworkUtils.close();
        mockedStaticMultiTenantUtils.close();
    }

    @BeforeMethod
    public void setUp() throws org.wso2.carbon.user.api.UserStoreException {

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
        when(identityRecoveryServiceDataHolder.getAccountLockService()).thenReturn(accountLockService);

        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
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
        org.wso2.carbon.identity.recovery.model.Property[] propertiesNoAccessUrl = new org.wso2.carbon.identity.recovery.model.Property[]{
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
    public void testGetMultiValuedClaim() throws IdentityEventException, org.wso2.carbon.user.core.UserStoreException {

        User user = getUser();
        String claimValue = "value1,value2,value3";
        List<String> expectedClaimList = Arrays.asList("value1", "value2", "value3");
        when(userStoreManager.getUserClaimValue(any(), anyString(), any()))
                .thenReturn(claimValue);

        List<String> result = Utils.getMultiValuedClaim(userStoreManager, user, "testClaim");
        assertEquals(expectedClaimList, result);
    }

    @Test
    public void testIsMultiEmailsAndMobileNumbersPerUserEnabled() {

        mockedStaticIdentityUtil.when(() -> IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig
                        .SUPPORT_MULTI_EMAILS_AND_MOBILE_NUMBERS_PER_USER))
                .thenReturn("true");
        boolean result = Utils.isMultiEmailsAndMobileNumbersPerUserEnabled();
        assertEquals(result, true);
    }

    private static User getUser() {

        User user = new User();
        user.setUserName(USER_NAME);
        user.setTenantDomain(TENANT_DOMAIN);
        user.setUserStoreDomain(USER_STORE_DOMAIN);
        return user;
    }

    private static String getUserStoreQualifiedUsername(String username, String userStoreDomainName) {

        return userStoreDomainName +  UserCoreConstants.DOMAIN_SEPARATOR + username;
    }
}
