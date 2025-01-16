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

package org.wso2.carbon.identity.password.expiry.listener;

import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.exceptions.ExpiredPasswordIdentificationException;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.common.User;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.model.UserClaimSearchEntry;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Unit test cases for PasswordExpiryEventListener.
 */
@WithCarbonHome
public class PasswordExpiryEventListenerTest {

    private static final String TENANT_DOMAIN = "test.com";
    private PasswordExpiryEventListener passwordExpiryEventListener;

    @Mock
    PrivilegedCarbonContext privilegedCarbonContext;
    @Mock
    AbstractUserStoreManager userStoreManager;
    @Mock
    User user;

    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    private MockedStatic<PasswordPolicyUtils> mockedPasswordPolicyUtils;

    @BeforeMethod
    public void setUp() throws UserStoreException {

        MockitoAnnotations.openMocks(this);
        passwordExpiryEventListener = new PasswordExpiryEventListener();

        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);

        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(privilegedCarbonContext.getUsername()).thenReturn("USERNAME");

        when(userStoreManager.getUser(any(), anyString())).thenReturn(user);
    }

    @BeforeClass
    public void init() {

        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedPasswordPolicyUtils = mockStatic(PasswordPolicyUtils.class);
    }

    @AfterClass
    public void close() {

        mockedPrivilegedCarbonContext.close();
        mockedPasswordPolicyUtils.close();
    }

    @Test
    public void testGetExecutionOrderId() {

        Assert.assertEquals(passwordExpiryEventListener.getExecutionOrderId(), 102);
    }

    @Test
    public void testDoPostGetUserClaimValuesWithPasswordExpiryClaim() throws UserStoreException {

        String username = "testUser";
        String[] claims;
        Map<String, String> claimMap = new HashMap<>();
        String profileName = "default";

        when(user.getDomainQualifiedUsername()).thenReturn(username);

        // Case 1: When claims contains PASSWORD_EXPIRY_TIME_CLAIM.
        claims = new String[]{PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM};

        mockedPasswordPolicyUtils.when(() -> PasswordPolicyUtils.getUserPasswordExpiryTime(
                eq(TENANT_DOMAIN), eq(username))).thenReturn(Optional.of(1000L));

        passwordExpiryEventListener.doPostGetUserClaimValues(username, claims, profileName, claimMap, userStoreManager);
        Assert.assertNotNull(claimMap.get(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM));

        // Case 2: PostAuthenticationFailedException is thrown.
        mockedPasswordPolicyUtils.when(() ->
                PasswordPolicyUtils.getUserPasswordExpiryTime(eq(TENANT_DOMAIN), eq(username)))
                .thenThrow(new ExpiredPasswordIdentificationException("test-error", "test-error"));
        try {
            passwordExpiryEventListener.doPostGetUserClaimValues(username, claims, profileName, claimMap, userStoreManager);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof UserStoreException);
        }
    }

    @Test
    public void testDoPostGetUserClaimValuesWithoutPasswordExpiryClaim() throws UserStoreException {

        String username = "testUser";
        String[] claims;
        Map<String, String> claimMap = new HashMap<>();
        String profileName = "default";
        claims = new String[]{"claim1", "claim2"};

        when(user.getDomainQualifiedUsername()).thenReturn(username);

        passwordExpiryEventListener.doPostGetUserClaimValues(username, claims, profileName, claimMap, userStoreManager);
        Assert.assertFalse(claimMap.containsKey(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM));
    }

    @Test
    public void testDoPostGetUserClaimValuesInAuthenticationFlow() throws UserStoreException {

        String username = "testUser";
        String[] claims;
        Map<String, String> claimMap = new HashMap<>();
        String profileName = "default";
        claims = new String[]{"claim1", "claim2"};

        when(privilegedCarbonContext.getUsername()).thenReturn(null);

        passwordExpiryEventListener.doPostGetUserClaimValues(username, claims, profileName, claimMap, userStoreManager);
        Assert.assertFalse(claimMap.containsKey(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM));
    }

    @Test
    public void testDoPostGetUsersClaimValuesWithPasswordExpiryClaim() throws UserStoreException {

        String[] userNames = new String[]{"testUser1", "testUser2"};
        String[] claims = new String[]{PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM};
        String profileName = "default";

        UserClaimSearchEntry[] userClaimSearchEntries = new UserClaimSearchEntry[2];
        userClaimSearchEntries[0] = new UserClaimSearchEntry();
        userClaimSearchEntries[0].setUserName("testUser1");
        userClaimSearchEntries[1] = new UserClaimSearchEntry();
        userClaimSearchEntries[1].setUserName("testUser1");

        mockedPasswordPolicyUtils.when(() ->
                        PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        mockedPasswordPolicyUtils.when(() ->
                        PasswordPolicyUtils.isSkipIfNoApplicableRulesEnabled(TENANT_DOMAIN)).thenReturn(false);
        mockedPasswordPolicyUtils.when(() ->
                PasswordPolicyUtils.getPasswordExpiryInDays(TENANT_DOMAIN)).thenReturn(30);
        mockedPasswordPolicyUtils.when(() ->
                PasswordPolicyUtils.getPasswordExpiryRules(TENANT_DOMAIN)).thenReturn(Collections.emptyList());
        mockedPasswordPolicyUtils.when(() -> PasswordPolicyUtils.getUserPasswordExpiryTime(
                eq(TENANT_DOMAIN), anyString(), eq(true), eq(false), any(), eq(30)))
                .thenReturn(Optional.of(1000L));

        passwordExpiryEventListener.doPostGetUsersClaimValues(userNames, claims, profileName, userClaimSearchEntries);
        Assert.assertNotNull(
                userClaimSearchEntries[0].getClaims().get(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM));
        Assert.assertNotNull(
                userClaimSearchEntries[1].getClaims().get(PasswordPolicyConstants.PASSWORD_EXPIRY_TIME_CLAIM));

        // Case 2: PostAuthenticationFailedException is thrown.
        mockedPasswordPolicyUtils.when(() -> PasswordPolicyUtils.getUserPasswordExpiryTime(
                eq(TENANT_DOMAIN), anyString(), eq(true), eq(false), any(), eq(30)))
                .thenThrow(new ExpiredPasswordIdentificationException("test-error", "test-error"));
        try {
            passwordExpiryEventListener.doPostGetUsersClaimValues(userNames, claims,
                    profileName, userClaimSearchEntries);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof UserStoreException);
        }
    }

    @Test
    public void testDoPostGetUsersClaimValuesWithoutPasswordExpiryClaims() throws UserStoreException {

        String[] userNames = new String[]{"testUser1", "testUser2"};
        String[] claims = new String[]{"claim1", "claim2"};
        String profileName = "default";

        UserClaimSearchEntry[] userClaimSearchEntries = new UserClaimSearchEntry[2];
        userClaimSearchEntries[0] = new UserClaimSearchEntry();
        userClaimSearchEntries[0].setUserName("testUser1");
        userClaimSearchEntries[1] = new UserClaimSearchEntry();
        userClaimSearchEntries[1].setUserName("testUser1");

        passwordExpiryEventListener.doPostGetUsersClaimValues(userNames, claims, profileName, userClaimSearchEntries);
        Assert.assertNull(userClaimSearchEntries[0].getClaims());
        Assert.assertNull(userClaimSearchEntries[1].getClaims());
    }
}
