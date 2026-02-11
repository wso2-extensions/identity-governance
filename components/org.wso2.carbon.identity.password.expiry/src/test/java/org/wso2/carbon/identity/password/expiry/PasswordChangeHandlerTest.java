/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.password.expiry;

import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.exception.PostAuthenticationFailedException;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.user.api.RealmConfiguration;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit test cases for PasswordChangeHandler.
 */
public class PasswordChangeHandlerTest {

    private static final String TENANT_DOMAIN = "test.com";
    private static final int TENANT_ID = 123;
    private static final String USERNAME = "tom@gmail.com";
    private static final String TENANT_AWARE_USERNAME = "tom@gmail.com@test.com";
    private PasswordChangeHandler passwordChangeHandler;
    @Mock
    private UserStoreManager userStoreManager;
    @Mock
    private UserRealm userRealm;
    @Mock
    private RealmService realmService;
    @Captor
    private ArgumentCaptor<Map<String, String>> claimValueArguementCaptor;

    private MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;
    private MockedStatic<PasswordPolicyUtils> mockedStaticPasswordPolicyUtils;
    private MockedStatic<UserStoreManager> mockedStaticUserStoreManager;
    private MockedStatic<MultitenantUtils> mockedStaticMultiTenantUtils;


    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        passwordChangeHandler = new PasswordChangeHandler();
    }

    @BeforeClass
    public void beforeTest() {

        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedStaticPasswordPolicyUtils = mockStatic(PasswordPolicyUtils.class);
        mockedStaticUserStoreManager = mockStatic(UserStoreManager.class);
        mockedStaticMultiTenantUtils = mockStatic(MultitenantUtils.class);
    }

    @AfterClass
    public void afterTest() {

        mockedStaticIdentityTenantUtil.close();
        mockedStaticPasswordPolicyUtils.close();
        mockedStaticUserStoreManager.close();
        mockedStaticMultiTenantUtils.close();
    }

    @Test
    public void testHandlePostUpdateCredentialByAdminEvent() throws UserStoreException, IdentityEventException,
            PostAuthenticationFailedException {

        when(IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        when(IdentityTenantUtil.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        RealmConfiguration realmConfig = new RealmConfiguration();
        realmConfig.getUserStoreProperties().put("DomainName", TENANT_DOMAIN);
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfig);

        Event event = new Event("POST_UPDATE_CREDENTIAL_BY_ADMIN");
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);

        when(PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        passwordChangeHandler.handleEvent(event);

        verify(userStoreManager, times(1))
                .setUserClaimValues(eq(USERNAME), claimValueArguementCaptor.capture(), isNull(String.class));
        Map<String, String> claims = claimValueArguementCaptor.getValue();
        Assert.assertEquals(claims.size(), 1);
        Assert.assertTrue(claims.containsKey(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM));
    }

    @Test
    public void testHandlePostUpdateCredentialEvent() throws UserStoreException, IdentityEventException,
            PostAuthenticationFailedException {

        when(IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        when(IdentityTenantUtil.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        RealmConfiguration realmConfig = new RealmConfiguration();
        realmConfig.getUserStoreProperties().put("DomainName", "domain");
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfig);

        Event event = new Event("POST_UPDATE_CREDENTIAL");
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);

        when(PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        passwordChangeHandler.handleEvent(event);

        verify(userStoreManager, times(1))
                .setUserClaimValues(eq(USERNAME), claimValueArguementCaptor.capture(), isNull(String.class));
        Map<String, String> claims = claimValueArguementCaptor.getValue();
        Assert.assertEquals(claims.size(), 1);
        Assert.assertTrue(claims.containsKey(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM));
    }

    @Test
    public void testHandleAddUserEvent() throws UserStoreException, IdentityEventException,
            PostAuthenticationFailedException {

        when(IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        when(IdentityTenantUtil.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(TENANT_ID)).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        RealmConfiguration realmConfig = new RealmConfiguration();
        realmConfig.getUserStoreProperties().put("DomainName", "domain");
        when(userStoreManager.getRealmConfiguration()).thenReturn(realmConfig);

        Event event = new Event("POST_UPDATE_CREDENTIAL");
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);

        when(PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        passwordChangeHandler.handleEvent(event);

        verify(userStoreManager, times(1))
                .setUserClaimValues(eq(USERNAME), claimValueArguementCaptor.capture(), isNull(String.class));

        Map<String, String> claims = claimValueArguementCaptor.getValue();
        Assert.assertEquals(claims.size(), 1);
        Assert.assertTrue(claims.containsKey(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM));
    }

    @Test
    public void testHandleEventForPasswordNonExpiredUserWithPasswordGrantType() throws UserStoreException,
            PostAuthenticationFailedException, IdentityEventException {

        Event event = new Event(PasswordPolicyConstants.PASSWORD_GRANT_POST_AUTHENTICATION_EVENT);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);
        event.getEventProperties().put(PasswordPolicyConstants.AUTHENTICATION_STATUS, true);
        when(MultitenantUtils.getTenantAwareUsername(USERNAME)).thenReturn(USERNAME);
        when(PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        Map<String, String> claimValueMap = new HashMap<>();
        String timestamp = String.valueOf(System.currentTimeMillis());
        claimValueMap.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, timestamp);
        String[] claimURIs = new String[]{PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM};
        when(PasswordPolicyUtils.isPasswordExpired(TENANT_DOMAIN, USERNAME)).thenReturn(false);
        when(userStoreManager.getUserClaimValues(USERNAME, claimURIs, null)).thenReturn(claimValueMap);
        passwordChangeHandler.handleEvent(event);

    }

    @Test
    public void testHandleEventForPasswordExpiredUserWithPasswordGrantType()
            throws UserStoreException, PostAuthenticationFailedException {

        Event event = new Event(PasswordPolicyConstants.PASSWORD_GRANT_POST_AUTHENTICATION_EVENT);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_NAME, USERNAME);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TENANT_DOMAIN);
        event.getEventProperties().put(PasswordPolicyConstants.AUTHENTICATION_STATUS, true);
        when(MultitenantUtils.getTenantAwareUsername(USERNAME)).thenReturn(TENANT_AWARE_USERNAME);
        when(PasswordPolicyUtils.isPasswordExpiryEnabled(TENANT_DOMAIN)).thenReturn(true);
        Map<String, String> claimValueMap = new HashMap<>();
        claimValueMap.put(PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, "1672559229000");
        String[] claimURIs = new String[]{PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM};
        when(PasswordPolicyUtils.isPasswordExpired(TENANT_DOMAIN, USERNAME)).thenReturn(true);

        when(userStoreManager.getUserClaimValues(USERNAME, claimURIs, null)).thenReturn(claimValueMap);
        try {
            passwordChangeHandler.handleEvent(event);
            Assert.fail("This should throw identity event exception");
        } catch (IdentityEventException e) {
            Assert.assertEquals(PasswordPolicyConstants.PASSWORD_EXPIRED_ERROR_MESSAGE, e.getMessage());
        }
    }

    @Test
    public void testGetName() {

        Assert.assertEquals(passwordChangeHandler.getName(),
                PasswordPolicyConstants.PASSWORD_CHANGE_EVENT_HANDLER_NAME);
    }
}
