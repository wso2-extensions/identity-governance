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

import org.wso2.carbon.identity.application.authentication.framework.internal.FrameworkServiceDataHolder;
import org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants;
import org.wso2.carbon.identity.password.expiry.internal.EnforcePasswordResetComponentDataHolder;
import org.wso2.carbon.identity.password.expiry.util.PasswordPolicyUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.config.model.SequenceConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.handler.request.PostAuthnHandlerFlowStatus;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedIdPData;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.utils.multitenancy.MultitenantUtils;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.AUTHENTICATOR_TYPE;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM;
import static org.wso2.carbon.identity.password.expiry.constants.PasswordPolicyConstants.LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Unit test cases for PasswordResetEnforcerHandler.
 */
public class PasswordResetEnforcerHandlerTest {

    private static final String TENANT_DOMAIN = "test.com";
    private static final int TENANT_ID = 123;
    private static final String USERNAME = "tom@gmail.com";
    private static final String TENANT_AWARE_USERNAME = "tom@gmail.com@test.com";
    private static final String USER_STORE_DOMAIN = "DEFAULT";
    private EnforcePasswordResetAuthenticationHandler enforcePasswordResetAuthenticationHandler;
    @Mock
    private AuthenticationContext authenticationContext;

    @Mock
    private UserStoreManager userStoreManager;

    @Mock
    private org.wso2.carbon.user.core.UserRealm userRealm;

    @Mock
    private RealmService realmService;

    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private HttpServletRequest httpServletRequest;

    @Mock
    private HttpServletResponse httpServletResponse;

    @Mock
    private Enumeration<String> requestHeaders;

    @Mock
    private FrameworkServiceDataHolder frameworkServiceDataHolder;

    private MockedStatic<PasswordPolicyUtils> mockedStaticPasswordPolicyUtils;
    private MockedStatic<MultitenantUtils> mockedStaticMultiTenantUtils;
    private MockedStatic<JDBCRecoveryDataStore> mockedStaticJDBCRecoveryDataStore;
    private MockedStatic<FrameworkUtils> mockedStaticFrameworkUtils;
    private MockedStatic<IdentityTenantUtil> mockedStaticIdentityTenantUtil;
    private MockedStatic<FrameworkServiceDataHolder> mockedStaticFrameworkServiceDataHolder;

    @BeforeClass
    public void beforeTest() {

        mockedStaticPasswordPolicyUtils = mockStatic(PasswordPolicyUtils.class);
        mockedStaticJDBCRecoveryDataStore = mockStatic(JDBCRecoveryDataStore.class);
        mockedStaticMultiTenantUtils = mockStatic(MultitenantUtils.class);
        mockedStaticFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedStaticIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedStaticFrameworkServiceDataHolder = mockStatic(FrameworkServiceDataHolder.class);
    }

    @AfterClass
    public void afterTest() {

        mockedStaticPasswordPolicyUtils.close();
        mockedStaticJDBCRecoveryDataStore.close();
        mockedStaticMultiTenantUtils.close();
        mockedStaticFrameworkUtils.close();
        mockedStaticIdentityTenantUtil.close();
        mockedStaticFrameworkServiceDataHolder.close();
    }

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        enforcePasswordResetAuthenticationHandler = new EnforcePasswordResetAuthenticationHandler();
        when(httpServletRequest.getHeaderNames()).thenReturn(requestHeaders);
        EnforcePasswordResetComponentDataHolder.getInstance().setRealmService(realmService);
    }

    @Test
    public void testGetFriendlyName() {

        Assert.assertEquals(enforcePasswordResetAuthenticationHandler.getName(),
                PasswordPolicyConstants.ENFORCE_PASSWORD_RESET_HANDLER);
    }

    @Test
    public void testHandle() throws Exception {

        AuthenticatedIdPData authenticatedIdPData = getAuthenticatedIdPData();
        Map<String, AuthenticatedIdPData> idPs = new HashMap<>();
        when(authenticationContext.getCurrentAuthenticatedIdPs()).thenReturn(idPs);
        idPs.put(AUTHENTICATOR_TYPE, authenticatedIdPData);

        // Case 1 : Password expiry is not enabled.
        when(PasswordPolicyUtils.isPasswordExpiryEnabled(anyString())).thenReturn(false);
        PostAuthnHandlerFlowStatus flowStatus1 = enforcePasswordResetAuthenticationHandler.handle(httpServletRequest,
                httpServletResponse, authenticationContext);
        Assert.assertEquals(flowStatus1, PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED);

        // Case 2 : Password expiry is enabled.
        List<AuthenticatorConfig> authenticators = getAuthenticatorConfigs();
        when(PasswordPolicyUtils.isPasswordExpiryEnabled(anyString())).thenReturn(true);
        when(PasswordPolicyUtils.isPasswordExpired(anyString(), anyString())).thenReturn(true);

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);

        String[] claimURIs
                = new String[]{LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM_NON_IDENTITY};
        Map<String, String> claimValueMap = new HashMap<>();
        claimValueMap.put
                (LAST_CREDENTIAL_UPDATE_TIMESTAMP_CLAIM, "1677845659");

        when(userStoreManager.getUserClaimValues(USERNAME, claimURIs, null))
                .thenReturn(claimValueMap);

        doReturn(authenticators).when(idPs.get(AUTHENTICATOR_TYPE)).getAuthenticators();
        when(JDBCRecoveryDataStore.getInstance()).thenReturn(userRecoveryDataStore);
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);

        PostAuthnHandlerFlowStatus flowStatus = enforcePasswordResetAuthenticationHandler.handle(httpServletRequest,
                httpServletResponse, authenticationContext);
        Assert.assertEquals(flowStatus, PostAuthnHandlerFlowStatus.INCOMPLETE);
        verify(httpServletResponse).sendRedirect(captor.capture());

        // Case 3 : Password expiry is enabled and password is not expired.
        when(PasswordPolicyUtils.isPasswordExpired(anyString(), anyString())).thenReturn(false);
        PostAuthnHandlerFlowStatus flowStatus2 = enforcePasswordResetAuthenticationHandler.handle(httpServletRequest,
                httpServletResponse, authenticationContext);
        Assert.assertEquals(flowStatus2, PostAuthnHandlerFlowStatus.SUCCESS_COMPLETED);
    }

    private static List<AuthenticatorConfig> getAuthenticatorConfigs() {

        List<AuthenticatorConfig> authenticators = new ArrayList<>();
        AuthenticatorConfig authenticatorConfig = new AuthenticatorConfig();
        authenticatorConfig.setName("BasicAuthenticator");
        authenticators.add(authenticatorConfig);
        return authenticators;
    }

    private AuthenticatedIdPData getAuthenticatedIdPData() {

        SequenceConfig sequenceConfig = mock(SequenceConfig.class);
        AuthenticatedUser authenticatedUser = mock(AuthenticatedUser.class);
        AuthenticatedIdPData authenticatedIdPData = mock(AuthenticatedIdPData.class);

        when(authenticationContext.getSequenceConfig()).thenReturn(sequenceConfig);
        when(sequenceConfig.getAuthenticatedUser()).thenReturn(authenticatedUser);
        when(FrameworkServiceDataHolder.getInstance()).thenReturn(frameworkServiceDataHolder);
        when(frameworkServiceDataHolder.getRealmService()).thenReturn(realmService);
        when(authenticatedUser.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(authenticatedUser.getUserName()).thenReturn(USERNAME);
        when(authenticatedUser.getUserStoreDomain()).thenReturn(USER_STORE_DOMAIN);
        when(authenticatedUser.isFederatedUser()).thenReturn(false);
        when(authenticatedUser.toFullQualifiedUsername()).thenReturn(TENANT_AWARE_USERNAME);

        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(TENANT_ID);
        when(MultitenantUtils.getTenantAwareUsername(anyString())).thenReturn(TENANT_AWARE_USERNAME);
        return authenticatedIdPData;
    }
}
