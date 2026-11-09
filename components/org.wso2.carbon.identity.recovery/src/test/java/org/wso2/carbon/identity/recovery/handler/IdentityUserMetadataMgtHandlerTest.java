/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
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

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.config.model.SequenceConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.event.bean.ModuleConfiguration;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.governance.service.IdentityDataStoreService;
import org.wso2.carbon.identity.mgt.constants.IdentityMgtConstants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.user.core.UserRealm;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

/**
 * Unit tests for IdentityUserMetadataMgtHandler.
 */
public class IdentityUserMetadataMgtHandlerTest {

    private static final String PRE_SET_USER_CLAIM_VALUES = "PreSetUserClaimValues";
    private static final String TEST_USERNAME = "testUser";
    private static final String TEST_TENANT_DOMAIN = "carbon.super";
    private static final String OTHER_TENANT_DOMAIN = "other.com";
    private static final String PRIMARY_DOMAIN = "PRIMARY";
    private static final String SECONDARY_DOMAIN = "SECONDARY";

    @Mock
    private IdentityDataStoreService identityDataStoreService;
    @Mock
    private RealmService realmService;
    @Mock
    private UserRealm userRealm;
    @Mock
    private UserStoreManager userStoreManager;
    @Mock
    private UserStoreManager secondaryUserStoreManager;
    @Mock
    private PrivilegedCarbonContext privilegedCarbonContext;

    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    private IdentityUserMetadataMgtHandler handler;

    @BeforeClass
    public void init() {

        // PrivilegedCarbonContext needs carbon home (repository/conf/carbon.xml in test resources) to initialise.
        System.setProperty(CarbonBaseConstants.CARBON_HOME,
                IdentityUserMetadataMgtHandlerTest.class.getResource("/").getFile());
    }

    @BeforeMethod
    public void setUp() throws Exception {

        MockitoAnnotations.openMocks(this);
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityDataStoreService(identityDataStoreService);
        IdentityRecoveryServiceDataHolder.getInstance().setRealmService(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getSecondaryUserStoreManager(SECONDARY_DOMAIN)).thenReturn(secondaryUserStoreManager);

        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(privilegedCarbonContext);
        when(privilegedCarbonContext.getTenantDomain()).thenReturn(TEST_TENANT_DOMAIN);

        handler = createHandler(true);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityTenantUtil.close();
        mockedPrivilegedCarbonContext.close();
        IdentityUtil.threadLocalProperties.get().remove(PRE_SET_USER_CLAIM_VALUES);
    }

    @Test(description = "Claims stored in the identity data store are not written to the user store.")
    public void testClaimsManagedInIdentityDataStore() throws Exception {

        mockIdentityDataStoreFilter(true);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(identityDataStoreService).storeInIdentityDataStore(eq(TEST_USERNAME), eq(userStoreManager),
                eq(PRE_SET_USER_CLAIM_VALUES), anyMap());
        verify(userStoreManager, never()).isReadOnly();
        verify(userStoreManager, never()).setUserClaimValues(anyString(), anyMap(), any());
    }

    @Test(description = "Claims left by the identity data store are written to the user store.")
    public void testClaimsManagedInUserStore() throws Exception {

        mockIdentityDataStoreFilter(false);
        Map<String, String> writtenClaims = captureUserStoreWrite(userStoreManager);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
        assertEquals(writtenClaims.keySet(), Collections.singleton(IdentityMgtConstants.LAST_LOGIN_TIME));
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::startTenantFlow, never());
    }

    @Test(description = "Claims of a secondary user store user are written to the secondary user store.")
    public void testClaimsManagedInSecondaryUserStore() throws Exception {

        mockIdentityDataStoreFilter(false);

        handler.handleEvent(createEvent(createUser(SECONDARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(secondaryUserStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
        verify(userStoreManager, never()).setUserClaimValues(anyString(), anyMap(), any());
    }

    @Test(description = "The user store write runs in the tenant of the user.")
    public void testUserStoreWriteInUserTenant() throws Exception {

        mockIdentityDataStoreFilter(false);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, OTHER_TENANT_DOMAIN)));

        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::startTenantFlow, times(1));
        verify(privilegedCarbonContext).setTenantDomain(OTHER_TENANT_DOMAIN, true);
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::endTenantFlow, times(1));
    }

    @Test(description = "Claims are not written to a read only user store.")
    public void testReadOnlyUserStore() throws Exception {

        mockIdentityDataStoreFilter(false);
        when(userStoreManager.isReadOnly()).thenReturn(true);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(userStoreManager, never()).setUserClaimValues(anyString(), anyMap(), any());
    }

    @Test(description = "Claims are not written to the user store when a claim update is already in progress.")
    public void testClaimUpdateInProgress() throws Exception {

        mockIdentityDataStoreFilter(false);
        IdentityUtil.threadLocalProperties.get().put(PRE_SET_USER_CLAIM_VALUES, true);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(userStoreManager, never()).setUserClaimValues(anyString(), anyMap(), any());
    }

    @Test(description = "A user store client error does not fail the event.")
    public void testUserStoreClientException() throws Exception {

        mockIdentityDataStoreFilter(false);
        doThrow(new UserStoreClientException("Not allowed")).when(userStoreManager)
                .setUserClaimValues(anyString(), anyMap(), any());

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
    }

    @Test(description = "A user store error does not fail the event and ends the started tenant flow.")
    public void testUserStoreException() throws Exception {

        mockIdentityDataStoreFilter(false);
        doThrow(new UserStoreException("Undefined attribute")).when(userStoreManager)
                .setUserClaimValues(anyString(), anyMap(), any());

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, OTHER_TENANT_DOMAIN)));

        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::endTenantFlow, times(1));
    }

    @Test(description = "A runtime error from the user store write does not fail the event and ends the started "
            + "tenant flow.")
    public void testRuntimeExceptionFromUserStoreWrite() throws Exception {

        mockIdentityDataStoreFilter(false);
        doThrow(new IllegalStateException("Listener failure")).when(userStoreManager)
                .setUserClaimValues(anyString(), anyMap(), any());

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, OTHER_TENANT_DOMAIN)));

        verify(userStoreManager, times(1)).setUserClaimValues(eq(TEST_USERNAME), anyMap(), eq(null));
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::endTenantFlow, times(1));
    }

    @Test(description = "An identity data store error fails the event as before.",
            expectedExceptions = IdentityEventException.class)
    public void testIdentityDataStoreException() throws Exception {

        when(identityDataStoreService.storeInIdentityDataStore(anyString(), any(), anyString(), anyMap()))
                .thenThrow(new UserStoreException("Database error"));

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));
    }

    @Test(description = "Nothing is stored when the handler is disabled.")
    public void testHandlerDisabled() throws Exception {

        handler = createHandler(false);

        handler.handleEvent(createEvent(createUser(PRIMARY_DOMAIN, TEST_TENANT_DOMAIN)));

        verify(identityDataStoreService, never()).storeInIdentityDataStore(anyString(), any(), anyString(),
                anyMap());
        verify(userStoreManager, never()).setUserClaimValues(anyString(), anyMap(), any());
    }

    private IdentityUserMetadataMgtHandler createHandler(boolean enable) {

        Properties properties = new Properties();
        properties.setProperty("identityUserMetadataMgtHandler.enable", Boolean.toString(enable));
        IdentityUserMetadataMgtHandler metadataMgtHandler = new IdentityUserMetadataMgtHandler();
        metadataMgtHandler.init(new ModuleConfiguration(properties, new ArrayList<>()));
        return metadataMgtHandler;
    }

    /**
     * Mock the identity data store filter. When the claims are managed in the identity data store, the filter
     * removes them from the map. Otherwise, it leaves them in the map.
     */
    private void mockIdentityDataStoreFilter(boolean managedInIdentityDataStore) throws UserStoreException {

        when(identityDataStoreService.storeInIdentityDataStore(anyString(), any(), anyString(), anyMap()))
                .thenAnswer(invocation -> {
                    if (managedInIdentityDataStore) {
                        Map<String, String> claims = invocation.getArgument(3);
                        claims.clear();
                    }
                    return true;
                });
    }

    private Map<String, String> captureUserStoreWrite(UserStoreManager manager) throws UserStoreException {

        Map<String, String> writtenClaims = new HashMap<>();
        doAnswer(invocation -> {
            Map<String, String> claims = invocation.getArgument(1);
            writtenClaims.putAll(claims);
            return null;
        }).when(manager).setUserClaimValues(anyString(), anyMap(), any());
        return writtenClaims;
    }

    private AuthenticatedUser createUser(String userStoreDomain, String tenantDomain) {

        AuthenticatedUser user = new AuthenticatedUser();
        user.setUserName(TEST_USERNAME);
        user.setUserStoreDomain(userStoreDomain);
        user.setTenantDomain(tenantDomain);
        return user;
    }

    private Event createEvent(AuthenticatedUser user) {

        SequenceConfig sequenceConfig = new SequenceConfig();
        sequenceConfig.setAuthenticatedUser(user);
        AuthenticationContext context = new AuthenticationContext();
        context.setSequenceConfig(sequenceConfig);
        Map<String, Object> properties = new HashMap<>();
        properties.put(IdentityEventConstants.EventProperty.CONTEXT, context);
        return new Event(IdentityEventConstants.EventName.AUTHENTICATION_SUCCESS.name(), properties);
    }
}
