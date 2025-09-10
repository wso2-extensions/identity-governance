/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.recovery.executor;

import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.common.testng.WithCarbonHome;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineException;
import org.wso2.carbon.identity.flow.execution.engine.model.ExecutorResponse;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowUser;
import org.wso2.carbon.identity.flow.execution.engine.util.FlowExecutionEngineUtils;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.user.api.UserRealm;
import org.wso2.carbon.user.core.UserCoreConstants;
import org.wso2.carbon.user.core.UserStoreClientException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.wso2.carbon.identity.application.authentication.framework.util.FrameworkConstants.EMAIL_ADDRESS_CLAIM;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_COMPLETE;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.ExecutorStatus.STATUS_USER_ERROR;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.PASSWORD_KEY;
import static org.wso2.carbon.identity.flow.execution.engine.Constants.USERNAME_CLAIM_URI;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.REGISTRATION;

/**
 * Unit tests for {@link UserProvisioningExecutor}.
 */
@WithCarbonHome
public class UserProvisioningExecutorTest {

    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USERNAME = "testuser";
    private static final String EMAIL = "testuser@example.com";
    private static final String PASSWORD = "Password123!";
    private static final String USER_ID = "abc123";
    private static final String CONTEXT_ID = "context-123";
    private static final String PRIMARY_DOMAIN = "PRIMARY";
    private static final String SECONDARY_DOMAIN = "SECONDARY";
    private static final String WSO2_CLAIM_DIALECT = "http://wso2.org/claims/";
    private static final int TENANT_ID = 1234;

    private UserProvisioningExecutor executor;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedDataHolder;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<FlowExecutionEngineUtils> mockedFlowEngineUtils;
    private MockedStatic<UserCoreUtil> mockedUserCoreUtil;

    @BeforeMethod
    public void setUp() {

        executor = new UserProvisioningExecutor();
        mockedDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedFlowEngineUtils = mockStatic(FlowExecutionEngineUtils.class);
        mockedUserCoreUtil = mockStatic(UserCoreUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedDataHolder.close();
        mockedIdentityTenantUtil.close();
        mockedIdentityUtil.close();
        mockedFlowEngineUtils.close();
        mockedUserCoreUtil.close();
    }

    @Test
    public void testGetName() {

        assertEquals(executor.getName(), "UserProvisioningExecutor");
    }

    @Test
    public void testGetInitiationData() {

        List<String> initiationData = executor.getInitiationData();
        assertNull(initiationData);
    }

    @Test
    public void testRollback() throws FlowEngineException {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        ExecutorResponse response = executor.rollback(context);
        assertNull(response);
    }

    @Test
    public void testExecuteWithRegistrationFlow() throws Exception {

        // Setup context and user
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(WSO2_CLAIM_DIALECT + "givenname", "John");
        userInputData.put(WSO2_CLAIM_DIALECT + "lastname", "Doe");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup mocks
        setupUserStoreManagerMocks();

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(flowUser).setUserStoreDomain(PRIMARY_DOMAIN);
        verify(flowUser).setUserId(USER_ID);
    }

    @Test
    public void testExecuteWithNonRegistrationFlow() throws Exception {

        // Setup context and user
        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        Map<String, String> userInputData = new HashMap<>();
        userInputData.put(WSO2_CLAIM_DIALECT + "givenname", "John");

        when(context.getFlowType()).thenReturn("INVITED_USER_REGISTRATION");
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(userInputData);
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup mocks
        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(userStoreManager).setUserClaimValues(eq(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME),
                any(Map.class), isNull());
    }

    @Test
    public void testExecuteWithUsernamePatternValidationSkipped() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(true);

        setupUserStoreManagerMocks();

        executor.execute(context);

        mockedUserCoreUtil.verify(() -> UserCoreUtil.setSkipUsernamePatternValidationThreadLocal(true));
    }

    @Test
    public void testExecuteWithSecondaryUserStore() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        String secondaryUsername = SECONDARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME;
        FlowUser flowUser = createTestFlowUser(secondaryUsername);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup mocks for secondary user store
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager primaryUserStoreManager = mock(AbstractUserStoreManager.class);
        AbstractUserStoreManager secondaryUserStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(primaryUserStoreManager);
        when(primaryUserStoreManager.getSecondaryUserStoreManager(SECONDARY_DOMAIN))
                .thenReturn(secondaryUserStoreManager);
        when(secondaryUserStoreManager.getUserIDFromUserName(USERNAME)).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
    }

    @Test
    public void testExecuteWithUsernameFromEmailClaim() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.addClaim(EMAIL_ADDRESS_CLAIM, EMAIL);

        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());
        flowUser.setUserCredentials(credentials);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        setupUserStoreManagerMocks();
        mockedFlowEngineUtils.when(() -> FlowExecutionEngineUtils.isEmailUsernameValidator(TENANT_DOMAIN))
                .thenReturn(true);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        assertEquals(flowUser.getUsername(), EMAIL);
    }

    @Test
    public void testExecuteWithRandomUsernameGeneration() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();

        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());
        flowUser.setUserCredentials(credentials);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        setupUserStoreManagerMocks();
        mockedFlowEngineUtils.when(() -> FlowExecutionEngineUtils.isEmailUsernameValidator(TENANT_DOMAIN))
                .thenReturn(false);
        mockedIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(false);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        assertNotNull(flowUser.getUsername());
        // Verify that the username pattern validation is skipped for random username.
        mockedUserCoreUtil.verify(() -> UserCoreUtil.setSkipUsernamePatternValidationThreadLocal(true));
    }

    @Test
    public void testExecuteWithUsernameAlreadyExists() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        Throwable cause  = new UserActionExecutionClientException("USER-ACTION-PRE-UPDATE-PASSWORD-60001",
                "User already exists with user name: " + USERNAME,
                "User name already exists");
        doThrow(new UserStoreClientException("Username already exists", "USER-ACTION-PRE-UPDATE-PASSWORD-60001", cause))
                .when(userStoreManager).addUser(anyString(), anyString(), isNull(), any(Map.class), isNull());

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_USER_ERROR);
        assertNotNull(response.getErrorCode());
    }

    @Test
    public void testExecuteWithPreUpdatePasswordActionFailure() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        UserStoreClientException clientException = new UserStoreClientException(
                "PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED",
                "Action failed");
        doThrow(clientException).when(userStoreManager).addUser(anyString(), anyString(),
                isNull(), any(Map.class), isNull());

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_ERROR);
        assertNotNull(response.getErrorCode());
    }

    @Test
    public void testExecuteWithFederatedAssociations() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);
        Map<String, String> federatedAssociations = new HashMap<>();
        federatedAssociations.put("testIdP", "testSubjectId");
        when(flowUser.getFederatedAssociations()).thenReturn(federatedAssociations);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup complete mocks including federated association manager
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);
        FederatedAssociationManager fedAssociationManager = mock(FederatedAssociationManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(dataHolder.getFederatedAssociationManager()).thenReturn(fedAssociationManager);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getUserIDFromUserName(anyString())).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(fedAssociationManager).createFederatedAssociation(any(User.class), eq("testIdP"), eq("testSubjectId"));
    }

    @Test
    public void testExecuteWithEmptyFederatedAssociations() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);
        Map<String, String> federatedAssociations = new HashMap<>();
        federatedAssociations.put("", ""); // Empty associations should be skipped
        when(flowUser.getFederatedAssociations()).thenReturn(federatedAssociations);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup complete mocks including federated association manager
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);
        FederatedAssociationManager fedAssociationManager = mock(FederatedAssociationManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(dataHolder.getFederatedAssociationManager()).thenReturn(fedAssociationManager);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getUserIDFromUserName(anyString())).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(fedAssociationManager, never()).createFederatedAssociation(any(User.class), anyString(), anyString());
    }

    @Test
    public void testResolveUserStoreDomainWithInternalDomain() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername("Internal/testuser");

        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());
        flowUser.setUserCredentials(credentials);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Setup mocks for internal domain
        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager primaryUserStoreManager = mock(AbstractUserStoreManager.class);
        AbstractUserStoreManager internalUserStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(primaryUserStoreManager);
        when(primaryUserStoreManager.getSecondaryUserStoreManager("Internal"))
                .thenReturn(internalUserStoreManager);
        when(internalUserStoreManager.getUserIDFromUserName("testuser")).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn("Internal/testuser");

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(internalUserStoreManager).addUser(eq("Internal/testuser"), anyString(),
                isNull(), any(Map.class), isNull());
    }

    private FlowUser createTestFlowUser(String username) {

        FlowUser flowUser = mock(FlowUser.class);
        Map<String, String> claims = new HashMap<>();
        claims.put(USERNAME_CLAIM_URI, USERNAME);
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());
        Map<String, String> federatedAssociations = new HashMap<>();

        when(flowUser.getUsername()).thenReturn(username);
        when(flowUser.getClaims()).thenReturn(claims);
        when(flowUser.getClaim(USERNAME_CLAIM_URI)).thenReturn(USERNAME);
        when(flowUser.getClaim(EMAIL_ADDRESS_CLAIM)).thenReturn(null);
        when(flowUser.getUserCredentials()).thenReturn(credentials);
        when(flowUser.getFederatedAssociations()).thenReturn(federatedAssociations);

        return flowUser;
    }

    private AbstractUserStoreManager setupUserStoreManagerMocks() throws Exception {

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(dataHolder.getFederatedAssociationManager()).thenReturn(mock(FederatedAssociationManager.class));
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getUserIDFromUserName(anyString())).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);

        return userStoreManager;
    }
}
