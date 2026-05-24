/*
 * Copyright (c) 2025, WSO2 LLC. (https://www.wso2.com).
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

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.PIICategory;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.consent.mgt.core.util.ConsentReceiptUtils;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.central.log.mgt.utils.LoggerUtils;
import org.wso2.carbon.identity.application.common.IdentityApplicationManagementException;
import org.wso2.carbon.identity.application.common.model.ApplicationBasicInfo;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
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
import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.common.AbstractUserStoreManager;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.util.UserCoreUtil;

import java.util.ArrayList;
import java.util.Collections;
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
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.PASSWORD_RECOVERY;
import static org.wso2.carbon.identity.flow.mgt.Constants.FlowTypes.REGISTRATION;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.DISPLAY_CLAIM_AVAILABILITY_CONFIG;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_INVALID_USERNAME;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USERNAME_ALREADY_EXISTS;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_EXISTENCE_CHECK_FAILURE;
import static org.wso2.carbon.identity.recovery.executor.ExecutorConstants.ExecutorErrorMessages.ERROR_CODE_USER_PROVISIONING_FAILURE;

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
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;
    private MockedStatic<PrivilegedCarbonContext> mockedPrivilegedCarbonContext;
    private MockedStatic<ConsentReceiptUtils> mockedConsentReceiptUtils;
    private MockedStatic<LoggerUtils> mockedLoggerUtils;

    @Mock
    private ApplicationManagementService applicationManagementService;

    @BeforeMethod
    public void setUp() throws IdentityApplicationManagementException {

        executor = new UserProvisioningExecutor();
        mockedDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedIdentityTenantUtil = mockStatic(IdentityTenantUtil.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedFlowEngineUtils = mockStatic(FlowExecutionEngineUtils.class);
        mockedUserCoreUtil = mockStatic(UserCoreUtil.class);
        mockedFrameworkUtils = mockStatic(FrameworkUtils.class);
        mockedPrivilegedCarbonContext = mockStatic(PrivilegedCarbonContext.class);
        mockedConsentReceiptUtils = mockStatic(ConsentReceiptUtils.class);
        mockedLoggerUtils = mockStatic(LoggerUtils.class);
        mockedLoggerUtils.when(LoggerUtils::isDiagnosticLogsEnabled).thenReturn(false);
        applicationManagementService = mock(ApplicationManagementService.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedDataHolder.close();
        mockedIdentityTenantUtil.close();
        mockedIdentityUtil.close();
        mockedFlowEngineUtils.close();
        mockedUserCoreUtil.close();
        mockedFrameworkUtils.close();
        mockedPrivilegedCarbonContext.close();
        mockedConsentReceiptUtils.close();
        mockedLoggerUtils.close();
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
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");
        testApp.setAccessUrl("https://myapp.com");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(testApp);

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
    public void testExecuteWithUsernameAlreadyExistsWithoutDisplayClaim() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);
        mockedIdentityTenantUtil.when(() ->
                IdentityUtil.getProperty(DISPLAY_CLAIM_AVAILABILITY_CONFIG)).thenReturn("false");

        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        doThrow(new UserStoreException("30004 - UserAlreadyExistingUsername testUser already exists in the system. " +
                "Please pick another username."))
                .when(userStoreManager).addUser(anyString(), anyString(), any(), any(Map.class), isNull());

        ExecutorResponse response = executor.execute(context);
        assertEquals(response.getResult(), STATUS_USER_ERROR);
        assertEquals(response.getErrorCode(), ERROR_CODE_USER_PROVISIONING_FAILURE.getCode());
    }

    @Test
    public void testExecuteWithUsernameAlreadyExistsWithDisplayClaim() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);
        mockedIdentityTenantUtil.when(() ->
                IdentityUtil.getProperty(DISPLAY_CLAIM_AVAILABILITY_CONFIG)).thenReturn("true");

        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        doThrow(new UserStoreException("30004 - UserAlreadyExistingUsername testUser already exists in the system. " +
                "Please pick another username."))
                .when(userStoreManager).addUser(anyString(), anyString(), any(), any(Map.class), isNull());

        ExecutorResponse response = executor.execute(context);
        assertEquals(response.getResult(), STATUS_USER_ERROR);
        assertEquals(response.getErrorCode(), ERROR_CODE_USERNAME_ALREADY_EXISTS.getCode());
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
        UserActionExecutionClientException clientException = new UserActionExecutionClientException(
                "USER-ACTION-PRE-UPDATE-PASSWORD-60001",
                "PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED",
                "Action failed");
        doThrow(clientException).when(userStoreManager).addUser(anyString(), anyString(),
                any(), any(Map.class), isNull());

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_USER_ERROR);
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
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");
        testApp.setAccessUrl("https://myapp.com");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(testApp);

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
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");
        testApp.setAccessUrl("https://myapp.com");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(testApp);

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
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");
        testApp.setAccessUrl("https://myapp.com");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(testApp);

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
                any(), any(Map.class), any());
    }

    @Test
    public void testExecutePasswordRecoveryWithNonEmailUsernameSkipsValidationForExistingUser() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(PASSWORD_RECOVERY.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Email username validation is enabled, but the username has no "@".
        // Since the user already exists the validation should be skipped.
        mockedIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(true);
        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        when(userStoreManager.isExistingUser(USERNAME)).thenReturn(true);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
    }

    @Test
    public void testExecuteWithNonEmailUsernameFailsValidationForNonExistingUser() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(PASSWORD_RECOVERY.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        // Email username validation is enabled, the username has no "@", and the user does not exist.
        // Validation should fire and return a user error.
        mockedIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(true);
        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        when(userStoreManager.isExistingUser(USERNAME)).thenReturn(false);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_USER_ERROR);
        assertEquals(response.getErrorCode(), ERROR_CODE_INVALID_USERNAME.getCode());
    }

    @Test
    public void testIsExistingUserWithUserStoreDomainSet() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);
        when(flowUser.getUserStoreDomain()).thenReturn(SECONDARY_DOMAIN);

        when(context.getFlowType()).thenReturn(PASSWORD_RECOVERY.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(true);

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager primaryStoreManager = mock(AbstractUserStoreManager.class);
        AbstractUserStoreManager secondaryStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(primaryStoreManager);
        when(primaryStoreManager.getSecondaryUserStoreManager(SECONDARY_DOMAIN)).thenReturn(secondaryStoreManager);
        // User exists in secondary domain, so isExistingUser returns true and validation is skipped.
        when(secondaryStoreManager.isExistingUser(USERNAME)).thenReturn(true);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(secondaryStoreManager).isExistingUser(USERNAME);
    }

    @Test
    public void testIsExistingUserThrowsExceptionOnUserStoreFailure() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(PASSWORD_RECOVERY.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedIdentityUtil.when(IdentityUtil::isEmailUsernameEnabled).thenReturn(true);
        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();
        doThrow(new UserStoreException("User store connection error"))
                .when(userStoreManager).isExistingUser(USERNAME);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_ERROR);
        assertEquals(response.getErrorCode(), ERROR_CODE_USER_EXISTENCE_CHECK_FAILURE.getCode());
    }

    @Test
    public void testProcessUserConsentSkippedWhenConsentV2Disabled() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUser(USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(false);

        AbstractUserStoreManager userStoreManager = setupUserStoreManagerMocks();

        executor.execute(context);

        // Consent processing should never start a tenant flow when V2 is disabled.
        mockedPrivilegedCarbonContext.verify(PrivilegedCarbonContext::startTenantFlow, org.mockito.Mockito.never());
    }

    @Test
    public void testProcessUserConsentWithAcceptedConsents() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.singletonList("purpose-uuid-1"), Collections.emptyList(), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        PIICategory piiCategory = new PIICategory("POLICY", null, false, "POLICY");
        ReceiptInput receiptInput = mock(ReceiptInput.class);

        ConsentManager consentManager = mock(ConsentManager.class);
        IdentityRecoveryServiceDataHolder dataHolder = setupUserStoreManagerMocksWithConsentManager(consentManager);

        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(eq("POLICY"),
                eq(consentManager))).thenReturn(piiCategory);
        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.buildReceiptInput(
                anyString(), anyString(), anyString(), any(), eq(false), any(), any(), anyString(),
                any(), eq(consentManager))).thenReturn(receiptInput);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(consentManager).addConsent(receiptInput);
    }

    @Test
    public void testProcessUserConsentWithRejectedConsents() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.emptyList(), Collections.singletonList("purpose-uuid-2"), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        PIICategory piiCategory = new PIICategory("POLICY", null, false, "POLICY");
        ReceiptInput receiptInput = mock(ReceiptInput.class);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(eq("POLICY"),
                eq(consentManager))).thenReturn(piiCategory);
        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.buildReceiptInput(
                anyString(), anyString(), anyString(), any(), eq(true), any(), any(), anyString(),
                any(), eq(consentManager))).thenReturn(receiptInput);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(consentManager).addConsent(receiptInput);
    }

    @Test
    public void testProcessUserConsentWithNullPiiCategorySkipsRejectedConsents() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.emptyList(), Collections.singletonList("purpose-uuid-3"), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(eq("POLICY"),
                eq(consentManager))).thenReturn(null);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(consentManager, org.mockito.Mockito.never()).addConsent(any(ReceiptInput.class));
    }

    @Test
    public void testProcessUserConsentWithBlankAcceptedPurposeUuidReturnsUserError() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.singletonList(""), Collections.emptyList(), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(anyString(),
                any())).thenReturn(new PIICategory("POLICY", null, false, "POLICY"));

        // FlowExecutionEngineUtils.handleClientException wraps as FlowEngineClientException.
        mockedFlowEngineUtils.when(() -> FlowExecutionEngineUtils.handleClientException(any(), anyString()))
                .thenReturn(new org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineClientException(
                        "60001", "Invalid user input", "POLICY consent"));

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_USER_ERROR);
    }

    @Test
    public void testProcessUserConsentWithBlankRejectedPurposeUuidReturnsUserError() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.emptyList(), Collections.singletonList(""), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        PIICategory piiCategory = new PIICategory("POLICY", null, false, "POLICY");
        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(eq("POLICY"),
                eq(consentManager))).thenReturn(piiCategory);

        mockedFlowEngineUtils.when(() -> FlowExecutionEngineUtils.handleClientException(any(), anyString()))
                .thenReturn(new org.wso2.carbon.identity.flow.execution.engine.exception.FlowEngineClientException(
                        "60001", "Invalid user input", "POLICY consent"));

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_USER_ERROR);
    }

    @Test
    public void testProcessUserConsentAddConsentFailureReturnsError() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        FlowUser flowUser = createTestFlowUserWithConsents(USERNAME,
                Collections.singletonList("purpose-uuid-1"), Collections.emptyList(), "POLICY");

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        PIICategory piiCategory = new PIICategory("POLICY", null, false, "POLICY");
        ReceiptInput receiptInput = mock(ReceiptInput.class);

        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.getDefaultPiiCategory(eq("POLICY"),
                eq(consentManager))).thenReturn(piiCategory);
        mockedConsentReceiptUtils.when(() -> ConsentReceiptUtils.buildReceiptInput(
                anyString(), anyString(), anyString(), any(), eq(false), any(), any(), anyString(),
                any(), eq(consentManager))).thenReturn(receiptInput);

        doThrow(new ConsentManagementException("Consent DB error", "60001"))
                .when(consentManager).addConsent(any(ReceiptInput.class));

        mockedFlowEngineUtils.when(() -> FlowExecutionEngineUtils.handleServerException(
                any(org.wso2.carbon.identity.flow.execution.engine.Constants.ErrorMessages.class),
                any(Throwable.class), any(String.class), any(String.class)))
                .thenCallRealMethod();

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_ERROR);
    }

    @Test
    public void testProcessUserConsentWithNoUserConsents() throws Exception {

        FlowExecutionContext context = mock(FlowExecutionContext.class);
        // Use a real FlowUser with an empty consent list.
        FlowUser flowUser = new FlowUser();
        flowUser.setUsername(USERNAME);
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());
        flowUser.setUserCredentials(credentials);
        flowUser.addClaim(USERNAME_CLAIM_URI, USERNAME);

        when(context.getFlowType()).thenReturn(REGISTRATION.getType());
        when(context.getFlowUser()).thenReturn(flowUser);
        when(context.getUserInputData()).thenReturn(new HashMap<>());
        when(context.getTenantDomain()).thenReturn(TENANT_DOMAIN);
        when(context.getContextIdentifier()).thenReturn(CONTEXT_ID);
        when(context.getProperty("isUsernamePatternValidationSkipped")).thenReturn(null);

        mockedFrameworkUtils.when(FrameworkUtils::isConsentV2APIEnabled).thenReturn(true);

        PrivilegedCarbonContext carbonContext = mock(PrivilegedCarbonContext.class);
        mockedPrivilegedCarbonContext.when(PrivilegedCarbonContext::getThreadLocalCarbonContext)
                .thenReturn(carbonContext);

        ConsentManager consentManager = mock(ConsentManager.class);
        setupUserStoreManagerMocksWithConsentManager(consentManager);

        ExecutorResponse response = executor.execute(context);

        assertEquals(response.getResult(), STATUS_COMPLETE);
        verify(consentManager, org.mockito.Mockito.never()).addConsent(any(ReceiptInput.class));
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

    private FlowUser createTestFlowUserWithConsents(String username, List<String> accepted, List<String> rejected,
                                                    String purposeType) {

        FlowUser flowUser = mock(FlowUser.class);
        Map<String, String> claims = new HashMap<>();
        claims.put(USERNAME_CLAIM_URI, USERNAME);
        Map<String, char[]> credentials = new HashMap<>();
        credentials.put(PASSWORD_KEY, PASSWORD.toCharArray());

        FlowUser.UserConsent userConsent = new FlowUser.UserConsent();
        userConsent.getPurposeType(); // ensure fields exist

        // Use a real UserConsent populated via reflection-free setters through fromJson-compatible approach.
        List<FlowUser.UserConsent> consents = FlowUser.UserConsent.fromJson(
                buildConsentJson(purposeType, accepted, rejected));

        when(flowUser.getUsername()).thenReturn(username);
        when(flowUser.getClaims()).thenReturn(claims);
        when(flowUser.getClaim(USERNAME_CLAIM_URI)).thenReturn(USERNAME);
        when(flowUser.getClaim(EMAIL_ADDRESS_CLAIM)).thenReturn(null);
        when(flowUser.getUserCredentials()).thenReturn(credentials);
        when(flowUser.getFederatedAssociations()).thenReturn(new HashMap<>());
        when(flowUser.getUserConsents()).thenReturn(consents);

        return flowUser;
    }

    private String buildConsentJson(String purposeType, List<String> accepted, List<String> rejected) {

        StringBuilder json = new StringBuilder("{\"").append(purposeType).append("\":{");
        json.append("\"accepted\":[");
        for (int i = 0; i < accepted.size(); i++) {
            json.append("\"").append(accepted.get(i)).append("\"");
            if (i < accepted.size() - 1) json.append(",");
        }
        json.append("],\"rejected\":[");
        for (int i = 0; i < rejected.size(); i++) {
            json.append("\"").append(rejected.get(i)).append("\"");
            if (i < rejected.size() - 1) json.append(",");
        }
        json.append("]}}");
        return json.toString();
    }

    private IdentityRecoveryServiceDataHolder setupUserStoreManagerMocksWithConsentManager(
            ConsentManager consentManager) throws Exception {

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);
        when(dataHolder.getConsentManager()).thenReturn(consentManager);
        when(dataHolder.getFederatedAssociationManager()).thenReturn(mock(FederatedAssociationManager.class));

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");
        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);

        when(realmService.getTenantUserRealm(anyInt())).thenReturn(userRealm);
        when(userRealm.getUserStoreManager()).thenReturn(userStoreManager);
        when(userStoreManager.getUserIDFromUserName(anyString())).thenReturn(USER_ID);

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(TENANT_DOMAIN)).thenReturn(TENANT_ID);
        mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(PRIMARY_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);
        mockedUserCoreUtil.when(() -> UserCoreUtil.addDomainToName(anyString(), anyString()))
                .thenReturn(PRIMARY_DOMAIN + UserCoreConstants.DOMAIN_SEPARATOR + USERNAME);

        return dataHolder;
    }

    private AbstractUserStoreManager setupUserStoreManagerMocks() throws Exception {

        IdentityRecoveryServiceDataHolder dataHolder = mock(IdentityRecoveryServiceDataHolder.class);
        RealmService realmService = mock(RealmService.class);
        UserRealm userRealm = mock(UserRealm.class);
        AbstractUserStoreManager userStoreManager = mock(AbstractUserStoreManager.class);

        mockedDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.getRealmService()).thenReturn(realmService);
        when(dataHolder.getApplicationManagementService()).thenReturn(applicationManagementService);

        ApplicationBasicInfo testApp = new ApplicationBasicInfo();
        testApp.setUuid("app-uuid-001");
        testApp.setApplicationName("testApp");
        testApp.setAccessUrl("https://myapp.com");

        ApplicationBasicInfo myAccount = new ApplicationBasicInfo();
        myAccount.setUuid("my-app-uuid");
        myAccount.setApplicationName("My Account");
        myAccount.setAccessUrl("https://myaccount.com");

        when(applicationManagementService.getApplicationBasicInfoByName(eq("My Account"),
                anyString())).thenReturn(myAccount);
        when(applicationManagementService.getApplicationBasicInfoByResourceId(eq("app-uuid-001"),
                anyString())).thenReturn(
                testApp);
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