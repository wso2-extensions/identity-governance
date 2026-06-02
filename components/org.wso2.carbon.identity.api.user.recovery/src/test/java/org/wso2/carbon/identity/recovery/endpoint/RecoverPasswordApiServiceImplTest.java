/*
 *
 *  Copyright (c) 2017, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.wso2.carbon.identity.recovery.endpoint;

import org.apache.commons.lang.StringUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginService;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;

import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecoverPasswordApiServiceImpl;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.apache.commons.logging.Log;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * This class covers unit tests for RecoverPasswordApiServiceImpl.java
 */
public class RecoverPasswordApiServiceImplTest {

    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private MockedStatic<Utils> mockedUtils;

    @Mock
    NotificationPasswordRecoveryManager notificationPasswordRecoveryManager;

    @Mock
    IdentityRecoveryServiceDataHolder mockIdentityRecoveryServiceDataHolder;

    @Mock
    MultiAttributeLoginService multiAttributeLoginService;

    @InjectMocks
    NotificationResponseBean notificationResponseBean;

    @InjectMocks
    RecoverPasswordApiServiceImpl recoverPasswordApiService;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedIdentityRecoveryServiceDataHolder = Mockito.mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedUtils = Mockito.mockStatic(Utils.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryUtil.close();
        mockedIdentityTenantUtil.close();
        mockedIdentityRecoveryServiceDataHolder.close();
        mockedUtils.close();
    }

    @DataProvider(name = "multiAttributeLoginEnableProperty")
    private Object[][] multiAttributeLoginEnablePropertyData() {

        return new Object[][]{
                {true},
                {false}
        };
    }

    @Test (dataProvider = "multiAttributeLoginEnableProperty")
    public void testRecoverPasswordPost(boolean isMultiAttributeLoginEnabled) throws IdentityRecoveryException {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        if(isMultiAttributeLoginEnabled) {
            mockedRecoveryUtil.when(() -> RecoveryUtil.getProperties(Mockito.any()))
                    .thenReturn(new Property[0]);
        }
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenReturn(notificationResponseBean);
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService()).thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(isMultiAttributeLoginEnabled);
        assertEquals(recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true).
                getStatus(), 202);
    }

    @Test(dataProvider = "multiAttributeLoginEnableProperty")
    public void testRecoverPasswordPost_IdentityRecoveryClientException_FederatedUser(boolean isMultiAttributeLoginEnabled) throws Exception {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        if (isMultiAttributeLoginEnabled) {
            mockedRecoveryUtil.when(() -> RecoveryUtil.getProperties(Mockito.any()))
                    .thenReturn(new Property[0]);
        }
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService()).thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(isMultiAttributeLoginEnabled);

        IdentityRecoveryClientException e = new IdentityRecoveryClientException(
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_FEDERATED_USER.getCode(), "Federated User");
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenThrow(e);

        Response response = recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true);
        assertEquals(response.getStatus(), 202);
    }

    @Test(dataProvider = "multiAttributeLoginEnableProperty")
    public void testRecoverPasswordPost_IdentityRecoveryClientException_InvalidRequest(boolean isMultiAttributeLoginEnabled) throws Exception {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        if (isMultiAttributeLoginEnabled) {
            mockedRecoveryUtil.when(() -> RecoveryUtil.getProperties(Mockito.any()))
                    .thenReturn(new Property[0]);
        }
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService()).thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(isMultiAttributeLoginEnabled);

        IdentityRecoveryClientException e = new IdentityRecoveryClientException(
                IdentityRecoveryConstants.ErrorMessages.INVALID_PASSWORD_RECOVERY_REQUEST.getCode(), "Invalid Request");
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenThrow(e);

        Response response = recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true);
        assertEquals(response.getStatus(), 202);
    }

    @Test
    public void testRecoverPasswordPost_IdentityRecoveryException() throws Exception {

        boolean isMultiAttributeLoginEnabled = false;
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService()).thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(isMultiAttributeLoginEnabled);

        // Mock handleInternalServerError to throw RuntimeException to simulate server error and avoid NPE in logic
        mockedRecoveryUtil.when(() -> RecoveryUtil.handleInternalServerError(anyString(), anyString(), Mockito.any(Log.class), Mockito.any(Throwable.class)))
                .thenThrow(new RuntimeException("Mocked Server Error"));

        IdentityRecoveryException e = new IdentityRecoveryException("Error", "ErrorCode", new Throwable("Cause"));
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenThrow(e);

        try {
            recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true);
        } catch (RuntimeException ex) {
            assertEquals(ex.getMessage(), "Mocked Server Error");
        }

        mockedRecoveryUtil.verify(() -> RecoveryUtil.handleInternalServerError(
                Mockito.any(),
                Mockito.any(),
                Mockito.any(),
                Mockito.any()
        ));
    }

    @Test
    public void testRecoverPasswordPost_Throwable() throws Exception {

        boolean isMultiAttributeLoginEnabled = false;
        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService()).thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(isMultiAttributeLoginEnabled);

        // Mock handleInternalServerError to throw RuntimeException
        mockedRecoveryUtil.when(() -> RecoveryUtil.handleInternalServerError(anyString(), anyString(), Mockito.any(Log.class), Mockito.any(Throwable.class)))
                .thenThrow(new RuntimeException("Mocked Server Error"));

        RuntimeException e = new RuntimeException("Unexpected Error");
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenThrow(e);

        try {
            recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true);
        } catch (RuntimeException ex) {
            assertEquals(ex.getMessage(), "Mocked Server Error");
        }

        mockedRecoveryUtil.verify(() -> RecoveryUtil.handleInternalServerError(
                Mockito.eq(Constants.SERVER_ERROR),
                Mockito.eq(IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_UNEXPECTED.getCode()),
                Mockito.any(Log.class),
                Mockito.eq(e)
        ));
    }

    // Regression coverage for issue #7200 — realm must scope the multi-attribute lookup to one user store.
    @DataProvider(name = "honorUserRealmCases")
    private Object[][] honorUserRealmCases() {

        return new Object[][]{
                // {description, username, realm, expectedLoginAttribute}
                {"realmAsField", "test@abc.com", "US1", "US1/test@abc.com"},
                {"blankRealm", "test@abc.com", "", "test@abc.com"},
                {"nullRealm", "test@abc.com", null, "test@abc.com"},
                {"domainQualifiedUsername-blankRealm", "US1/test@abc.com", "", "US1/test@abc.com"},
                {"domainQualifiedUsername-withRealm", "US1/test@abc.com", "US1", "US1/test@abc.com"},
                {"blankUsername", "", "US1", ""},
                {"nonExistentRealm", "test@abc.com", "NOPE", "NOPE/test@abc.com"},
        };
    }

    @Test(dataProvider = "honorUserRealmCases")
    public void testHonorUserRealmLoginAttributeResolution(String description, String username, String realm,
                                                           String expectedLoginAttribute)
            throws IdentityRecoveryException {

        try (MockedStatic<IdentityUtil> mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class)) {

            mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
            mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager)
                    .thenReturn(notificationPasswordRecoveryManager);
            mockedRecoveryUtil.when(() -> RecoveryUtil.getProperties(Mockito.any())).thenReturn(new Property[0]);
            mockedRecoveryUtil.when(() -> RecoveryUtil.getUser(Mockito.any())).thenReturn(null);

            mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                    .thenReturn(mockIdentityRecoveryServiceDataHolder);
            when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService())
                    .thenReturn(multiAttributeLoginService);
            when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(true);
            // FAIL result so the impl skips sendRecoveryNotification; we only assert the resolveUser argument.
            ResolvedUserResult failResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
            when(multiAttributeLoginService.resolveUser(Mockito.any(), anyString())).thenReturn(failResult);

            RecoveryInitiatingRequestDTO request = new RecoveryInitiatingRequestDTO();
            UserDTO userDTO = new UserDTO();
            userDTO.setUsername(username);
            userDTO.setRealm(realm);
            request.setUser(userDTO);
            request.setProperties(buildPropertyDTO());

            assertEquals(recoverPasswordApiService.recoverPasswordPost(request, "email", true).getStatus(), 202,
                    "Case [" + description + "] should still return HTTP 202 regardless of resolver outcome.");

            ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
            verify(multiAttributeLoginService).resolveUser(captor.capture(), anyString());
            assertEquals(captor.getValue(), expectedLoginAttribute,
                    "Case [" + description + "] — login attribute passed to MultiAttributeLoginService.resolveUser " +
                            "did not match expected value.");
        }
    }

    // Multi-attribute login disabled → resolver (and the realm helper) is never reached.
    @Test
    public void testHonorUserRealmIgnoredWhenMultiAttributeLoginDisabled() throws IdentityRecoveryException {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager)
                .thenReturn(notificationPasswordRecoveryManager);

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService())
                .thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(false);
        when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenReturn(notificationResponseBean);

        RecoveryInitiatingRequestDTO request = new RecoveryInitiatingRequestDTO();
        UserDTO userDTO = new UserDTO();
        userDTO.setUsername("test@abc.com");
        userDTO.setRealm("US1");
        request.setUser(userDTO);
        request.setProperties(buildPropertyDTO());

        assertEquals(recoverPasswordApiService.recoverPasswordPost(request, "email", true).getStatus(), 202);
        // Resolver must never be invoked when multi-attribute login is disabled.
        verify(multiAttributeLoginService, Mockito.never()).resolveUser(anyString(), anyString());
    }

    // Store-domain precedence: realm wins only when the username is not domain-qualified; otherwise the
    // in-username domain (the resolved store) is authoritative — even if a different realm is also supplied.
    @DataProvider(name = "resolvedStoreDomainCases")
    private Object[][] resolvedStoreDomainCases() {

        return new Object[][]{
                // {description, username, realm, resolvedDomain, expectedDomain}
                {"unqualified-realmWins", "test@abc.com", "US1", "PRIMARY", "US1"},
                {"qualified-conflict-resolvedWins", "US1/user2", "PRIMARY", "US1", "US1"},
                {"qualified-blankRealm-resolved", "US1/user2", "", "US1", "US1"},
        };
    }

    @Test(dataProvider = "resolvedStoreDomainCases")
    public void testResolvedUserStoreDomainPrecedence(String description, String username, String realm,
                                                      String resolvedDomain, String expectedDomain)
            throws IdentityRecoveryException {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager)
                .thenReturn(notificationPasswordRecoveryManager);
        mockedRecoveryUtil.when(() -> RecoveryUtil.getProperties(Mockito.any())).thenReturn(new Property[0]);

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(mockIdentityRecoveryServiceDataHolder);
        when(mockIdentityRecoveryServiceDataHolder.getMultiAttributeLoginService())
                .thenReturn(multiAttributeLoginService);
        when(multiAttributeLoginService.isEnabled(anyString())).thenReturn(true);

        org.wso2.carbon.user.core.common.User resolvedCoreUser =
                Mockito.mock(org.wso2.carbon.user.core.common.User.class);
        when(resolvedCoreUser.getUsername()).thenReturn(resolvedDomain + "/user2");
        when(resolvedCoreUser.getUserStoreDomain()).thenReturn(resolvedDomain);
        when(resolvedCoreUser.getTenantDomain()).thenReturn("carbon.super");
        ResolvedUserResult success = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.SUCCESS);
        success.setUser(resolvedCoreUser);
        when(multiAttributeLoginService.resolveUser(Mockito.any(), anyString())).thenReturn(success);

        when(notificationPasswordRecoveryManager.sendRecoveryNotification(
                Mockito.any(org.wso2.carbon.identity.application.common.model.User.class), anyString(), anyBoolean(),
                Mockito.any())).thenReturn(notificationResponseBean);

        RecoveryInitiatingRequestDTO request = new RecoveryInitiatingRequestDTO();
        UserDTO userDTO = new UserDTO();
        userDTO.setUsername(username);
        userDTO.setRealm(realm);
        request.setUser(userDTO);
        request.setProperties(buildPropertyDTO());

        assertEquals(recoverPasswordApiService.recoverPasswordPost(request, "email", true).getStatus(), 202);

        ArgumentCaptor<org.wso2.carbon.identity.application.common.model.User> captor =
                ArgumentCaptor.forClass(org.wso2.carbon.identity.application.common.model.User.class);
        verify(notificationPasswordRecoveryManager).sendRecoveryNotification(captor.capture(), anyString(),
                anyBoolean(), Mockito.any());
        assertEquals(captor.getValue().getUserStoreDomain(), expectedDomain,
                "Case [" + description + "] — resolved user store domain did not match expected.");
    }

    private RecoveryInitiatingRequestDTO buildRecoveryInitiatingRequestDTO() {

        RecoveryInitiatingRequestDTO recoveryInitiatingRequestDTO = new RecoveryInitiatingRequestDTO();
        recoveryInitiatingRequestDTO.setUser(buildUserDTO());
        recoveryInitiatingRequestDTO.setProperties(buildPropertyDTO());
        return recoveryInitiatingRequestDTO;
    }

    private UserDTO buildUserDTO() {

        UserDTO userDTO = new UserDTO();
        userDTO.setUsername("dummy");
        return userDTO;
    }

    private List<PropertyDTO> buildPropertyDTO() {

        PropertyDTO propertyDTO = new PropertyDTO();
        List<PropertyDTO> propertyDTOList = new ArrayList<>();
        propertyDTO.setValue("Dummy Value");
        propertyDTO.setKey("Dummy Key");
        propertyDTOList.add(propertyDTO);
        return propertyDTOList;
    }

    @Test
    public void testHandleRecoveryException_UserExistenceHidden_EmailNotFound() throws Exception {
        // Arrange: user existence hidden and rootCause message equals email-not-found constant
        mockedUtils.when(Utils::isUserExistenceHidden).thenReturn(true);

        // Mock RecoveryUtil.getUser to return a user object (we only need it to be non-null here)
        org.wso2.carbon.identity.application.common.model.User mockedUser =
                new org.wso2.carbon.identity.application.common.model.User();
        mockedUser.setUserName("dummyUser");
        mockedRecoveryUtil.when(() -> RecoveryUtil.getUser(Mockito.any())).thenReturn(mockedUser);

        Throwable rootCause = new Throwable(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND);
        RecoveryInitiatingRequestDTO request = buildRecoveryInitiatingRequestDTO();

        // Act: invoke protected method via reflection
        Method method = RecoverPasswordApiServiceImpl.class.getDeclaredMethod("handleRecoveryException",
                Throwable.class, String.class, RecoveryInitiatingRequestDTO.class);
        method.setAccessible(true);
        Object result = method.invoke(recoverPasswordApiService, rootCause, Constants.ERROR_CODE_EMAIL_NOT_FOUND, request);

        // Assert
        assertNotNull(result, "Expected a NotificationResponseBean when user existence is hidden and email not found");
        // Optionally assert it's of the expected type
        assertEquals(result.getClass(), NotificationResponseBean.class);
    }

    @Test
    public void testHandleRecoveryException_EmailNotFound_NotHidden_ThrowsBadRequest() throws Exception {
        // Arrange: user existence not hidden
        mockedUtils.when(Utils::isUserExistenceHidden).thenReturn(false);

        // Mock RecoveryUtil.handleBadRequest to throw a RuntimeException to emulate failure path
        mockedRecoveryUtil.when(() -> RecoveryUtil.handleBadRequest(Mockito.anyString(), Mockito.anyString()))
                .thenThrow(new RuntimeException("bad request called"));

        Throwable rootCause = new Throwable(Constants.ERROR_MESSAGE_EMAIL_NOT_FOUND);
        RecoveryInitiatingRequestDTO request = buildRecoveryInitiatingRequestDTO();

        // Act: invoke protected method via reflection and expect the RuntimeException
        Method method = RecoverPasswordApiServiceImpl.class.getDeclaredMethod("handleRecoveryException",
                Throwable.class, String.class, RecoveryInitiatingRequestDTO.class);
        method.setAccessible(true);
        try {
            method.invoke(recoverPasswordApiService, rootCause, Constants.ERROR_CODE_EMAIL_NOT_FOUND, request);
        } catch (java.lang.reflect.InvocationTargetException ite) {
            // The invoked method threw an exception; unwrap and assert
            Throwable cause = ite.getCause();
            assertNotNull(cause);
            assertEquals(cause.getMessage(), "bad request called");
            return;
        }
        // If no exception thrown, fail the test
        throw new AssertionError("Expected RuntimeException to be thrown by RecoveryUtil.handleBadRequest");
    }
}
