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
package org.wso2.carbon.identity.user.endpoint.impl;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.internal.FrameworkServiceDataHolder;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.MultiAttributeLoginService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;

import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.Response;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * This class contains unit tests for ResendCodeApiServiceImpl.java
 */
public class ResendCodeApiServiceImplTest {

    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<Utils> mockedUtils;

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private NotificationResponseBean notificationResponseBean;

    @Mock
    private UserRecoveryData userRecoveryData;

    @Mock
    private MultiAttributeLoginService mockedMultiAttributeLoginService;

    @Mock
    private ResendConfirmationManager resendConfirmationManager;

    @InjectMocks
    private ResendCodeApiServiceImpl resendCodeApiService;

    private final String TEST_USERNAME = "testUser";
    private final String TEST_TENANT_DOMAIN = "testTenantDomain";
    private final String TEST_USER_STORE_DOMAIN = "testUserStoreDomain";
    private static final String RECOVERY_SCENARIO_KEY = "RecoveryScenario";

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedUtils = Mockito.mockStatic(Utils.class);
        mockedUtils.when(Utils::getUserSelfRegistrationManager).thenReturn(userSelfRegistrationManager);
        mockedUtils.when(Utils::getUserSelfRegistrationManager).thenReturn(userSelfRegistrationManager);
        FrameworkServiceDataHolder.getInstance().setMultiAttributeLoginService(mockedMultiAttributeLoginService);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedUtils.close();
    }

    @Test
    public void testResendCodePost() throws IdentityRecoveryException {

        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenReturn(notificationResponseBean);

        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 201);
        assertEquals(resendCodeApiService.resendCodePost(emptyResendCodeRequestDTO()).getStatus(), 201);
        assertEquals(resendCodeApiService.resendCodePost(emptyPropertyResendCodeRequestDTO()).getStatus(),
                201);
        assertEquals(resendCodeApiService.resendCodePost(multipleResendCodeRequestDTO()).getStatus(), 201);

        mockedUtils.when(() -> Utils.getUserRecoveryData(recoveryScenarioResendCodeRequestDTO()))
                .thenReturn(null);
        assertEquals(resendCodeApiService.resendCodePost(recoveryScenarioResendCodeRequestDTO()).getStatus(),
                400);

        mockedUtils.when(() -> Utils.getUserRecoveryData(recoveryScenarioResendCodeRequestDTO())).thenReturn(
                userRecoveryData);
        assertEquals(resendCodeApiService.resendCodePost(recoveryScenarioResendCodeRequestDTO()).getStatus(),
                400);
        assertEquals(resendCodeApiService.resendCodePost(duplicateScenarioResendCodeRequestDTO()).getStatus(),
                201);
    }

    @Test
    public void testIdentityRecoveryExceptioninResendCodePost() throws IdentityRecoveryException {

        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenThrow(new IdentityRecoveryException("Recovery Exception"));
        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 400);
    }

    @Test
    public void testIdentityRecoveryClientExceptioninResendCodePost() throws IdentityRecoveryException {

        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenThrow(new IdentityRecoveryClientException("Recovery Exception"));
        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 400);
    }

    @Test (description = "Test resendCodePost when recovery scenario is ask password, user in pending ask password " +
            "state and no userRecoveryData exists.")
    public void testResendCodePostForAskPasswordRecoveryWithoutUserRecoveryDataAndPendingAskPassword()
            throws Exception {

        ResendCodeRequestDTO requestDTO = createResendCodeRequestDTO(RecoveryScenarios.ASK_PASSWORD.name());

        mockedUtils.when(() -> Utils.getUserRecoveryData(any(), anyString())).thenReturn(null);
        mockedUtils.when(() -> Utils.getAccountState(anyString(), anyString()))
                .thenReturn(IdentityRecoveryConstants.PENDING_ASK_PASSWORD);
        mockedUtils.when(Utils::getResendConfirmationManager).thenReturn(resendConfirmationManager);

        when(resendConfirmationManager.resendConfirmationCode(
                any(), anyString(), anyString(), anyString(), any()))
                .thenReturn(new NotificationResponseBean(createUser()));
        
        // Call the method to test
        Response response = resendCodeApiService.resendCodePost(requestDTO);
        
        // Verify the response
        assertNotNull(response);
        assertEquals(response.getStatus(), 201);
        
        // Verify the ResendConfirmationManager was called with correct parameters
        verify(resendConfirmationManager).resendConfirmationCode(
                any(),
                eq(RecoveryScenarios.ASK_PASSWORD.toString()),
                eq(RecoverySteps.UPDATE_PASSWORD.toString()),
                eq(IdentityRecoveryConstants.NOTIFICATION_TYPE_RESEND_ASK_PASSWORD),
                any());
    }

    @Test (description = "Test resendCodePost when recovery scenario is ask password, user not in pending " +
            "ask password state and no userRecoveryData exists.")
    public void testResendCodePostForAskPasswordRecoveryWithoutUserRecoveryDataAndNoPendingAskPassword()
            throws Exception {

        ResendCodeRequestDTO requestDTO = createResendCodeRequestDTO(RecoveryScenarios.ASK_PASSWORD.name());

        mockedUtils.when(() -> Utils.getUserRecoveryData(any(), anyString())).thenReturn(null);
        mockedUtils.when(() -> Utils.getAccountState(anyString(), anyString()))
                .thenReturn(IdentityRecoveryConstants.PENDING_SELF_REGISTRATION);
        mockedUtils.when(Utils::getResendConfirmationManager).thenReturn(resendConfirmationManager);

        when(resendConfirmationManager.resendConfirmationCode(
                any(), anyString(), anyString(), anyString(), any()))
                .thenReturn(null);

        // Call the method to test
        Response response = resendCodeApiService.resendCodePost(requestDTO);

        // Verify the response
        assertNotNull(response);
        assertEquals(response.getStatus(), 400);

        // Verify the ResendConfirmationManager was never called
        verify(resendConfirmationManager, Mockito.never()).resendConfirmationCode(
                any(),
                anyString(),
                anyString(),
                anyString(),
                any());
    }

    private ResendCodeRequestDTO resendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        resendCodeRequestDTO.setUser(buildUserDTO());
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(buildPropertyDTO());
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private ResendCodeRequestDTO emptyResendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        UserDTO userDTO = new UserDTO();
        resendCodeRequestDTO.setUser(userDTO);
        List<PropertyDTO> listProperty = new ArrayList<>();
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private ResendCodeRequestDTO emptyPropertyResendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        resendCodeRequestDTO.setUser(buildUserDTO());
        List<PropertyDTO> listProperty = new ArrayList<>();
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private ResendCodeRequestDTO multipleResendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        resendCodeRequestDTO.setUser(buildUserDTO());
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(buildPropertyDTO());
        listProperty.add(buildPropertyDTO());
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private ResendCodeRequestDTO duplicateScenarioResendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        resendCodeRequestDTO.setUser(buildUserDTO());
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(recoveryScenarioPropertyDTO());
        listProperty.add(recoveryScenarioPropertyDTO());
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private ResendCodeRequestDTO recoveryScenarioResendCodeRequestDTO() {

        ResendCodeRequestDTO resendCodeRequestDTO = new ResendCodeRequestDTO();
        resendCodeRequestDTO.setUser(buildUserDTO());
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(recoveryScenarioPropertyDTO());
        resendCodeRequestDTO.setProperties(listProperty);
        return resendCodeRequestDTO;
    }

    private UserDTO buildUserDTO() {

        UserDTO userDTO = new UserDTO();
        userDTO.setUsername("TestUser");
        userDTO.setTenantDomain("TestTenantDomain");
        userDTO.setRealm("TestRealm");
        return userDTO;
    }

    private PropertyDTO buildPropertyDTO() {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("TestKey");
        propertyDTO.setKey("TestValue");
        return propertyDTO;
    }

    private PropertyDTO recoveryScenarioPropertyDTO() {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("RecoveryScenario");
        propertyDTO.setValue("ASK_PASSWORD");
        return propertyDTO;
    }

    @DataProvider(name = "recoveryScenarioProvider")
    public Object[][] recoveryScenarioProvider() {

        return new Object[][] {
                {RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD, RecoveryScenarios.ASK_PASSWORD,
                        RecoverySteps.UPDATE_PASSWORD, 201},
                {RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD,
                        RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, RecoverySteps.UPDATE_PASSWORD, 201},
                {RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP, RecoveryScenarios.SELF_SIGN_UP,
                        RecoverySteps.CONFIRM_SIGN_UP, 201},
                {RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK, RecoverySteps.UPDATE_PASSWORD,
                        RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK, RecoverySteps.UPDATE_PASSWORD, 201},
                {RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP, RecoverySteps.UPDATE_PASSWORD,
                        RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP, RecoverySteps.UPDATE_PASSWORD, 201},
                {RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD,
                        RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD, 201},
                {RecoveryScenarios.LITE_SIGN_UP, RecoverySteps.CONFIRM_LITE_SIGN_UP, RecoveryScenarios.LITE_SIGN_UP,
                        RecoverySteps.CONFIRM_LITE_SIGN_UP, 201},

                {RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL, 201},
                {RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL, 400},
                {RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER, 400},

                {RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL, 201},
                {RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL, 400},
                {RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL,
                        RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 400},

                {RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER, 201},
                {RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 400},
                {RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL, 400},

                {RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 201},
                {RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 400},
                {RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_EMAIL, 400},

                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 201},
                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 400},
                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_EMAIL, 400},

                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 201},
                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER, 400},
                {RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER,
                        RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_EMAIL, 400},
        };
    }

    @Test(dataProvider = "recoveryScenarioProvider")
    public void testRecoveryScenarios(RecoveryScenarios scenario, RecoverySteps step,
                                      RecoveryScenarios userRecoveryDataScenario, RecoverySteps userRecoveryDataStep,
                                      int expectedStatusCode) throws Exception {

        ResendCodeRequestDTO requestDTO = createResendCodeRequestDTO(scenario.name());
        User user = new User();
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        UserRecoveryData recoveryData = new UserRecoveryData(user, "test-secret",
                userRecoveryDataScenario, userRecoveryDataStep);
        when(Utils.getUserRecoveryData(any(), anyString())).thenReturn(recoveryData);
        when(Utils.getResendConfirmationManager()).thenReturn(resendConfirmationManager);
        mockedUtils.when(() -> Utils.getSignUpConfigs(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_SEND_OTP_IN_EMAIL,
                TEST_TENANT_DOMAIN)).thenReturn("true");

        NotificationResponseBean expectedResponse = new NotificationResponseBean(user);
        expectedResponse.setKey("test-key");
        when(resendConfirmationManager.resendConfirmationCode(any(), anyString(), anyString(), anyString(), any()))
                .thenReturn(expectedResponse);

        Response result = resendCodeApiService.resendCodePost(requestDTO);

        assertNotNull(result);
        assertEquals(result.getStatus(), expectedStatusCode);
    }

    private ResendCodeRequestDTO createResendCodeRequestDTO(String recoveryScenario) {

        ResendCodeRequestDTO requestDTO = new ResendCodeRequestDTO();
        UserDTO userDTO = new UserDTO();
        userDTO.setTenantDomain(TEST_TENANT_DOMAIN);
        userDTO.setUsername(TEST_USERNAME);
        requestDTO.setUser(userDTO);

        List<PropertyDTO> properties = new ArrayList<>();

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey(RECOVERY_SCENARIO_KEY);
        propertyDTO.setValue(recoveryScenario);

        properties.add(propertyDTO);
        requestDTO.setProperties(properties);
        return requestDTO;
    }

    private User createUser() {
        
        User user = new User();
        user.setUserName(TEST_USERNAME);
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        user.setUserStoreDomain(TEST_USER_STORE_DOMAIN);
        return user;
    }
}
