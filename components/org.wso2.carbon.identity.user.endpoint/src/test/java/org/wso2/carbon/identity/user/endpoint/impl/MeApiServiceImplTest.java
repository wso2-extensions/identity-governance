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
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeCodeValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.MeResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfRegistrationUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.export.core.UserExportException;
import org.wso2.carbon.user.api.Claim;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertEquals;

/**
 * This class contains unit tests for MeApiServiceImpl.java
 */
@PrepareForTest({IdentityUtil.class, Utils.class})
public class MeApiServiceImplTest extends PowerMockTestCase {

    private static final String USERNAME = "dummyUser";
    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private ResendConfirmationManager resendConfirmationManager;

    @Mock
    private UserRecoveryData userRecoveryData;

    @Mock
    private NotificationResponseBean notificationResponseBean;

    @InjectMocks
    private MeApiServiceImpl meApiService;

    @Test
    public void testMePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.registerUser(any(User.class), anyString(), any(Claim[].class),
                any(Property[].class))).thenReturn(notificationResponseBean);
        assertEquals(meApiService.mePost(selfUserRegistrationRequestDTO()).getStatus(), 201);
        assertEquals(meApiService.mePost(null).getStatus(), 201);
    }

    @Test
    public void testIdentityRecoveryExceptionInMePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.registerUser(any(User.class), anyString(), any(Claim[].class),
                any(Property[].class))).thenThrow(new IdentityRecoveryException("Recovery Exception"));
        assertEquals(meApiService.mePost(selfUserRegistrationRequestDTO()).getStatus(), 201);
    }

    @Test
    public void testIdentityRecoveryClientExceptionInMePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.registerUser(any(User.class), anyString(), any(Claim[].class),
                any(Property[].class))).thenThrow(new IdentityRecoveryClientException("Recovery Exception"));
        assertEquals(meApiService.mePost(selfUserRegistrationRequestDTO()).getStatus(), 201);
    }

    @Test
    public void testMeGet() throws UserExportException {

        mockClasses();
        try {
            String carbonHome = Paths.get(System.getProperty("user.dir"), "src", "test", "resources").toString();
            System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setUsername(USERNAME);
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(-1234);
            when(Utils.getUserInformationService()).thenReturn(new MockUserInformationService());

            assertEquals(meApiService.getMe().getStatus(), 200);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    @Test
    public void testMeValidateCodePost() {

        mockClasses();
        assertEquals(meApiService.meValidateCodePost(createMeCodeValidationRequestDTO()).getStatus(), 202);
        assertEquals(meApiService.meValidateCodePost(null).getStatus(), 202);
    }

    @Test
    public void testMeResendCodePost() throws IdentityRecoveryException {

        mockClasses();
        try {
            String carbonHome = Paths.get(System.getProperty("user.dir"), "src", "test", "resources").toString();
            System.setProperty(CarbonBaseConstants.CARBON_HOME, carbonHome);
            PrivilegedCarbonContext.startTenantFlow();
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setUsername(USERNAME);
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(-1234);
            when(resendConfirmationManager.resendConfirmationCode(any(User.class), anyString(), anyString(),
                    anyString(), any(Property[].class))).thenReturn(notificationResponseBean);
            when(Utils.getUserRecoveryData(any(ResendCodeRequestDTO.class), anyString())).thenReturn(userRecoveryData);
            when(Utils.getResendConfirmationManager()).thenReturn(resendConfirmationManager);
            when(userRecoveryData.getRecoveryScenario()).thenReturn(RecoveryScenarios.
                    getRecoveryScenario("MOBILE_VERIFICATION_ON_UPDATE"));
            when(userRecoveryData.getRecoveryStep()).thenReturn(RecoverySteps.getRecoveryStep("VERIFY_MOBILE_NUMBER"));

            assertEquals(meApiService.meResendCodePost(meResendCodeRequestDTO()).getStatus(), 201);
            assertEquals(meApiService.meResendCodePost(
                    meResendCodeRequestDTOWithInvalidScenarioProperty()).getStatus(), 400);

            when(Utils.getUserRecoveryData(any(ResendCodeRequestDTO.class), anyString())).thenReturn(null);
            assertEquals(meApiService.meResendCodePost(meResendCodeRequestDTO()).getStatus(), 400);

            when(userRecoveryData.getRecoveryScenario()).thenReturn(RecoveryScenarios.
                    getRecoveryScenario("ASK_PASSWORD"));
            assertEquals(meApiService.meResendCodePost(meResendCodeRequestDTO()).getStatus(), 400);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    private void mockClasses() {

        mockStatic(IdentityUtil.class);
        mockStatic(Utils.class);
        when(Utils.getUserSelfRegistrationManager()).thenReturn(userSelfRegistrationManager);
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

    private SelfRegistrationUserDTO buildSelfRegistartion() {

        SelfRegistrationUserDTO selfRegistrationUserDTO = new SelfRegistrationUserDTO();
        selfRegistrationUserDTO.setUsername("TestUser");
        selfRegistrationUserDTO.setRealm(null);
        selfRegistrationUserDTO.setTenantDomain("TestTenantDomain");
        selfRegistrationUserDTO.setPassword("TestPassword");
        return selfRegistrationUserDTO;
    }

    private PropertyDTO buildSelfUserRegistrationRequestDTO() {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("TestKey");
        propertyDTO.setKey("TestValue");
        return propertyDTO;
    }

    private ClaimDTO buildClaimDTO() {

        ClaimDTO claimDTO = new ClaimDTO();
        claimDTO.setUri("http://wso2.org.email");
        claimDTO.setValue("test@gmail.com");
        return claimDTO;
    }

    private SelfUserRegistrationRequestDTO selfUserRegistrationRequestDTO() {

        SelfUserRegistrationRequestDTO selfUserRegistrationRequestDTO = new SelfUserRegistrationRequestDTO();
        List<ClaimDTO> listClaimDTO = new ArrayList<>();
        listClaimDTO.add(buildClaimDTO());
        buildSelfRegistartion().setClaims(listClaimDTO);
        List<PropertyDTO> listPropertyDTOs = new ArrayList<>();
        listPropertyDTOs.add(buildSelfUserRegistrationRequestDTO());
        selfUserRegistrationRequestDTO.setProperties(listPropertyDTOs);
        selfUserRegistrationRequestDTO.setUser(buildSelfRegistartion());
        return selfUserRegistrationRequestDTO;
    }

    private MeCodeValidationRequestDTO createMeCodeValidationRequestDTO() {

        MeCodeValidationRequestDTO codeValidationRequestDTO = new MeCodeValidationRequestDTO();
        codeValidationRequestDTO.setCode("TestCode");
        return codeValidationRequestDTO;
    }

    private MeResendCodeRequestDTO meResendCodeRequestDTO() {

        MeResendCodeRequestDTO meResendCodeRequestDTO = new MeResendCodeRequestDTO();
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(buildPropertyDTO("RecoveryScenario", "MOBILE_VERIFICATION_ON_UPDATE"));
        meResendCodeRequestDTO.setProperties(listProperty);
        return meResendCodeRequestDTO;
    }

    private MeResendCodeRequestDTO meResendCodeRequestDTOWithInvalidScenarioProperty() {

        MeResendCodeRequestDTO meResendCodeRequestDTO = new MeResendCodeRequestDTO();
        List<PropertyDTO> listProperty = new ArrayList<>();
        listProperty.add(buildPropertyDTO("RecoveryScenario", "INVALID_SCENARIO"));
        meResendCodeRequestDTO.setProperties(listProperty);
        return meResendCodeRequestDTO;
    }

    private PropertyDTO buildPropertyDTO(String key, String value) {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey(key);
        propertyDTO.setValue(value);
        return propertyDTO;
    }
}
