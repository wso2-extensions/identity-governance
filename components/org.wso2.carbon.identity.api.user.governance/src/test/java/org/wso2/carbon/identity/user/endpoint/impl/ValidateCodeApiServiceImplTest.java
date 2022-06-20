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
import org.testng.annotations.Test;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.dto.CodeValidationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.VerifiedChannelDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.testng.Assert.assertEquals;

/**
 * This class contains Unit tests for ValidateCodeApiServiceImpl.java
 */
public class ValidateCodeApiServiceImplTest {

    private MockedStatic<Utils> mockedUtils;

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @InjectMocks
    private ValidateCodeApiServiceImpl validateCodeApiServiceImpl;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedUtils = Mockito.mockStatic(Utils.class);
        mockedUtils.when(Utils::getUserSelfRegistrationManager).thenReturn(userSelfRegistrationManager);
    }

    @AfterMethod
    public void tearDown() {

        mockedUtils.close();
    }

    @Test
    public void testValidateCodePost() {

        assertEquals(validateCodeApiServiceImpl.validateCodePost(createCodeValidationRequestDTO()).getStatus(), 202);
        assertEquals(validateCodeApiServiceImpl.validateCodePost(null).getStatus(), 202);
    }

    @Test
    public void testIdentityRecoveryExceptioninResendCodePost() throws IdentityRecoveryException {

        Mockito.when(userSelfRegistrationManager.getConfirmedSelfRegisteredUser(anyString(), anyString(), anyString(),
                anyMap())).thenThrow(new IdentityRecoveryException("Recovery Exception"));
        assertEquals(validateCodeApiServiceImpl.validateCodePost(createCodeValidationRequestDTO()).getStatus(), 202);
    }

    @Test
    public void testIdentityRecoveryClientExceptioninResendCodePost() throws IdentityRecoveryException {

        Mockito.when(userSelfRegistrationManager.getConfirmedSelfRegisteredUser(anyString(), anyString(), anyString(),
                anyMap())).thenThrow(new IdentityRecoveryClientException("Recovery Exception"));
        assertEquals(validateCodeApiServiceImpl.validateCodePost(createCodeValidationRequestDTO()).getStatus(), 202);
    }

    private CodeValidationRequestDTO createCodeValidationRequestDTO() {

        CodeValidationRequestDTO codeValidationRequestDTO = new CodeValidationRequestDTO();
        VerifiedChannelDTO mockVerifiedChannelDTO =new VerifiedChannelDTO();
        mockVerifiedChannelDTO.setClaim("claim");
        mockVerifiedChannelDTO.setType("type");
        codeValidationRequestDTO.setCode("DummyCode");
        codeValidationRequestDTO.setProperties(buildPropertyListDTO());
        codeValidationRequestDTO.setVerifiedChannel(mockVerifiedChannelDTO);
        return codeValidationRequestDTO;
    }

    private List<PropertyDTO> buildPropertyListDTO() {

        PropertyDTO propertyDTO = new PropertyDTO();
        propertyDTO.setKey("DummyPropertyKey");
        propertyDTO.setValue("Dummy property value");
        List<PropertyDTO> propertyDTOList = new ArrayList<>();
        propertyDTOList.add(propertyDTO);
        return propertyDTOList;
    }

}
