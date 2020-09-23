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
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ResendCodeRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.UserDTO;

import java.util.ArrayList;
import java.util.List;

import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertEquals;

/**
 * This class contains unit tests for ResendCodeApiServiceImpl.java
 */
@PrepareForTest({IdentityUtil.class, Utils.class})
public class ResendCodeApiServiceImplTest extends PowerMockTestCase {

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private NotificationResponseBean notificationResponseBean;

    @Mock
    private UserRecoveryData userRecoveryData;

    @InjectMocks
    private ResendCodeApiServiceImpl resendCodeApiService;


    @Test
    public void testResendCodePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenReturn(notificationResponseBean);
        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 501);
        assertEquals(resendCodeApiService.resendCodePost(emptyResendCodeRequestDTO()).getStatus(), 501);
        assertEquals(resendCodeApiService.resendCodePost(emptyPropertyResendCodeRequestDTO()).getStatus(), 501);
        assertEquals(resendCodeApiService.resendCodePost(multipleResendCodeRequestDTO()).getStatus(), 501);

        when(Utils.getUserRecoveryData(recoveryScenarioResendCodeRequestDTO())).thenReturn(null);
        assertEquals(resendCodeApiService.resendCodePost(recoveryScenarioResendCodeRequestDTO()).getStatus(), 501);

        when(Utils.getUserRecoveryData(recoveryScenarioResendCodeRequestDTO())).thenReturn(userRecoveryData);
        assertEquals(resendCodeApiService.resendCodePost(recoveryScenarioResendCodeRequestDTO()).getStatus(), 501);
        assertEquals(resendCodeApiService.resendCodePost(duplicateScenarioResendCodeRequestDTO()).getStatus(), 501);
    }

    @Test
    public void testIdentityRecoveryExceptioninResendCodePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenThrow(new IdentityRecoveryException("Recovery Exception"));
        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 501);
    }

    @Test
    public void testIdentityRecoveryClientExceptioninResendCodePost() throws IdentityRecoveryException {

        mockClasses();
        when(userSelfRegistrationManager.resendConfirmationCode(
                Utils.getUser(resendCodeRequestDTO().getUser()),
                Utils.getProperties(resendCodeRequestDTO().getProperties()))).thenThrow(new IdentityRecoveryClientException("Recovery Exception"));
        assertEquals(resendCodeApiService.resendCodePost(resendCodeRequestDTO()).getStatus(), 501);
    }


  
    private void mockClasses() {

        mockStatic(IdentityUtil.class);
        mockStatic(Utils.class);
        when(Utils.getUserSelfRegistrationManager()).thenReturn(userSelfRegistrationManager);
        when(Utils.getUserSelfRegistrationManager()).thenReturn(userSelfRegistrationManager);
    }


    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
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
}
