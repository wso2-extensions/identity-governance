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
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.Constants;
import org.wso2.carbon.identity.user.endpoint.Util.Utils;
import org.wso2.carbon.identity.user.endpoint.dto.SelfRegistrationUserDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.dto.ClaimDTO;
import org.wso2.carbon.identity.user.endpoint.dto.SelfUserRegistrationRequestDTO;
import org.wso2.carbon.user.api.Claim;
import org.wso2.carbon.identity.recovery.model.Property;

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

    @Mock
    private UserSelfRegistrationManager userSelfRegistrationManager;

    @Mock
    private NotificationResponseBean notificationResponseBean;

    @InjectMocks
    private MeApiServiceImpl meApiService;

    @Test
    public void testResendCodePost() throws Exception {

        mockStatic(IdentityUtil.class);
        mockStatic(Utils.class);
        PowerMockito.when(IdentityUtil.class, (String) IdentityUtil.threadLocalProperties.get().
                get(Constants.TENANT_NAME_FROM_CONTEXT)).thenReturn("TestTenant");
        when(Utils.getUserSelfRegistrationManager()).thenReturn(userSelfRegistrationManager);

        when(userSelfRegistrationManager.registerUser(any(User.class), anyString(), any(Claim[].class),
                any(Property[].class))).thenReturn(notificationResponseBean);

        assertEquals(meApiService.mePost(selfUserRegistrationRequestDTO()).getStatus(), 201);
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

    private SelfRegistrationUserDTO buildSelfregistartion() {

        SelfRegistrationUserDTO selfRegistrationUserDTO = new SelfRegistrationUserDTO();
        selfRegistrationUserDTO.setUsername("TestUser");
        selfRegistrationUserDTO.setRealm("TestRealm");
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
        buildSelfregistartion().setClaims(listClaimDTO);
        List<PropertyDTO> listpropertyDTOs = new ArrayList<>();
        listpropertyDTOs.add(buildSelfUserRegistrationRequestDTO());
        selfUserRegistrationRequestDTO.setProperties(listpropertyDTOs);
        selfUserRegistrationRequestDTO.setUser(buildSelfregistartion());
        return selfUserRegistrationRequestDTO;
    }
}