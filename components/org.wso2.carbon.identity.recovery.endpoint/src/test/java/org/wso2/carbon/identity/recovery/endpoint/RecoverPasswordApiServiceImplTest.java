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

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.IObjectFactory;
import org.testng.annotations.ObjectFactory;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecoverPasswordApiServiceImpl;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyString;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.testng.Assert.assertEquals;

/**
 * This class covers unit tests for RecoverPasswordApiServiceImpl.java
 */
@PrepareForTest({RecoveryUtil.class, IdentityTenantUtil.class, FrameworkUtils.class})
public class RecoverPasswordApiServiceImplTest extends PowerMockTestCase {
    
    @Mock
    NotificationPasswordRecoveryManager notificationPasswordRecoveryManager;

    @Mock
    NotificationResponseBean notificationResponseBean;

    @InjectMocks
    RecoverPasswordApiServiceImpl recoverPasswordApiService;

    @Test
    public void testRecoverPasswordPost() throws IdentityRecoveryException {

        mockStatic(IdentityTenantUtil.class);
        mockStatic(RecoveryUtil.class);
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        when(RecoveryUtil.getNotificationBasedPwdRecoveryManager()).thenReturn(notificationPasswordRecoveryManager);
        mockStatic(FrameworkUtils.class);
        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        when(notificationPasswordRecoveryManager.sendRecoveryNotification(any(User.class), anyString(), anyBoolean(),
                any(Property[].class))).thenReturn(notificationResponseBean);
        when(FrameworkUtils.processMultiAttributeLoginIdentification(anyString(), anyString())).thenReturn(resolvedUserResult);
        assertEquals(recoverPasswordApiService.recoverPasswordPost(buildRecoveryInitiatingRequestDTO(), "", true).
                getStatus(), 202);
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

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

}
