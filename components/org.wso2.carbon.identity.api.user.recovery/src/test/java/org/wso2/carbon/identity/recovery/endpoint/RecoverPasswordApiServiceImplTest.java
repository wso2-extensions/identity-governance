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
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.multi.attribute.login.mgt.ResolvedUserResult;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.RecoveryInitiatingRequestDTO;
import org.wso2.carbon.identity.recovery.endpoint.dto.UserDTO;
import org.wso2.carbon.identity.recovery.endpoint.impl.RecoverPasswordApiServiceImpl;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isNull;
import static org.testng.Assert.assertEquals;

/**
 * This class covers unit tests for RecoverPasswordApiServiceImpl.java
 */
public class RecoverPasswordApiServiceImplTest {

    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;
    private MockedStatic<FrameworkUtils> mockedFrameworkUtils;

    @Mock
    NotificationPasswordRecoveryManager notificationPasswordRecoveryManager;

    @InjectMocks
    NotificationResponseBean notificationResponseBean;

    @InjectMocks
    RecoverPasswordApiServiceImpl recoverPasswordApiService;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
        mockedFrameworkUtils = Mockito.mockStatic(FrameworkUtils.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryUtil.close();
        mockedIdentityTenantUtil.close();
        mockedFrameworkUtils.close();
    }

    @Test
    public void testRecoverPasswordPost() throws IdentityRecoveryException {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedPwdRecoveryManager).thenReturn(
                notificationPasswordRecoveryManager);
        ResolvedUserResult resolvedUserResult = new ResolvedUserResult(ResolvedUserResult.UserResolvedStatus.FAIL);
        Mockito.when(notificationPasswordRecoveryManager.sendRecoveryNotification(isNull(), anyString(), anyBoolean(),
                isNull())).thenReturn(notificationResponseBean);
        mockedFrameworkUtils.when(
                () -> FrameworkUtils.processMultiAttributeLoginIdentification(anyString(), anyString())).thenReturn(
                resolvedUserResult);
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
}
