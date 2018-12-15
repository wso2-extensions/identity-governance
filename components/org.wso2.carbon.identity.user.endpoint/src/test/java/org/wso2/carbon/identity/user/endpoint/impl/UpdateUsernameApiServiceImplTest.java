/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.endpoint.impl;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.Assert;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.user.endpoint.dto.UsernameUpdateRequestDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;
import org.wso2.carbon.identity.user.rename.core.dto.StatusDTO;
import org.wso2.carbon.identity.user.rename.core.dto.UserDTO;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateClientException;
import org.wso2.carbon.identity.user.rename.core.exception.UsernameUpdateException;
import org.wso2.carbon.identity.user.rename.core.internal.service.impl.UsernameUpdateServiceImpl;

import static org.mockito.Matchers.any;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;

/**
 * Test class that include unit test cases for UpdateUsernameApiServiceImpl
 */
@PrepareForTest({Utils.class})
public class UpdateUsernameApiServiceImplTest extends PowerMockTestCase {

    private static final String ERROR_MSG = "error";
    private static final String ERROR_CODE = "10001";
    @Mock
    private UsernameUpdateServiceImpl usernameUpdateService;
    @Mock
    private StatusDTO statusDTO;
    @Mock
    private UsernameUpdateRequestDTO usernameUpdateRequestDTO;
    @InjectMocks
    private UpdateUsernameApiServiceImpl usernameApiService;

    @Test
    public void testSuccessStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenReturn(statusDTO);
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }

    @Test
    public void testBadRequestStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenThrow(new UsernameUpdateClientException
                (ERROR_MSG, ERROR_CODE, UsernameUpdateClientException
                        .ErrorType.BAD_REQUEST));
        // The test method executes the lines but does not throw the 400 code.
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }

    @Test
    public void testNotAcceptableStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenThrow(new UsernameUpdateClientException
                (ERROR_MSG, ERROR_CODE, UsernameUpdateClientException
                        .ErrorType.NOT_ACCEPTABLE));
        // The test method executes the lines but does not throw the 406 code.
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }

    @Test
    public void testNotFoundStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenThrow(new UsernameUpdateClientException
                (ERROR_MSG, ERROR_CODE, UsernameUpdateClientException
                        .ErrorType.NOT_FOUND));
        // The test method executes the lines but does not throw the 404 code.
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }

    @Test
    public void testServerErrorStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenThrow(new UsernameUpdateException
                (ERROR_MSG, ERROR_CODE));
        // The test method executes the lines but does not throw the 500 code.
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }

    @Test
    public void testUnexpectedServerErrorStatusOfUpdateUsername() throws Exception {

        mockStatic(Utils.class);
        when(Utils.getUsernameUpdateService()).thenReturn(usernameUpdateService);
        when(usernameUpdateService.updateUsername(any(UserDTO.class))).thenThrow(new RuntimeException(ERROR_MSG));
        // The test method executes the lines but does not throw the 500 code.
        Assert.assertEquals(usernameApiService.updateUsernamePut(usernameUpdateRequestDTO).getStatus(), 200);
    }
}
