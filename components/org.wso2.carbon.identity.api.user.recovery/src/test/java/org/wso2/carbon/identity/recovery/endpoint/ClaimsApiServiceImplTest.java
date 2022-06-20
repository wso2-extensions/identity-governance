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
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.impl.ClaimsApiServiceImpl;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.recovery.username.NotificationUsernameRecoveryManager;
import org.wso2.carbon.user.core.claim.Claim;

import static org.testng.Assert.assertEquals;

/**
 * Unit tests for ClaimsApiServiceImpl.java
 */
public class ClaimsApiServiceImplTest {

    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;

    @Mock
    NotificationPasswordRecoveryManager notificationPasswordRecoveryManager;

    @Mock
    NotificationUsernameRecoveryManager notificationUsernameRecoveryManager;

    @InjectMocks
    ClaimsApiServiceImpl claimsApiService;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedRecoveryUtil.close();
    }

    @Test
    public void testClaimsGet() throws IdentityException {

        mockedRecoveryUtil.when(RecoveryUtil::getNotificationBasedUsernameRecoveryManager).thenReturn(
                notificationUsernameRecoveryManager);
        Claim[] userClaims = new Claim[2];
        Mockito.when(notificationUsernameRecoveryManager
                .getIdentitySupportedClaims("test", "carbon.super")).thenReturn(userClaims);
        assertEquals(claimsApiService.claimsGet(null).getStatus(), 200);
    }

    @Test
    public void testThrowableinClaimsGet() throws IdentityException {

        claimsApiService.claimsGet("dummy");
    }

}
