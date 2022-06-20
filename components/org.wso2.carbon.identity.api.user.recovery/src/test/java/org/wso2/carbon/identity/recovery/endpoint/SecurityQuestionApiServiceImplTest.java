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
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.ChallengeQuestionResponse;
import org.wso2.carbon.identity.recovery.endpoint.Utils.RecoveryUtil;
import org.wso2.carbon.identity.recovery.endpoint.impl.SecurityQuestionApiServiceImpl;
import org.wso2.carbon.identity.recovery.password.SecurityQuestionPasswordRecoveryManager;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * Unit tests for SecurityQuestionApiServiceImpl.java class
 */
public class SecurityQuestionApiServiceImplTest {

    @Mock
    SecurityQuestionPasswordRecoveryManager securityQuestionPasswordRecoveryManager;

    @Mock
    ChallengeQuestionResponse challengeQuestionResponse;

    @InjectMocks
    SecurityQuestionApiServiceImpl securityQuestionApiService;

    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<RecoveryUtil> mockedRecoveryUtil;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtil;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
        mockedRecoveryUtil = Mockito.mockStatic(RecoveryUtil.class);
        mockedIdentityTenantUtil = Mockito.mockStatic(IdentityTenantUtil.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityUtil.close();
        mockedRecoveryUtil.close();
        mockedIdentityTenantUtil.close();
    }

    @Test
    public void testSecurityQuestionGet() {

        mockClasses();
        assertEquals(securityQuestionApiService.securityQuestionGet("admin", null, null).getStatus(), 202);
    }

    @Test
    public void testIdentityRecoveryClientExceptionforSecurityQuestionGet() throws IdentityRecoveryException {

        mockClasses();
        Mockito.when(securityQuestionPasswordRecoveryManager.initiateUserChallengeQuestion(any(User.class))).thenThrow
                (new IdentityRecoveryClientException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND.getCode(), ""));
        assertNotNull(securityQuestionApiService.securityQuestionGet("admin", null, null));
    }

    @Test
    public void testIdentityRecoveryExceptionforSecurityQuestionGet() throws IdentityRecoveryException {

        mockClasses();
        Mockito.when(securityQuestionPasswordRecoveryManager.initiateUserChallengeQuestion(any(User.class))).thenThrow
                (new IdentityRecoveryException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND.getCode(), ""));
        assertNotNull(securityQuestionApiService.securityQuestionGet("admin", null, null));
    }

    private void mockClasses() {

        mockedIdentityTenantUtil.when(() -> IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        String[] userList = new String[1];
        userList[0] = "admin";
        mockedRecoveryUtil.when(() -> RecoveryUtil.getUserList(-1234, "admin")).thenReturn(userList);
        mockedIdentityUtil.when(() -> IdentityUtil.extractDomainFromName(userList[0])).thenReturn("PRIMARY");
        mockedRecoveryUtil.when(RecoveryUtil::getSecurityQuestionBasedPwdRecoveryManager).thenReturn(
                securityQuestionPasswordRecoveryManager);
    }
}
