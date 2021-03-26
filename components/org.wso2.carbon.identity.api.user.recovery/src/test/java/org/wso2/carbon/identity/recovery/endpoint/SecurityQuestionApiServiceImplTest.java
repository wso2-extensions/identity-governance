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

import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.when;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * Unit tests for SecurityQuestionApiServiceImpl.java class
 */

@PrepareForTest({RecoveryUtil.class, IdentityTenantUtil.class, IdentityUtil.class})
public class SecurityQuestionApiServiceImplTest extends PowerMockTestCase {
    @Mock
    SecurityQuestionPasswordRecoveryManager securityQuestionPasswordRecoveryManager;

    @Mock
    ChallengeQuestionResponse challengeQuestionResponse;

    @InjectMocks
    SecurityQuestionApiServiceImpl securityQuestionApiService;

    @Test
    public void testSecurityQuestionGet() {

        mockClasses();
        assertEquals(securityQuestionApiService.securityQuestionGet("admin", null, null).getStatus(), 202);
    }

    @Test
    public void testIdentityRecoveryClientExceptionforSecurityQuestionGet() throws IdentityRecoveryException {

        mockClasses();
        when(securityQuestionPasswordRecoveryManager.initiateUserChallengeQuestion(any(User.class))).thenThrow
                (new IdentityRecoveryClientException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND.getCode(), ""));
        assertNotNull(securityQuestionApiService.securityQuestionGet("admin", null, null));
    }

    @Test
    public void testIdentityRecoveryExceptionforSecurityQuestionGet() throws IdentityRecoveryException {

        mockClasses();
        when(securityQuestionPasswordRecoveryManager.initiateUserChallengeQuestion(any(User.class))).thenThrow
                (new IdentityRecoveryException(IdentityRecoveryConstants.ErrorMessages.
                        ERROR_CODE_CHALLENGE_QUESTION_NOT_FOUND.getCode(), ""));
        assertNotNull(securityQuestionApiService.securityQuestionGet("admin", null, null));
    }


    private void mockClasses() {

        mockStatic(IdentityTenantUtil.class);
        mockStatic(RecoveryUtil.class);
        mockStatic(IdentityUtil.class);
        when(IdentityTenantUtil.getTenantId(anyString())).thenReturn(-1234);
        String[] userList = new String[1];
        userList[0] = "admin";
        when(RecoveryUtil.getUserList(-1234, "admin")).thenReturn(userList);
        when(IdentityUtil.extractDomainFromName(userList[0])).thenReturn("PRIMARY");
        when(RecoveryUtil.getSecurityQuestionBasedPwdRecoveryManager()).thenReturn(securityQuestionPasswordRecoveryManager);
    }

    @ObjectFactory
    public IObjectFactory getObjectFactory() {

        return new org.powermock.modules.testng.PowerMockObjectFactory();
    }

}
