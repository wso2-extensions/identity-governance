/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
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

package org.wso2.carbon.identity.recovery.handler;

import org.mockito.Mock;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.event.event.Event;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

/**
 * Unit tests for OTPBasedTemporaryLoginHandler.
 */
public class OTPBasedTemporaryLoginHandlerTest {

    @Mock
    private UserStoreManager userStoreManager;
    @Mock
    private UserRecoveryDataStore userRecoveryDataStore;

    private static final String TEST_USERNAME = "testUser";
    private static final String TEST_TENANT_DOMAIN = "carbon.super";
    private static final String TEST_USER_STORE_DOMAIN = "PRIMARY";
    private static final String TEST_DUMMY_CODE = "dummy-code";

    private  OTPBasedTemporaryLoginHandler otpBasedTemporaryLoginHandler;
    @BeforeMethod
    public void setUp() throws Exception {

        otpBasedTemporaryLoginHandler = new OTPBasedTemporaryLoginHandler();
    }

    @DataProvider(name = "otpAuthenticateDataProvider")
    public Object[][] getOTPAuthenticateDataProvider() {

        return new Object[][] {
                { mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK,
                        RecoverySteps.UPDATE_PASSWORD), null,
                        "needs to reset the password using the given link in email" },
                { mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP,
                        RecoverySteps.UPDATE_PASSWORD), "dummy-code", "has given correct OTP" },
                { mockRecoveryData(RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_SMS_OTP,
                        RecoverySteps.UPDATE_PASSWORD), "", "has given in-correct OTP" },
                { mockRecoveryData(RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP, RecoverySteps.SET_PASSWORD),
                        "dummy-code", "has given correct OTP" },
                { mockRecoveryData(RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP, RecoverySteps.SET_PASSWORD),
                        "", "has given in-correct OTP" }
        };
    }

    @Test(description = "Test handleEvent() with PRE_AUTHENTICATION event.",
            dataProvider = "otpAuthenticateDataProvider")
    public void testOTPAuthenticateEvents(UserRecoveryData recoveryData, String credential, String errorMessage)
            throws IdentityRecoveryException {

        Event event = createEvent(IdentityEventConstants.Event.PRE_AUTHENTICATION);
        event.getEventProperties().put(IdentityEventConstants.EventProperty.CREDENTIAL, credential);

        when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(any(User.class))).thenReturn(recoveryData);

        try {
            otpBasedTemporaryLoginHandler.handleEvent(event);
        } catch (IdentityEventException e) {
            assert e.getMessage().contains(errorMessage);
        }
    }

    private org.wso2.carbon.identity.event.event.Event createEvent(String eventName) {
        Map<String, Object> eventProperties = new HashMap<>();
        eventProperties.put(IdentityEventConstants.EventProperty.USER_NAME, TEST_USERNAME);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_DOMAIN, TEST_USER_STORE_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, TEST_TENANT_DOMAIN);
        eventProperties.put(IdentityEventConstants.EventProperty.USER_STORE_MANAGER, userStoreManager);
        return new org.wso2.carbon.identity.event.event.Event(eventName, eventProperties);
    }

    private UserRecoveryData mockRecoveryData(RecoveryScenarios recoveryScenario, RecoverySteps recoveryStep) {

        User user = new User();
        user.setUserName(TEST_USERNAME);
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        user.setUserStoreDomain(TEST_USER_STORE_DOMAIN);

        return new UserRecoveryData(user, TEST_DUMMY_CODE, recoveryScenario, recoveryStep);
    }
}
