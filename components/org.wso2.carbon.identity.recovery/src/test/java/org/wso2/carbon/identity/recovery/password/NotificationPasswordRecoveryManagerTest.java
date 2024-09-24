/*
 * Copyright (c) 2024, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.password;

import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.lang.reflect.Method;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.wso2.carbon.identity.recovery.IdentityRecoveryConstants.ConnectorConfig.*;

public class NotificationPasswordRecoveryManagerTest {

    private static final String NOTIFICATION_CHANNEL_EMAIL = "EMAIL";
    private static final String OTP = "PnRuLm";
    private static final String FLOW_CONFIRMATION_CODE = "4273129d-344a-423c-a889-e5d36ea9d960";
    private static final String TENANT_DOMAIN = "carbon.super";

    @Mock
    private IdentityGovernanceService identityGovernanceService;

    @Mock
    UserRecoveryDataStore userRecoveryDataStore;

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        IdentityRecoveryServiceDataHolder.getInstance().setIdentityGovernanceService(identityGovernanceService);
    }

    @DataProvider(name = "generateNewConfirmationCodeForOTPFlowData")
    public Object[][] generateNewConfirmationCodeForOTPFlowData() {
        return new Object[][] {
                {"true", "true", OTP},
                {"true", "false", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP},
                {"false", "true", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP},
                {"false", "false", FLOW_CONFIRMATION_CODE + IdentityRecoveryConstants.CONFIRMATION_CODE_SEPARATOR + OTP}
        };
    }

    @Test(dataProvider = "generateNewConfirmationCodeForOTPFlowData")
    public void testGenerateNewConfirmationCodeForOTPFlow(String sendOTPInEmail, String sendOnlyOtpAsConfirmationCode,
                                                          String confirmationCode) throws Exception {

        User user = new User();
        user.setTenantDomain(TENANT_DOMAIN);

        Property property1 = new Property();
        property1.setName(PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL);
        property1.setValue(sendOTPInEmail);
        Property[] properties = new Property[1];
        properties[0] = property1;

        UserRecoveryData userRecoveryData = new UserRecoveryData(user, OTP, RecoveryScenarios
                .NOTIFICATION_BASED_PW_RECOVERY);
        userRecoveryData.setRecoveryFlowId(FLOW_CONFIRMATION_CODE);

        when(identityGovernanceService.getConfiguration(new String[]{PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL},
                TENANT_DOMAIN)).thenReturn(properties);

        try (MockedStatic<IdentityUtil> identityUtil = Mockito.mockStatic(IdentityUtil.class);
            MockedStatic<Utils> mockedUtils = Mockito.mockStatic(Utils.class, Mockito.CALLS_REAL_METHODS);
            MockedStatic<JDBCRecoveryDataStore> mockedJDBCRecoveryDataStore =
                     Mockito.mockStatic(JDBCRecoveryDataStore.class)) {

            identityUtil.when(() -> IdentityUtil.getProperty(
                    IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_ONLY_OTP_AS_CONFIRMATION_CODE))
                    .thenReturn(sendOnlyOtpAsConfirmationCode);
            mockedUtils.when(() -> Utils.generateSecretKey(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(),
                    ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).thenReturn(OTP);

            mockedJDBCRecoveryDataStore.when(JDBCRecoveryDataStore::getInstance).thenReturn(userRecoveryDataStore);
            when(userRecoveryDataStore.loadWithoutCodeExpiryValidation(ArgumentMatchers.any())).
                    thenReturn(userRecoveryData);

            Method generateNewConfirmationCodeMethod = NotificationPasswordRecoveryManager.class.getDeclaredMethod(
                    "generateNewConfirmationCode", User.class, String.class);
            generateNewConfirmationCodeMethod.setAccessible(true);

            NotificationPasswordRecoveryManager notificationPasswordRecoveryManager =
                    NotificationPasswordRecoveryManager.getInstance();
            UserRecoveryData recoveryData = (UserRecoveryData) generateNewConfirmationCodeMethod.invoke(
                    notificationPasswordRecoveryManager, user, NOTIFICATION_CHANNEL_EMAIL);

            assertEquals(recoveryData.getSecret(), confirmationCode);
        }

    }
}
