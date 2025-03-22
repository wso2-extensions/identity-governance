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

package org.wso2.carbon.identity.recovery.internal.service.impl.password;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.dto.NotificationChannelDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryChannelInfoDTO;
import org.wso2.carbon.identity.recovery.dto.RecoveryInformationDTO;
import org.wso2.carbon.identity.recovery.dto.ResendConfirmationDTO;
import org.wso2.carbon.identity.recovery.internal.service.impl.UserAccountRecoveryManager;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertThrows;

public class PasswordRecoveryManagerImplTest {

    // Constants.
    private static final String TRUE_STRING = "true";
    private static final String FALSE_STRING = "false";
    private static final String USERNAME = "testUsername";
    private static final String FLOW_ID = "12fesfhb43-3243nsdsdvb-3143";
    private static final String RECOVERY_CODE = "asdkjaskjb0sdbkbjbsdb";
    private static final String EMAIL = "EMAIL";
    private static final String SMS = "SMS";
    private static final String TEST_EMAIL = "test@gmail.com";
    private static final String TEST_MOBILE = "+9471736746";
    private static final String TENANT_DOMAIN = "carbon.super";

    @Mock
    UserAccountRecoveryManager userAccountRecoveryManager;

    @Mock
    ResendConfirmationManager resendConfirmationManager;

    private MockedStatic<Utils> utilsMockedStatic;
    private MockedStatic<UserAccountRecoveryManager> userAccountRecoveryManagerMockedStatic;
    private MockedStatic<ResendConfirmationManager> resendConfirmationManagerMockedStatic;

    @BeforeMethod
    public void init() {

        openMocks(this);
        utilsMockedStatic = mockStatic(Utils.class);
        userAccountRecoveryManagerMockedStatic = mockStatic(UserAccountRecoveryManager.class);
        resendConfirmationManagerMockedStatic = mockStatic(ResendConfirmationManager.class);
        resendConfirmationManagerMockedStatic.when(ResendConfirmationManager::getInstance).
                thenReturn(resendConfirmationManager);
    }

    @AfterMethod
    public void tearDown() {

        utilsMockedStatic.close();
        userAccountRecoveryManagerMockedStatic.close();
        resendConfirmationManagerMockedStatic.close();
    }

    @DataProvider(name = "generateInitiateData")
    public Object[][] generateInitiateData() {

        return new Object[][]{

                {true, false, false, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(true, false)},
                {true, true, false, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(true, false)},
                {false, true, false, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(true, false)},
                {false, false, true, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(false, true)},
                {true, false, true, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(true, true)},
                {false, true, true, generateRecoveryChannelInfoDTO(true, true),
                        generateRecoveryChannelInfoDTO(true, true)}};
    }

    private RecoveryChannelInfoDTO generateRecoveryChannelInfoDTO(boolean isIncludeEmail, boolean isIncludeSMS) {

        RecoveryChannelInfoDTO recoveryChannelInfoDTO = new RecoveryChannelInfoDTO();
        recoveryChannelInfoDTO.setUsername(USERNAME);
        recoveryChannelInfoDTO.setRecoveryFlowId(FLOW_ID);
        recoveryChannelInfoDTO.setRecoveryCode(RECOVERY_CODE);

        NotificationChannelDTO notificationChannelDTOEmail = new NotificationChannelDTO();
        notificationChannelDTOEmail.setId(1);
        notificationChannelDTOEmail.setType(EMAIL);
        notificationChannelDTOEmail.setValue(TEST_EMAIL);

        NotificationChannelDTO notificationChannelDTOSms = new NotificationChannelDTO();
        notificationChannelDTOSms.setId(2);
        notificationChannelDTOSms.setType(SMS);
        notificationChannelDTOSms.setValue(TEST_MOBILE);

        if (isIncludeEmail && isIncludeSMS) {
            recoveryChannelInfoDTO.setNotificationChannelDTOs(
                    new NotificationChannelDTO[]{notificationChannelDTOEmail, notificationChannelDTOSms});
        } else if (isIncludeEmail) {
            recoveryChannelInfoDTO.setNotificationChannelDTOs(
                    new NotificationChannelDTO[]{notificationChannelDTOEmail});
        } else if (isIncludeSMS) {
            recoveryChannelInfoDTO.setNotificationChannelDTOs(new NotificationChannelDTO[]{notificationChannelDTOSms});
        }

        return recoveryChannelInfoDTO;
    }

    @Test(dataProvider = "generateInitiateData")
    public void testInitiate(boolean isEmailLinkEnabled, boolean isEmailOtpEnabled, boolean isSmsOtpEnabled,
                             RecoveryChannelInfoDTO recoveryChannelInfoDTO,
                             RecoveryChannelInfoDTO expectedRecoveryChannelInfo) throws IdentityRecoveryException {

        HashMap<String, String> claims = new HashMap<>();

        HashMap<String, String> properties = new HashMap<>();

        if (isEmailLinkEnabled) {
            utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_EMAIL_LINK_ENABLE, TENANT_DOMAIN))
                    .thenReturn(TRUE_STRING);
        }

        if (isEmailOtpEnabled) {
            utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL, TENANT_DOMAIN))
                    .thenReturn(TRUE_STRING);
        }

        if (isSmsOtpEnabled) {
            utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                            IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SMS_OTP_ENABLE, TENANT_DOMAIN))
                    .thenReturn(TRUE_STRING);
        }

        PasswordRecoveryManagerImpl passwordRecoveryManager = new PasswordRecoveryManagerImpl();

        utilsMockedStatic.when(
                () -> Utils.getRecoveryConfigs(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                        TENANT_DOMAIN)).thenReturn(TRUE_STRING);

        userAccountRecoveryManagerMockedStatic.when(UserAccountRecoveryManager::getInstance)
                .thenReturn(userAccountRecoveryManager);

        when(userAccountRecoveryManager.retrieveUserRecoveryInformation(claims, TENANT_DOMAIN,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, properties)).thenReturn(recoveryChannelInfoDTO);

        RecoveryInformationDTO recoveryInformationDTO =
                passwordRecoveryManager.initiate(claims, TENANT_DOMAIN, properties);

        assertEquals(recoveryInformationDTO.getUsername(), expectedRecoveryChannelInfo.getUsername());
        assertEquals(recoveryInformationDTO.getRecoveryFlowId(), expectedRecoveryChannelInfo.getRecoveryFlowId());

        NotificationChannelDTO[] expectedChannels = expectedRecoveryChannelInfo.getNotificationChannelDTOs();
        NotificationChannelDTO[] actualChannels =
                recoveryInformationDTO.getRecoveryChannelInfoDTO().getNotificationChannelDTOs();
        assertEquals(
                recoveryInformationDTO.getRecoveryChannelInfoDTO().getNotificationChannelDTOs().length,
                expectedRecoveryChannelInfo.getNotificationChannelDTOs().length, "Array lengths do not match");

        for (int i = 0; i < expectedRecoveryChannelInfo.getNotificationChannelDTOs().length; i++) {
            assertEquals(expectedChannels[i].getId(), actualChannels[i].getId(), "Mismatch at channel id: " + i);
            assertEquals(expectedChannels[i].getType(), actualChannels[i].getType(), "Mismatch at channel id: " + i);
            assertEquals(expectedChannels[i].getValue(), actualChannels[i].getValue(), "Mismatch at channel id:" + i);
        }
    }

    @Test
    public void testEmailOtpRecoveryConfigError() throws IdentityRecoveryException {

        HashMap<String, String> claims = new HashMap<>();
        String tenantDomain = "carbon.super";
        HashMap<String, String> properties = new HashMap<>();
        PasswordRecoveryManagerImpl passwordRecoveryManager = new PasswordRecoveryManagerImpl();

        utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, tenantDomain))
                .thenReturn(TRUE_STRING);

        userAccountRecoveryManagerMockedStatic.when(UserAccountRecoveryManager::getInstance)
                .thenReturn(userAccountRecoveryManager);

        NotificationChannelDTO notificationChannelDTOEmail = new NotificationChannelDTO();
        notificationChannelDTOEmail.setId(1);
        notificationChannelDTOEmail.setType(EMAIL);
        notificationChannelDTOEmail.setValue(TEST_EMAIL);

        RecoveryChannelInfoDTO recoveryChannelInfoDTO = new RecoveryChannelInfoDTO();
        recoveryChannelInfoDTO.setNotificationChannelDTOs(new NotificationChannelDTO[]{
                notificationChannelDTOEmail
        });

        when(userAccountRecoveryManager.retrieveUserRecoveryInformation(claims, tenantDomain,
                RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY, properties))
                .thenReturn(recoveryChannelInfoDTO);

        utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_EMAIL_LINK_ENABLE, tenantDomain))
                .thenReturn(FALSE_STRING);

        utilsMockedStatic.when(() -> Utils.getRecoveryConfigs(
                        IdentityRecoveryConstants.ConnectorConfig.PASSWORD_RECOVERY_SEND_OTP_IN_EMAIL, tenantDomain))
                .thenThrow(new IdentityRecoveryServerException("test error"));

        utilsMockedStatic.when(() -> Utils.prependOperationScenarioToErrorCode(anyString(), anyString())).thenReturn(
                "11343");

        utilsMockedStatic.when(() -> Utils.handleServerException(anyString(), anyString(), nullable(String.class))).
                thenReturn(new IdentityRecoveryServerException("test error."));

        assertThrows(IdentityRecoveryServerException.class,
                () -> passwordRecoveryManager.initiate(claims, tenantDomain, properties));
    }

    @Test
    public void testResend() throws IdentityRecoveryException {

        String resendCode = "test-resend-code";
        Map<String, String> properties = new HashMap<>();
        String resendPasswordResetTemplateName = "resendPasswordReset";
        String resendPasswordResetOTPTemplateName = "resendpasswordresetotp";

        ResendConfirmationDTO resendConfirmationDTO = new ResendConfirmationDTO();

        PasswordRecoveryManagerImpl passwordRecoveryManager = new PasswordRecoveryManagerImpl();
        when(resendConfirmationManager.resendConfirmation(anyString(), anyString(), anyString(), anyString(),
                anyString(), any())).thenReturn(resendConfirmationDTO);

        // Case 1: Email OTP password reset is disabled.
        passwordRecoveryManager.resend(TENANT_DOMAIN, resendCode, properties);

        verify(resendConfirmationManager).resendConfirmation(eq(TENANT_DOMAIN), eq(resendCode),
                eq(RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name()), eq(RecoverySteps.UPDATE_PASSWORD.name()),
                eq(resendPasswordResetTemplateName), any());

        // Case 2: Email OTP password reset is enabled.
        utilsMockedStatic.when(() -> Utils.isPasswordRecoveryEmailOtpEnabled(TENANT_DOMAIN)).thenReturn(true);

        passwordRecoveryManager.resend(TENANT_DOMAIN, resendCode, properties);
        verify(resendConfirmationManager).resendConfirmation(eq(TENANT_DOMAIN), eq(resendCode),
                eq(RecoveryScenarios.NOTIFICATION_BASED_PW_RECOVERY.name()), eq(RecoverySteps.UPDATE_PASSWORD.name()),
                eq(resendPasswordResetOTPTemplateName), any());
    }
}
