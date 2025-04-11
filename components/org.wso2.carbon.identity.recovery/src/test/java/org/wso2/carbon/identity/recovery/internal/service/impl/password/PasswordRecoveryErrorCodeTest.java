package org.wso2.carbon.identity.recovery.internal.service.impl.password;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.base.IdentityConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.password.NotificationPasswordRecoveryManager;
import org.wso2.carbon.identity.user.action.api.exception.UserActionExecutionClientException;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;
import static org.wso2.carbon.identity.user.action.api.constant.UserActionError.PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED;

public class PasswordRecoveryErrorCodeTest {

    @InjectMocks
    private PasswordRecoveryManagerImpl recoveryManager;

    @Mock
    private NotificationPasswordRecoveryManager notificationManagerMock;

    private AutoCloseable mocks;

    @BeforeMethod
    public void init() {

        mocks = openMocks(this);
    }

    @Test
    void testV2PasswordRecoveryErrorFromActionExtensionFailure() throws Exception {

        String resetCode = "aBc6eD";
        char[] password = "StrongPass@123".toCharArray();
        String messageFromExtension = "invalid_format";
        String descriptionFromExtension = "Invalid password format";
        String exceptionErrorCode =
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_PRE_UPDATE_PASSWORD_ACTION_FAILURE.getCode();
        Map<String, String> properties = new HashMap<>();

        doThrow(new IdentityRecoveryClientException(exceptionErrorCode, messageFromExtension, descriptionFromExtension,
                new UserActionExecutionClientException(PRE_UPDATE_PASSWORD_ACTION_EXECUTION_FAILED,
                        messageFromExtension, descriptionFromExtension))).when(notificationManagerMock)
                .updatePassword(anyString(), anyString(), any());

        try (MockedStatic<NotificationPasswordRecoveryManager> staticMock =
                     mockStatic(NotificationPasswordRecoveryManager.class);
             MockedStatic<IdentityUtil> identityUtilMock = mockStatic(IdentityUtil.class)) {

            identityUtilMock.when(() -> IdentityUtil.getProperty(IdentityConstants.Recovery.RECOVERY_V1_API_ENABLE))
                    .thenReturn("true");
            staticMock.when(NotificationPasswordRecoveryManager::getInstance)
                    .thenReturn(notificationManagerMock);

            try {
                recoveryManager.reset(resetCode, password, properties);
                fail("Expected IdentityRecoveryClientException was not thrown");
            } catch (IdentityRecoveryClientException ex) {
                assertEquals(ex.getErrorCode(), exceptionErrorCode);
                assertEquals(ex.getMessage(), messageFromExtension);
                assertEquals(ex.getDescription(), descriptionFromExtension);
            }
        }
    }

    @AfterMethod
    public void tearDown() {

        if (mocks != null) {
            try {
                mocks.close();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }
}
