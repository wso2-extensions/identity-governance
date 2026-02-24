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

package org.wso2.carbon.identity.recovery.store;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.services.IdentityEventService;
import org.wso2.carbon.identity.flow.mgt.model.FlowConfigDTO;
import org.wso2.carbon.identity.flow.mgt.utils.FlowMgtConfigUtils;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryServerException;
import org.wso2.carbon.identity.recovery.RecoveryScenarios;
import org.wso2.carbon.identity.recovery.RecoverySteps;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;
import org.wso2.carbon.identity.recovery.util.Utils;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Calendar;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Unit tests for JDBCRecoveryDataStore.
 */
public class JDBCRecoveryDataStoreTest {

    private UserRecoveryDataStore userRecoveryDataStore;

    @Mock
    private Connection mockConnection;

    @Mock
    private PreparedStatement mockPreparedStatement;

    @Mock
    private ResultSet mockResultSet;

    @Mock
    private IdentityRecoveryServiceDataHolder identityRecoveryServiceDataHolder;

    @Mock
    private IdentityEventService identityEventService;

    private MockedStatic<IdentityDatabaseUtil> mockedIdentityDatabaseUtil;
    private MockedStatic<IdentityTenantUtil> mockedIdentityTenantUtils;
    private MockedStatic<IdentityUtil> mockedIdentityUtil;
    private MockedStatic<Utils> mockedUtils;
    private MockedStatic<IdentityRecoveryServiceDataHolder> mockedIdentityRecoveryServiceDataHolder;
    private FlowConfigDTO mockedFlowConfigDTO;

    private static final int TEST_TENANT_ID = 12;
    private static final String TEST_TENANT_DOMAIN = "test.com";
    private static final String TEST_USER_NAME = "testUser";
    private static final String TEST_USER_STORE_DOMAIN = "testUserStore";
    private static final String TEST_SECRET_CODE = "test-sec";

    @BeforeMethod
    public void setUp() throws Exception {

        MockitoAnnotations.openMocks(this);
        userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();

        mockedIdentityDatabaseUtil = mockStatic(IdentityDatabaseUtil.class);
        mockedIdentityTenantUtils = mockStatic(IdentityTenantUtil.class);
        mockedIdentityUtil = mockStatic(IdentityUtil.class);
        mockedUtils = mockStatic(Utils.class);
        mockedIdentityRecoveryServiceDataHolder = mockStatic(IdentityRecoveryServiceDataHolder.class);
        mockedFlowConfigDTO = mock(FlowConfigDTO.class);

        mockedIdentityRecoveryServiceDataHolder.when(IdentityRecoveryServiceDataHolder::getInstance)
                .thenReturn(identityRecoveryServiceDataHolder);
        mockedIdentityDatabaseUtil.when(() -> IdentityDatabaseUtil.getDBConnection(anyBoolean()))
                .thenReturn(mockConnection);
        mockedIdentityTenantUtils.when(() -> IdentityTenantUtil.getTenantId(TEST_TENANT_DOMAIN))
                .thenReturn(TEST_TENANT_ID);
        mockedIdentityTenantUtils.when(() -> IdentityTenantUtil.getTenantDomain(TEST_TENANT_ID))
                .thenReturn(TEST_TENANT_DOMAIN);
        mockedIdentityUtil.when(() -> IdentityUtil.isUserStoreCaseSensitive(anyString(), anyInt()))
                .thenReturn(true);

        when(mockedFlowConfigDTO.getIsEnabled()).thenReturn(false);
        when(identityRecoveryServiceDataHolder.getIdentityEventService()).thenReturn(identityEventService);
        when(mockConnection.prepareStatement(anyString())).thenReturn(mockPreparedStatement);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityDatabaseUtil.close();
        mockedIdentityTenantUtils.close();
        mockedIdentityUtil.close();
        mockedUtils.close();
        mockedIdentityRecoveryServiceDataHolder.close();

        reset(mockConnection, mockPreparedStatement, mockResultSet);
    }

    @Test
    public void testStore() throws Exception {

        UserRecoveryData recoveryData = createSampleUserRecoveryData();

        userRecoveryDataStore.store(recoveryData);

        verify(mockPreparedStatement).setString(1, recoveryData.getUser().getUserName());
        verify(mockPreparedStatement).setString(2,
                recoveryData.getUser().getUserStoreDomain().toUpperCase());
        verify(mockPreparedStatement).setInt(3, TEST_TENANT_ID);
        verify(mockPreparedStatement).setString(4, TEST_SECRET_CODE);
        verify(mockPreparedStatement).setString(5, String.valueOf(recoveryData.getRecoveryScenario()));
        verify(mockPreparedStatement).setString(6, String.valueOf(recoveryData.getRecoveryStep()));
        verify(mockPreparedStatement).execute();
        mockedIdentityDatabaseUtil.verify(() -> IdentityDatabaseUtil.commitTransaction(mockConnection));
        mockedIdentityDatabaseUtil.verify(() -> IdentityDatabaseUtil.closeConnection(mockConnection));

        // Case 2: SQL Exception.
        mockUtilsErrors();
        doThrow(new SQLException()).when(mockPreparedStatement).execute();
        try {
            userRecoveryDataStore.store(recoveryData);
            fail("Expected IdentityRecoveryException was not thrown.");
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryServerException);
        }
        mockedIdentityDatabaseUtil.verify(() -> IdentityDatabaseUtil.closeConnection(mockConnection),
                times(2));
    }

    @DataProvider(name = "loadData")
    private Object[][] loadData() {

        return new Object[][] {
                { RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER },
                { RecoveryScenarios.MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER },
                { RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER },
                { RecoveryScenarios.PROGRESSIVE_PROFILE_MOBILE_VERIFICATION_ON_VERIFIED_LIST_UPDATE,
                        RecoverySteps.VERIFY_MOBILE_NUMBER },
                { RecoveryScenarios.EMAIL_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_EMAIL },
                { RecoveryScenarios.EMAIL_VERIFICATION_ON_VERIFIED_LIST_UPDATE, RecoverySteps.VERIFY_EMAIL },
                { RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD, RecoverySteps.VERIFY_EMAIL },
                { RecoveryScenarios.SELF_SIGN_UP, RecoverySteps.CONFIRM_SIGN_UP },
                { RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD },
                { RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP, RecoverySteps.SET_PASSWORD },
                { RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP, RecoverySteps.SET_PASSWORD },
                { RecoveryScenarios.TENANT_ADMIN_ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD },
                { RecoveryScenarios.LITE_SIGN_UP, RecoverySteps.CONFIRM_LITE_SIGN_UP },
                { RecoveryScenarios.LITE_SIGN_UP, RecoverySteps.VALIDATE_CHALLENGE_QUESTION },
                { RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_OTP, RecoverySteps.UPDATE_PASSWORD },
                { RecoveryScenarios.ADMIN_FORCED_PASSWORD_RESET_VIA_EMAIL_LINK, RecoverySteps.UPDATE_PASSWORD },
                { RecoveryScenarios.ADMIN_INVITE_SET_PASSWORD_OFFLINE, RecoverySteps.UPDATE_PASSWORD }
        };
    }

    @Test(dataProvider = "loadData")
    public void testLoadSuccessful(RecoveryScenarios recoveryScenario, RecoverySteps recoveryStep) throws Exception {

        User user = createSampleUser();
        String remainingSetId = "0777898721";

        when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
        when(mockResultSet.next()).thenReturn(true);
        when(mockResultSet.getString("REMAINING_SETS")).thenReturn(remainingSetId);
        when(mockResultSet.getTimestamp(eq("TIME_CREATED"), any(Calendar.class)))
                .thenReturn(new Timestamp(System.currentTimeMillis() - 60000));

        mockExpiryTimes();
        UserRecoveryData result = userRecoveryDataStore.load(user, recoveryScenario, recoveryStep, TEST_SECRET_CODE);

        assertNotNull(result);
        assertEquals(result.getUser().getUserName(), user.getUserName());
        assertEquals(result.getSecret(), TEST_SECRET_CODE);
        assertEquals(result.getRecoveryScenario(), recoveryScenario);
        assertEquals(result.getRecoveryStep(), recoveryStep);
        assertEquals(result.getRemainingSetIds(), remainingSetId);

        verify(identityEventService, times(2)).handleEvent(any());
        mockedIdentityDatabaseUtil.verify(() -> IdentityDatabaseUtil
                .closeAllConnections(mockConnection, mockResultSet, mockPreparedStatement));
    }

    @Test()
    public void testLoadExpiredCode() throws Exception {

        User user = createSampleUser();
        String remainingSetId = "0777898721";

        when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
        when(mockResultSet.next()).thenReturn(true);
        when(mockResultSet.getString("REMAINING_SETS")).thenReturn(remainingSetId);
        when(mockResultSet.getTimestamp(eq("TIME_CREATED"), any(Calendar.class)))
                .thenReturn(new Timestamp(System.currentTimeMillis() - 660000));

        mockExpiryTimes();
        mockUtilsErrors();
        try {
            userRecoveryDataStore.load(user,
                    RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE, RecoverySteps.VERIFY_MOBILE_NUMBER,
                    TEST_SECRET_CODE);
            fail();
        } catch (Exception e) {
            assertTrue(e instanceof IdentityRecoveryClientException);
        }
    }

    @DataProvider(name = "askPasswordUserOnboardScenarios")
    private Object[][] askPasswordUserOnboardScenarios() {

        return new Object[][] {
                { RecoveryScenarios.ASK_PASSWORD, RecoverySteps.UPDATE_PASSWORD },
                { RecoveryScenarios.ASK_PASSWORD_VIA_EMAIL_OTP, RecoverySteps.SET_PASSWORD },
                { RecoveryScenarios.ASK_PASSWORD_VIA_SMS_OTP, RecoverySteps.SET_PASSWORD }
        };
    }

    @Test(dataProvider = "askPasswordUserOnboardScenarios")
    public void testLoadExpiredAskPasswordOTPCode(RecoveryScenarios recoveryScenario, RecoverySteps recoveryStep) throws Exception {

        when(mockPreparedStatement.executeQuery()).thenReturn(mockResultSet);
        when(mockResultSet.next()).thenReturn(true);
        when(mockResultSet.getTimestamp(eq("TIME_CREATED"), any(Calendar.class)))
                .thenReturn(new Timestamp(System.currentTimeMillis() - 660000));
        when(mockResultSet.getString("SCENARIO")).thenReturn(recoveryScenario.toString());
        when(mockResultSet.getString("STEP")).thenReturn(recoveryStep.toString());
        when(mockResultSet.getInt("TENANT_ID")).thenReturn(TEST_TENANT_ID);

        mockExpiryTimes();
        mockUtilsErrors();
        try (MockedStatic<FlowMgtConfigUtils> mockedStatic = mockStatic(FlowMgtConfigUtils.class)) {

            mockedStatic.when(() -> FlowMgtConfigUtils.getFlowConfig(any(), any()))
                    .thenReturn(mockedFlowConfigDTO);

            try {
                userRecoveryDataStore.load(TEST_SECRET_CODE, false);
                fail();
            } catch (Exception e) {
                assertTrue(e instanceof IdentityRecoveryClientException);
            }
        }
    }

    private void mockExpiryTimes() {

        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedIdentityUtil.when(() -> IdentityUtil.getProperty(IdentityRecoveryConstants
                        .ConnectorConfig.TENANT_ADMIN_ASK_PASSWORD_EXPIRY_TIME))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.ADMIN_PASSWORD_RESET_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.SELF_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.SELF_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.RESEND_CODE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.LITE_REGISTRATION_VERIFICATION_CODE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.LITE_REGISTRATION_SMSOTP_VERIFICATION_CODE_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
        mockedUtils.when(() -> Utils.getRecoveryConfigs(IdentityRecoveryConstants
                        .ConnectorConfig.ASK_PASSWORD_EXPIRY_TIME, TEST_TENANT_DOMAIN))
                .thenReturn("10");
    }

    private void mockUtilsErrors() {

        mockedUtils.when(() -> Utils.handleServerException(
                IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_ISSUE_IN_LOADING_RECOVERY_CONFIGS, null))
                .thenReturn(IdentityException.error(IdentityRecoveryServerException.class,
                        "err-code",
                        ""));

        mockedUtils.when(() -> Utils.handleServerException(
                        any(IdentityRecoveryConstants.ErrorMessages.class),
                        isNull(),
                        any()))
                .thenReturn(IdentityException.error(IdentityRecoveryServerException.class,
                        "err-code",
                        new Throwable()));

        mockedUtils.when(() -> Utils.handleClientException(any(IdentityRecoveryConstants.ErrorMessages.class),
                anyString())).thenReturn(IdentityException.error(IdentityRecoveryClientException.class,
                        "err-code",
                        ""));
    }

    private UserRecoveryData createSampleUserRecoveryData() {

        User user = createSampleUser();
        return new UserRecoveryData(user, TEST_SECRET_CODE,
                RecoveryScenarios.MOBILE_VERIFICATION_ON_UPDATE,
                RecoverySteps.VERIFY_MOBILE_NUMBER);
    }

    private User createSampleUser() {

        User user = new User();
        user.setUserName(TEST_USER_NAME);
        user.setTenantDomain(TEST_TENANT_DOMAIN);
        user.setUserStoreDomain(TEST_USER_STORE_DOMAIN);
        return user;
    }
}
