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
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.user.endpoint.impl;

import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryClientException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.bean.NotificationResponseBean;
import org.wso2.carbon.identity.recovery.confirmation.ResendConfirmationManager;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.signup.UserSelfRegistrationManager;
import org.wso2.carbon.identity.user.endpoint.dto.LiteUserRegistrationRequestDTO;
import org.wso2.carbon.identity.user.endpoint.dto.PropertyDTO;
import org.wso2.carbon.identity.user.endpoint.util.Utils;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;

/**
 * Unit test class for LiteApiServiceImpl.
 */
public class LiteApiServiceImplTest {

    private LiteApiServiceImpl liteApiService;

    private static final String TEST_DOMAIN = "test.com";

    @BeforeMethod
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        liteApiService = new LiteApiServiceImpl();
    }

    /**
     * Data provider for litePost test.
     *
     * @return Object[][] with test data.
     * @throws IdentityRecoveryException on to replicate the behaviour when a user already exists.
     */
    @DataProvider
    public Object[][] getLiteUserRegistrationRequestData() throws IdentityRecoveryException {

        // Data for testing umt_parameter_filtering
        LiteUserRegistrationRequestDTO liteUserRegistrationRequestDTO = new LiteUserRegistrationRequestDTO();
        liteUserRegistrationRequestDTO.setEmail("unitTestUser@wso2.com");
        liteUserRegistrationRequestDTO.setProperties(new ArrayList<PropertyDTO>() {
            {
                add(new PropertyDTO() {{
                    setKey("utm_source");
                    setValue("test_utm_source_value");
                }});
                add(new PropertyDTO() {{
                    setKey("not_utm_medium");
                    setValue("test_not_utm_medium_value");
                }});
            }
        });
        UserSelfRegistrationManager mockedUserSelfRegistrationManager =
                Mockito.mock(UserSelfRegistrationManager.class);
        when(mockedUserSelfRegistrationManager.registerLiteUser(any(), any(), any()))
                .thenThrow(new IdentityRecoveryClientException(
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS.getCode(),
                        IdentityRecoveryConstants.ErrorMessages.ERROR_CODE_USER_ALREADY_EXISTS.getMessage()));

        ArgumentCaptor<Property[]> propertiesListCaptor = ArgumentCaptor.forClass(Property[].class);
        ResendConfirmationManager mockedResendConfirmationManager = Mockito.mock(ResendConfirmationManager.class);
        when(mockedResendConfirmationManager.resendConfirmationCode(any(), any(), any(), any(),
                propertiesListCaptor.capture())).thenReturn(new NotificationResponseBean(new User()));

        return new Object[][]{
                {liteUserRegistrationRequestDTO, mockedUserSelfRegistrationManager,
                        mockedResendConfirmationManager, propertiesListCaptor}
        };
    }

    /**
     * Test for litePost method in LiteApiServiceImpl.
     *
     * @param liteUserRegistrationRequestDTO     liteUserRegistrationRequestDTO object to replicate the real request.
     * @param mockedUserSelfRegistrationManager  mocked UserSelfRegistrationManager.
     * @param mockedResendConfirmationManager    mocked ResendConfirmationManager.
     * @param propertiesListCaptor               captor for properties list.
     */
    @Test(dataProvider = "getLiteUserRegistrationRequestData")
    public void testLitePost(LiteUserRegistrationRequestDTO liteUserRegistrationRequestDTO,
                             UserSelfRegistrationManager mockedUserSelfRegistrationManager,
                             ResendConfirmationManager mockedResendConfirmationManager,
                             ArgumentCaptor<Property[]> propertiesListCaptor) {

        try (MockedStatic<IdentityUtil> mockedIdentityUtil = Mockito.mockStatic(IdentityUtil.class);
             MockedStatic<Utils> mockedUtils = Mockito.mockStatic(Utils.class);
             MockedStatic<org.wso2.carbon.identity.recovery.util.Utils> mockedRecoveryUtils =
                     Mockito.mockStatic(org.wso2.carbon.identity.recovery.util.Utils.class)) {

            mockedIdentityUtil.when(IdentityUtil::getPrimaryDomainName).thenReturn(TEST_DOMAIN);
            mockedUtils.when(Utils::getUserSelfRegistrationManager).thenReturn(mockedUserSelfRegistrationManager);
            mockedRecoveryUtils.when(() -> org.wso2.carbon.identity.recovery.util.Utils.getConnectorConfig(
                    anyString(), anyString())).thenReturn("true");
            mockedUtils.when(() -> Utils.getProperties(any(List.class))).thenCallRealMethod();

            mockedUtils.when(Utils::getResendConfirmationManager).thenReturn(mockedResendConfirmationManager);
            liteApiService.litePost(liteUserRegistrationRequestDTO);

            Property[] capturedProperties = propertiesListCaptor.getValue();
            assertEquals(capturedProperties.length, 1);
        }
    }
}
