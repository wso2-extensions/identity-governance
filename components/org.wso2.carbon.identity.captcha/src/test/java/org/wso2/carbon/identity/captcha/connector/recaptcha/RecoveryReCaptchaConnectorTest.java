/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import java.util.Collections;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for recovery reCAPTCHA connectors.
 */
public class RecoveryReCaptchaConnectorTest {

    private static final String RECOVER_V1_USERNAME_URL = "/api/users/v1/recovery/username/init";
    private static final String RECOVER_V2_USERNAME_URL = "/api/users/v2/recovery/username/init";
    private static final String RECOVER_V1_PASSWORD_URL = "/api/users/v1/recovery/password/init";
    private static final String RECOVER_V2_PASSWORD_URL = "/api/users/v2/recovery/password/init";

    @DataProvider(name = "usernameRecoveryApiPaths")
    public Object[][] usernameRecoveryApiPaths() {

        return new Object[][]{
                {RECOVER_V1_USERNAME_URL},
                {RECOVER_V2_USERNAME_URL}
        };
    }

    @DataProvider(name = "passwordRecoveryApiPaths")
    public Object[][] passwordRecoveryApiPaths() {

        return new Object[][]{
                {RECOVER_V1_PASSWORD_URL},
                {RECOVER_V2_PASSWORD_URL}
        };
    }

    @Test(dataProvider = "usernameRecoveryApiPaths")
    public void testUsernameRecoveryConnectorCanHandleV1AndV2ApiPaths(String path) throws CaptchaException {

        UsernameRecoveryReCaptchaConnector connector = new UsernameRecoveryReCaptchaConnector();
        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        HttpServletRequest request = getRequest(path);

        try (MockedStatic<CaptchaDataHolder> dataHolderStatic = Mockito.mockStatic(CaptchaDataHolder.class);
                MockedStatic<CaptchaUtil> captchaUtilStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            dataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(true);
            mockPathAvailability(captchaUtilStatic);

            assertTrue(connector.canHandle(request, mock(HttpServletResponse.class)));
        }
    }

    @Test(dataProvider = "usernameRecoveryApiPaths")
    public void testUsernameRecoveryConnectorPreValidateRequiresCaptchaForV1AndV2ApiPaths(String path)
            throws CaptchaException {

        UsernameRecoveryReCaptchaConnector connector = new UsernameRecoveryReCaptchaConnector();
        HttpServletRequest request = getRequest(path);
        HttpServletResponse response = mock(HttpServletResponse.class);

        try (MockedStatic<CaptchaUtil> captchaUtilStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            mockPathAvailability(captchaUtilStatic);

            CaptchaPreValidationResponse preValidationResponse = connector.preValidate(request, response);

            assertTrue(preValidationResponse.isCaptchaValidationRequired());
            Mockito.verify(response).setHeader("reCaptcha", "conditional");
        }
    }

    @Test(dataProvider = "passwordRecoveryApiPaths")
    public void testPasswordRecoveryConnectorCanHandleV1AndV2ApiPaths(String path) throws CaptchaException {

        PasswordRecoveryReCaptchaConnector connector = new PasswordRecoveryReCaptchaConnector();
        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        HttpServletRequest request = getRequest(path);

        try (MockedStatic<CaptchaDataHolder> dataHolderStatic = Mockito.mockStatic(CaptchaDataHolder.class);
                MockedStatic<CaptchaUtil> captchaUtilStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            dataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            when(dataHolder.getReCaptchaBypassedApiEndpoints()).thenReturn(Collections.emptyList());
            mockPathAvailability(captchaUtilStatic);

            assertTrue(connector.canHandle(request, mock(HttpServletResponse.class)));
        }
    }

    @Test(dataProvider = "passwordRecoveryApiPaths")
    public void testPasswordRecoveryConnectorPreValidateRequiresCaptchaForV1AndV2ApiPaths(String path)
            throws CaptchaException {

        PasswordRecoveryReCaptchaConnector connector = new PasswordRecoveryReCaptchaConnector();
        connector.init(mock(IdentityGovernanceService.class));
        CaptchaDataHolder dataHolder = mock(CaptchaDataHolder.class);
        HttpServletRequest request = getRequest(path);

        try (MockedStatic<CaptchaDataHolder> dataHolderStatic = Mockito.mockStatic(CaptchaDataHolder.class);
                MockedStatic<CaptchaUtil> captchaUtilStatic = Mockito.mockStatic(CaptchaUtil.class)) {
            dataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
            when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
            captchaUtilStatic.when(() -> CaptchaUtil.isRecaptchaEnabledForConnector(
                    any(IdentityGovernanceService.class), any(ServletRequest.class), anyString())).thenReturn(true);
            mockPathAvailability(captchaUtilStatic);

            CaptchaPreValidationResponse preValidationResponse =
                    connector.preValidate(request, mock(HttpServletResponse.class));

            assertTrue(preValidationResponse.isCaptchaValidationRequired());
        }
    }

    private HttpServletRequest getRequest(String path) {

        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getRequestURI()).thenReturn(path);
        return request;
    }

    private void mockPathAvailability(MockedStatic<CaptchaUtil> captchaUtilStatic) {

        captchaUtilStatic.when(() -> CaptchaUtil.isPathAvailable(anyString(), anyString()))
                .thenAnswer(invocation -> invocation.getArgument(0).equals(invocation.getArgument(1)));
    }
}
