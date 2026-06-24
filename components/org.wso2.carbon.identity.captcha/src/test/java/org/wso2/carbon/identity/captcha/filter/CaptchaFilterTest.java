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

package org.wso2.carbon.identity.captcha.filter;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.captcha.connector.CaptchaConnector;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.core.util.IdentityUtil;

import java.io.IOException;
import java.util.Collections;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

/**
 * Unit tests for {@link CaptchaFilter}.
 */
public class CaptchaFilterTest {

    private static final String SESSION_DATA_KEY_VALUE = "test-session-data-key";
    private static final String COMMON_AUTH_URI = "/commonauth";
    private static final String OTHER_URI = "/oauth2/authorize";

    @Mock
    private HttpServletRequest request;

    @Mock
    private HttpServletResponse response;

    @Mock
    private FilterChain filterChain;

    @Mock
    private CaptchaDataHolder captchaDataHolder;

    @Mock
    private CaptchaConnector captchaConnector;

    @Mock
    private CaptchaPreValidationResponse preValidationResponse;

    private CaptchaFilter captchaFilter;
    private MockedStatic<CaptchaDataHolder> captchaDataHolderStatic;
    private MockedStatic<FrameworkUtils> frameworkUtilsStatic;
    private MockedStatic<IdentityUtil> identityUtilStatic;
    private AutoCloseable mocks;

    @BeforeMethod
    public void setUp() throws Exception {

        mocks = openMocks(this);
        captchaFilter = new CaptchaFilter();
        captchaDataHolderStatic = Mockito.mockStatic(CaptchaDataHolder.class);
        frameworkUtilsStatic = Mockito.mockStatic(FrameworkUtils.class);
        identityUtilStatic = Mockito.mockStatic(IdentityUtil.class);

        captchaDataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(captchaDataHolder);
        when(captchaDataHolder.getReCaptchaRequestWrapUrls()).thenReturn("");
        when(captchaDataHolder.getCaptchaConnectors()).thenReturn(Collections.singletonList(captchaConnector));

        when(captchaConnector.canHandle(any(), any())).thenReturn(true);
        when(captchaConnector.getPriority()).thenReturn(10);
        when(captchaConnector.preValidate(any(), any())).thenReturn(preValidationResponse);
        when(captchaConnector.verifyCaptcha(any(), any())).thenReturn(true);

        when(preValidationResponse.isCaptchaValidationRequired()).thenReturn(true);
        when(preValidationResponse.isEnableCaptchaForRequestPath()).thenReturn(false);
        when(preValidationResponse.isPostValidationRequired()).thenReturn(false);
        when(preValidationResponse.isMaxFailedLimitReached()).thenReturn(false);
        when(preValidationResponse.getWrappedHttpServletRequest()).thenReturn(null);

        frameworkUtilsStatic.when(() -> FrameworkUtils.getContextData(request)).thenReturn(null);
    }

    @AfterMethod
    public void tearDown() throws Exception {

        captchaDataHolderStatic.close();
        frameworkUtilsStatic.close();
        identityUtilStatic.close();
        if (mocks != null) {
            mocks.close();
        }
    }

    @Test(description = "When reCaptcha is globally disabled the filter must pass through immediately, " +
            "regardless of any console-login property or URI")
    public void testDoFilter_ReCaptchaGloballyDisabled_AlwaysPassThrough()
            throws IOException, ServletException, CaptchaException {

        when(captchaDataHolder.isReCaptchaEnabled()).thenReturn(false);
        when(request.getRequestURI()).thenReturn(OTHER_URI);

        captchaFilter.doFilter(request, response, filterChain);

        verify(filterChain).doFilter(request, response);
        verify(captchaConnector, never()).verifyCaptcha(any(), any());
    }

    /**
     * Tests {@code isCaptchaDisabledForConsoleLogin} with reCaptcha globally enabled.
     * Because the early-exit condition is {@code !isReCaptchaEnabled() || isCaptchaDisabledForConsoleLogin(...)},
     * with reCaptcha ON the only way to bypass captcha is via a valid console-login request.
     *
     * Each row: disableFlag, requestUri, sessionDataKey (null = absent), spName (null = no context), expectSkip.
     */
    @DataProvider(name = "consoleLoginBypassScenarios")
    public Object[][] consoleLoginBypassScenarios() {

        return new Object[][]{
            // disableFlag, requestUri,      sessionDataKey,        spName,    expectSkip
            {"true",  COMMON_AUTH_URI, SESSION_DATA_KEY_VALUE, "Console", true},   // all conditions met → bypass
            {"false", COMMON_AUTH_URI, SESSION_DATA_KEY_VALUE, "Console", false},  // property disabled → no bypass
            {"true",  OTHER_URI,       SESSION_DATA_KEY_VALUE, "Console", false},  // wrong URI → no bypass
            {"true",  COMMON_AUTH_URI, null,                   "Console", false},  // no sessionDataKey → no bypass
            {"true",  COMMON_AUTH_URI, SESSION_DATA_KEY_VALUE, null,      false},  // context not in cache → no bypass
            {"true",  COMMON_AUTH_URI, SESSION_DATA_KEY_VALUE, "MyApp",   false},  // non-Console app → no bypass
        };
    }

    @Test(description = "Verify console-login captcha bypass when reCaptcha is globally enabled",
          dataProvider = "consoleLoginBypassScenarios")
    public void testDoFilter_ReCaptchaEnabled_ConsoleLoginBypassBehavior(String disableFlag, String requestUri,
                                                                         String sessionDataKey, String spName,
                                                                         boolean expectSkip)
            throws IOException, ServletException, CaptchaException {

        // reCaptcha is globally enabled — bypass is only possible via isCaptchaDisabledForConsoleLogin.
        when(captchaDataHolder.isReCaptchaEnabled()).thenReturn(true);
        when(request.getRequestURI()).thenReturn(requestUri);
        when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(sessionDataKey);

        AuthenticationContext context = null;
        if (spName != null && sessionDataKey != null) {
            context = mock(AuthenticationContext.class);
            when(context.getServiceProviderName()).thenReturn(spName);
        }
        frameworkUtilsStatic.when(
                () -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_DATA_KEY_VALUE)).thenReturn(context);

        identityUtilStatic.when(
                () -> IdentityUtil.getProperty(CaptchaConstants.DISABLE_CAPTCHA_FOR_CONSOLE_LOGIN))
                .thenReturn(disableFlag);

        captchaFilter.doFilter(request, response, filterChain);

        if (expectSkip) {
            verify(filterChain).doFilter(request, response);
            verify(captchaConnector, never()).verifyCaptcha(any(), any());
        } else {
            verify(captchaConnector).verifyCaptcha(any(), any());
        }
    }
}
