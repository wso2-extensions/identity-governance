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

package org.wso2.carbon.identity.captcha.connector.recaptcha;

import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.ApplicationAuthenticator;
import org.wso2.carbon.identity.application.authentication.framework.AuthenticationFlowHandler;
import org.wso2.carbon.identity.application.authentication.framework.config.model.AuthenticatorConfig;
import org.wso2.carbon.identity.application.authentication.framework.context.AuthenticationContext;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedIdPData;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.authentication.framework.util.FrameworkUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.captcha.connector.CaptchaPostValidationResponse;
import org.wso2.carbon.identity.captcha.connector.CaptchaPreValidationResponse;
import org.wso2.carbon.identity.captcha.exception.CaptchaException;
import org.wso2.carbon.identity.captcha.internal.CaptchaDataHolder;
import org.wso2.carbon.identity.captcha.util.CaptchaConstants;
import org.wso2.carbon.identity.captcha.util.CaptchaUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

/**
 * Unit tests for {@link AbstractOTPCaptchaConnector}.
 */
public class AbstractOTPCaptchaConnectorTest {

    private static final String SESSION_KEY = "dummy-session-key";
    private static final String AUTHENTICATOR_NAME = "email-otp-authenticator";
    private static final String REDIRECT_KEY = "isRedirectToEmailOTP";
    private static final String RESEND_PARAM = "resendCode";
    private static final String FAIL_CLAIM = "http://wso2.org/claims/identity/failedEmailOtpAttempts";
    private static final String FAIL_URL = "/authenticationendpoint/email_otp.do";
    private static final String OTP_PARAM_NAME = "OTPCode";
    private static final String TENANT_DOMAIN = "example.com";
    private static final String USERNAME = "alice@example.com";

    @Mock
    private IdentityGovernanceService governanceService;

    @Mock
    private HttpServletRequest request;

    @Mock
    private ServletResponse response;

    @Mock
    private AuthenticationContext context;

    @Mock
    private AuthenticatedUser user;

    @Mock
    private CaptchaDataHolder dataHolder;

    private TestOTPCaptchaConnector connector;
    private MockedStatic<CaptchaUtil> captchaUtilStatic;
    private MockedStatic<FrameworkUtils> frameworkUtilsStatic;
    private MockedStatic<CaptchaDataHolder> captchaDataHolderStatic;
    private MockedStatic<CaptchaConstants> captchaConstantsStatic;

    /**
     * Concrete test connector that implements the abstract hooks.
     */
    private static class TestOTPCaptchaConnector extends AbstractOTPCaptchaConnector {

        @Override
        protected boolean isOTPParamPresent(ServletRequest servletRequest) {

            return servletRequest.getParameter(OTP_PARAM_NAME) != null;
        }

        @Override
        protected String getAuthenticatorName() {

            return AUTHENTICATOR_NAME;
        }

        @Override
        protected String getRedirectContextPropertyKey() {

            return REDIRECT_KEY;
        }

        @Override
        protected String getResendParamName() {

            return RESEND_PARAM;
        }

        @Override
        protected String getFailedAttemptsClaimUri() {

            return FAIL_CLAIM;
        }

        @Override
        protected String getOnFailRedirectUrl() {

            return FAIL_URL;
        }
    }

    @BeforeMethod
    public void setUp() {

        openMocks(this);
        connector = new TestOTPCaptchaConnector();
        connector.init(governanceService);

        captchaUtilStatic = Mockito.mockStatic(CaptchaUtil.class);
        frameworkUtilsStatic = Mockito.mockStatic(FrameworkUtils.class);
        captchaDataHolderStatic = Mockito.mockStatic(CaptchaDataHolder.class);
        captchaConstantsStatic = Mockito.mockStatic(CaptchaConstants.class);

        setupDefaultMocks();
    }

    @AfterMethod
    public void tearDown() {

        captchaUtilStatic.close();
        frameworkUtilsStatic.close();
        captchaDataHolderStatic.close();
        captchaConstantsStatic.close();
    }

    private void setupDefaultMocks() {

        // Default request setup
        when(request.getRequestURI()).thenReturn("/commonauth");
        when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(SESSION_KEY);
        when(request.getParameter(OTP_PARAM_NAME)).thenReturn("123456");
        when(request.getParameter(RESEND_PARAM)).thenReturn("false");

        // Default context setup
        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(context);
        when(context.getCurrentAuthenticator()).thenReturn(AUTHENTICATOR_NAME);
        when(context.getProperty(REDIRECT_KEY)).thenReturn("true");
        when(context.getCurrentStep()).thenReturn(1);
        when(context.getLastAuthenticatedUser()).thenReturn(user);
        when(context.getUserTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Default user setup
        when(user.getUserName()).thenReturn(USERNAME);
        when(user.getTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Default CaptchaUtil setup
        captchaUtilStatic.when(() -> CaptchaUtil.isPathAvailable(anyString(), anyString())).thenReturn(true);
        captchaUtilStatic.when(CaptchaUtil::isCaptchaValidationEnabledForLocalOTPAuthenticators).thenReturn(true);
        captchaUtilStatic.when(() ->
                        CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);

        // Default CaptchaDataHolder setup
        captchaDataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(true);
        when(dataHolder.getReCaptchaErrorRedirectUrls()).thenReturn("https://x,https://y");
        doNothing().when(dataHolder).setReCaptchaErrorRedirectUrls(anyString());

        // Default governance service setup
        setupGovernanceServiceDefaults();

        // Default CaptchaConstants setup
        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("");
    }

    private void setupGovernanceServiceDefaults() {

        Property pAlways = new Property();
        pAlways.setValue("false");
        Property pEnable = new Property();
        pEnable.setValue("true");
        Property pMax = new Property();
        pMax.setValue("3");

        try {
            when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                    .thenReturn(new Property[]{pAlways, pEnable, pMax});
        } catch (Exception ignored) {
            // Ignore exceptions in test setup
        }
    }

    // ---------- canHandle() tests ----------

    @Test
    public void testCanHandle() throws Exception {

        assertTrue(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandlePathNotSecured() throws Exception {

        captchaUtilStatic.when(() -> CaptchaUtil.isPathAvailable(anyString(), anyString())).thenReturn(false);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleGlobalFlagDisabled() throws Exception {

        captchaUtilStatic.when(CaptchaUtil::isCaptchaValidationEnabledForLocalOTPAuthenticators).thenReturn(false);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleSessionKeyMissing() throws Exception {

        when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(null);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleContextNull() throws Exception {

        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(null);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleWrongAuthenticator() throws Exception {

        when(context.getCurrentAuthenticator()).thenReturn("other-authenticator");
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleRedirectFlagFalse() throws Exception {

        when(context.getProperty(REDIRECT_KEY)).thenReturn("false");
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleOtpParamMissing() throws Exception {

        when(request.getParameter(OTP_PARAM_NAME)).thenReturn(null);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleNotFirstStep() throws Exception {

        when(context.getCurrentStep()).thenReturn(2);
        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandlePreviousIdpAreFlowHandlers() throws Exception {

        // Build previous IdPs so that isPreviousIdPAuthenticationFlowHandler() returns true
        AuthenticatorConfig cfg = mock(AuthenticatorConfig.class);
        // application authenticator that also implements AuthenticationFlowHandler
        ApplicationAuthenticator appAuth =
                mock(org.wso2.carbon.identity.application.authentication.framework.ApplicationAuthenticator.class,
                withSettings().extraInterfaces(AuthenticationFlowHandler.class));
        when(cfg.getApplicationAuthenticator()).thenReturn(
                appAuth);

        AuthenticatedIdPData idpData = mock(AuthenticatedIdPData.class);
        when(idpData.getAuthenticators()).thenReturn(Collections.singletonList(cfg));

        Map<String, AuthenticatedIdPData> idps = new HashMap<>();
        idps.put("someIdp", idpData);
        when(context.getCurrentAuthenticatedIdPs()).thenReturn(idps);

        assertFalse(connector.canHandle(request, response));
    }

    @Test
    public void testCanHandleResendTrue() throws Exception {

        when(request.getParameter(RESEND_PARAM)).thenReturn("true");
        assertFalse(connector.canHandle(request, response));
    }

    // ---------- preValidate() tests ----------

    @Test
    public void testPreValidateAlwaysOnSetsCaptcha() throws Exception {

        // Enable ALWAYS through governance config
        Property pAlways = new Property();
        pAlways.setValue("true");
        Property pEnable = new Property();
        pEnable.setValue("false");
        Property pMax = new Property();
        pMax.setValue("5");
        when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                .thenReturn(new Property[]{pAlways});

        CaptchaPreValidationResponse resp = connector.preValidate(request, response);
        assertTrue(resp.isCaptchaValidationRequired());
        assertNotNull(resp.getOnCaptchaFailRedirectUrls());
        assertEquals(resp.getOnCaptchaFailRedirectUrls().get(resp.getOnCaptchaFailRedirectUrls().size() - 1),
                FAIL_URL);
        assertTrue(resp.isPostValidationRequired());
    }

    @Test
    public void testPreValidateMaxAttemptsSetsCaptcha() throws Exception {

        // Always=false; enable attempts path
        Property pAlways = new Property();
        pAlways.setValue("false");
        when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                .thenReturn(new Property[]{pAlways});
        captchaUtilStatic.when(() ->
                        CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);

        CaptchaPreValidationResponse resp = connector.preValidate(request, response);
        assertTrue(resp.isCaptchaValidationRequired());
        assertTrue(resp.isMaxFailedLimitReached());
        assertTrue(resp.isPostValidationRequired());
    }

    @Test(expectedExceptions = CaptchaException.class)
    public void testPreValidateContextNull() throws Exception {

        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(null);
        connector.preValidate(request, response);
    }

    @Test(expectedExceptions = CaptchaException.class)
    public void testPreValidateNoUserAndNoTenant() throws Exception {

        // No last user; no username param → triggers exception path
        when(context.getLastAuthenticatedUser()).thenReturn(null);
        when(request.getParameter("username")).thenReturn(null);
        // getTenantDomainFromContext returns null (both last user & userTenantDomain null)
        when(context.getUserTenantDomain()).thenReturn(null);

        connector.preValidate(request, response);
    }

    // ---------- postValidate() tests ----------

    @Test
    public void testPostValidateSecurityMechanismDisabled() throws Exception {

        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("");
        assertNull(connector.postValidate(request, response));
    }

    @Test
    public void testPostValidateSecurityMechanismEnabled() throws Exception {

        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("on");
        // removeEnabledSecurityMechanism() is void; no-op via static
        CaptchaPostValidationResponse r = connector.postValidate(request, response);
        assertNotNull(r);
        assertFalse(r.isSuccessfulAttempt());
        assertTrue(r.isEnableCaptchaResponsePath());
        assertEquals(r.getCaptchaAttributes().get("reCaptcha"), "true");
    }

    // ---------- isRecaptchaEnabled() tests ----------

    @Test
    public void testIsRecaptchaEnabledForceAllTenantsTrue() throws Exception {

        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(true);
        assertTrue(connector.isRecaptchaEnabled(request));
    }

    @Test
    public void testIsRecaptchaEnabledNoSessionKey() throws Exception {

        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(null);
        assertFalse(connector.isRecaptchaEnabled(request));
    }

    @Test
    public void testIsRecaptchaEnabledNoContextOrUser() throws Exception {

        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(context);
        when(context.getLastAuthenticatedUser()).thenReturn(null);
        assertFalse(connector.isRecaptchaEnabled(request));
    }

    @Test
    public void testIsRecaptchaEnabledConfigArrayInvalid() throws Exception {

        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(context);
        when(context.getLastAuthenticatedUser()).thenReturn(user);
        when(user.getUserName()).thenReturn("bob@example.com");
        when(user.getTenantDomain()).thenReturn(TENANT_DOMAIN);

        try {
            when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                    .thenReturn(new Property[]{});
        } catch (Exception ignored) {
            // Ignore exceptions in test setup
        }
        assertFalse(connector.isRecaptchaEnabled(request));
    }

    @Test
    public void testIsRecaptchaEnabledEnableTrueButNotMaxAttempts() throws Exception {

        // ENABLE_ALWAYS=false, ENABLE=true, but max attempts NOT reached
        Property pAlways = new Property();
        pAlways.setValue("false");
        Property pEnable = new Property();
        pEnable.setValue("true");
        Property pMax = new Property();
        pMax.setValue("5");
        try {
            when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                    .thenReturn(new Property[]{pAlways, pEnable, pMax});
        } catch (Exception ignored) {
            // Ignore exceptions in test setup
        }
        captchaUtilStatic.when(() ->
                        CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(false);

        assertFalse(connector.isRecaptchaEnabled(request));
    }

    @Test
    public void testIsRecaptchaEnabledAllConditionsMet() throws Exception {

        // ENABLE_ALWAYS=false, ENABLE=true, max attempts reached, dataHolder enabled true
        Property pAlways = new Property();
        pAlways.setValue("false");
        Property pEnable = new Property();
        pEnable.setValue("true");
        Property pMax = new Property();
        pMax.setValue("3");
        try {
            when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                    .thenReturn(new Property[]{pAlways, pEnable, pMax});
        } catch (Exception ignored) {
            // Ignore exceptions in test setup
        }

        captchaUtilStatic.when(() ->
                        CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(true);

        assertTrue(connector.isRecaptchaEnabled(request));
    }

    // ---------- helper path coverage: getUserNameFromContext fallback & tenant via context.getUserTenantDomain() ----------

    @Test
    public void testPreValidateUsesUsernameFromRequestAndTenantFromUserTenantDomain() throws Exception {

        // Remove lastAuthenticatedUser to force username fallback from request param
        when(context.getLastAuthenticatedUser()).thenReturn(null);
        when(request.getParameter("username")).thenReturn("charlie@example.com");
        when(context.getUserTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Disable ALWAYS; trigger attempts path
        Property pAlways = new Property();
        pAlways.setValue("false");
        when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                .thenReturn(new Property[]{pAlways});
        captchaUtilStatic.when(() ->
                        CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);

        CaptchaPreValidationResponse resp = connector.preValidate(request, response);
        assertTrue(resp.isCaptchaValidationRequired());
        assertTrue(resp.isPostValidationRequired());
    }
}
