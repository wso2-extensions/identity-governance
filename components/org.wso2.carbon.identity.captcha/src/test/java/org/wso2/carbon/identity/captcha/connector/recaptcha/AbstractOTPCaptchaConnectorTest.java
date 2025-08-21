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
import org.testng.annotations.DataProvider;
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

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
    private static final int AUTHENTICATOR_PRIORITY = 30;

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
     * Provides test data for canHandle negative test cases.
     * Each array contains: description, setup lambda, expected result
     */
    @DataProvider(name = "canHandleNegativeScenarios")
    public Object[][] canHandleNegativeScenarios() {

        return new Object[][]{
            {"Path not secured", (Runnable) () -> 
                captchaUtilStatic.when(() ->
                        CaptchaUtil.isPathAvailable(anyString(), anyString())).thenReturn(false),
                false},
            {"Global captcha flag disabled", (Runnable) () -> 
                captchaUtilStatic.when(
                        CaptchaUtil::isCaptchaValidationEnabledForLocalOTPAuthenticators).thenReturn(false),
                        false},
            {"Session key missing", (Runnable) () -> 
                when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(null), false},
            {"Authentication context is null", (Runnable) () -> 
                frameworkUtilsStatic.when(() ->
                        FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY)).thenReturn(null), false},
            {"Wrong authenticator in context", (Runnable) () -> 
                when(context.getCurrentAuthenticator()).thenReturn("other-authenticator"), false},
            {"Redirect flag is false", (Runnable) () -> 
                when(context.getProperty(REDIRECT_KEY)).thenReturn("false"), false},
            {"OTP parameter missing from request", (Runnable) () -> 
                when(request.getParameter(OTP_PARAM_NAME)).thenReturn(null), false},
            {"Not first authentication step", (Runnable) () -> 
                when(context.getCurrentStep()).thenReturn(2), false},
            {"Resend parameter is true", (Runnable) () -> 
                when(request.getParameter(RESEND_PARAM)).thenReturn("true"), false}
        };
    }
    
    /**
     * Provides test data for reCAPTCHA enablement scenarios.
     * Each array contains: description, setup lambda, expected result
     */
    @DataProvider(name = "recaptchaEnablementScenarios")
    public Object[][] recaptchaEnablementScenarios() {

        return new Object[][]{
            {"Force enabled for all tenants", (Runnable) () -> 
                when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(true), 
                true},
            {"No session key in request", (Runnable) () -> {
                when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
                when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(null);
            }, false},
            {"No context or user available", (Runnable) () -> {
                when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
                frameworkUtilsStatic.when(() ->
                        FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY)).thenReturn(context);
                when(context.getLastAuthenticatedUser()).thenReturn(null);
            }, false}
        };
    }
    
    /**
     * Provides governance configuration test data.
     * Each array contains: description, properties array, setup lambda
     */
    @DataProvider(name = "governanceConfigScenarios")
    public Object[][] governanceConfigScenarios() {

        return new Object[][]{
            {"Always enable CAPTCHA", new String[]{"true", "false", "3"}, (Runnable) () -> {}},
            {"Enable on max attempts reached", new String[]{"false", "true", "3"}, (Runnable) () -> 
                captchaUtilStatic.when(() -> 
                    CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                        .thenReturn(true)},
            {"Max attempts not reached", new String[]{"false", "true", "3"}, (Runnable) () -> 
                captchaUtilStatic.when(() -> 
                    CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                        .thenReturn(false)}
        };
    }

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

        @Override
        protected int getAuthenticatorPriority() {

            return AUTHENTICATOR_PRIORITY;
        }
    }

    /**
     * Sets up the test environment before each test method.
     * Initialize mocks, creates connector instance, and sets up default mock behaviors.
     */
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

    /**
     * Cleans up resources after each test method.
     * Closes all static mocks to prevent memory leaks and interference between tests.
     */
    @AfterMethod
    public void tearDown() {

        captchaUtilStatic.close();
        frameworkUtilsStatic.close();
        captchaDataHolderStatic.close();
        captchaConstantsStatic.close();
    }

    /**
     * Sets up default mock behaviors for all test dependencies.
     * This provides a consistent baseline state for all tests.
     */
    private void setupDefaultMocks() {

        setupRequestMocks();
        setupContextMocks();
        setupUserMocks();
        setupCaptchaUtilMocks();
        setupCaptchaDataHolderMocks();
        setupGovernanceServiceDefaults();
        setupCaptchaConstantsMocks();
    }
    
    /**
     * Configures default request mock behaviors.
     */
    private void setupRequestMocks() {

        when(request.getRequestURI()).thenReturn("/commonauth");
        when(request.getParameter(FrameworkUtils.SESSION_DATA_KEY)).thenReturn(SESSION_KEY);
        when(request.getParameter(OTP_PARAM_NAME)).thenReturn("123456");
        when(request.getParameter(RESEND_PARAM)).thenReturn("false");
    }
    
    /**
     * Configures default authentication context mock behaviors.
     */
    private void setupContextMocks() {

        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(context);
        when(context.getCurrentAuthenticator()).thenReturn(AUTHENTICATOR_NAME);
        when(context.getProperty(REDIRECT_KEY)).thenReturn("true");
        when(context.getCurrentStep()).thenReturn(1);
        when(context.getLastAuthenticatedUser()).thenReturn(user);
        when(context.getUserTenantDomain()).thenReturn(TENANT_DOMAIN);
    }
    
    /**
     * Configures default authenticated user mock behaviors.
     */
    private void setupUserMocks() {

        when(user.getUserName()).thenReturn(USERNAME);
        when(user.getTenantDomain()).thenReturn(TENANT_DOMAIN);
    }
    
    /**
     * Configures default CaptchaUtil static mock behaviors.
     */
    private void setupCaptchaUtilMocks() {

        captchaUtilStatic.when(() -> CaptchaUtil.isPathAvailable(anyString(), anyString())).thenReturn(true);
        captchaUtilStatic.when(CaptchaUtil::isCaptchaValidationEnabledForLocalOTPAuthenticators).thenReturn(true);
        captchaUtilStatic.when(() ->
                CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);
    }
    
    /**
     * Configures default CaptchaDataHolder mock behaviors.
     */
    private void setupCaptchaDataHolderMocks() {

        captchaDataHolderStatic.when(CaptchaDataHolder::getInstance).thenReturn(dataHolder);
        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(true);
        when(dataHolder.getReCaptchaErrorRedirectUrls()).thenReturn("https://x,https://y");
        doNothing().when(dataHolder).setReCaptchaErrorRedirectUrls(anyString());
    }
    
    /**
     * Configures default CaptchaConstants static mock behaviors.
     */
    private void setupCaptchaConstantsMocks() {

        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("");
    }
    
    /**
     * Creates a governance service property with the specified value.
     *
     * @param value the property value
     * @return configured Property instance
     */
    private Property createProperty(String value) {

        Property property = new Property();
        property.setValue(value);
        return property;
    }
    
    /**
     * Configures governance service to return properties with specified values.
     *
     * @param values array of property values in order: [alwaysEnable, enable, maxAttempts]
     */
    private void setupGovernanceServiceProperties(String[] values) {

        Property[] properties = new Property[values.length];
        for (int i = 0; i < values.length; i++) {
            properties[i] = createProperty(values[i]);
        }
        
        try {
            when(governanceService.getConfiguration(any(String[].class), eq(TENANT_DOMAIN)))
                    .thenReturn(properties);
        } catch (Exception ignored) {
            // Ignore exceptions in test setup
        }
    }

    /**
     * Sets up default governance service configuration.
     * Default: always=false, enable=true, maxAttempts=3
     */
    private void setupGovernanceServiceDefaults() {

        setupGovernanceServiceProperties(new String[]{"false", "true", "3"});
    }

    @Test(description = "Verify that the connector returns the correct authenticator priority")
    public void testGetAuthenticatorPriority() {

        assertEquals(connector.getAuthenticatorPriority(), AUTHENTICATOR_PRIORITY,
                "Authenticator priority should match the expected value");
    }

    @Test(description = "Verify that connector can handle valid OTP authentication request")
    public void testCanHandleValidRequest() throws Exception {

        assertTrue(connector.canHandle(request, response),
                "Connector should handle valid OTP authentication request");
    }

    @Test(description = "Verify that connector correctly rejects invalid requests",
            dataProvider = "canHandleNegativeScenarios")
    public void testCanHandleNegativeScenarios(String scenario, Runnable setup, boolean expectedResult)
            throws Exception {

        // Apply the specific test setup
        setup.run();
        assertEquals(connector.canHandle(request, response), expectedResult,
                "Connector should correctly handle scenario: " + scenario);
    }
    
    @Test(description = "Verify that connector handles requests when previous IdP authenticators are flow handlers")
    public void testCanHandleWithPreviousIdpFlowHandlers() throws Exception {

        // Build previous IdPs so that isPreviousIdPAuthenticationFlowHandler() returns true
        AuthenticatorConfig cfg = mock(AuthenticatorConfig.class);
        // Create application authenticator that also implements AuthenticationFlowHandler
        ApplicationAuthenticator appAuth = mock(ApplicationAuthenticator.class,
                withSettings().extraInterfaces(AuthenticationFlowHandler.class));
        when(cfg.getApplicationAuthenticator()).thenReturn(appAuth);

        AuthenticatedIdPData idpData = mock(AuthenticatedIdPData.class);
        when(idpData.getAuthenticators()).thenReturn(Collections.singletonList(cfg));

        Map<String, AuthenticatedIdPData> idps = new HashMap<>();
        idps.put("testIdp", idpData);
        when(context.getCurrentAuthenticatedIdPs()).thenReturn(idps);
        assertTrue(connector.canHandle(request, response),
                "Connector should handle requests with previous IdP flow handlers");
    }

    @Test(description = "Verify CAPTCHA is required when always-enable flag is set")
    public void testPreValidateAlwaysEnableCaptcha() throws Exception {

        // Configure governance to always enable CAPTCHA
        setupGovernanceServiceProperties(new String[]{"true", "false", "5"});
        CaptchaPreValidationResponse response = connector.preValidate(request, this.response);
        assertTrue(response.isCaptchaValidationRequired(), 
                "CAPTCHA validation should be required when always-enable is true");
        assertNotNull(response.getOnCaptchaFailRedirectUrls(),
                "Redirect URLs should be available on CAPTCHA failure");
        assertEquals(response.getOnCaptchaFailRedirectUrls().get(response.getOnCaptchaFailRedirectUrls().size() - 1),
                FAIL_URL, "Last redirect URL should be the failure URL");
        assertTrue(response.isPostValidationRequired(),
                "Post-validation should be required");
    }

    @Test(description = "Verify CAPTCHA is required when maximum failed attempts are reached")
    public void testPreValidateMaxAttemptsReachedEnablesCaptcha() throws Exception {

        // Configure governance: always=false, enable=true, trigger max attempts
        setupGovernanceServiceProperties(new String[]{"false", "true", "3"});
        captchaUtilStatic.when(() ->
                CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);

        CaptchaPreValidationResponse response = connector.preValidate(request, this.response);
        assertTrue(response.isCaptchaValidationRequired(),
                "CAPTCHA validation should be required when max attempts reached");
        assertTrue(response.isMaxFailedLimitReached(),
                "Max failed limit flag should be set");
        assertTrue(response.isPostValidationRequired(),
                "Post-validation should be required");
    }

    @Test(expectedExceptions = CaptchaException.class,
          description = "Verify exception is thrown when authentication context is null")
    public void testPreValidateThrowsExceptionForNullContext() throws Exception {

        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(null);
        connector.preValidate(request, response);
    }

    @Test(expectedExceptions = CaptchaException.class,
          description = "Verify exception is thrown when user and tenant information is unavailable")
    public void testPreValidateThrowsExceptionForMissingUserAndTenant() throws Exception {

        // Remove user and tenant information to trigger exception
        when(context.getLastAuthenticatedUser()).thenReturn(null);
        when(request.getParameter("username")).thenReturn(null);
        when(context.getUserTenantDomain()).thenReturn(null);
        connector.preValidate(request, response);
    }
    
    @Test(description = "Verify pre-validation works with username from request parameter")
    public void testPreValidateWithUsernameFromRequest() throws Exception {

        // Remove authenticated user to force username fallback from request
        when(context.getLastAuthenticatedUser()).thenReturn(null);
        when(request.getParameter("username")).thenReturn("charlie@example.com");
        when(context.getUserTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Configure to trigger max attempts path
        setupGovernanceServiceProperties(new String[]{"false", "true", "3"});
        captchaUtilStatic.when(() ->
                CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);

        CaptchaPreValidationResponse response = connector.preValidate(request, this.response);
        assertTrue(response.isCaptchaValidationRequired(),
                "CAPTCHA validation should work with username from request parameter");
        assertTrue(response.isPostValidationRequired(),
                "Post-validation should be required");
    }

    @Test(description = "Verify post-validation returns null when security mechanism is disabled")
    public void testPostValidateWithSecurityMechanismDisabled() throws Exception {

        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("");
        assertNull(connector.postValidate(request, response),
                "Post-validation should return null when security mechanism is disabled");
    }

    @Test(description = "Verify post-validation response when security mechanism is enabled")
    public void testPostValidateWithSecurityMechanismEnabled() throws Exception {

        captchaConstantsStatic.when(CaptchaConstants::getEnableSecurityMechanism).thenReturn("on");
        CaptchaPostValidationResponse response = connector.postValidate(request, this.response);
        assertNotNull(response, "Post-validation response should not be null");
        assertFalse(response.isSuccessfulAttempt(),
                "Attempt should not be marked as successful");
        assertTrue(response.isEnableCaptchaResponsePath(),
                "CAPTCHA response path should be enabled");
        assertEquals(response.getCaptchaAttributes().get("reCaptcha"), "true",
                "reCaptcha attribute should be set to true");
    }

    @Test(description = "Verify reCAPTCHA enablement logic for various scenarios",
            dataProvider = "recaptchaEnablementScenarios")
    public void testIsRecaptchaEnabledScenarios(String scenario, Runnable setup, boolean expectedResult)
            throws Exception {

        // Apply the specific test setup
        setup.run();
        
        assertEquals(connector.isRecaptchaEnabled(request), expectedResult,
                "reCAPTCHA enablement should match expected result for scenario: " + scenario);
    }
    
    @Test(description = "Verify reCAPTCHA is disabled when governance configuration is invalid")
    public void testIsRecaptchaEnabledWithInvalidConfig() throws Exception {

        when(dataHolder.isForcefullyEnabledRecaptchaForAllTenants()).thenReturn(false);
        frameworkUtilsStatic.when(() -> FrameworkUtils.getAuthenticationContextFromCache(SESSION_KEY))
                .thenReturn(context);
        when(context.getLastAuthenticatedUser()).thenReturn(user);
        when(user.getUserName()).thenReturn("bob@example.com");
        when(user.getTenantDomain()).thenReturn(TENANT_DOMAIN);

        // Return empty configuration array to simulate invalid config
        setupGovernanceServiceProperties(new String[]{});
        assertFalse(connector.isRecaptchaEnabled(request),
                "reCAPTCHA should be disabled when governance configuration is invalid");
    }

    @Test(description = "Verify reCAPTCHA is disabled when enabled but max attempts not reached")
    public void testIsRecaptchaEnabledButMaxAttemptsNotReached() throws Exception {

        // Configure: always=false, enable=true, but max attempts NOT reached
        setupGovernanceServiceProperties(new String[]{"false", "true", "5"});
        captchaUtilStatic.when(() ->
                CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(false);
        assertFalse(connector.isRecaptchaEnabled(request),
                "reCAPTCHA should be disabled when max attempts are not reached");
    }

    @Test(description = "Verify reCAPTCHA is enabled when all conditions are met")
    public void testIsRecaptchaEnabledWithAllConditionsMet() throws Exception {

        // Configure: always=false, enable=true, max attempts reached, data holder enabled
        setupGovernanceServiceProperties(new String[]{"false", "true", "3"});
        captchaUtilStatic.when(() ->
                CaptchaUtil.isMaximumFailedLoginAttemptsReached(anyString(), anyString(), anyString()))
                .thenReturn(true);
        when(dataHolder.isReCaptchaEnabled()).thenReturn(true);
        assertTrue(connector.isRecaptchaEnabled(request),
                "reCAPTCHA should be enabled when all conditions are met");
    }
}
