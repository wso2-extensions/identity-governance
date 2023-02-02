/*
 * Copyright (c) 2023, WSO2 LLC. (https://www.wso2.com) All Rights Reserved.
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.auth.attribute.handler;

import org.apache.commons.lang.StringUtils;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.AuthenticationStep;
import org.wso2.carbon.identity.application.common.model.LocalAndOutboundAuthenticationConfig;
import org.wso2.carbon.identity.application.common.model.LocalAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.ServiceProvider;
import org.wso2.carbon.identity.application.mgt.ApplicationManagementService;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerClientException;
import org.wso2.carbon.identity.auth.attribute.handler.exception.AuthAttributeHandlerException;
import org.wso2.carbon.identity.auth.attribute.handler.internal.AuthAttributeHandlerServiceDataHolder;
import org.wso2.carbon.identity.auth.attribute.handler.model.AuthAttributeHolder;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Unit test for AuthAttributeHandlerManager.
 */
public class AuthAttributeHandlerManagerTest {

    private AuthAttributeHandlerManager authAttributeHandlerManager = AuthAttributeHandlerManager.getInstance();
    private static final String APP_ID = "0181313d-5c84-4ec4-a931-b73085408eff";
    private static final String TENANT_DOMAIN = "abcd";
    private static final String AUTHENTICATOR_BASICAUTH = "BasicAuthenticator";
    private static final String AUTHENTICATOR_MAGICLINK = "MagicLinkAuthenticator";
    private static final String AUTHENTICATOR_FIDO = "FIDOAuthenticator";
    private static final String AUTHENTICATOR_TOTP = "TOTPAuthenticator";
    private static final String AUTHENTICATOR_SMSOTP = "SMSOTPAuthenticator";
    private static final String[] EXPECTED_HANDLERS = new String[]{AUTHENTICATOR_BASICAUTH, AUTHENTICATOR_MAGICLINK,
            AUTHENTICATOR_FIDO};

    @Mock
    ApplicationManagementService applicationManagementService;

    @Mock
    AuthAttributeHandlerServiceDataHolder authAttributeHandlerServiceDataHolder;

    private MockedStatic<AuthAttributeHandlerServiceDataHolder> mockedAuthAttributeHandlerServiceDataHolder;

    @BeforeClass
    private void setup() {

        MockitoAnnotations.openMocks(this);
        initiateMocks();
    }

    @AfterClass
    public void tearDown() {

        mockedAuthAttributeHandlerServiceDataHolder.close();
    }

    @Test
    public void testGetAvailableAuthAttributeHolders() {

        try {
            when(applicationManagementService.getApplicationByResourceId(anyString(), anyString()))
                    .thenReturn(getServiceProvider());

            List<AuthAttributeHolder> authAttributeHolders =
                    authAttributeHandlerManager.getAvailableAuthAttributeHolders(APP_ID, TENANT_DOMAIN);
            Assert.assertEquals(authAttributeHolders.size(), EXPECTED_HANDLERS.length, "Expected 3 auth attribute " +
                    "holders but there is " + authAttributeHolders.size());
            String[] authAttributeHandlers = getAuthAttributeHandlers(authAttributeHolders);
            Assert.assertEqualsNoOrder(authAttributeHandlers, EXPECTED_HANDLERS,
                    String.format("Expected auth attribute handlers: %s actual auth attribute handlers: %s",
                            StringUtils.join(EXPECTED_HANDLERS, ","),
                            StringUtils.join(authAttributeHandlers, ",")));

        } catch (Exception e) {
            Assert.fail("Test threw an unexpected exception.", e);
        }
    }

    @Test
    public void testGetAvailableAuthAttributeHoldersWithNullSP() {

        boolean isClientExceptionThrown = false;
        try {
            when(applicationManagementService.getApplicationByResourceId(anyString(), anyString())).thenReturn(null);
            authAttributeHandlerManager.getAvailableAuthAttributeHolders(APP_ID, TENANT_DOMAIN);
        } catch (AuthAttributeHandlerClientException e) {
            isClientExceptionThrown = true;
            Assert.assertEquals(e.getErrorCode(),
                    AuthAttributeHandlerConstants.ErrorMessages.ERROR_CODE_SERVICE_PROVIDER_NOT_FOUND.getCode(),
                    "Expected error code not found.");
        } catch (Exception e) {
            Assert.fail("Test threw an unexpected exception.", e);
        }

        if (!isClientExceptionThrown) {
            Assert.fail("Expected to throw a AuthAttributeHandlerClientException but no exception was thrown.");
        }
    }

    private String[] getAuthAttributeHandlers(List<AuthAttributeHolder> authAttributeHolderList) {

        List<String> authAttributeHandlers = new ArrayList<>();
        for (AuthAttributeHolder authAttributeHolder :
                authAttributeHolderList) {
            authAttributeHandlers.add(authAttributeHolder.getHandlerName());
        }

        return authAttributeHandlers.toArray(new String[0]);
    }

    private void initiateMocks() {

        mockedAuthAttributeHandlerServiceDataHolder = Mockito.mockStatic(AuthAttributeHandlerServiceDataHolder.class);
        mockedAuthAttributeHandlerServiceDataHolder.when(AuthAttributeHandlerServiceDataHolder::getInstance)
                .thenReturn(authAttributeHandlerServiceDataHolder);
        when(authAttributeHandlerServiceDataHolder.getApplicationManagementService())
                .thenReturn(applicationManagementService);
        when(authAttributeHandlerServiceDataHolder.getAuthAttributeHandlers())
                .thenReturn(getAuthAttributeHandlers());
    }

    private ServiceProvider getServiceProvider() {

        ServiceProvider sp = new ServiceProvider();
        List<AuthenticationStep> authenticationSteps = new ArrayList<>();
        authenticationSteps.add(getAuthenticationStep(new String[]{AUTHENTICATOR_BASICAUTH, AUTHENTICATOR_MAGICLINK}));
        authenticationSteps.add(getAuthenticationStep(new String[]{AUTHENTICATOR_FIDO, AUTHENTICATOR_SMSOTP}));

        LocalAndOutboundAuthenticationConfig localAndOutboundAuthenticationConfig =
                new LocalAndOutboundAuthenticationConfig();
        localAndOutboundAuthenticationConfig.setAuthenticationSteps(
                authenticationSteps.toArray(new AuthenticationStep[0]));

        sp.setLocalAndOutBoundAuthenticationConfig(localAndOutboundAuthenticationConfig);

        return sp;
    }

    private LocalAuthenticatorConfig getLocalAuthenticatorConfig(String name) {

        LocalAuthenticatorConfig localAuthenticatorConfig = new LocalAuthenticatorConfig();
        localAuthenticatorConfig.setName(name);

        return localAuthenticatorConfig;
    }

    private AuthenticationStep getAuthenticationStep(String[] authenticators) {

        List<LocalAuthenticatorConfig> localAuthenticatorConfigs = new ArrayList<>();
        for (String authenticator : authenticators) {
            localAuthenticatorConfigs.add(getLocalAuthenticatorConfig(authenticator));
        }

        AuthenticationStep authenticationStep = new AuthenticationStep();
        authenticationStep.setLocalAuthenticatorConfigs(
                localAuthenticatorConfigs.toArray(new LocalAuthenticatorConfig[0]));
        return authenticationStep;
    }

    private List<AuthAttributeHandler> getAuthAttributeHandlers() {

        List<AuthAttributeHandler> authAttributeHandlers = new ArrayList<>();
        authAttributeHandlers.add(new BasicAuthAttributeHandler());
        authAttributeHandlers.add(new MagicLinkAttributeHandler());
        authAttributeHandlers.add(new FIDOAttributeHandler());
        authAttributeHandlers.add(new TOTPAttributeHandler());

        return authAttributeHandlers;
    }

    private AuthAttributeHolder getAuthAttributeHolder(String name) {

        AuthAttributeHolder authAttributeHolder = new AuthAttributeHolder();
        authAttributeHolder.setHandlerName(name);
        authAttributeHolder.setHandlerBinding(AuthAttributeHandlerBindingType.AUTHENTICATOR);
        authAttributeHolder.setHandlerBoundIdentifier(name);

        return authAttributeHolder;
    }

    class BasicAuthAttributeHandler extends MockAbstractAuthAttributeHandler {

        @Override
        public AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException {

            return getAuthAttributeHolder(AUTHENTICATOR_BASICAUTH);
        }

        @Override
        public String getBoundIdentifier() throws AuthAttributeHandlerException {

            return AUTHENTICATOR_BASICAUTH;
        }
    }

    class MagicLinkAttributeHandler extends MockAbstractAuthAttributeHandler {

        @Override
        public AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException {

            return getAuthAttributeHolder(AUTHENTICATOR_MAGICLINK);
        }

        @Override
        public String getBoundIdentifier() throws AuthAttributeHandlerException {

            return AUTHENTICATOR_MAGICLINK;
        }
    }

    class FIDOAttributeHandler extends MockAbstractAuthAttributeHandler {

        @Override
        public AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException {

            return getAuthAttributeHolder(AUTHENTICATOR_FIDO);
        }

        @Override
        public String getBoundIdentifier() throws AuthAttributeHandlerException {

            return AUTHENTICATOR_FIDO;
        }
    }

    class TOTPAttributeHandler extends MockAbstractAuthAttributeHandler {

        @Override
        public AuthAttributeHolder getAuthAttributeData() throws AuthAttributeHandlerException {

            return getAuthAttributeHolder(AUTHENTICATOR_TOTP);
        }

        @Override
        public String getBoundIdentifier() throws AuthAttributeHandlerException {

            return AUTHENTICATOR_TOTP;
        }
    }
}
