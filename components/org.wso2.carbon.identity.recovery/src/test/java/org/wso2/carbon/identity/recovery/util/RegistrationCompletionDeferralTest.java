/*
 * Copyright (c) 2026, WSO2 LLC. (https://www.wso2.com).
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

package org.wso2.carbon.identity.recovery.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.handler.FlowRegistrationCompletionHandler;
import org.wso2.carbon.identity.recovery.model.Property;

import java.nio.file.Paths;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.wso2.carbon.identity.recovery.util.RegistrationCompletionDeferral.NOTIFICATION_PROPERTIES;

public class RegistrationCompletionDeferralTest {

    @Test
    public void testNestedScopesRestoreThePreviousContext() {

        FlowExecutionContext outer = new FlowExecutionContext();
        FlowExecutionContext inner = new FlowExecutionContext();
        try (RegistrationCompletionDeferral.Scope outerScope = RegistrationCompletionDeferral.begin(outer)) {
            try (RegistrationCompletionDeferral.Scope innerScope = RegistrationCompletionDeferral.begin(inner)) {
                assertTrue(defer());
                assertFalse(inner.getProperties().isEmpty());
                assertTrue(outer.getProperties().isEmpty());
            }
            assertTrue(defer());
            assertFalse(outer.getProperties().isEmpty());
        }
        assertFalse(RegistrationCompletionDeferral.isActive());
        assertFalse(defer());
    }

    @Test
    public void testNotificationMetadataIsIndependentOfSubsequentThreadLocalChanges() {

        FlowExecutionContext context = new FlowExecutionContext();
        Property callback = new Property("callback", "https://example.com");
        try (MockedStatic<Utils> utils = mockStatic(Utils.class)) {
            utils.when(Utils::getArbitraryProperties).thenReturn(new Property[]{
                    new Property("spId", "original-application"), new Property("sp", "Original application"),
                    callback});
            try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
                assertTrue(defer());
            }
            callback.setValue("https://changed.example.com");
            utils.when(Utils::getArbitraryProperties).thenReturn(null);

            Map<String, Object> completion = pending(context);
            assertEquals(completion.get(NOTIFICATION_PROPERTIES), Map.of(
                    "spId", "original-application", "sp", "Original application", "callback", "https://example.com"));
            assertEquals(completion.size(), 6);
            assertFalse(completion.containsKey(IdentityEventConstants.EventProperty.USER_CLAIMS));
            assertFalse(completion.containsKey(IdentityEventConstants.EventProperty.CREDENTIAL));
            assertFalse(completion.containsKey(IdentityEventConstants.EventProperty.USER_STORE_MANAGER));
        }
    }

    @Test
    public void testMissingNotificationMetadataProducesEmptyMap() {

        FlowExecutionContext context = new FlowExecutionContext();
        try (MockedStatic<Utils> utils = mockStatic(Utils.class)) {
            utils.when(Utils::getArbitraryProperties).thenReturn(null);
            try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
                assertTrue(defer());
            }
            assertEquals(pending(context).get(NOTIFICATION_PROPERTIES), Map.of());
        }
    }

    @DataProvider(name = "completionTenantProvider")
    public Object[][] completionTenantProvider() {

        return new Object[][]{{"carbon.super", false}, {"parent.example.com", true}};
    }

    @Test(dataProvider = "completionTenantProvider")
    public void testCompletionSurvivesJsonRestoredContextAndRunsOnce(String currentTenantDomain,
                                                                  boolean tenantSwitchRequired) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setProperty("unrelated", "preserved");
        context.setTenantDomain("parent.example.com");
        context.getFlowUser().setUsername("testuser");
        context.getFlowUser().setUserStoreDomain("SECONDARY");
        context.getFlowUser().addClaim("email", "original@example.com");
        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            assertTrue(defer());
        }
        ObjectMapper mapper = new ObjectMapper();
        FlowExecutionContext restored = mapper.readValue(mapper.writeValueAsString(context), FlowExecutionContext.class);
        assertTrue(restored.getProperties().values().stream().anyMatch(Map.class::isInstance));
        Map<String, Object> completion = pending(restored);

        System.setProperty(CarbonBaseConstants.CARBON_HOME,
                Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString());
        try (MockedStatic<PrivilegedCarbonContext> carbon = mockStatic(PrivilegedCarbonContext.class);
             MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            PrivilegedCarbonContext tenantContext = mock(PrivilegedCarbonContext.class);
            when(tenantContext.getTenantDomain()).thenReturn(currentTenantDomain);
            carbon.when(PrivilegedCarbonContext::getThreadLocalCarbonContext).thenReturn(tenantContext);

            RegistrationCompletionDeferral.complete(restored);
            RegistrationCompletionDeferral.complete(restored);

            assertEquals(handlers.constructed().size(), 1);
            verify(handlers.constructed().get(0)).completeRegistration(restored, completion);
            verify(tenantContext, times(tenantSwitchRequired ? 1 : 0)).setTenantDomain("carbon.super", true);
            carbon.verify(PrivilegedCarbonContext::startTenantFlow, times(tenantSwitchRequired ? 1 : 0));
            carbon.verify(PrivilegedCarbonContext::endTenantFlow, times(tenantSwitchRequired ? 1 : 0));
            assertEquals(restored.getProperties(), Map.of("unrelated", "preserved"));
            assertEquals(restored.getTenantDomain(), "parent.example.com");
            assertEquals(restored.getFlowUser().getUserStoreDomain(), "SECONDARY");
            assertEquals(restored.getFlowUser().getClaims().get("email"), "original@example.com");
        }
    }

    @Test(dataProvider = "completionTenantProvider")
    public void testCompletionFailureDoesNotEscapeOrLeavePendingWork(String currentTenantDomain,
                                                                   boolean tenantSwitchRequired) throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setProperty("unrelated", "preserved");
        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            assertTrue(defer());
        }
        System.setProperty(CarbonBaseConstants.CARBON_HOME,
                Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString());
        try (MockedStatic<PrivilegedCarbonContext> carbon = mockStatic(PrivilegedCarbonContext.class);
             MockedConstruction<FlowRegistrationCompletionHandler> handlers = mockConstruction(
                     FlowRegistrationCompletionHandler.class, (handler, construction) ->
                             doThrow(new IdentityEventException("Notification failed"))
                                     .when(handler).completeRegistration(any(), any()))) {
            PrivilegedCarbonContext tenantContext = mock(PrivilegedCarbonContext.class);
            when(tenantContext.getTenantDomain()).thenReturn(currentTenantDomain);
            carbon.when(PrivilegedCarbonContext::getThreadLocalCarbonContext).thenReturn(tenantContext);

            RegistrationCompletionDeferral.complete(context);
            RegistrationCompletionDeferral.complete(context);

            assertEquals(handlers.constructed().size(), 1);
            verify(tenantContext, times(tenantSwitchRequired ? 1 : 0)).setTenantDomain("carbon.super", true);
            carbon.verify(PrivilegedCarbonContext::startTenantFlow, times(tenantSwitchRequired ? 1 : 0));
            carbon.verify(PrivilegedCarbonContext::endTenantFlow, times(tenantSwitchRequired ? 1 : 0));
            assertEquals(context.getProperties(), Map.of("unrelated", "preserved"));
        }
    }

    @Test
    public void testDiscardDoesNotRemoveUnrelatedContextProperties() {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setProperty("unrelated", "preserved");
        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            defer();
        }
        RegistrationCompletionDeferral.discard(context);
        assertEquals(context.getProperties(), Map.of("unrelated", "preserved"));
    }

    private boolean defer() {

        return RegistrationCompletionDeferral.defer("carbon.super", "EMAIL", false, true, true);
    }

    private Map<String, Object> pending(FlowExecutionContext context) {

        return (Map<String, Object>) context.getProperty("pendingRegistrationCompletion");
    }
}
