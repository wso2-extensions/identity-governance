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
import org.testng.annotations.Test;
import org.wso2.carbon.base.CarbonBaseConstants;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.handler.FlowRegistrationCompletionHandler;
import org.wso2.carbon.identity.recovery.model.RegistrationCompletionData;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class RegistrationCompletionDeferralTest {

    @Test
    public void testNestedScopesRestoreThePreviousContext() {

        FlowExecutionContext outer = new FlowExecutionContext();
        FlowExecutionContext inner = new FlowExecutionContext();
        RegistrationCompletionData data = data(Map.of(), Map.of());
        try (RegistrationCompletionDeferral.Scope outerScope = RegistrationCompletionDeferral.begin(outer)) {
            try (RegistrationCompletionDeferral.Scope innerScope = RegistrationCompletionDeferral.begin(inner)) {
                assertTrue(RegistrationCompletionDeferral.defer(data));
                assertTrue(inner.getProperties().containsValue(data));
                assertTrue(outer.getProperties().isEmpty());
            }
            assertTrue(RegistrationCompletionDeferral.defer(data));
            assertTrue(outer.getProperties().containsValue(data));
        }
        assertFalse(RegistrationCompletionDeferral.isActive());
        assertFalse(RegistrationCompletionDeferral.defer(data));
    }

    @Test
    public void testCompletionSnapshotIsIndependentAndSerializable() throws Exception {

        Map<String, String> claims = new HashMap<>();
        claims.put("email", "original@example.com");
        Map<String, String> properties = new HashMap<>();
        properties.put("callback", "https://example.com");
        RegistrationCompletionData original = data(claims, properties);
        claims.clear();
        properties.clear();

        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (ObjectOutputStream output = new ObjectOutputStream(bytes)) {
            output.writeObject(original);
        }
        try (ObjectInputStream input = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
            RegistrationCompletionData restored = (RegistrationCompletionData) input.readObject();
            assertEquals(restored, original);
            assertEquals(restored.userClaims().get("email"), "original@example.com");
            assertEquals(restored.toNotificationProperties()[0].getValue(), "https://example.com");
        }
    }

    @Test
    public void testCompletionSurvivesJsonRestoredContextAndRunsOnce() throws Exception {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setProperty("unrelated", "preserved");
        RegistrationCompletionData original = data(Map.of("email", "original@example.com"),
                Map.of("callback", "https://example.com"));
        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            assertTrue(RegistrationCompletionDeferral.defer(original));
        }
        ObjectMapper mapper = new ObjectMapper();
        FlowExecutionContext restored = mapper.readValue(mapper.writeValueAsString(context), FlowExecutionContext.class);
        assertTrue(restored.getProperties().values().stream().anyMatch(Map.class::isInstance));

        System.setProperty(CarbonBaseConstants.CARBON_HOME,
                Paths.get(System.getProperty("user.dir"), "target", "test-classes").toString());
        try (MockedStatic<PrivilegedCarbonContext> carbon = mockStatic(PrivilegedCarbonContext.class);
             MockedConstruction<FlowRegistrationCompletionHandler> handlers =
                     mockConstruction(FlowRegistrationCompletionHandler.class)) {
            PrivilegedCarbonContext tenantContext = mock(PrivilegedCarbonContext.class);
            carbon.when(PrivilegedCarbonContext::getThreadLocalCarbonContext).thenReturn(tenantContext);

            RegistrationCompletionDeferral.complete(restored);
            RegistrationCompletionDeferral.complete(restored);

            assertEquals(handlers.constructed().size(), 1);
            verify(handlers.constructed().get(0)).completeRegistration(original);
            verify(tenantContext).setTenantDomain(original.tenantDomain(), true);
            carbon.verify(PrivilegedCarbonContext::endTenantFlow);
            assertEquals(restored.getProperties(), Map.of("unrelated", "preserved"));
        }
    }

    @Test
    public void testDiscardDoesNotRemoveUnrelatedContextProperties() {

        FlowExecutionContext context = new FlowExecutionContext();
        context.setProperty("unrelated", "preserved");
        try (RegistrationCompletionDeferral.Scope ignored = RegistrationCompletionDeferral.begin(context)) {
            RegistrationCompletionDeferral.defer(data(Map.of(), Map.of()));
        }
        RegistrationCompletionDeferral.discard(context);
        assertEquals(context.getProperties(), Map.of("unrelated", "preserved"));
    }

    private RegistrationCompletionData data(Map<String, String> claims, Map<String, String> properties) {

        return new RegistrationCompletionData("testuser", "carbon.super", "PRIMARY", "EMAIL", false,
                true, true, true, true, true, true, claims, properties);
    }
}
