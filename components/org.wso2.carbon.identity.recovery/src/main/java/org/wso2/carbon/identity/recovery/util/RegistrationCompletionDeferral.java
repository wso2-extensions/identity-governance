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
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.handler.FlowRegistrationCompletionHandler;
import org.wso2.carbon.identity.recovery.model.Property;
import org.wso2.carbon.identity.recovery.model.RegistrationCompletionData;

import java.util.HashMap;
import java.util.Map;

/**
 * Bridges synchronous post-add-user handlers to the dispatcher's flow context. Only completion data is
 * retained in the flow context; the thread-local scope is always restored when user provisioning returns.
 */
public final class RegistrationCompletionDeferral {

    private static final Log LOG = LogFactory.getLog(RegistrationCompletionDeferral.class);
    private static final String PENDING_COMPLETION = "pendingRegistrationCompletion";
    private static final ThreadLocal<FlowExecutionContext> CONTEXT = new ThreadLocal<>();
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private RegistrationCompletionDeferral() {

    }

    public static Scope begin(FlowExecutionContext context) {

        FlowExecutionContext previous = CONTEXT.get();
        CONTEXT.set(context);
        return new Scope(previous);
    }

    public static boolean defer(RegistrationCompletionData data) {

        FlowExecutionContext context = CONTEXT.get();
        if (context == null) {
            return false;
        }
        context.setProperty(PENDING_COMPLETION, data);
        return true;
    }

    public static boolean isActive() {

        return CONTEXT.get() != null;
    }

    /**
     * Copies notification properties which would otherwise be lost when the request returns on a retry.
     */
    public static Map<String, String> snapshotNotificationProperties() {

        Map<String, String> properties = new HashMap<>();
        String serviceProviderUUID = (String) IdentityUtil.threadLocalProperties.get()
                .get(IdentityEventConstants.EventProperty.SERVICE_PROVIDER_UUID);
        if (StringUtils.isNotBlank(serviceProviderUUID)) {
            properties.put(IdentityEventConstants.EventProperty.SERVICE_PROVIDER_UUID, serviceProviderUUID);
        }
        Property[] arbitraryProperties = Utils.getArbitraryProperties();
        if (arbitraryProperties != null) {
            for (Property property : arbitraryProperties) {
                properties.put(property.getKey(), property.getValue());
            }
        }
        return properties;
    }

    public static void discard(FlowExecutionContext context) {

        context.getProperties().remove(PENDING_COMPLETION);
    }

    public static void complete(FlowExecutionContext context) {

        Object pending = context.getProperties().remove(PENDING_COMPLETION);
        if (pending == null) {
            return;
        }

        // The dispatcher restores the parent tenant after provisioning in a new organization.
        PrivilegedCarbonContext.startTenantFlow();
        try {
            // JSON-restored flow properties contain a map rather than the original record.
            RegistrationCompletionData data = pending instanceof RegistrationCompletionData completion ? completion :
                    OBJECT_MAPPER.convertValue(pending, RegistrationCompletionData.class);
            PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(data.tenantDomain(), true);
            new FlowRegistrationCompletionHandler().completeRegistration(data);
        } catch (IdentityEventException | RuntimeException e) {
            // Provisioning has succeeded. A notification failure must not delete the provisioned resources.
            LOG.error("Unable to complete registration notifications for flow: " + context.getContextIdentifier(), e);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    public static final class Scope implements AutoCloseable {

        private final FlowExecutionContext previous;

        private Scope(FlowExecutionContext previous) {

            this.previous = previous;
        }

        @Override
        public void close() {

            if (previous == null) {
                CONTEXT.remove();
            } else {
                CONTEXT.set(previous);
            }
        }
    }
}
