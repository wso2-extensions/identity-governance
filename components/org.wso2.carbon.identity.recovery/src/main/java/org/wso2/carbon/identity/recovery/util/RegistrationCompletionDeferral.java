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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.event.IdentityEventException;
import org.wso2.carbon.identity.flow.execution.engine.model.FlowExecutionContext;
import org.wso2.carbon.identity.recovery.handler.FlowRegistrationCompletionHandler;
import org.wso2.carbon.identity.recovery.model.Property;

import java.util.HashMap;
import java.util.Map;

/**
 * Bridges synchronous post-add-user handlers to the dispatcher's flow context. Only completion data is
 * retained in the flow context; the thread-local scope is always restored when user provisioning returns.
 */
public final class RegistrationCompletionDeferral {

    public static final String NOTIFY_ACCOUNT_CREATION = "notifyAccountCreation";
    public static final String PUBLISH_SUCCESS_EVENT = "publishSuccessEvent";
    public static final String SEND_CONFIRMATION = "sendConfirmation";
    public static final String NOTIFICATION_PROPERTIES = "notificationProperties";

    private static final Log LOG = LogFactory.getLog(RegistrationCompletionDeferral.class);
    private static final String PENDING_COMPLETION = "pendingRegistrationCompletion";
    private static final ThreadLocal<FlowExecutionContext> CONTEXT = new ThreadLocal<>();

    private RegistrationCompletionDeferral() {

    }

    public static Scope begin(FlowExecutionContext context) {

        FlowExecutionContext previous = CONTEXT.get();
        CONTEXT.set(context);
        return new Scope(previous);
    }

    public static boolean defer(String tenantDomain, String preferredChannel, boolean notifyAccountCreation,
                                boolean publishSuccessEvent, boolean sendConfirmation) {

        FlowExecutionContext context = CONTEXT.get();
        if (context == null) {
            return false;
        }
        // Retain only completion decisions and notification metadata, not another copy of the flow user.
        Map<String, Object> completion = new HashMap<>();
        completion.put(IdentityEventConstants.EventProperty.TENANT_DOMAIN, tenantDomain);
        completion.put(IdentityEventConstants.EventProperty.NOTIFICATION_CHANNEL, preferredChannel);
        completion.put(NOTIFY_ACCOUNT_CREATION, notifyAccountCreation);
        completion.put(PUBLISH_SUCCESS_EVENT, publishSuccessEvent);
        completion.put(SEND_CONFIRMATION, sendConfirmation);
        completion.put(NOTIFICATION_PROPERTIES, captureNotificationProperties());
        context.setProperty(PENDING_COMPLETION, completion);
        return true;
    }

    public static boolean isActive() {

        return CONTEXT.get() != null;
    }

    /**
     * Preserves registration notification metadata while subsequent provisioning runs.
     */
    private static Map<String, String> captureNotificationProperties() {

        Map<String, String> properties = new HashMap<>();
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

        boolean tenantFlowStarted = false;
        try {
            Map<String, Object> completion = (Map<String, Object>) pending;
            String tenantDomain = (String) completion.get(IdentityEventConstants.EventProperty.TENANT_DOMAIN);
            // Re-enter the user's tenant only when provisioning restored a different (parent) tenant.
            if (!StringUtils.equals(PrivilegedCarbonContext.getThreadLocalCarbonContext().getTenantDomain(),
                    tenantDomain)) {
                PrivilegedCarbonContext.startTenantFlow();
                tenantFlowStarted = true;
                PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(tenantDomain, true);
            }
            new FlowRegistrationCompletionHandler().completeRegistration(context, completion);
        } catch (IdentityEventException | RuntimeException e) {
            // Provisioning has succeeded. A notification failure must not delete the provisioned resources.
            LOG.error("Unable to complete registration notifications for flow: " + context.getContextIdentifier(), e);
        } finally {
            if (tenantFlowStarted) {
                PrivilegedCarbonContext.endTenantFlow();
            }
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
