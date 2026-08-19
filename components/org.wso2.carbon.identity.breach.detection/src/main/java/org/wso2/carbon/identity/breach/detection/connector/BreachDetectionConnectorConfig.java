/*
 * Copyright (c) 2026, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.breach.detection.connector;

import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;
import org.wso2.carbon.identity.breach.detection.engine.SourceRegistry;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicyResolver;
import org.wso2.carbon.identity.breach.source.BreachSource;
import org.wso2.carbon.identity.breach.source.Capability;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Per-organization policy, carried on the existing governance connector mechanism.
 * <p>
 * Reusing it brings a per-tenant store, tenant isolation, and a working management API without inventing any of
 * them. Source credentials stay out of it entirely: connector properties are readable over REST, and that is
 * exactly how a credential ends up in a response body.
 * <p>
 * The property list is built from what is bound right now, so installing a connector JAR adds its failure-policy
 * setting with no core change.
 */
public class BreachDetectionConnectorConfig implements IdentityConnectorConfig {

    private final SourceRegistry registry;

    public BreachDetectionConnectorConfig(SourceRegistry registry) {

        this.registry = registry;
    }

    @Override
    public String getName() {

        return BreachDetectionConstants.CONNECTOR_NAME;
    }

    @Override
    public String getFriendlyName() {

        return BreachDetectionConstants.CONNECTOR_FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return BreachDetectionConstants.CONNECTOR_CATEGORY;
    }

    @Override
    public String getSubCategory() {

        return BreachDetectionConstants.CONNECTOR_SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return BreachDetectionConstants.CONNECTOR_ORDER;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> names = new LinkedHashMap<>();
        names.put(BreachDetectionConstants.PROPERTY_ENABLE, "Refuse breached passwords");
        names.put(BreachDetectionConstants.PROPERTY_SOURCES, "Sources");
        for (BreachSource source : registry.installed()) {
            names.put(BreachPolicyResolver.onErrorProperty(source.getId()),
                    "If " + source.getDescriptor().getDisplayName() + " cannot be reached");
        }
        return names;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptions = new LinkedHashMap<>();
        descriptions.put(BreachDetectionConstants.PROPERTY_ENABLE,
                "Refuse passwords that have appeared in known data breaches. Turning this on can refuse "
                        + "passwords that were previously accepted.");
        descriptions.put(BreachDetectionConstants.PROPERTY_SOURCES,
                "Comma-separated ids of the sources to consult. Only sources installed on this server can "
                        + "return a verdict.");
        for (BreachSource source : registry.installed()) {
            descriptions.put(BreachPolicyResolver.onErrorProperty(source.getId()),
                    "Whether to allow or deny a password when " + source.getDescriptor().getDisplayName()
                            + " cannot answer. Choose deny only if you would rather block sign-ups than risk "
                            + "accepting a breached password.");
        }
        return descriptions;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> names = new ArrayList<>();
        names.add(BreachDetectionConstants.PROPERTY_ENABLE);
        names.add(BreachDetectionConstants.PROPERTY_SOURCES);
        for (BreachSource source : registry.installed()) {
            names.add(BreachPolicyResolver.onErrorProperty(source.getId()));
        }
        return names.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Properties defaults = new Properties();
        // Off by default. Upgrading with no configuration change must change nothing.
        defaults.put(BreachDetectionConstants.PROPERTY_ENABLE, BreachDetectionConstants.DEFAULT_ENABLE);
        defaults.put(BreachDetectionConstants.PROPERTY_SOURCES, BreachDetectionConstants.DEFAULT_SOURCES);
        for (BreachSource source : registry.installed()) {
            defaults.put(BreachPolicyResolver.onErrorProperty(source.getId()), defaultOnError(source));
        }
        return defaults;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        Map<String, String> defaults = new HashMap<>();
        Properties all = getDefaultPropertyValues(tenantDomain);
        for (String name : propertyNames) {
            Object value = all.get(name);
            if (value != null) {
                defaults.put(name, String.valueOf(value));
            }
        }
        return defaults;
    }

    @Override
    public Map<String, Property> getMetaData() {

        Map<String, Property> metadata = new LinkedHashMap<>();
        metadata.put(BreachDetectionConstants.PROPERTY_ENABLE, booleanProperty());
        return metadata;
    }

    private static String defaultOnError(BreachSource source) {

        return source.getCapabilities().contains(Capability.OFFLINE)
                ? BreachDetectionConstants.ON_ERROR_DENY : BreachDetectionConstants.ON_ERROR_ALLOW;
    }

    private static Property booleanProperty() {

        Property property = new Property();
        property.setType("boolean");
        return property;
    }
}
