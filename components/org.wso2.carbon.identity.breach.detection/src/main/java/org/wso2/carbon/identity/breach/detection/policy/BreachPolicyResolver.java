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

package org.wso2.carbon.identity.breach.detection.policy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;
import org.wso2.carbon.identity.breach.detection.engine.SourceRegistry;
import org.wso2.carbon.identity.breach.detection.internal.BreachDetectionDataHolder;
import org.wso2.carbon.identity.breach.source.BreachSource;
import org.wso2.carbon.identity.governance.IdentityGovernanceService;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Resolves one organization's effective policy from the governance store.
 * <p>
 * Enabling the capability in one organization leaves every other one unchanged, an organization with no
 * explicit configuration inherits the deployment default of disabled, and sub-organizations resolve through the
 * governance service's existing inheritance rules rather than anything invented here.
 */
public class BreachPolicyResolver {

    private static final Log LOG = LogFactory.getLog(BreachPolicyResolver.class);

    private final SourceRegistry registry;

    public BreachPolicyResolver(SourceRegistry registry) {

        this.registry = registry;
    }

    /**
     * @param tenantDomain the organization asking.
     * @return the effective policy. Never null: an unreadable store resolves to disabled rather than to an
     * assumption that it was on.
     */
    public BreachPolicy resolve(String tenantDomain) {

        IdentityGovernanceService governanceService =
                BreachDetectionDataHolder.getInstance().getIdentityGovernanceService();
        if (governanceService == null) {
            LOG.debug("The identity governance service is not available yet. Treating breach detection as "
                    + "disabled for tenant " + tenantDomain + ".");
            return BreachPolicy.disabled();
        }

        try {
            Map<String, String> values = read(governanceService, tenantDomain, baseProperties());
            boolean enabled = Boolean.parseBoolean(values.get(BreachDetectionConstants.PROPERTY_ENABLE));
            List<String> sourceIds = splitSources(values.get(BreachDetectionConstants.PROPERTY_SOURCES));
            if (!enabled) {
                return new BreachPolicy(false, sourceIds, new LinkedHashMap<>());
            }

            List<String> missing = new ArrayList<>();
            for (String sourceId : sourceIds) {
                if (!values.containsKey(onErrorProperty(sourceId))) {
                    missing.add(onErrorProperty(sourceId));
                }
            }
            if (!missing.isEmpty()) {
                // Policy can name a source that is not installed, so its failure policy is not in the first read.
                values.putAll(read(governanceService, tenantDomain, missing));
            }

            Map<String, FailurePolicy> failurePolicies = new LinkedHashMap<>();
            for (String sourceId : sourceIds) {
                String configured = values.get(onErrorProperty(sourceId));
                failurePolicies.put(sourceId, configured == null
                        ? defaultFailurePolicy(sourceId) : FailurePolicy.from(configured));
            }
            return new BreachPolicy(true, sourceIds, failurePolicies);
        } catch (Exception e) {
            LOG.error("Could not read breached password detection policy for tenant '" + tenantDomain
                    + "'. Treating it as disabled.", e);
            return BreachPolicy.disabled();
        }
    }

    /**
     * An offline source that cannot answer means its file is broken, which an operator can fix and which
     * carries no third-party outage risk - so it defaults to refusing. A remote source defaults to allowing,
     * because fail-closed on a third party turns their outage into an outage of every password write here.
     *
     * @param sourceId the source.
     * @return the default failure policy.
     */
    private FailurePolicy defaultFailurePolicy(String sourceId) {

        return registry.get(sourceId)
                .filter(source -> source.getCapabilities()
                        .contains(org.wso2.carbon.identity.breach.source.Capability.OFFLINE))
                .map(source -> FailurePolicy.DENY)
                .orElse(FailurePolicy.ALLOW);
    }

    private List<String> baseProperties() {

        List<String> names = new ArrayList<>();
        names.add(BreachDetectionConstants.PROPERTY_ENABLE);
        names.add(BreachDetectionConstants.PROPERTY_SOURCES);
        for (BreachSource source : registry.installed()) {
            names.add(onErrorProperty(source.getId()));
        }
        return names;
    }

    private Map<String, String> read(IdentityGovernanceService governanceService, String tenantDomain,
                                     List<String> names) throws Exception {

        Map<String, String> values = new LinkedHashMap<>();
        Property[] properties = governanceService.getConfiguration(names.toArray(new String[0]), tenantDomain);
        if (properties == null) {
            return values;
        }
        for (Property property : properties) {
            if (property != null && property.getName() != null) {
                values.put(property.getName(), property.getValue());
            }
        }
        return values;
    }

    /**
     * @param sourceId source id.
     * @return the governance property naming that source's failure policy.
     */
    public static String onErrorProperty(String sourceId) {

        return BreachDetectionConstants.PROPERTY_ON_ERROR_PREFIX + sourceId
                + BreachDetectionConstants.PROPERTY_ON_ERROR_SUFFIX;
    }

    private static List<String> splitSources(String value) {

        Set<String> ids = new LinkedHashSet<>();
        if (value != null) {
            for (String part : value.split(",")) {
                String trimmed = part.trim();
                if (!trimmed.isEmpty()) {
                    ids.add(trimmed);
                }
            }
        }
        return new ArrayList<>(ids);
    }
}
