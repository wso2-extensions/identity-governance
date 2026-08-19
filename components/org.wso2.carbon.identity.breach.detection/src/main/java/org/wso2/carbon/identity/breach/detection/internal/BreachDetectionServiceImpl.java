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

package org.wso2.carbon.identity.breach.detection.internal;

import org.wso2.carbon.identity.breach.detection.config.BreachDetectionConfig;
import org.wso2.carbon.identity.breach.detection.engine.SourceRegistry;
import org.wso2.carbon.identity.breach.detection.mgt.BreachDetectionService;
import org.wso2.carbon.identity.breach.detection.mgt.BreachDetectionStatus;
import org.wso2.carbon.identity.breach.detection.mgt.EnforcementStatus;
import org.wso2.carbon.identity.breach.detection.mgt.SourceState;
import org.wso2.carbon.identity.breach.detection.mgt.SourceView;
import org.wso2.carbon.identity.breach.detection.policy.BreachPolicy;
import org.wso2.carbon.identity.breach.detection.source.LocalBlocklistSource;
import org.wso2.carbon.identity.breach.source.BreachSource;
import org.wso2.carbon.identity.breach.source.SourceStatus;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Assembles what an administrator sees.
 * <p>
 * Installed and enabled are reported as separate sets, because their difference is the actionable state: a
 * source enabled in policy with nothing bound behind it needs a deployment action, not a configuration change.
 * Nothing here accepts a candidate password.
 */
public class BreachDetectionServiceImpl implements BreachDetectionService {

    @Override
    public BreachDetectionStatus getStatus(String tenantDomain) {

        BreachDetectionDataHolder holder = BreachDetectionDataHolder.getInstance();
        SourceRegistry registry = holder.getSourceRegistry();
        BreachDetectionConfig config = BreachDetectionConfig.getInstance();
        BreachPolicy policy = holder.getPolicyResolver() == null
                ? BreachPolicy.disabled() : holder.getPolicyResolver().resolve(tenantDomain);

        Set<String> ids = new LinkedHashSet<>();
        for (BreachSource source : registry.installed()) {
            ids.add(source.getId());
        }
        ids.addAll(policy.getSourceIds());

        List<SourceView> views = new ArrayList<>(ids.size());
        int enabledCount = 0;
        int readyCount = 0;
        for (String id : ids) {
            boolean enabled = policy.getSourceIds().stream().anyMatch(id::equalsIgnoreCase);
            SourceView view = toView(id, enabled, tenantDomain, policy);
            views.add(view);
            if (enabled) {
                enabledCount++;
                if (view.getState() == SourceState.READY) {
                    readyCount++;
                }
            }
        }

        EnforcementStatus status = resolveStatus(config, policy, enabledCount, readyCount);
        return new BreachDetectionStatus(tenantDomain, config.isEnabledAtDeployment(), policy.isEnabled(),
                status, views, SourceConfigurator.orphanedNamespaces(registry));
    }

    @Override
    public String reloadSources() {

        BreachDetectionConfig.reload();
        SourceRegistry registry = BreachDetectionDataHolder.getInstance().getSourceRegistry();
        SourceConfigurator.configureAll(registry);
        LocalBlocklistSource localList = BreachDetectionDataHolder.getInstance().getLocalBlocklistSource();
        String listOutcome = localList == null ? "No local blocklist source is present." : localList.reload();
        return "Reconfigured " + registry.installed().size() + " bound sources. " + listOutcome;
    }

    private EnforcementStatus resolveStatus(BreachDetectionConfig config, BreachPolicy policy, int enabledCount,
                                            int readyCount) {

        if (!config.isEnabledAtDeployment()) {
            return EnforcementStatus.DISABLED;
        }
        if (!policy.isEnabled()) {
            return EnforcementStatus.OFF;
        }
        if (enabledCount == 0 || readyCount == 0) {
            // Enabled with nothing that can answer is not enforcing, and is never reported as if it were.
            return EnforcementStatus.NOT_ENFORCING;
        }
        return readyCount == enabledCount ? EnforcementStatus.ENFORCING : EnforcementStatus.DEGRADED;
    }

    private SourceView toView(String id, boolean enabled, String tenantDomain, BreachPolicy policy) {

        BreachDetectionDataHolder holder = BreachDetectionDataHolder.getInstance();
        Optional<BreachSource> bound = holder.getSourceRegistry().get(id);
        SourceView.Builder builder = SourceView.builder(id)
                .enabled(enabled)
                .failurePolicy(policy.getFailurePolicy(id).toConfigValue())
                .stats(holder.getMetrics().snapshot(tenantDomain, id));

        if (!bound.isPresent()) {
            // Distinct from unreachable, because the fix is adding a connector rather than changing a setting.
            return builder.installed(false)
                    .state(SourceState.NOT_INSTALLED)
                    .displayName(id)
                    .description("Switched on in this organization's policy, but no connector with this id is "
                            + "present on this server.")
                    .fact("NEEDED", "connector in repository/components/dropins")
                    .build();
        }

        BreachSource source = bound.get();
        builder.installed(true)
                .displayName(source.getDescriptor().getDisplayName())
                .description(source.getDescriptor().getDescription())
                .vendor(source.getDescriptor().getVendor().orElse(null))
                .documentationUrl(source.getDescriptor().getDocumentationUrl().orElse(null))
                .privacyNotice(source.getDescriptor().getPrivacyNotice().orElse(null))
                .priority(source.getPriority())
                .capabilities(source.getCapabilities())
                .properties(source.getProperties());

        SourceStatus status;
        try {
            status = source.getStatus(tenantDomain);
        } catch (Throwable t) {
            status = SourceStatus.builder(SourceStatus.State.UNAVAILABLE)
                    .summary("The source failed to report its status.")
                    .build();
        }
        builder.facts(status.getFacts()).summary(status.getSummary().orElse(null));

        if (!enabled) {
            return builder.state(SourceState.OFF).build();
        }
        switch (status.getState()) {
            case READY:
                return builder.state(SourceState.READY).build();
            case NOT_CONFIGURED:
                return builder.state(SourceState.NOT_CONFIGURED).build();
            default:
                return builder.state(SourceState.UNAVAILABLE).build();
        }
    }
}
