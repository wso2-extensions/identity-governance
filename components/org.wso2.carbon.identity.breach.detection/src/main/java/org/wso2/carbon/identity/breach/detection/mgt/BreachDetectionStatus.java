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

package org.wso2.carbon.identity.breach.detection.mgt;

import java.util.Collections;
import java.util.List;

/**
 * What the capability is doing for one organization, right now.
 * <p>
 * This is the object the administrator surface renders. It reports installed and enabled sources as separate
 * facts and never claims to be enforcing when nothing can answer.
 */
public final class BreachDetectionStatus {

    private final String tenantDomain;
    private final boolean enabledAtDeployment;
    private final boolean enabledForOrganization;
    private final EnforcementStatus status;
    private final List<SourceView> sources;
    private final List<String> orphanedConfigurationNamespaces;

    public BreachDetectionStatus(String tenantDomain, boolean enabledAtDeployment,
                                 boolean enabledForOrganization, EnforcementStatus status,
                                 List<SourceView> sources, List<String> orphanedConfigurationNamespaces) {

        this.tenantDomain = tenantDomain;
        this.enabledAtDeployment = enabledAtDeployment;
        this.enabledForOrganization = enabledForOrganization;
        this.status = status;
        this.sources = Collections.unmodifiableList(sources);
        this.orphanedConfigurationNamespaces = Collections.unmodifiableList(orphanedConfigurationNamespaces);
    }

    public String getTenantDomain() {

        return tenantDomain;
    }

    public boolean isEnabledAtDeployment() {

        return enabledAtDeployment;
    }

    public boolean isEnabledForOrganization() {

        return enabledForOrganization;
    }

    public EnforcementStatus getStatus() {

        return status;
    }

    /**
     * @return every source that is either installed or named in policy, so the difference is visible.
     */
    public List<SourceView> getSources() {

        return sources;
    }

    /**
     * @return deployment configuration namespaces naming a source id nothing has registered. Usually a missing
     * connector JAR.
     */
    public List<String> getOrphanedConfigurationNamespaces() {

        return orphanedConfigurationNamespaces;
    }
}
