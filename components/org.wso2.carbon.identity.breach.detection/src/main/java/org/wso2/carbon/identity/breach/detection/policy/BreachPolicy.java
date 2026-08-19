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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * One organization's effective policy: whether the capability is on, which sources it names, and what to do
 * when one of them cannot answer.
 */
public final class BreachPolicy {

    private static final BreachPolicy DISABLED =
            new BreachPolicy(false, Collections.emptyList(), Collections.emptyMap());

    private final boolean enabled;
    private final List<String> sourceIds;
    private final Map<String, FailurePolicy> failurePolicies;

    public BreachPolicy(boolean enabled, List<String> sourceIds, Map<String, FailurePolicy> failurePolicies) {

        this.enabled = enabled;
        this.sourceIds = Collections.unmodifiableList(sourceIds);
        this.failurePolicies = Collections.unmodifiableMap(new LinkedHashMap<>(failurePolicies));
    }

    /**
     * @return the policy a tenant with no explicit configuration inherits.
     */
    public static BreachPolicy disabled() {

        return DISABLED;
    }

    public boolean isEnabled() {

        return enabled;
    }

    /**
     * @return source ids as named in policy, in configuration order. The engine re-orders by declared priority.
     */
    public List<String> getSourceIds() {

        return sourceIds;
    }

    /**
     * @param sourceId source id as named in policy.
     * @return the failure policy for that source, defaulting to allow.
     */
    public FailurePolicy getFailurePolicy(String sourceId) {

        return failurePolicies.getOrDefault(sourceId, FailurePolicy.ALLOW);
    }

    public Map<String, FailurePolicy> getFailurePolicies() {

        return failurePolicies;
    }

    @Override
    public String toString() {

        return "BreachPolicy{enabled=" + enabled + ", sources=" + sourceIds + '}';
    }
}
