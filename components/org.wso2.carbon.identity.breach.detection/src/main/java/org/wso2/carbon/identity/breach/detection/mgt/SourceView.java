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

import org.wso2.carbon.identity.breach.source.Capability;
import org.wso2.carbon.identity.breach.source.PropertyDescriptor;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * One source as the administrator surface renders it.
 * <p>
 * <em>Installed</em> and <em>enabled</em> are reported separately on purpose. Their difference is the
 * actionable state: a source enabled in policy but not installed is a deployment problem, and one installed but
 * not enabled is simply available.
 */
public final class SourceView {

    private final String id;
    private final String displayName;
    private final String description;
    private final String vendor;
    private final String documentationUrl;
    private final String privacyNotice;
    private final boolean installed;
    private final boolean enabled;
    private final int priority;
    private final Set<Capability> capabilities;
    private final SourceState state;
    private final String summary;
    private final Map<String, String> facts;
    private final List<PropertyDescriptor> properties;
    private final SourceStats stats;
    private final String failurePolicy;

    private SourceView(Builder builder) {

        this.id = builder.id;
        this.displayName = builder.displayName;
        this.description = builder.description;
        this.vendor = builder.vendor;
        this.documentationUrl = builder.documentationUrl;
        this.privacyNotice = builder.privacyNotice;
        this.installed = builder.installed;
        this.enabled = builder.enabled;
        this.priority = builder.priority;
        this.capabilities = builder.capabilities;
        this.state = builder.state;
        this.summary = builder.summary;
        this.facts = Collections.unmodifiableMap(new LinkedHashMap<>(builder.facts));
        this.properties = Collections.unmodifiableList(builder.properties);
        this.stats = builder.stats;
        this.failurePolicy = builder.failurePolicy;
    }

    public String getId() {

        return id;
    }

    public String getDisplayName() {

        return displayName;
    }

    public String getDescription() {

        return description;
    }

    public Optional<String> getVendor() {

        return Optional.ofNullable(vendor);
    }

    public Optional<String> getDocumentationUrl() {

        return Optional.ofNullable(documentationUrl);
    }

    /**
     * @return what this source is told about the user, rendered before it can be enabled.
     */
    public Optional<String> getPrivacyNotice() {

        return Optional.ofNullable(privacyNotice);
    }

    /**
     * @return whether a service is bound right now.
     */
    public boolean isInstalled() {

        return installed;
    }

    /**
     * @return whether this organization's policy names it.
     */
    public boolean isEnabled() {

        return enabled;
    }

    public int getPriority() {

        return priority;
    }

    public Set<Capability> getCapabilities() {

        return capabilities;
    }

    public SourceState getState() {

        return state;
    }

    public Optional<String> getSummary() {

        return Optional.ofNullable(summary);
    }

    /**
     * @return what the source proves about itself: entry count, format, load time, skipped lines, last success.
     */
    public Map<String, String> getFacts() {

        return facts;
    }

    /**
     * @return the settings the source declares. A secret renders write-only and is never returned as a value.
     */
    public List<PropertyDescriptor> getProperties() {

        return properties;
    }

    public SourceStats getStats() {

        return stats;
    }

    /**
     * @return {@code allow} or {@code deny}: what happens when this source cannot be reached.
     */
    public String getFailurePolicy() {

        return failurePolicy;
    }

    public static Builder builder(String id) {

        return new Builder(id);
    }

    /**
     * Builder for {@link SourceView}.
     */
    public static final class Builder {

        private final String id;
        private final Map<String, String> facts = new LinkedHashMap<>();
        private String displayName;
        private String description;
        private String vendor;
        private String documentationUrl;
        private String privacyNotice;
        private boolean installed;
        private boolean enabled;
        private int priority;
        private Set<Capability> capabilities = Collections.emptySet();
        private SourceState state = SourceState.OFF;
        private String summary;
        private List<PropertyDescriptor> properties = Collections.emptyList();
        private SourceStats stats;
        private String failurePolicy;

        private Builder(String id) {

            this.id = id;
            this.displayName = id;
        }

        public Builder displayName(String displayName) {

            this.displayName = displayName;
            return this;
        }

        public Builder description(String description) {

            this.description = description;
            return this;
        }

        public Builder vendor(String vendor) {

            this.vendor = vendor;
            return this;
        }

        public Builder documentationUrl(String documentationUrl) {

            this.documentationUrl = documentationUrl;
            return this;
        }

        public Builder privacyNotice(String privacyNotice) {

            this.privacyNotice = privacyNotice;
            return this;
        }

        public Builder installed(boolean installed) {

            this.installed = installed;
            return this;
        }

        public Builder enabled(boolean enabled) {

            this.enabled = enabled;
            return this;
        }

        public Builder priority(int priority) {

            this.priority = priority;
            return this;
        }

        public Builder capabilities(Set<Capability> capabilities) {

            this.capabilities = Collections.unmodifiableSet(capabilities);
            return this;
        }

        public Builder state(SourceState state) {

            this.state = state;
            return this;
        }

        public Builder summary(String summary) {

            this.summary = summary;
            return this;
        }

        public Builder fact(String label, String value) {

            if (label != null && value != null) {
                facts.put(label, value);
            }
            return this;
        }

        public Builder facts(Map<String, String> values) {

            if (values != null) {
                facts.putAll(values);
            }
            return this;
        }

        public Builder properties(List<PropertyDescriptor> properties) {

            this.properties = properties;
            return this;
        }

        public Builder stats(SourceStats stats) {

            this.stats = stats;
            return this;
        }

        public Builder failurePolicy(String failurePolicy) {

            this.failurePolicy = failurePolicy;
            return this;
        }

        public SourceView build() {

            return new SourceView(this);
        }
    }
}
