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

package org.wso2.carbon.identity.breach.detection.config;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.breach.source.PropertyDescriptor;
import org.wso2.carbon.identity.breach.source.PropertyType;
import org.wso2.carbon.identity.breach.source.SourceConfiguration;
import org.wso2.carbon.identity.breach.detection.util.BreachDetectionUtils;
import org.wso2.carbon.utils.CarbonUtils;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The settings one source declared, resolved against what the operator configured.
 * <p>
 * The source itself reads nothing: it declares what it needs and the core hands the values over. That is what
 * makes the {@code secret} flag enforceable rather than advisory, and it is why a connector holds no
 * filesystem or vault access of its own.
 */
public class ResolvedSourceConfiguration implements SourceConfiguration {

    private static final Log LOG = LogFactory.getLog(ResolvedSourceConfiguration.class);

    private final String sourceId;
    private final Map<String, String> values;
    private final Map<String, PropertyDescriptor> declared;

    public ResolvedSourceConfiguration(String sourceId, List<PropertyDescriptor> descriptors,
                                       SourceNamespace namespace) {

        this.sourceId = sourceId;
        this.declared = new LinkedHashMap<>();
        for (PropertyDescriptor descriptor : descriptors) {
            declared.put(descriptor.getName(), descriptor);
        }
        this.values = new HashMap<>();
        if (namespace != null) {
            values.putAll(namespace.getProperties());
            reportUnrecognisedKeys(namespace);
        }
        reportMissingRequired();
    }

    @Override
    public Optional<String> getString(String name) {

        String value = values.get(name);
        if (value != null && !value.trim().isEmpty()) {
            return Optional.of(value.trim());
        }
        PropertyDescriptor descriptor = declared.get(name);
        return descriptor == null ? Optional.empty() : descriptor.getDefaultValue();
    }

    @Override
    public int getInt(String name, int defaultValue) {

        return BreachDetectionUtils.parseInt(getString(name).orElse(null), defaultValue);
    }

    @Override
    public long getLong(String name, long defaultValue) {

        return BreachDetectionUtils.parseLong(getString(name).orElse(null), defaultValue);
    }

    @Override
    public boolean getBoolean(String name, boolean defaultValue) {

        return BreachDetectionUtils.parseBoolean(getString(name).orElse(null), defaultValue);
    }

    @Override
    public Optional<char[]> getSecret(String name) {

        PropertyDescriptor descriptor = declared.get(name);
        if (descriptor == null || !descriptor.isSecret()) {
            // Refusing here is the point: a value not declared secret must not be reachable as one.
            LOG.warn("Source '" + sourceId + "' asked for '" + name + "' as a secret, but it is not declared "
                    + "as one. Returning nothing.");
            return Optional.empty();
        }
        String value = values.get(name);
        if (value == null || value.trim().isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(value.trim().toCharArray());
    }

    @Override
    public Optional<String> getPath(String name) {

        Optional<String> configured = getString(name);
        if (!configured.isPresent()) {
            return Optional.empty();
        }
        String raw = expand(configured.get());
        Path candidate;
        try {
            candidate = Paths.get(raw).toAbsolutePath().normalize();
        } catch (Exception e) {
            LOG.error("Source '" + sourceId + "' was configured with an unusable path for '" + name + "'.");
            return Optional.empty();
        }
        if (!isWithinPermittedRoots(candidate)) {
            // Blocklist data is evaluation data, never a path reference the file itself can redirect.
            LOG.error("Source '" + sourceId + "' was configured with a path for '" + name
                    + "' outside the permitted locations. Ignoring it.");
            return Optional.empty();
        }
        return Optional.of(candidate.toString());
    }

    /**
     * @return the declared settings, for the administrator surface.
     */
    public Map<String, PropertyDescriptor> getDeclaredProperties() {

        return declared;
    }

    private boolean isWithinPermittedRoots(Path candidate) {

        Set<Path> roots = new HashSet<>();
        addRoot(roots, safeCarbonHome());
        addRoot(roots, System.getProperty("carbon.config.dir.path"));
        if (roots.isEmpty()) {
            // With no resolvable deployment root there is nothing to confine against; fail closed.
            return false;
        }
        for (Path root : roots) {
            if (candidate.startsWith(root)) {
                return true;
            }
        }
        return false;
    }

    private static void addRoot(Set<Path> roots, String raw) {

        if (raw == null || raw.trim().isEmpty()) {
            return;
        }
        try {
            roots.add(Paths.get(raw).toAbsolutePath().normalize());
        } catch (Exception ignored) {
            // An unusable root simply does not widen the permitted set.
        }
    }

    private static String safeCarbonHome() {

        try {
            return CarbonUtils.getCarbonHome();
        } catch (Throwable t) {
            return System.getProperty("carbon.home");
        }
    }

    private static String expand(String value) {

        String carbonHome = safeCarbonHome();
        String expanded = value;
        if (carbonHome != null) {
            expanded = expanded.replace("${carbon.home}", carbonHome);
        }
        return expanded.replace('/', File.separatorChar);
    }

    private void reportUnrecognisedKeys(SourceNamespace namespace) {

        for (String key : namespace.getProperties().keySet()) {
            if (!declared.containsKey(key)) {
                // Reported rather than ignored: a typo must not silently leave a connector on its default.
                LOG.warn("Breach detection source '" + sourceId + "' has no setting named '" + key
                        + "'. Check the [breach_detection.sources." + namespace.getId() + "] configuration.");
            }
        }
    }

    private void reportMissingRequired() {

        for (PropertyDescriptor descriptor : declared.values()) {
            if (descriptor.isRequired() && !getString(descriptor.getName()).isPresent()) {
                LOG.warn("Breach detection source '" + sourceId + "' requires '" + descriptor.getName()
                        + "', which is not configured. The source will report itself as not configured.");
            }
        }
    }
}
