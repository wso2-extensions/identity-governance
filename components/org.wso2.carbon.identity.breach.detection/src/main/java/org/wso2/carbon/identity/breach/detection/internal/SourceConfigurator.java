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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.breach.detection.config.BreachDetectionConfig;
import org.wso2.carbon.identity.breach.detection.config.ResolvedSourceConfiguration;
import org.wso2.carbon.identity.breach.detection.config.SourceNamespace;
import org.wso2.carbon.identity.breach.detection.engine.SourceRegistry;
import org.wso2.carbon.identity.breach.detection.util.BreachDetectionUtils;
import org.wso2.carbon.identity.breach.source.BreachSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Hands each source the settings it declared, resolved from operator configuration.
 * <p>
 * This is the only place a source's configuration is assembled. A connector reads nothing itself and receives
 * no filesystem or vault access of its own, which is what makes the {@code secret} flag on a property
 * descriptor enforceable.
 */
public final class SourceConfigurator {

    private static final Log LOG = LogFactory.getLog(SourceConfigurator.class);

    private SourceConfigurator() {

    }

    /**
     * @param source the source to configure.
     */
    public static void configure(BreachSource source) {

        if (source == null) {
            return;
        }
        try {
            SourceNamespace namespace = BreachDetectionConfig.getInstance()
                    .getSourceNamespace(BreachDetectionUtils.normalizeSourceId(source.getId()));
            source.configure(new ResolvedSourceConfiguration(source.getId(), source.getProperties(), namespace));
        } catch (Throwable t) {
            // Contained: a connector that cannot be configured must not stop the others from starting.
            LOG.error("Failed to configure breach source '" + source.getId()
                    + "'. It will report itself as not configured.", t);
        }
    }

    /**
     * @param registry the registry to walk.
     */
    public static void configureAll(SourceRegistry registry) {

        for (BreachSource source : registry.installed()) {
            configure(source);
        }
    }

    /**
     * Configuration namespaces naming a source id that nothing has registered.
     * <p>
     * Reported rather than ignored: it usually means a connector JAR is missing, and that is a deployment
     * action rather than a configuration one.
     *
     * @param registry the registry to check against.
     * @return the orphaned namespace ids, as written in configuration.
     */
    public static List<String> orphanedNamespaces(SourceRegistry registry) {

        List<String> orphaned = new ArrayList<>();
        for (Map.Entry<String, SourceNamespace> entry
                : BreachDetectionConfig.getInstance().getSourceNamespaces().entrySet()) {
            if (!registry.get(entry.getKey()).isPresent()) {
                orphaned.add(entry.getValue().getId());
            }
        }
        return orphaned;
    }
}
