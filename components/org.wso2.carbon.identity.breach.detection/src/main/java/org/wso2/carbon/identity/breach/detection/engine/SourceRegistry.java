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

package org.wso2.carbon.identity.breach.detection.engine;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.identity.breach.detection.util.BreachDetectionUtils;
import org.wso2.carbon.identity.breach.source.BreachSource;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * What is bound right now.
 * <p>
 * The engine holds no reference to any concrete source: it tracks whatever registers itself. Removing a
 * connector JAR unbinds one service and changes nothing else.
 * <p>
 * Because the set is dynamic, the registry is worth observing in its own right - the bound ids and their
 * declared priorities are logged on startup and on every bind and unbind, so an operator can answer "is the
 * connector actually loaded" without inspecting a directory.
 */
public class SourceRegistry {

    private static final Log LOG = LogFactory.getLog(SourceRegistry.class);

    private final ConcurrentHashMap<String, BreachSource> sources = new ConcurrentHashMap<>();

    public void bind(BreachSource source) {

        if (source == null || source.getId() == null || source.getId().trim().isEmpty()) {
            LOG.warn("Ignoring a breach source that did not declare an id.");
            return;
        }
        String key = BreachDetectionUtils.normalizeSourceId(source.getId());
        BreachSource previous = sources.put(key, source);
        if (previous != null) {
            LOG.warn("Breach source id '" + source.getId() + "' was already registered. The newly bound "
                    + "service replaces it.");
        }
        LOG.info("Breach source bound: id=" + source.getId() + ", priority=" + source.getPriority()
                + ", capabilities=" + source.getCapabilities() + ". Bound sources are now " + describe() + ".");
    }

    public void unbind(BreachSource source) {

        if (source == null || source.getId() == null) {
            return;
        }
        sources.remove(BreachDetectionUtils.normalizeSourceId(source.getId()), source);
        LOG.info("Breach source unbound: id=" + source.getId() + ". Bound sources are now " + describe() + ".");
    }

    /**
     * @param sourceId an id as written in policy or configuration.
     * @return the bound source, or empty when policy names something that is not installed.
     */
    public Optional<BreachSource> get(String sourceId) {

        return Optional.ofNullable(sources.get(BreachDetectionUtils.normalizeSourceId(sourceId)));
    }

    /**
     * @return every bound source, ordered by declared priority ascending. Ordering is data, not code: a local
     * in-process source declares a low number and therefore runs first without the engine knowing what it is.
     */
    public List<BreachSource> installed() {

        List<BreachSource> ordered = new ArrayList<>(sources.values());
        ordered.sort(Comparator.comparingInt(BreachSource::getPriority)
                .thenComparing(BreachSource::getId));
        return ordered;
    }

    /**
     * @return a compact operator-facing description of the bound set.
     */
    public String describe() {

        StringBuilder builder = new StringBuilder("[");
        boolean first = true;
        for (BreachSource source : installed()) {
            if (!first) {
                builder.append(", ");
            }
            builder.append(source.getId()).append('@').append(source.getPriority());
            first = false;
        }
        return builder.append(']').toString();
    }
}
