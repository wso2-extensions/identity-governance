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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The operator configuration written under one source's namespace, before any source has claimed it.
 * <p>
 * A namespace whose id matches no registered source is reported at load rather than ignored - it usually means
 * a connector JAR is missing.
 */
public final class SourceNamespace {

    private final String id;
    private final Map<String, String> properties;
    private final Map<String, String> secretAliases;

    SourceNamespace(String id, Map<String, String> properties, Map<String, String> secretAliases) {

        this.id = id;
        this.properties = Collections.unmodifiableMap(new LinkedHashMap<>(properties));
        this.secretAliases = Collections.unmodifiableMap(new LinkedHashMap<>(secretAliases));
    }

    /**
     * @return the id exactly as written in configuration.
     */
    public String getId() {

        return id;
    }

    public Map<String, String> getProperties() {

        return properties;
    }

    /**
     * @return property name to secure-vault alias, for values written as {@code $secret{alias}}.
     */
    public Map<String, String> getSecretAliases() {

        return secretAliases;
    }
}
