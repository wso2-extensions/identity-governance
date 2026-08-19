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

package org.wso2.carbon.identity.breach.detection.source;

import org.wso2.carbon.identity.breach.source.SourceConfiguration;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * The settings a source would have been handed, without the deployment configuration layer in the way.
 */
class MapSourceConfiguration implements SourceConfiguration {

    private final Map<String, String> values = new HashMap<>();

    MapSourceConfiguration set(String name, Object value) {

        values.put(name, value == null ? null : String.valueOf(value));
        return this;
    }

    @Override
    public Optional<String> getString(String name) {

        return Optional.ofNullable(values.get(name));
    }

    @Override
    public int getInt(String name, int defaultValue) {

        return getString(name).map(Integer::parseInt).orElse(defaultValue);
    }

    @Override
    public long getLong(String name, long defaultValue) {

        return getString(name).map(Long::parseLong).orElse(defaultValue);
    }

    @Override
    public boolean getBoolean(String name, boolean defaultValue) {

        return getString(name).map(Boolean::parseBoolean).orElse(defaultValue);
    }

    @Override
    public Optional<char[]> getSecret(String name) {

        return getString(name).map(String::toCharArray);
    }

    @Override
    public Optional<String> getPath(String name) {

        return getString(name);
    }
}
