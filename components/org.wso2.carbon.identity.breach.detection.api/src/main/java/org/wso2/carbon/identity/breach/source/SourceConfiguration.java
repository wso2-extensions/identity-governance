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

package org.wso2.carbon.identity.breach.source;

import java.util.Optional;

/**
 * The resolved deployment settings for one source, handed to it by the core.
 * <p>
 * A source never reads {@code deployment.toml} and never touches the vault: it declares what it needs through
 * {@link BreachSource#getProperties()} and receives the values here. That is what keeps the {@code secret}
 * flag on a {@link PropertyDescriptor} enforceable rather than advisory.
 */
public interface SourceConfiguration {

    /**
     * @param name declared property name.
     * @return the configured value, or the declared default, or empty.
     */
    Optional<String> getString(String name);

    /**
     * @param name         declared property name.
     * @param defaultValue value to use when unset or unparseable.
     * @return the resolved integer.
     */
    int getInt(String name, int defaultValue);

    /**
     * @param name         declared property name.
     * @param defaultValue value to use when unset or unparseable.
     * @return the resolved long.
     */
    long getLong(String name, long defaultValue);

    /**
     * @param name         declared property name.
     * @param defaultValue value to use when unset.
     * @return the resolved flag.
     */
    boolean getBoolean(String name, boolean defaultValue);

    /**
     * Resolve a property declared as {@code secret} through the platform secret store.
     * <p>
     * The caller owns the returned array and must wipe it after use. Never render it, log it, or return it
     * from an API.
     *
     * @param name declared property name.
     * @return the secret, or empty when unset.
     */
    Optional<char[]> getSecret(String name);

    /**
     * Resolve a property declared as a {@link PropertyType#PATH}, confined to the permitted locations. A value
     * that escapes them resolves to empty and is reported at load.
     *
     * @param name declared property name.
     * @return the resolved absolute path, or empty.
     */
    Optional<String> getPath(String name);
}
