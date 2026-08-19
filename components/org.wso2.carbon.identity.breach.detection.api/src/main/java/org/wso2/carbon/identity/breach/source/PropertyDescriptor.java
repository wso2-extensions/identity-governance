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
 * One deployment setting a source needs.
 * <p>
 * The core does not know a connector's settings, so the connector declares them and the core resolves the
 * values - the connector reads no file and holds no vault handle of its own. The {@code secret} flag is what
 * makes that enforceable: a secret is resolved through the platform secret store, is never written to the
 * tenant governance store, and is never returned by any management API.
 */
public final class PropertyDescriptor {

    private final String name;
    private final PropertyType type;
    private final boolean required;
    private final boolean secret;
    private final String defaultValue;
    private final String displayName;
    private final String description;

    private PropertyDescriptor(Builder builder) {

        this.name = builder.name;
        this.type = builder.type;
        this.required = builder.required;
        this.secret = builder.secret;
        this.defaultValue = builder.defaultValue;
        this.displayName = builder.displayName == null ? builder.name : builder.displayName;
        this.description = builder.description;
    }

    /**
     * @return the setting name, as it appears under {@code [breach_detection.sources.&lt;id&gt;]}.
     */
    public String getName() {

        return name;
    }

    public PropertyType getType() {

        return type;
    }

    public boolean isRequired() {

        return required;
    }

    /**
     * @return {@code true} if the value is a credential: vault-resolved, never returned, never logged.
     */
    public boolean isSecret() {

        return secret;
    }

    public Optional<String> getDefaultValue() {

        return Optional.ofNullable(defaultValue);
    }

    public String getDisplayName() {

        return displayName;
    }

    public Optional<String> getDescription() {

        return Optional.ofNullable(description);
    }

    public static Builder builder(String name, PropertyType type) {

        return new Builder(name, type);
    }

    /**
     * Builder for {@link PropertyDescriptor}.
     */
    public static final class Builder {

        private final String name;
        private final PropertyType type;
        private boolean required;
        private boolean secret;
        private String defaultValue;
        private String displayName;
        private String description;

        private Builder(String name, PropertyType type) {

            this.name = name;
            this.type = type;
        }

        public Builder required(boolean required) {

            this.required = required;
            return this;
        }

        public Builder secret(boolean secret) {

            this.secret = secret;
            return this;
        }

        public Builder defaultValue(String defaultValue) {

            this.defaultValue = defaultValue;
            return this;
        }

        public Builder displayName(String displayName) {

            this.displayName = displayName;
            return this;
        }

        public Builder description(String description) {

            this.description = description;
            return this;
        }

        public PropertyDescriptor build() {

            return new PropertyDescriptor(this);
        }
    }
}
