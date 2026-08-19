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
 * Everything a source is given to reach a verdict: the candidate password, who it is for, where, and for what
 * kind of operation.
 * <p>
 * It carries more than a password-only source needs, on purpose. A validator context carrying only the value
 * and the tenant is structurally incapable of supporting credential-pair intelligence, and that limitation is
 * not recoverable once connectors exist in the field.
 */
public final class BreachContext {

    private final Credential credential;
    private final Subject subject;
    private final String tenantDomain;
    private final String organizationId;
    private final Operation operation;

    private BreachContext(Builder builder) {

        this.credential = builder.credential;
        this.subject = builder.subject;
        this.tenantDomain = builder.tenantDomain;
        this.organizationId = builder.organizationId;
        this.operation = builder.operation == null ? Operation.UNKNOWN : builder.operation;
    }

    public Credential getCredential() {

        return credential;
    }

    public Subject getSubject() {

        return subject;
    }

    public String getTenantDomain() {

        return tenantDomain;
    }

    /**
     * @return the organization id where the operation ran inside one; empty otherwise.
     */
    public Optional<String> getOrganizationId() {

        return Optional.ofNullable(organizationId);
    }

    public Operation getOperation() {

        return operation;
    }

    public static Builder builder() {

        return new Builder();
    }

    /**
     * Builder for {@link BreachContext}.
     */
    public static final class Builder {

        private Credential credential;
        private Subject subject;
        private String tenantDomain;
        private String organizationId;
        private Operation operation;

        private Builder() {

        }

        public Builder credential(Credential credential) {

            this.credential = credential;
            return this;
        }

        public Builder subject(Subject subject) {

            this.subject = subject;
            return this;
        }

        public Builder tenantDomain(String tenantDomain) {

            this.tenantDomain = tenantDomain;
            return this;
        }

        public Builder organizationId(String organizationId) {

            this.organizationId = organizationId;
            return this;
        }

        public Builder operation(Operation operation) {

            this.operation = operation;
            return this;
        }

        public BreachContext build() {

            if (credential == null) {
                throw new IllegalStateException("A breach context requires a credential.");
            }
            if (tenantDomain == null) {
                throw new IllegalStateException("A breach context requires a tenant domain.");
            }
            return new BreachContext(this);
        }
    }
}
