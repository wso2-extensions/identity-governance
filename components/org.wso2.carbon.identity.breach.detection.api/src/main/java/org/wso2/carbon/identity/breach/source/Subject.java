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
import java.util.function.Function;

/**
 * Who the password is being set for.
 * <p>
 * Present on every context so that a source keyed on credential pairs rather than passwords alone can be added
 * without redesigning anything. A source declaring {@link Capability#PASSWORD_ONLY} ignores this entirely, and
 * the administrator surface uses {@link Capability#NEEDS_SUBJECT} to disclose that identity is transmitted
 * before such a source can be enabled.
 */
public final class Subject {

    private final String username;
    private final String userId;
    private final String userStoreDomain;
    private final Function<String, String> claimResolver;

    private Subject(Builder builder) {

        this.username = builder.username;
        this.userId = builder.userId;
        this.userStoreDomain = builder.userStoreDomain;
        this.claimResolver = builder.claimResolver;
    }

    public String getUsername() {

        return username;
    }

    /**
     * @return the user id where the operation carried one; empty on create, where no id exists yet.
     */
    public Optional<String> getUserId() {

        return Optional.ofNullable(userId);
    }

    public String getUserStoreDomain() {

        return userStoreDomain;
    }

    /**
     * Resolve a claim for this subject. Lazily evaluated - a source that never asks costs no store round trip.
     *
     * @param claimUri claim URI.
     * @return the claim value, or empty if unset or unresolvable.
     */
    public Optional<String> getClaim(String claimUri) {

        if (claimResolver == null || claimUri == null) {
            return Optional.empty();
        }
        return Optional.ofNullable(claimResolver.apply(claimUri));
    }

    public static Builder builder(String username) {

        return new Builder(username);
    }

    /**
     * Builder for {@link Subject}.
     */
    public static final class Builder {

        private final String username;
        private String userId;
        private String userStoreDomain;
        private Function<String, String> claimResolver;

        private Builder(String username) {

            this.username = username;
        }

        public Builder userId(String userId) {

            this.userId = userId;
            return this;
        }

        public Builder userStoreDomain(String userStoreDomain) {

            this.userStoreDomain = userStoreDomain;
            return this;
        }

        public Builder claimResolver(Function<String, String> claimResolver) {

            this.claimResolver = claimResolver;
            return this;
        }

        public Subject build() {

            return new Subject(this);
        }
    }
}
