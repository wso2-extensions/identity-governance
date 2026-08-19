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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/**
 * What a source reports about itself to an administrator.
 * <p>
 * A source proves it is working rather than asserting it: the facts map carries the entry count, the format,
 * the load time, the skipped-line count, the last success. An administrator who supplied a blocklist file can
 * confirm it was actually read, which is the state that separates enforcing from a silent no-op.
 */
public final class SourceStatus {

    /**
     * The state a source can report about itself. Whether a source is <em>installed</em> at all is the
     * engine's determination, not the source's.
     */
    public enum State {

        /** Set up and answering. */
        READY,

        /** Installed but not set up - no file, no required credential. */
        NOT_CONFIGURED,

        /** Set up but currently unable to answer. */
        UNAVAILABLE
    }

    private final State state;
    private final String summary;
    private final Map<String, String> facts;
    private final Long lastSuccessEpochMillis;

    private SourceStatus(Builder builder) {

        this.state = builder.state;
        this.summary = builder.summary;
        this.facts = Collections.unmodifiableMap(new LinkedHashMap<>(builder.facts));
        this.lastSuccessEpochMillis = builder.lastSuccessEpochMillis;
    }

    public State getState() {

        return state;
    }

    /**
     * @return a one-line operator-facing summary. Never contains a credential.
     */
    public Optional<String> getSummary() {

        return Optional.ofNullable(summary);
    }

    /**
     * @return ordered display facts, rendered as a label/value list in the administrator surface.
     */
    public Map<String, String> getFacts() {

        return facts;
    }

    public Optional<Long> getLastSuccessEpochMillis() {

        return Optional.ofNullable(lastSuccessEpochMillis);
    }

    public static Builder builder(State state) {

        return new Builder(state);
    }

    /**
     * Builder for {@link SourceStatus}.
     */
    public static final class Builder {

        private final State state;
        private final Map<String, String> facts = new LinkedHashMap<>();
        private String summary;
        private Long lastSuccessEpochMillis;

        private Builder(State state) {

            this.state = state;
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

        public Builder lastSuccess(Long epochMillis) {

            this.lastSuccessEpochMillis = epochMillis;
            return this;
        }

        public SourceStatus build() {

            return new SourceStatus(this);
        }
    }
}
