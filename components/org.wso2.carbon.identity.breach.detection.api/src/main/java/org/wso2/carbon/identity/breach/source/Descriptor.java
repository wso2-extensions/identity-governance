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
 * How a source describes itself to an administrator.
 * <p>
 * The Console cannot hard-code a label for a source it has never seen, so the copy travels with the source.
 * Write it in an administrator's language, not the mechanism's - "sends only a partial, irreversible
 * fingerprint of the password", not "k-anonymity".
 */
public final class Descriptor {

    private final String displayName;
    private final String description;
    private final String vendor;
    private final String documentationUrl;
    private final String privacyNotice;

    private Descriptor(Builder builder) {

        this.displayName = builder.displayName;
        this.description = builder.description;
        this.vendor = builder.vendor;
        this.documentationUrl = builder.documentationUrl;
        this.privacyNotice = builder.privacyNotice;
    }

    public String getDisplayName() {

        return displayName;
    }

    public String getDescription() {

        return description;
    }

    public Optional<String> getVendor() {

        return Optional.ofNullable(vendor);
    }

    public Optional<String> getDocumentationUrl() {

        return Optional.ofNullable(documentationUrl);
    }

    /**
     * What this source is told about the user, in the administrator's language. Rendered before a source
     * declaring {@link Capability#NEEDS_SUBJECT} can be enabled.
     *
     * @return the disclosure, if the source supplies one.
     */
    public Optional<String> getPrivacyNotice() {

        return Optional.ofNullable(privacyNotice);
    }

    public static Builder builder(String displayName) {

        return new Builder(displayName);
    }

    /**
     * Builder for {@link Descriptor}.
     */
    public static final class Builder {

        private final String displayName;
        private String description;
        private String vendor;
        private String documentationUrl;
        private String privacyNotice;

        private Builder(String displayName) {

            this.displayName = displayName;
        }

        public Builder description(String description) {

            this.description = description;
            return this;
        }

        public Builder vendor(String vendor) {

            this.vendor = vendor;
            return this;
        }

        public Builder documentationUrl(String documentationUrl) {

            this.documentationUrl = documentationUrl;
            return this;
        }

        public Builder privacyNotice(String privacyNotice) {

            this.privacyNotice = privacyNotice;
            return this;
        }

        public Descriptor build() {

            return new Descriptor(this);
        }
    }
}
