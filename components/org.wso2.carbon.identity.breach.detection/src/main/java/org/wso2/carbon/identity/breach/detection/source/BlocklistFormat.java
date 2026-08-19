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

import java.util.Locale;

/**
 * How the operator's file is written.
 * <p>
 * Hashed entries are canonical. Plaintext is accepted and hashed at load, so the in-memory representation is
 * uniform and a plaintext corpus never persists in that form inside the product.
 */
public enum BlocklistFormat {

    SHA1("SHA-1", 40),
    SHA256("SHA-256", 64),
    /** Hashed at load with SHA-256, so a plaintext file and a SHA-256 file index identically. */
    PLAINTEXT("SHA-256", -1),
    /** Decided by looking at the first usable line. */
    AUTO(null, -1);

    private final String digestAlgorithm;
    private final int hexLength;

    BlocklistFormat(String digestAlgorithm, int hexLength) {

        this.digestAlgorithm = digestAlgorithm;
        this.hexLength = hexLength;
    }

    /**
     * @return the algorithm a candidate password is hashed with to look it up.
     */
    public String getDigestAlgorithm() {

        return digestAlgorithm;
    }

    /**
     * @return the expected hex digest length, or -1 when entries are not hashed in the file.
     */
    public int getHexLength() {

        return hexLength;
    }

    public boolean isHashed() {

        return this == SHA1 || this == SHA256;
    }

    public static BlocklistFormat from(String value) {

        if (value == null) {
            return AUTO;
        }
        switch (value.trim().toLowerCase(Locale.ROOT)) {
            case "sha1":
            case "sha-1":
                return SHA1;
            case "sha256":
            case "sha-256":
                return SHA256;
            case "plaintext":
            case "plain":
                return PLAINTEXT;
            default:
                return AUTO;
        }
    }

    /**
     * @return the value as it appears in configuration.
     */
    public String toConfigValue() {

        switch (this) {
            case SHA1:
                return "sha1";
            case SHA256:
                return "sha256";
            case PLAINTEXT:
                return "plaintext";
            default:
                return "auto";
        }
    }
}
