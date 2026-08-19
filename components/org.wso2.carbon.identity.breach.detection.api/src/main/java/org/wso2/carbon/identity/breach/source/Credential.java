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

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.util.Arrays;

/**
 * The candidate password, held as a {@code char[]} rather than a {@code String} so it can be cleared and so it
 * never lands in the string pool or a heap dump for longer than the evaluation takes.
 * <p>
 * It has no meaningful {@code toString}, and must never enter a log statement, an exception message, a metric
 * label, or a cache key. The engine clears it after the last source returns; a source must not retain it.
 */
public final class Credential {

    private static final String MASK = "Credential{****}";

    private final char[] chars;
    private volatile boolean cleared;

    /**
     * @param chars the candidate password. Taken by reference: the caller must not mutate or reuse it.
     */
    public Credential(char[] chars) {

        if (chars == null) {
            throw new IllegalArgumentException("Credential characters cannot be null.");
        }
        this.chars = chars;
    }

    /**
     * The raw characters. Never copy these into a {@code String}.
     *
     * @return the backing array, by reference.
     */
    public char[] getChars() {

        assertUsable();
        return chars;
    }

    /**
     * @return the number of characters, safe to log.
     */
    public int length() {

        return chars.length;
    }

    /**
     * The canonical byte form: Unicode NFC, encoded UTF-8. No case folding and no trimming - passwords are
     * case- and whitespace-significant, and folding them would refuse credentials nobody listed.
     * <p>
     * The caller owns the returned array and should wipe it once done.
     *
     * @return canonical UTF-8 bytes.
     */
    public byte[] canonicalBytes() {

        assertUsable();
        CharBuffer canonical = canonicalChars();
        ByteBuffer encoded = StandardCharsets.UTF_8.encode(canonical);
        byte[] bytes = new byte[encoded.remaining()];
        encoded.get(bytes);
        if (encoded.hasArray()) {
            Arrays.fill(encoded.array(), (byte) 0);
        }
        if (canonical.hasArray() && canonical.array() != chars) {
            Arrays.fill(canonical.array(), '\0');
        }
        return bytes;
    }

    /**
     * Digest of the canonical byte form, uppercase hex. This is what a source matches on; the credential
     * itself never leaves the process.
     *
     * @param algorithm a {@link MessageDigest} algorithm name, for example {@code SHA-1} or {@code SHA-256}.
     * @return uppercase hex digest.
     * @throws IllegalArgumentException if the algorithm is not available.
     */
    public String digestHex(String algorithm) {

        byte[] bytes = canonicalBytes();
        try {
            MessageDigest digest = MessageDigest.getInstance(algorithm);
            byte[] out = digest.digest(bytes);
            char[] hex = new char[out.length * 2];
            for (int i = 0; i < out.length; i++) {
                int v = out[i] & 0xFF;
                hex[i * 2] = Character.forDigit(v >>> 4, 16);
                hex[i * 2 + 1] = Character.forDigit(v & 0x0F, 16);
            }
            return new String(hex).toUpperCase(java.util.Locale.ROOT);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalArgumentException("Unsupported digest algorithm: " + algorithm, e);
        } finally {
            Arrays.fill(bytes, (byte) 0);
        }
    }

    /**
     * Zero the backing array. Called by the engine once every source has returned.
     */
    public void clear() {

        Arrays.fill(chars, '\0');
        cleared = true;
    }

    /**
     * @return {@code true} once {@link #clear()} has run.
     */
    public boolean isCleared() {

        return cleared;
    }

    private CharBuffer canonicalChars() {

        CharBuffer raw = CharBuffer.wrap(chars);
        if (Normalizer.isNormalized(raw, Normalizer.Form.NFC)) {
            return raw;
        }
        return CharBuffer.wrap(Normalizer.normalize(raw, Normalizer.Form.NFC).toCharArray());
    }

    private void assertUsable() {

        if (cleared) {
            throw new IllegalStateException("The credential has already been cleared.");
        }
    }

    @Override
    public String toString() {

        return MASK;
    }
}
