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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * Builds a {@link BlocklistSnapshot} from the operator's file.
 * <p>
 * File contents are treated strictly as literal evaluation data. A line is a password or a digest, never a
 * directive, a path, or markup - there is nothing a line can say that changes what the loader does.
 */
public class BlocklistLoader {

    private static final Log LOG = LogFactory.getLog(BlocklistLoader.class);

    private static final int DETECTION_SAMPLE_LINES = 32;
    private static final int ESTIMATION_SAMPLE_LINES = 1000;

    private BlocklistLoader() {

    }

    /**
     * Read the file and build an index.
     *
     * @param path             the file, already confined to a permitted location by the configuration layer.
     * @param requestedFormat  the configured format, or {@link BlocklistFormat#AUTO} to detect it.
     * @param mmapThresholdBytes above this size a hashed file is memory-mapped rather than read into heap.
     * @param maxHeapEntries   the in-heap ceiling, beyond which loading stops and reports truncation.
     * @return the snapshot.
     * @throws IOException if the file cannot be read.
     */
    public static BlocklistSnapshot load(Path path, BlocklistFormat requestedFormat, long mmapThresholdBytes,
                                         int maxHeapEntries) throws IOException {

        long size = Files.size(path);
        long modified = Files.getLastModifiedTime(path).toMillis();
        BlocklistFormat format = requestedFormat == BlocklistFormat.AUTO ? detect(path) : requestedFormat;

        if (format.isHashed() && size > mmapThresholdBytes) {
            long estimated = estimateEntries(path, size);
            BlocklistIndex index = new MappedBlocklistIndex(path, estimated);
            LOG.info("Loaded the breach blocklist from a " + (size / (1024 * 1024)) + " MB " + format
                    + " file using the memory-mapped regime. The file must be sorted by digest.");
            return new BlocklistSnapshot(index, format, estimated, true, 0, false, System.currentTimeMillis(),
                    path.toString(), size, modified);
        }

        Set<String> digests = new HashSet<>();
        long skipped = 0;
        boolean truncated = false;
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (digests.size() >= maxHeapEntries) {
                    truncated = true;
                    break;
                }
                String content = strip(line);
                if (content == null) {
                    continue;
                }
                String digest = toDigest(content, format);
                if (digest == null) {
                    skipped++;
                    continue;
                }
                digests.add(digest);
            }
        }

        if (truncated) {
            LOG.error("The breach blocklist at " + path + " exceeds the maximum of " + maxHeapEntries
                    + " in-heap entries and was loaded only up to that point. Raise the memory-map threshold "
                    + "and supply a digest-sorted file to index it in full.");
        }
        if (skipped > 0) {
            LOG.warn("Loaded the breach blocklist from " + path + " with " + skipped
                    + " malformed or unrecognised entries ignored.");
        }
        LOG.info("Loaded the breach blocklist: entries=" + digests.size() + ", skipped=" + skipped
                + ", format=" + format.toConfigValue() + ", regime=in-heap.");
        return new BlocklistSnapshot(new HeapBlocklistIndex(digests), format, digests.size(), false, skipped,
                truncated, System.currentTimeMillis(), path.toString(), size, modified);
    }

    /**
     * Blank lines and comments are not entries and are not counted as skipped. Only the line ending is
     * stripped: passwords are whitespace-significant, so nothing else is trimmed.
     */
    private static String strip(String line) {

        String content = line;
        if (content.endsWith("\r")) {
            content = content.substring(0, content.length() - 1);
        }
        if (content.isEmpty() || content.startsWith("#")) {
            return null;
        }
        return content;
    }

    private static String toDigest(String content, BlocklistFormat format) {

        if (format.isHashed()) {
            int separator = content.indexOf(':');
            String candidate = separator < 0 ? content : content.substring(0, separator);
            candidate = candidate.trim();
            if (candidate.length() != format.getHexLength() || !isHex(candidate)) {
                return null;
            }
            return candidate.toUpperCase(Locale.ROOT);
        }
        return digestOf(content, format.getDigestAlgorithm());
    }

    private static BlocklistFormat detect(Path path) throws IOException {

        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))) {
            String line;
            int inspected = 0;
            while ((line = reader.readLine()) != null && inspected < DETECTION_SAMPLE_LINES) {
                String content = strip(line);
                if (content == null) {
                    continue;
                }
                inspected++;
                int separator = content.indexOf(':');
                String candidate = (separator < 0 ? content : content.substring(0, separator)).trim();
                if (candidate.length() == 40 && isHex(candidate)) {
                    return BlocklistFormat.SHA1;
                }
                if (candidate.length() == 64 && isHex(candidate)) {
                    return BlocklistFormat.SHA256;
                }
                return BlocklistFormat.PLAINTEXT;
            }
        }
        return BlocklistFormat.PLAINTEXT;
    }

    private static long estimateEntries(Path path, long size) throws IOException {

        long bytes = 0;
        long lines = 0;
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))) {
            String line;
            while (lines < ESTIMATION_SAMPLE_LINES && (line = reader.readLine()) != null) {
                bytes += line.length() + 1;
                lines++;
            }
        }
        if (lines == 0 || bytes == 0) {
            return 0;
        }
        return size / Math.max(1, bytes / lines);
    }

    private static boolean isHex(String value) {

        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            boolean hex = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
            if (!hex) {
                return false;
            }
        }
        return true;
    }

    /**
     * Hash a file entry exactly as a candidate password is hashed: Unicode NFC, UTF-8, no case folding, no
     * trimming. Anything else would refuse credentials the operator never listed.
     */
    static String digestOf(String value, String algorithm) {

        try {
            String normalized = Normalizer.isNormalized(value, Normalizer.Form.NFC)
                    ? value : Normalizer.normalize(value, Normalizer.Form.NFC);
            MessageDigest digest = MessageDigest.getInstance(algorithm);
            byte[] out = digest.digest(normalized.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder(out.length * 2);
            for (byte b : out) {
                hex.append(Character.forDigit((b & 0xFF) >>> 4, 16));
                hex.append(Character.forDigit(b & 0x0F, 16));
            }
            return hex.toString().toUpperCase(Locale.ROOT);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("Unsupported digest algorithm: " + algorithm, e);
        }
    }
}
