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

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;

/**
 * A full offline corpus, memory-mapped and searched in place.
 * <p>
 * The HIBP offline download is already ordered by hash, so this regime needs no preprocessing - which is what
 * makes an air-gapped deployment practical rather than merely possible. Heap cost is negligible and pages are
 * faulted on demand.
 * <p>
 * Mapped in chunks because a single {@link MappedByteBuffer} cannot exceed {@link Integer#MAX_VALUE} bytes and
 * these files run to tens of gigabytes.
 */
public class MappedBlocklistIndex implements BlocklistIndex {

    private static final long CHUNK_SIZE = 1L << 30;
    /** Beyond this many probes the search is not converging; give up rather than spin. */
    private static final int MAX_TAIL_SCAN_LINES = 64;

    private final RandomAccessFile file;
    private final FileChannel channel;
    private final MappedByteBuffer[] chunks;
    private final long length;
    private final long entries;

    MappedBlocklistIndex(Path path, long entries) throws IOException {

        this.file = new RandomAccessFile(path.toFile(), "r");
        this.channel = file.getChannel();
        this.length = channel.size();
        this.entries = entries;
        int chunkCount = (int) ((length + CHUNK_SIZE - 1) / CHUNK_SIZE);
        this.chunks = new MappedByteBuffer[Math.max(1, chunkCount)];
        for (int i = 0; i < chunkCount; i++) {
            long offset = (long) i * CHUNK_SIZE;
            long size = Math.min(CHUNK_SIZE, length - offset);
            chunks[i] = channel.map(FileChannel.MapMode.READ_ONLY, offset, size);
        }
    }

    @Override
    public boolean contains(String uppercaseHexDigest) {

        if (length == 0 || uppercaseHexDigest == null) {
            return false;
        }
        long low = 0;
        long high = length;
        while (low < high) {
            long mid = (low + high) >>> 1;
            long lineStart = mid == 0 ? 0 : nextLineStart(mid, high);
            if (lineStart >= high) {
                return scanRange(low, high, uppercaseHexDigest);
            }
            String key = readKey(lineStart);
            int comparison = key.compareTo(uppercaseHexDigest);
            if (comparison == 0) {
                return true;
            }
            if (comparison < 0) {
                low = endOfLine(lineStart) + 1;
            } else {
                high = lineStart;
            }
        }
        return false;
    }

    @Override
    public long size() {

        return entries;
    }

    @Override
    public String getRegime() {

        return "memory-mapped sorted hash file";
    }

    @Override
    public void close() throws IOException {

        try {
            channel.close();
        } finally {
            file.close();
        }
    }

    private boolean scanRange(long from, long to, String target) {

        long position = from == 0 ? 0 : nextLineStart(from - 1, to);
        int lines = 0;
        while (position < to && lines++ < MAX_TAIL_SCAN_LINES) {
            String key = readKey(position);
            int comparison = key.compareTo(target);
            if (comparison == 0) {
                return true;
            }
            if (comparison > 0) {
                return false;
            }
            position = endOfLine(position) + 1;
        }
        return false;
    }

    private long nextLineStart(long from, long limit) {

        long position = from;
        while (position < limit && byteAt(position) != '\n') {
            position++;
        }
        return position + 1;
    }

    private long endOfLine(long from) {

        long position = from;
        while (position < length && byteAt(position) != '\n') {
            position++;
        }
        return position;
    }

    private String readKey(long lineStart) {

        StringBuilder builder = new StringBuilder(64);
        long position = lineStart;
        while (position < length) {
            byte b = byteAt(position);
            if (b == '\n' || b == '\r' || b == ':') {
                break;
            }
            builder.append(toUpperAscii((char) (b & 0xFF)));
            position++;
        }
        return builder.toString();
    }

    private byte byteAt(long position) {

        int chunk = (int) (position / CHUNK_SIZE);
        int offset = (int) (position % CHUNK_SIZE);
        return chunks[chunk].get(offset);
    }

    private static char toUpperAscii(char c) {

        return c >= 'a' && c <= 'z' ? (char) (c - 32) : c;
    }
}
