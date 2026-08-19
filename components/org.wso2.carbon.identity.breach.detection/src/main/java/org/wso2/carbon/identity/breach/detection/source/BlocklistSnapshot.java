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

/**
 * One consistent view of the operator's file.
 * <p>
 * A refresh builds a whole new snapshot before swapping the reference, so an evaluation in flight always sees
 * one consistent view and a half-written file can never produce partial matching.
 */
public final class BlocklistSnapshot {

    private final BlocklistIndex index;
    private final BlocklistFormat format;
    private final long entries;
    private final boolean entriesEstimated;
    private final long skipped;
    private final boolean truncated;
    private final long loadedAtEpochMillis;
    private final String path;
    private final long fileSize;
    private final long fileModified;

    BlocklistSnapshot(BlocklistIndex index, BlocklistFormat format, long entries, boolean entriesEstimated,
                      long skipped, boolean truncated, long loadedAtEpochMillis, String path, long fileSize,
                      long fileModified) {

        this.index = index;
        this.format = format;
        this.entries = entries;
        this.entriesEstimated = entriesEstimated;
        this.skipped = skipped;
        this.truncated = truncated;
        this.loadedAtEpochMillis = loadedAtEpochMillis;
        this.path = path;
        this.fileSize = fileSize;
        this.fileModified = fileModified;
    }

    public BlocklistIndex getIndex() {

        return index;
    }

    public BlocklistFormat getFormat() {

        return format;
    }

    public long getEntries() {

        return entries;
    }

    /**
     * @return whether the entry count is an estimate, which it is for the memory-mapped regime - counting the
     * lines of a tens-of-gigabytes file at every load would defeat the point of not reading it into heap.
     */
    public boolean isEntriesEstimated() {

        return entriesEstimated;
    }

    /**
     * @return how many entries were malformed, blank or unrecognised. Reported on every load: an operator has
     * to be able to tell the difference between a file that loaded and a file that mostly did not.
     */
    public long getSkipped() {

        return skipped;
    }

    /**
     * @return whether the file exceeded the maximum in-heap entry count and was cut short.
     */
    public boolean isTruncated() {

        return truncated;
    }

    public long getLoadedAtEpochMillis() {

        return loadedAtEpochMillis;
    }

    public String getPath() {

        return path;
    }

    public long getFileSize() {

        return fileSize;
    }

    public long getFileModified() {

        return fileModified;
    }
}
