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
import org.wso2.carbon.identity.breach.detection.constants.BreachDetectionConstants;
import org.wso2.carbon.identity.breach.source.BreachContext;
import org.wso2.carbon.identity.breach.source.BreachSource;
import org.wso2.carbon.identity.breach.source.BreachVerdict;
import org.wso2.carbon.identity.breach.source.Capability;
import org.wso2.carbon.identity.breach.source.Descriptor;
import org.wso2.carbon.identity.breach.source.PropertyDescriptor;
import org.wso2.carbon.identity.breach.source.PropertyType;
import org.wso2.carbon.identity.breach.source.SourceConfiguration;
import org.wso2.carbon.identity.breach.source.SourceStatus;
import org.wso2.carbon.identity.breach.source.UnavailableCause;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The operator's own list of forbidden passwords, answered without touching the network.
 * <p>
 * The only source that ships in the core, and the only one that is not a connector. It earns that on two
 * grounds: it crosses no boundary, so there is no third-party API to track, and it is what makes the capability
 * work at all in a network-isolated deployment. It is also the floor that keeps enforcing when every remote
 * source is down - which is what makes {@code allow} a defensible default failure policy rather than a shrug.
 * <p>
 * It registers through the same registry as any connector and gets no privileged path from the engine.
 */
public class LocalBlocklistSource implements BreachSource {

    private static final Log LOG = LogFactory.getLog(LocalBlocklistSource.class);

    public static final String PROPERTY_PATH = "path";
    public static final String PROPERTY_FORMAT = "format";
    public static final String PROPERTY_MMAP_THRESHOLD_MB = "mmap_threshold_mb";
    public static final String PROPERTY_REFRESH_SECONDS = "refresh_interval_seconds";
    public static final String PROPERTY_MAX_HEAP_ENTRIES = "max_heap_entries";

    /**
     * Measured, not guessed. A hashed file holds roughly 41 bytes per entry, so 32 MB is about 780,000
     * entries and about 90 MB of heap. Above that the memory-mapped regime costs about 7 microseconds a
     * lookup instead of 0.2, and no heap at all - a trade worth making long before a corpus reaches a
     * gigabyte.
     */
    private static final int DEFAULT_MMAP_THRESHOLD_MB = 32;
    private static final int DEFAULT_REFRESH_SECONDS = 300;
    /**
     * The documented in-heap ceiling: about 570 MB of heap at this size. Above it a digest-sorted file
     * should be supplied so the memory-mapped regime takes over instead.
     */
    private static final int DEFAULT_MAX_HEAP_ENTRIES = 5_000_000;

    private final AtomicReference<BlocklistSnapshot> snapshot = new AtomicReference<>();
    private final AtomicReference<String> lastError = new AtomicReference<>();
    private final ScheduledExecutorService scheduler;

    private volatile Path path;
    private volatile BlocklistFormat configuredFormat = BlocklistFormat.AUTO;
    private volatile long mmapThresholdBytes = (long) DEFAULT_MMAP_THRESHOLD_MB * 1024 * 1024;
    private volatile int maxHeapEntries = DEFAULT_MAX_HEAP_ENTRIES;
    private volatile ScheduledFuture<?> refreshTask;

    public LocalBlocklistSource() {

        this.scheduler = Executors.newSingleThreadScheduledExecutor(runnable -> {
            Thread thread = new Thread(runnable, "breach-blocklist-refresh");
            thread.setDaemon(true);
            return thread;
        });
    }

    @Override
    public String getId() {

        return BreachDetectionConstants.LOCAL_LIST_SOURCE_ID;
    }

    @Override
    public Descriptor getDescriptor() {

        return Descriptor.builder("Password list on this server")
                .description("Checks against a list maintained by your deployment team. "
                        + "Works without internet access.")
                .vendor("WSO2")
                .build();
    }

    @Override
    public List<PropertyDescriptor> getProperties() {

        return Arrays.asList(
                PropertyDescriptor.builder(PROPERTY_PATH, PropertyType.PATH)
                        .required(true)
                        .displayName("Blocklist file")
                        .description("Absolute path to the file, inside the deployment directory.")
                        .build(),
                PropertyDescriptor.builder(PROPERTY_FORMAT, PropertyType.STRING)
                        .defaultValue(BlocklistFormat.AUTO.toConfigValue())
                        .displayName("Format")
                        .description("sha1, sha256, plaintext, or auto. Hashed is the recommended default.")
                        .build(),
                PropertyDescriptor.builder(PROPERTY_MMAP_THRESHOLD_MB, PropertyType.INTEGER)
                        .defaultValue(String.valueOf(DEFAULT_MMAP_THRESHOLD_MB))
                        .displayName("Memory-map threshold (MB)")
                        .description("Above this size a digest-sorted file is searched in place instead of "
                                + "being read into heap.")
                        .build(),
                PropertyDescriptor.builder(PROPERTY_REFRESH_SECONDS, PropertyType.INTEGER)
                        .defaultValue(String.valueOf(DEFAULT_REFRESH_SECONDS))
                        .displayName("Refresh interval (seconds)")
                        .description("How often the file is checked for replacement. No restart is needed.")
                        .build(),
                PropertyDescriptor.builder(PROPERTY_MAX_HEAP_ENTRIES, PropertyType.INTEGER)
                        .defaultValue(String.valueOf(DEFAULT_MAX_HEAP_ENTRIES))
                        .displayName("Maximum in-heap entries")
                        .build());
    }

    @Override
    public int getPriority() {

        // In-process and certain. Consulted before any network round trip, so the passwords an operator most
        // wants blocked never leave the deployment and never consume third-party quota.
        return 100;
    }

    @Override
    public EnumSet<Capability> getCapabilities() {

        return EnumSet.of(Capability.OFFLINE, Capability.PASSWORD_ONLY);
    }

    @Override
    public void configure(SourceConfiguration configuration) {

        BlocklistFormat format = BlocklistFormat.from(configuration.getString(PROPERTY_FORMAT).orElse(null));
        long threshold = configuration.getLong(PROPERTY_MMAP_THRESHOLD_MB, DEFAULT_MMAP_THRESHOLD_MB)
                * 1024 * 1024;
        int maxEntries = configuration.getInt(PROPERTY_MAX_HEAP_ENTRIES, DEFAULT_MAX_HEAP_ENTRIES);
        String configuredPath = configuration.getPath(PROPERTY_PATH).orElse(null);

        // Configuration is handed over on bind and again on every reconfiguration, so an unchanged
        // configuration must not rebuild an index that is already correct.
        boolean unchanged = snapshot.get() != null
                && format == configuredFormat
                && threshold == mmapThresholdBytes
                && maxEntries == maxHeapEntries
                && path != null && path.toString().equals(configuredPath);

        this.configuredFormat = format;
        this.mmapThresholdBytes = threshold;
        this.maxHeapEntries = maxEntries;
        this.path = configuredPath == null ? null : Paths.get(configuredPath);
        if (path == null) {
            snapshot.set(null);
            lastError.set("No blocklist file is configured.");
            cancelRefresh();
            return;
        }
        if (!unchanged) {
            reload();
        }
        scheduleRefresh(configuration.getInt(PROPERTY_REFRESH_SECONDS, DEFAULT_REFRESH_SECONDS));
    }

    @Override
    public boolean isConfigured(String tenantDomain) {

        return path != null && snapshot.get() != null;
    }

    @Override
    public SourceStatus getStatus(String tenantDomain) {

        BlocklistSnapshot current = snapshot.get();
        if (path == null) {
            return SourceStatus.builder(SourceStatus.State.NOT_CONFIGURED)
                    .summary("No blocklist file is configured.")
                    .build();
        }
        if (current == null) {
            return SourceStatus.builder(SourceStatus.State.UNAVAILABLE)
                    .summary(lastError.get() == null ? "The blocklist file could not be read." : lastError.get())
                    .fact("FILE", path.toString())
                    .build();
        }
        SourceStatus.Builder builder = SourceStatus.builder(SourceStatus.State.READY)
                .lastSuccess(current.getLoadedAtEpochMillis())
                .fact("ENTRIES", formatEntries(current))
                .fact("FORMAT", describeFormat(current.getFormat()))
                .fact("LAST LOADED", formatTimestamp(current.getLoadedAtEpochMillis()))
                .fact("SKIPPED", current.getSkipped() + " malformed lines")
                .fact("STORAGE", current.getIndex().getRegime());
        if (current.isTruncated()) {
            builder.summary("The file exceeded the maximum in-heap entry count and was loaded only in part.");
        }
        if (lastError.get() != null) {
            builder.fact("LAST LOAD ERROR", lastError.get());
        }
        return builder.build();
    }

    @Override
    public BreachVerdict evaluate(BreachContext context) {

        BlocklistSnapshot current = snapshot.get();
        if (current == null) {
            // Not being able to check is not the same as finding nothing, and is never reported as if it were.
            return BreachVerdict.unavailable(getId(), UnavailableCause.MISCONFIGURED,
                    lastError.get() == null ? "No blocklist is loaded." : lastError.get());
        }
        String digest = context.getCredential().digestHex(current.getFormat().getDigestAlgorithm());
        if (current.getIndex().contains(digest)) {
            return BreachVerdict.found(getId());
        }
        return BreachVerdict.notFound(getId());
    }

    /**
     * Rebuild the index from the configured file.
     * <p>
     * The new index is built in full before the reference is swapped, so an evaluation in flight always sees one
     * consistent view. A file that cannot be parsed leaves the previously loaded list in effect and reports the
     * failure rather than quietly emptying the list.
     *
     * @return a human-readable summary of what happened.
     */
    public String reload() {

        Path current = path;
        if (current == null) {
            return "No blocklist file is configured.";
        }
        if (!Files.isReadable(current)) {
            String message = "The blocklist file is not readable.";
            lastError.set(message);
            LOG.error(message + " Path: " + current);
            return message;
        }
        try {
            BlocklistSnapshot loaded =
                    BlocklistLoader.load(current, configuredFormat, mmapThresholdBytes, maxHeapEntries);
            BlocklistSnapshot previous = snapshot.getAndSet(loaded);
            lastError.set(null);
            closeQuietly(previous);
            return "Loaded " + loaded.getEntries() + " entries with " + loaded.getSkipped() + " ignored.";
        } catch (Exception e) {
            String message = "The blocklist file could not be parsed. The previously loaded list stays in "
                    + "effect.";
            lastError.set(message);
            LOG.error(message + " Path: " + current, e);
            return message;
        }
    }

    /**
     * Release the index and stop the refresh task.
     */
    public void shutdown() {

        cancelRefresh();
        scheduler.shutdownNow();
        closeQuietly(snapshot.getAndSet(null));
    }

    private void scheduleRefresh(int intervalSeconds) {

        cancelRefresh();
        if (intervalSeconds <= 0) {
            return;
        }
        refreshTask = scheduler.scheduleWithFixedDelay(this::refreshIfChanged, intervalSeconds,
                intervalSeconds, TimeUnit.SECONDS);
    }

    private void cancelRefresh() {

        ScheduledFuture<?> task = refreshTask;
        if (task != null) {
            task.cancel(false);
            refreshTask = null;
        }
    }

    private void refreshIfChanged() {

        try {
            BlocklistSnapshot current = snapshot.get();
            Path file = path;
            if (file == null || !Files.isReadable(file)) {
                return;
            }
            long size = Files.size(file);
            long modified = Files.getLastModifiedTime(file).toMillis();
            if (current != null && current.getFileSize() == size && current.getFileModified() == modified) {
                return;
            }
            LOG.info("The breach blocklist file changed. Rebuilding the index.");
            reload();
        } catch (Exception e) {
            LOG.error("Failed to check the breach blocklist file for changes.", e);
        }
    }

    private static void closeQuietly(BlocklistSnapshot value) {

        if (value == null) {
            return;
        }
        try {
            value.getIndex().close();
        } catch (IOException e) {
            LOG.debug("Failed to release a superseded blocklist index.", e);
        }
    }

    private static String formatEntries(BlocklistSnapshot value) {

        String count = String.format(Locale.ROOT, "%,d", value.getEntries());
        return value.isEntriesEstimated() ? count + " (estimated)" : count;
    }

    private static String describeFormat(BlocklistFormat format) {

        switch (format) {
            case SHA1:
                return "SHA-1 hashes";
            case SHA256:
                return "SHA-256 hashes";
            default:
                return "plaintext, hashed on load";
        }
    }

    private static String formatTimestamp(long epochMillis) {

        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm 'UTC'", Locale.ROOT);
        format.setTimeZone(TimeZone.getTimeZone("UTC"));
        return format.format(new Date(epochMillis));
    }
}
