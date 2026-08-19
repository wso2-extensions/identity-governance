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

import org.testng.annotations.Test;
import org.wso2.carbon.identity.breach.source.BreachContext;
import org.wso2.carbon.identity.breach.source.Capability;
import org.wso2.carbon.identity.breach.source.Credential;
import org.wso2.carbon.identity.breach.source.Operation;
import org.wso2.carbon.identity.breach.source.Outcome;
import org.wso2.carbon.identity.breach.source.SourceStatus;
import org.wso2.carbon.identity.breach.source.UnavailableCause;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * The offline source as the engine and the administrator surface see it.
 */
public class LocalBlocklistSourceTest {

    private static final String TENANT = "carbon.super";

    @Test
    public void declaresItselfAsOfflineAndCheapSoTheEngineConsultsItFirst() {

        LocalBlocklistSource source = new LocalBlocklistSource();
        assertTrue(source.getCapabilities().contains(Capability.OFFLINE));
        assertTrue(source.getPriority() < 500);
        source.shutdown();
    }

    @Test
    public void refusesAListedPasswordAndAcceptsAnUnlistedOne() throws IOException {

        Path file = write(Arrays.asList("Password@1", "Qwerty@123"));
        LocalBlocklistSource source = configured(file, "plaintext");

        assertEquals(source.evaluate(context("Password@1")).getOutcome(), Outcome.FOUND);
        assertEquals(source.evaluate(context("Zx9q!Kt7#Lm2vRb4")).getOutcome(), Outcome.NOT_FOUND);
        source.shutdown();
    }

    @Test
    public void withNoFileConfiguredItIsUnavailableRatherThanReportingEveryPasswordClean() {

        LocalBlocklistSource source = new LocalBlocklistSource();
        source.configure(new MapSourceConfiguration());

        assertFalse(source.isConfigured(TENANT));
        assertEquals(source.evaluate(context("Password@1")).getOutcome(), Outcome.UNAVAILABLE);
        assertEquals(source.evaluate(context("Password@1")).getCause().orElse(null),
                UnavailableCause.MISCONFIGURED);
        assertEquals(source.getStatus(TENANT).getState(), SourceStatus.State.NOT_CONFIGURED);
        source.shutdown();
    }

    @Test
    public void itProvesItLoadedRatherThanAssertingIt() throws IOException {

        Path file = write(Arrays.asList("Password@1", "NOTAHASH-but-a-valid-plaintext-entry"));
        LocalBlocklistSource source = configured(file, "plaintext");

        SourceStatus status = source.getStatus(TENANT);
        assertEquals(status.getState(), SourceStatus.State.READY);
        assertEquals(status.getFacts().get("ENTRIES"), "2");
        assertEquals(status.getFacts().get("SKIPPED"), "0 malformed lines");
        assertTrue(status.getFacts().containsKey("LAST LOADED"));
        assertTrue(status.getLastSuccessEpochMillis().isPresent());
        source.shutdown();
    }

    @Test
    public void aReplacedFileTakesEffectWithoutARestart() throws IOException {

        Path file = write(Arrays.asList("Password@1"));
        LocalBlocklistSource source = configured(file, "plaintext");
        assertEquals(source.evaluate(context("Summer2023!")).getOutcome(), Outcome.NOT_FOUND);

        Files.write(file, "Password@1\nSummer2023!\n".getBytes(StandardCharsets.UTF_8));
        source.reload();

        assertEquals(source.evaluate(context("Summer2023!")).getOutcome(), Outcome.FOUND);
        source.shutdown();
    }

    @Test
    public void aFileThatCannotBeReadLeavesThePreviouslyLoadedListInEffect() throws IOException {

        Path file = write(Arrays.asList("Password@1"));
        LocalBlocklistSource source = configured(file, "plaintext");
        assertEquals(source.evaluate(context("Password@1")).getOutcome(), Outcome.FOUND);

        Files.delete(file);
        String outcome = source.reload();

        assertTrue(outcome.toLowerCase().contains("not readable"));
        assertEquals(source.evaluate(context("Password@1")).getOutcome(), Outcome.FOUND,
                "The previous list must stay in effect rather than emptying itself.");
        source.shutdown();
    }

    @Test
    public void reconfiguringWithTheSameSettingsDoesNotRebuildTheIndex() throws IOException {

        Path file = write(Arrays.asList("Password@1"));
        LocalBlocklistSource source = configured(file, "plaintext");
        Long firstLoad = source.getStatus(TENANT).getLastSuccessEpochMillis().orElse(0L);

        source.configure(new MapSourceConfiguration()
                .set(LocalBlocklistSource.PROPERTY_PATH, file.toString())
                .set(LocalBlocklistSource.PROPERTY_FORMAT, "plaintext")
                .set(LocalBlocklistSource.PROPERTY_REFRESH_SECONDS, 0));

        assertEquals(source.getStatus(TENANT).getLastSuccessEpochMillis().orElse(0L), (Long) firstLoad);
        source.shutdown();
    }

    private LocalBlocklistSource configured(Path file, String format) {

        LocalBlocklistSource source = new LocalBlocklistSource();
        source.configure(new MapSourceConfiguration()
                .set(LocalBlocklistSource.PROPERTY_PATH, file.toString())
                .set(LocalBlocklistSource.PROPERTY_FORMAT, format)
                .set(LocalBlocklistSource.PROPERTY_REFRESH_SECONDS, 0));
        return source;
    }

    private BreachContext context(String password) {

        return BreachContext.builder()
                .credential(new Credential(password.toCharArray()))
                .tenantDomain(TENANT)
                .operation(Operation.REGISTER)
                .build();
    }

    private static Path write(List<String> lines) throws IOException {

        Path file = Files.createTempFile("local-blocklist-", ".txt");
        file.toFile().deleteOnExit();
        Files.write(file, String.join("\n", lines).concat("\n").getBytes(StandardCharsets.UTF_8));
        return file;
    }
}
