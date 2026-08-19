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

package org.wso2.carbon.identity.breach.detection.config;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.breach.source.PropertyDescriptor;
import org.wso2.carbon.identity.breach.source.PropertyType;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Resolving a source's declared settings, and the two disciplines that make the declaration mean something:
 * a path cannot escape the deployment, and a value not declared secret cannot be read as one.
 */
public class ResolvedSourceConfigurationTest {

    private static final List<PropertyDescriptor> DESCRIPTORS = Arrays.asList(
            PropertyDescriptor.builder("path", PropertyType.PATH).required(true).build(),
            PropertyDescriptor.builder("format", PropertyType.STRING).defaultValue("auto").build(),
            PropertyDescriptor.builder("read_timeout_ms", PropertyType.DURATION_MS)
                    .defaultValue("1500").build(),
            PropertyDescriptor.builder("api_key", PropertyType.STRING).secret(true).build(),
            PropertyDescriptor.builder("verbose", PropertyType.BOOLEAN).defaultValue("false").build());

    private Path carbonHome;

    @BeforeClass
    public void setUp() throws IOException {

        carbonHome = Files.createTempDirectory("carbon-home-");
        carbonHome.toFile().deleteOnExit();
        System.setProperty("carbon.home", carbonHome.toString());
    }

    @Test
    public void anUnsetSettingFallsBackToTheDeclaredDefault() {

        ResolvedSourceConfiguration configuration = resolve(new LinkedHashMap<>());
        assertEquals(configuration.getString("format").orElse(null), "auto");
        assertEquals(configuration.getInt("read_timeout_ms", 9999), 1500);
        assertFalse(configuration.getBoolean("verbose", false));
    }

    @Test
    public void aConfiguredValueWins() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("format", "sha1");
        values.put("read_timeout_ms", "400");
        assertEquals(resolve(values).getString("format").orElse(null), "sha1");
        assertEquals(resolve(values).getInt("read_timeout_ms", 9999), 400);
    }

    @Test
    public void anUnparseableNumberFallsBackRatherThanFailingTheSource() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("read_timeout_ms", "soon");
        assertEquals(resolve(values).getInt("read_timeout_ms", 777), 777);
    }

    @Test
    public void aSecretIsReadableOnlyWhereItWasDeclaredSecret() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("api_key", "s3cr3t");
        values.put("format", "sha1");
        ResolvedSourceConfiguration configuration = resolve(values);

        assertEquals(new String(configuration.getSecret("api_key").orElse(new char[0])), "s3cr3t");
        assertFalse(configuration.getSecret("format").isPresent(),
                "A value not declared secret must not be reachable as one.");
        assertFalse(configuration.getSecret("unknown").isPresent());
    }

    @Test
    public void aPathInsideTheDeploymentResolves() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("path", "${carbon.home}/repository/resources/security/breached-passwords.txt");
        String resolved = resolve(values).getPath("path").orElse(null);
        assertTrue(resolved != null && resolved.startsWith(carbonHome.toString()));
    }

    @Test
    public void aPathOutsideTheDeploymentIsRefused() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("path", "/etc/shadow");
        assertFalse(resolve(values).getPath("path").isPresent());
    }

    @Test
    public void aTraversalOutOfTheDeploymentIsRefused() {

        Map<String, String> values = new LinkedHashMap<>();
        values.put("path", "${carbon.home}/../../../../etc/passwd");
        assertFalse(resolve(values).getPath("path").isPresent());
    }

    private ResolvedSourceConfiguration resolve(Map<String, String> values) {

        return new ResolvedSourceConfiguration("test",
                DESCRIPTORS, new SourceNamespace("test", values, new LinkedHashMap<>()));
    }
}
