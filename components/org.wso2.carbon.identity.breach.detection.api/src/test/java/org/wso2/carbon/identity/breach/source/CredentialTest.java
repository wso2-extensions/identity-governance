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

import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Locale;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * The credential is the one object in this design that must not leak, so its handling is pinned down here
 * rather than left to review.
 */
public class CredentialTest {

    @Test
    public void digestMatchesAPlainSha1OfTheUtf8Bytes() {

        Credential credential = new Credential("Password@1".toCharArray());
        assertEquals(credential.digestHex("SHA-1"), sha("SHA-1", "Password@1"));
    }

    @Test
    public void digestSupportsSha256() {

        Credential credential = new Credential("correct horse battery staple".toCharArray());
        assertEquals(credential.digestHex("SHA-256"), sha("SHA-256", "correct horse battery staple"));
    }

    @Test
    public void composedAndDecomposedFormsAgree() {

        // The same password typed on two platforms must not hash differently.
        Credential composed = new Credential("cafépass".toCharArray());
        Credential decomposed = new Credential("cafépass".toCharArray());
        assertEquals(composed.digestHex("SHA-1"), decomposed.digestHex("SHA-1"));
    }

    @Test
    public void caseAndWhitespaceAreSignificant() {

        // Folding either would refuse credentials nobody listed.
        assertNotEquals(new Credential("Secret".toCharArray()).digestHex("SHA-1"),
                new Credential("secret".toCharArray()).digestHex("SHA-1"));
        assertNotEquals(new Credential(" Secret".toCharArray()).digestHex("SHA-1"),
                new Credential("Secret".toCharArray()).digestHex("SHA-1"));
        assertNotEquals(new Credential("Secret ".toCharArray()).digestHex("SHA-1"),
                new Credential("Secret".toCharArray()).digestHex("SHA-1"));
    }

    @Test
    public void toStringNeverRevealsTheCredential() {

        Credential credential = new Credential("Password@1".toCharArray());
        assertFalse(credential.toString().contains("Password"));
        assertFalse(credential.toString().contains("@1"));
    }

    @Test
    public void clearZeroesTheBackingArray() {

        char[] chars = "Password@1".toCharArray();
        Credential credential = new Credential(chars);
        credential.clear();
        assertTrue(credential.isCleared());
        for (char c : chars) {
            assertEquals(c, '\0');
        }
    }

    @Test
    public void aClearedCredentialCannotBeReadAgain() {

        Credential credential = new Credential("Password@1".toCharArray());
        credential.clear();
        try {
            credential.digestHex("SHA-1");
            fail("A cleared credential must not still answer.");
        } catch (IllegalStateException expected) {
            assertTrue(expected.getMessage().contains("cleared"));
        }
    }

    @Test
    public void canonicalBytesDoNotAliasTheBackingArray() {

        char[] chars = "Password@1".toCharArray();
        Credential credential = new Credential(chars);
        byte[] first = credential.canonicalBytes();
        java.util.Arrays.fill(first, (byte) 0);
        // Wiping what the caller was handed must not damage the credential itself.
        assertEquals(credential.digestHex("SHA-1"), sha("SHA-1", "Password@1"));
    }

    @Test
    public void lengthIsSafeToExpose() {

        assertEquals(new Credential("Password@1".toCharArray()).length(), 10);
    }

    private static String sha(String algorithm, String value) {

        try {
            byte[] out = MessageDigest.getInstance(algorithm).digest(value.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder();
            for (byte b : out) {
                hex.append(String.format("%02x", b));
            }
            return hex.toString().toUpperCase(Locale.ROOT);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }
}
