/*
 * Copyright (c) 2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.password.history.store.Impl;

import org.mockito.Mockito;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityDatabaseUtil;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;
import org.wso2.carbon.user.core.exceptions.PasswordHashingException;
import org.wso2.carbon.user.core.hash.PasswordHashProcessor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.PreparedStatement;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertSame;
import static org.testng.Assert.assertTrue;

/**
 * Default PasswordHistory Data Store rest cases.
 */
public class DefaultPasswordHistoryDataStoreTest {

    private final String constPasswordHashProcessor = "passwordHashProcessor";
    private final String methodGetPasswordHashProcessor = "getPasswordHashProcessor";
    private final String password = "TestPassword123!";
    private final String salt = "TestSalt";
    private final String expectedHash = "hashedPassword";
    private final String digestFunction = "PBKDF2";
    private final int maxHistoryCount = 5;

    @Test
    public void testValidateWhenCredentialIsNull() throws Exception {

        DefaultPasswordHistoryDataStore dataStore = new DefaultPasswordHistoryDataStore();
        User mockUser = mock(User.class);

        boolean result = dataStore.validate(mockUser, null);

        assertTrue(result, "Expected validate() to return true when credential is null.");
    }

    @Test
    public void testPreparePassword() throws Exception {

        PasswordHashProcessor mockProcessor = mock(PasswordHashProcessor.class);
        when(mockProcessor.hashPassword(password, salt)).thenReturn(expectedHash);

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);

        // Inject the mock using reflection.
        Field field = DefaultPasswordHistoryDataStore.class.getDeclaredField(constPasswordHashProcessor);
        field.setAccessible(true);
        field.set(dataStore, mockProcessor);

        String actualHash = dataStore.preparePassword(password, salt);
        assertEquals(expectedHash, actualHash);
    }

    @Test(expectedExceptions = IdentityPasswordHistoryException.class)
    public void testPreparePasswordNullProcessor() throws Exception {

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);

        // Explicitly set processor to null.
        Field field = DefaultPasswordHistoryDataStore.class.getDeclaredField(constPasswordHashProcessor);
        field.setAccessible(true);
        field.set(dataStore, null);

        dataStore.preparePassword(password, salt);
    }

    @Test(expectedExceptions = IdentityPasswordHistoryException.class)
    public void testPreparePasswordHashFails() throws Exception {

        PasswordHashProcessor mockProcessor = mock(PasswordHashProcessor.class);
        when(mockProcessor.hashPassword(anyString(), anyString()))
                .thenThrow(new PasswordHashingException("Hash failed."));

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);
        Field field = DefaultPasswordHistoryDataStore.class.getDeclaredField(constPasswordHashProcessor);
        field.setAccessible(true);
        field.set(dataStore, mockProcessor);

        dataStore.preparePassword(password, salt);
    }

    @Test
    public void testGetPasswordHashProcessorInitializeSuccess() {

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);

        PasswordHashProcessor processor = invokeGetPasswordHashProcessor(dataStore);
        assertNotNull(processor);
    }

    @Test
    public void testGetPasswordHashProcessorExisting() throws Exception {

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);

        PasswordHashProcessor mockProcessor = mock(PasswordHashProcessor.class);
        Field field = DefaultPasswordHistoryDataStore.class.getDeclaredField(constPasswordHashProcessor);
        field.setAccessible(true);
        field.set(dataStore, mockProcessor);

        PasswordHashProcessor returned = invokeGetPasswordHashProcessor(dataStore);
        assertSame(mockProcessor, returned);
    }

    private PasswordHashProcessor invokeGetPasswordHashProcessor(DefaultPasswordHistoryDataStore dataStore) {

        try {
            Method method = DefaultPasswordHistoryDataStore.class.getDeclaredMethod(methodGetPasswordHashProcessor);
            method.setAccessible(true);
            return (PasswordHashProcessor) method.invoke(dataStore);
        } catch (Exception e) {
            throw new RuntimeException("Failed to invoke getPasswordHashProcessor.", e);
        }
    }
}
