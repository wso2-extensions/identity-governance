package org.wso2.carbon.identity.password.history.store.Impl;

import org.mockito.Mockito;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;
import org.wso2.carbon.user.core.exceptions.PasswordHashingException;
import org.wso2.carbon.user.core.hash.PasswordHashProcessor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertSame;

public class DefaultPasswordHistoryDataStoreTest {

    String constPasswordHashProcessor = "passwordHashProcessor";
    String methodGetPasswordHashProcessor = "getPasswordHashProcessor";
    String password = "TestPassword123!";
    String salt = "TestSalt";
    String expectedHash = "hashedPassword";
    String digestFunction = "PBKDF2";
    int maxHistoryCount = 5;

    @Test
    public void testPreparePassword() throws Exception {

        PasswordHashProcessor mockProcessor = Mockito.mock(PasswordHashProcessor.class);
        when(mockProcessor.hashPassword(password, salt)).thenReturn(expectedHash);

        DefaultPasswordHistoryDataStore dataStore =
                new DefaultPasswordHistoryDataStore(digestFunction, maxHistoryCount);

        // Inject the mock using reflection
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

        // Explicitly set processor to null
        Field field = DefaultPasswordHistoryDataStore.class.getDeclaredField(constPasswordHashProcessor);
        field.setAccessible(true);
        field.set(dataStore, null);

        dataStore.preparePassword(password, salt);
    }

    @Test(expectedExceptions = IdentityPasswordHistoryException.class)
    public void testPreparePasswordHashFails() throws Exception {

        PasswordHashProcessor mockProcessor = Mockito.mock(PasswordHashProcessor.class);
        when(mockProcessor.hashPassword(anyString(), anyString()))
                .thenThrow(new PasswordHashingException("Hash failed"));

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

        PasswordHashProcessor mockProcessor = Mockito.mock(PasswordHashProcessor.class);
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
            throw new RuntimeException("Failed to invoke getPasswordHashProcessor", e);
        }
    }
}

