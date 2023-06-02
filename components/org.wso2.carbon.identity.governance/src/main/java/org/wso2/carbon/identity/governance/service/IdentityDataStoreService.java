package org.wso2.carbon.identity.governance.service;

import org.wso2.carbon.user.core.UserStoreException;
import org.wso2.carbon.user.core.UserStoreManager;

import java.util.Map;

/**
 * This interface is used to store data in the identity data store.
 */
public interface IdentityDataStoreService {

    /**
     * Store identity claims data in the identity data store.
     *
     * @param userName              Username of the user.
     * @param userStoreManager      User store manager.
     * @param operationType         Operation type.
     * @param claims                Identity Claims.
     * @return                      True if the data is stored successfully.
     * @throws UserStoreException   User store exception.
     */
    boolean storeInIdentityDataStore(String userName, UserStoreManager userStoreManager, String operationType,
                                     Map<String, String> claims) throws UserStoreException;
}
