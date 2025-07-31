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
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.governance.model;

import java.io.Serializable;
import java.util.List;

/**
 * This class holds the custom persistence enabled claims for user store and identity store.
 */
public class CustomPersistenceEnabledClaims implements Serializable {
    private static List<String> userStorePersistentClaims;
    private static List<String> identityStorePersistentClaims;

    /**
     * Constructor to initialize the custom persistence enabled claims.
     *
     * @param userStorePersistentClaims List of user store persistent claims.
     * @param identityStorePersistentClaims List of identity store persistent claims.
     */
    public CustomPersistenceEnabledClaims(List<String> userStorePersistentClaims,
                                          List<String> identityStorePersistentClaims) {
        CustomPersistenceEnabledClaims.userStorePersistentClaims = userStorePersistentClaims;
        CustomPersistenceEnabledClaims.identityStorePersistentClaims = identityStorePersistentClaims;
    }

    public List<String> getUserStorePersistentClaims() {
        return userStorePersistentClaims;
    }
    public void setUserStorePersistentClaims(List<String> userStorePersistentClaims) {
        CustomPersistenceEnabledClaims.userStorePersistentClaims = userStorePersistentClaims;
    }
    public List<String> getIdentityStorePersistentClaims() {
        return identityStorePersistentClaims;
    }
    public void setIdentityStorePersistentClaims(List<String> identityStorePersistentClaims) {
        CustomPersistenceEnabledClaims.identityStorePersistentClaims = identityStorePersistentClaims;
    }
}
