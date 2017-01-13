/*
 * Copyright (c) 2014, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 *  Version 2.0 (the "License"); you may not use this file except
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

package org.wso2.carbon.identity.recovery.store;

import org.wso2.carbon.identity.mgt.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

/**
 * User recovery data store.
 */
public interface UserRecoveryDataStore {
    void store(UserRecoveryData recoveryDataDO) throws IdentityRecoveryException;

    /*
     * returns UserRecoveryData if the code is validated. Otherwise returns an exception.
     */
    UserRecoveryData load(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws
            IdentityRecoveryException;

    UserRecoveryData load(String code) throws IdentityRecoveryException;

    UserRecoveryData load(User user) throws IdentityRecoveryException;

    void invalidate(String code) throws IdentityRecoveryException;

    void invalidate(User user) throws IdentityRecoveryException;

}
