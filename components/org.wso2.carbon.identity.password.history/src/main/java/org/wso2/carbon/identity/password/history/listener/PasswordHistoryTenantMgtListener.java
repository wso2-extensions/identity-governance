/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.password.history.listener;

import org.wso2.carbon.identity.core.AbstractIdentityTenantMgtListener;
import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;
import org.wso2.carbon.identity.password.history.store.Impl.DefaultPasswordHistoryDataStore;
import org.wso2.carbon.stratos.common.exception.StratosException;

/**
 * Password History Tenant Management Listener.
 */
public class PasswordHistoryTenantMgtListener extends AbstractIdentityTenantMgtListener {

    private static final int EXEC_ORDER = 41;

    /**
     * Get the execution order of the listener.
     *
     * @return int The order of the listener.
     */
    @Override
    public int getListenerOrder() {

        return EXEC_ORDER;
    }

    /**
     * Delete password history data before tenant deletion.
     *
     * @param tenantId The id of the tenant.
     * @throws StratosException
     */
    @Override
    public void onPreDelete(int tenantId) throws StratosException {

        DefaultPasswordHistoryDataStore defaultPasswordHistoryDataStore = new DefaultPasswordHistoryDataStore();
        try {
            defaultPasswordHistoryDataStore.deletePasswordHistoryData(tenantId);
        } catch (IdentityPasswordHistoryException e) {
            throw new StratosException("Error in deleting password history data of the tenant: " + tenantId, e);
        }
    }
}
