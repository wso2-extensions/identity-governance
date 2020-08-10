/*
 * Copyright (c) 2016, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.wso2.carbon.identity.recovery.listener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wso2.carbon.context.PrivilegedCarbonContext;
import org.wso2.carbon.identity.recovery.ChallengeQuestionManager;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.store.JDBCRecoveryDataStore;
import org.wso2.carbon.identity.recovery.store.UserRecoveryDataStore;
import org.wso2.carbon.stratos.common.beans.TenantInfoBean;
import org.wso2.carbon.stratos.common.exception.StratosException;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;

/**
 *  Tenant Management listener for Identity Recovery functionality.
 */
public class TenantManagementListener implements TenantMgtListener {
    private static final int EXEC_ORDER = 40;
    private static final Log log = LogFactory.getLog(TenantManagementListener.class);


    @Override
    public void onTenantCreate(TenantInfoBean tenantInfoBean) throws StratosException {
        String tenantDomain = tenantInfoBean.getTenantDomain();

        PrivilegedCarbonContext.getThreadLocalCarbonContext().startTenantFlow();
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantDomain(tenantDomain);
        PrivilegedCarbonContext.getThreadLocalCarbonContext().setTenantId(tenantInfoBean.getTenantId());

        ChallengeQuestionManager questionManager = ChallengeQuestionManager.getInstance();
        try {
            questionManager.setDefaultChallengeQuestions(tenantDomain);
            if (log.isDebugEnabled()) {
                log.debug("Default Challenge Questions persisted to the " + tenantDomain + " tenant");
            }
        } catch (IdentityRecoveryException e) {
            log.error("Error when trying to set default challenge question for tenant : " + tenantDomain, e);
        } finally {
            PrivilegedCarbonContext.endTenantFlow();
        }
    }

    @Override
    public void onTenantUpdate(TenantInfoBean tenantInfoBean) throws StratosException {

    }

    @Override
    public void onTenantDelete(int i) {

    }

    @Override
    public void onTenantRename(int i, String s, String s1) throws StratosException {

    }

    @Override
    public void onTenantInitialActivation(int i) throws StratosException {

    }

    @Override
    public void onTenantActivation(int i) throws StratosException {

    }

    @Override
    public void onTenantDeactivation(int i) throws StratosException {

    }

    @Override
    public void onSubscriptionPlanChange(int i, String s, String s1) throws StratosException {

    }

    @Override
    public int getListenerOrder() {
        return 0;
    }

    @Override
    public void onPreDelete(int tenantId) throws StratosException {

        try {
            UserRecoveryDataStore userRecoveryDataStore = JDBCRecoveryDataStore.getInstance();
            userRecoveryDataStore.deleteRecoveryDataByTenantId(tenantId);
        } catch (IdentityRecoveryException e) {
            throw new StratosException("Error in deleting recovery data of the tenant:" + tenantId, e);
        }
    }
}
