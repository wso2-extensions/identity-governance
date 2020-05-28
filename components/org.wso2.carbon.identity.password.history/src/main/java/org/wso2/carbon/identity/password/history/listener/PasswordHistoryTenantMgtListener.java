package org.wso2.carbon.identity.password.history.listener;

import org.wso2.carbon.identity.password.history.exeption.IdentityPasswordHistoryException;
import org.wso2.carbon.identity.password.history.store.Impl.DefaultPasswordHistoryDataStore;
import org.wso2.carbon.stratos.common.beans.TenantInfoBean;
import org.wso2.carbon.stratos.common.exception.StratosException;
import org.wso2.carbon.stratos.common.listeners.TenantMgtListener;

public class PasswordHistoryTenantMgtListener implements TenantMgtListener {
    private static final int EXEC_ORDER = 41;

    @Override
    public void onTenantCreate(TenantInfoBean tenantInfoBean) throws StratosException {

    }

    @Override
    public void onTenantUpdate(TenantInfoBean tenantInfoBean) throws StratosException {

    }

    @Override
    public void onTenantDelete(int i) {

    }

    @Override
    public void onTenantRename(int i, String s, String s2) throws StratosException {

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
    public void onSubscriptionPlanChange(int i, String s, String s2) throws StratosException {

    }

    @Override
    public int getListenerOrder() {
        return EXEC_ORDER;
    }

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
