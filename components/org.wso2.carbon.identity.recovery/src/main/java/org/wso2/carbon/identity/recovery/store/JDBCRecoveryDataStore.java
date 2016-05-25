package org.wso2.carbon.identity.recovery.store;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.base.IdentityException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

public class JDBCRecoveryDataStore implements UserRecoveryDataStore {
    @Override
    public void store(UserRecoveryData recoveryDataDO) throws IdentityException {

    }

    @Override
    public void store(UserRecoveryData[] recoveryDataDOs) throws IdentityException {

    }

    @Override
    public UserRecoveryData load(User user, int sequence, String code) throws IdentityException {
        return null;
    }

    @Override
    public UserRecoveryData invalidate(User user, int sequence, String code) throws IdentityException {
        return null;
    }

    @Override
    public UserRecoveryData invalidate(User user) throws IdentityException {
        return null;
    }
}
