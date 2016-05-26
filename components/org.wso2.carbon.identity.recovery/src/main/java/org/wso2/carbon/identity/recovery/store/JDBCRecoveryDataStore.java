package org.wso2.carbon.identity.recovery.store;

import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.recovery.IdentityRecoveryException;
import org.wso2.carbon.identity.recovery.model.UserRecoveryData;

public class JDBCRecoveryDataStore implements UserRecoveryDataStore {
    @Override
    public void store(UserRecoveryData recoveryDataDO) throws IdentityRecoveryException {

    }

    @Override
    public void store(UserRecoveryData[] recoveryDataDOs) throws IdentityRecoveryException {

    }

    @Override
    public UserRecoveryData load(String code) throws IdentityRecoveryException {
        return null;
    }

    @Override
    public boolean validateCode(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws IdentityRecoveryException {
        return false;
    }

    @Override
    public UserRecoveryData invalidate(User user, Enum recoveryScenario, Enum recoveryStep, String code) throws IdentityRecoveryException {
        return null;
    }


    @Override
    public UserRecoveryData invalidate(User user) throws IdentityRecoveryException {
        return null;
    }
}
