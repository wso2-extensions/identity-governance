package org.wso2.carbon.identity.recovery;

public interface UserWorkflowManagementService {

    public boolean isUserExists(String userName, String tenantDomain) throws IdentityRecoveryException;
}
