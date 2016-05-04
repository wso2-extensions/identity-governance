package org.wso2.carbon.identity.mgt;

import org.wso2.carbon.identity.mgt.common.IdentityGovernanceConnector;

import java.util.List;
import java.util.Map;

public interface IdentityGovernanceService {

    void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails) throws
            IdentityGovernanceException;

    Map<String, String> getConfiguration(String tenantDomain) throws IdentityGovernanceException;

    Map<String, String> getConfiguration(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException;

    List<IdentityGovernanceConnector> getConnectorList() throws IdentityGovernanceException;


}
