package org.wso2.carbon.identity.mgt.common;

import org.wso2.carbon.identity.mgt.IdentityGovernanceException;

import java.util.Map;
import java.util.Properties;

public interface IdentityGovernanceConnector {

    String getName();

    String getFriendlyName();

    String[] getPropertyNames();

    Properties getDefaultPropertyValues (String tenantDomain) throws IdentityGovernanceException;

    Map<String, String> getDefaultPropertyValues (String[] propertyNames, String tenantDomain) throws IdentityGovernanceException;

}
