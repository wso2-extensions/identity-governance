//package org.wso2.carbon.identity.recovery.connector;
//
//import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
//
//import java.util.Map;
//import java.util.Properties;
//
//public class RecoveryConnector implements IdentityGovernanceConnector {
//
//    protected static final Properties properties = new Properties();
//
//    static {
//        properties.put("isRecoveryPolicyAccountLockCheck", "test");
//        properties.put("isRecoveryPolicyAccountDisableCheck", "test");
//
//    }
//
//
//    @Override
//    public String getName() {
//        return "recovery";
//    }
//
//    @Override
//    public String getFriendlyName() {
//        return "Account Recovery";
//    }
//
//    @Override
//    public String[] getPropertyNames() {
//        String[] arr = properties.keySet().toArray(new String[properties.keySet().size()]);
//        return arr;
//    }
//
//    @Override
//    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {
//        return properties;
//    }
//
//    @Override
//    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
//        return null;
//    }
//}
