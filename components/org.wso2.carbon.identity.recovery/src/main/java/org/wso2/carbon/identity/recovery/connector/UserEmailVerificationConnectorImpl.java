package org.wso2.carbon.identity.recovery.connector;

import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 *  UserEmailVerification Connector.
 */
public class UserEmailVerificationConnectorImpl implements IdentityGovernanceConnector {

    private static String connectorName = "user-email-verification";

    @Override
    public String getName() {
        return connectorName;
    }

    @Override
    public String getFriendlyName() {
        return "User Email Verification";
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {
        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                "Enable User Email Verification");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation Enabled");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                "Enable Notification Internally Management");
        return nameMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE);

        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMIL_VERIFICATION));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.EMAIL_ACCOUNT_LOCK_ON_CREATION));
        defaultProperties.put(
                IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE,
                IdentityUtil.getProperty(
                        IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_NOTIFICATION_INTERNALLY_MANAGE));

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {
        return null;
    }


}
