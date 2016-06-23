package org.wso2.carbon.identity.recovery.connector;

import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityGovernanceConnector;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.*;

public class RecoveryConnectorImpl implements IdentityGovernanceConnector {

    private static String connectorName = "account-recovery";

    @Override
    public String getName() {
        return connectorName;
    }

    @Override
    public String getFriendlyName() {
        return "Account Recover";
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {
        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE, IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY, IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY, IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR, IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER, IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE, IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME, IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);
        return nameMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME);
        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {


        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_INTERNALLY_MANAGE));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.NOTIFICATION_BASED_PW_RECOVERY));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.QUESTION_BASED_PW_RECOVERY));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.QUESTION_CHALLENGE_SEPARATOR));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.QUESTION_MIN_NO_ANSWER));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.USERNAME_RECOVERY_ENABLE));
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME,
                IdentityUtil.getProperty(IdentityRecoveryConstants.ConnectorConfig.EXPIRY_TIME));

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws IdentityGovernanceException {
        return null;
    }


}
