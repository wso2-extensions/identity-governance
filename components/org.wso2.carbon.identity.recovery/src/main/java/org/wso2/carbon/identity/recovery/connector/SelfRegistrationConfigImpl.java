package org.wso2.carbon.identity.recovery.connector;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * Self Registration Identity connector config implementation
 */
public class SelfRegistrationConfigImpl implements IdentityConnectorConfig {

    private static String connectorName = "self-sign-up";

    @Override
    public String getName() {
        return connectorName;
    }

    @Override
    public String getFriendlyName() {
        return "User Self Registration";
    }

    @Override
    public String getCategory() {
        return "Account Management Policies";
    }

    @Override
    public String getSubCategory() {
        return "DEFAULT";
    }

    @Override
    public int getOrder() {
        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {
        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable Self User Registration");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Enable Account Lock On Creation");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Enable Notification Internally Management");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable reCaptcha");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {
        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP,
                "Enable self user registration");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                "Lock user account during user registration");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                "Set false if the client application handles notification sending");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                "Enable captcha verification during self registration");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);

        return properties.toArray(new String[properties.size()]);
    }

    @Override
    public Properties getDefaultPropertyValues() throws IdentityGovernanceException {

        String enableSelfSignUp = "false";
        String enableAccountLockOnCreation = "true";
        String enableNotificationInternallyManage = "true";
        String enableSelfRegistrationReCaptcha = "true";

        String selfSignUpProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP);
        String accountLockProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION);
        String notificationInternallyMangedProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE);
        String reCaptchaProperty = IdentityUtil.getProperty(
                IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA);

        if (StringUtils.isNotEmpty(selfSignUpProperty)) {
            enableSelfSignUp = selfSignUpProperty;
        }
        if (StringUtils.isNotEmpty(accountLockProperty)) {
            enableAccountLockOnCreation = accountLockProperty;
        }
        if (StringUtils.isNotEmpty(notificationInternallyMangedProperty)) {
            enableNotificationInternallyManage = notificationInternallyMangedProperty;
        }
        if (StringUtils.isNotEmpty(reCaptchaProperty)) {
            enableSelfRegistrationReCaptcha = reCaptchaProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_SELF_SIGNUP, enableSelfSignUp);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.ACCOUNT_LOCK_ON_CREATION,
                enableAccountLockOnCreation);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SIGN_UP_NOTIFICATION_INTERNALLY_MANAGE,
                enableNotificationInternallyManage);
        defaultProperties.put(IdentityRecoveryConstants.ConnectorConfig.SELF_REGISTRATION_RE_CAPTCHA,
                enableSelfRegistrationReCaptcha);
        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames)
            throws IdentityGovernanceException {
        return null;
    }


}
