/*
 * Copyright (c) 2016-2025, WSO2 LLC. (http://www.wso2.com).
 *
 * WSO2 LLC. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.wso2.carbon.identity.governance;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.osgi.annotation.bundle.Capability;
import org.wso2.carbon.identity.application.common.model.FederatedAuthenticatorConfig;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.IdentityProviderProperty;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.application.common.util.IdentityApplicationConstants;
import org.wso2.carbon.identity.event.IdentityEventConstants;
import org.wso2.carbon.identity.governance.bean.ConnectorConfig;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.governance.exceptions.general.IdentityGovernanceClientException;
import org.wso2.carbon.identity.governance.internal.IdentityMgtServiceDataHolder;
import org.wso2.carbon.identity.organization.management.service.exception.OrganizationManagementException;
import org.wso2.carbon.identity.organization.management.service.util.OrganizationManagementUtil;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementClientException;
import org.wso2.carbon.idp.mgt.IdentityProviderManagementException;
import org.wso2.carbon.idp.mgt.IdpManager;
import org.wso2.carbon.idp.mgt.util.IdPManagementUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Class which contains exposed identity governance services.
 */
@Capability(
        namespace = "osgi.service",
        attribute = {
                "objectClass=org.wso2.carbon.identity.governance.IdentityGovernanceService",
                "service.scope=singleton"
        }
)
public class IdentityGovernanceServiceImpl implements IdentityGovernanceService {

    private static final Log log = LogFactory.getLog(IdentityGovernanceServiceImpl.class);
    private static final String EMAIL_OTP_AUTHENTICATOR = "email-otp-authenticator";
    public static final String EMAIL_OTP_USE_ALPHANUMERIC_CHARS = "EmailOTP.UseAlphanumericChars";
    public static final String EMAIL_OTP_USE_NUMERIC_CHARS = "EmailOTP.OtpRegex.UseNumericChars";
    private static final String RECOVERY_NOTIFICATION_PASSWORD_PROPERTY = "Recovery.Notification.Password.Enable";
    private static final String EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY
            = "Recovery.Notification.Password.emailLink.Enable";
    private static final String EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY =
            "Recovery.Notification.Password.OTP.SendOTPInEmail";
    private static final String SMS_OTP_PASSWORD_RECOVERY_PROPERTY = "Recovery.Notification.Password.smsOtp.Enable";
    private static final String USERNAME_RECOVERY_ENABLE = "Recovery.Notification.Username.Enable";
    private  static final String USERNAME_RECOVERY_EMAIL_ENABLE = "Recovery.Notification.Username.Email.Enable";
    private static final String USERNAME_RECOVERY_SMS_ENABLE = "Recovery.Notification.Username.SMS.Enable";
    private static final String FALSE_STRING = "false";

    public void updateConfiguration(String tenantDomain, Map<String, String> configurationDetails)
            throws IdentityGovernanceException {

        try {
            IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
            IdentityProvider residentIdp = identityProviderManager.getResidentIdP(tenantDomain);

            IdentityProviderProperty[] identityMgtProperties = residentIdp.getIdpProperties();
            List<IdentityProviderProperty> newProperties = new ArrayList<>();
            updateEmailOTPNumericPropertyValue(configurationDetails);
            IdPManagementUtil.validatePasswordRecoveryWithCurrentAndPreviousConfigs(configurationDetails,
                    identityMgtProperties);
            IdPManagementUtil.validateAdminPasswordResetWithCurrentAndPreviousConfigs(configurationDetails,
                    identityMgtProperties);
            IdPManagementUtil.validateAskPasswordBasedPasswordSetWithCurrentAndPreviousConfigs(configurationDetails,
                    identityMgtProperties);
            updatePasswordRecoveryPropertyValues(configurationDetails, identityMgtProperties);
            updateUsernameRecoveryPropertyValues(configurationDetails, identityMgtProperties);
            if (!OrganizationManagementUtil.isOrganization(tenantDomain)) {
                for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
                    IdentityProviderProperty prop = new IdentityProviderProperty();
                    String key = identityMgtProperty.getName();
                    prop.setName(key);
                    if (configurationDetails.containsKey(key)) {
                        prop.setValue(configurationDetails.get(key));
                    } else {
                        prop.setValue(identityMgtProperty.getValue());
                    }
                    newProperties.add(prop);
                    configurationDetails.remove(key);
                }
            }

            for (Map.Entry<String, String> entry : configurationDetails.entrySet()) {
                IdentityProviderProperty prop = new IdentityProviderProperty();
                prop.setName(entry.getKey());
                prop.setValue(entry.getValue());
                newProperties.add(prop);
            }

            residentIdp.setIdpProperties(newProperties.toArray(new IdentityProviderProperty[newProperties.size()]));
            FederatedAuthenticatorConfig[] authenticatorConfigs = residentIdp.getFederatedAuthenticatorConfigs();
            List<FederatedAuthenticatorConfig> configsToSave = new ArrayList<>();
            if (!OrganizationManagementUtil.isOrganization(tenantDomain)) {
                for (FederatedAuthenticatorConfig authenticatorConfig : authenticatorConfigs) {
                    if (IdentityApplicationConstants.Authenticator.PassiveSTS.NAME.equals(authenticatorConfig.getName
                            ()) || IdentityApplicationConstants.Authenticator.SAML2SSO.NAME.equals(authenticatorConfig
                            .getName())) {
                        configsToSave.add(authenticatorConfig);
                    }
                }
            }
            residentIdp.setFederatedAuthenticatorConfigs(configsToSave.toArray(new
                    FederatedAuthenticatorConfig[configsToSave.size()]));
            identityProviderManager.updateResidentIdP(residentIdp, tenantDomain);
        } catch (IdentityProviderManagementClientException e) {
            log.debug("Client error while updating identityManagement properties of Resident IdP.", e);
            throw new IdentityGovernanceClientException(e.getMessage(), e);
        } catch (IdentityProviderManagementException e) {
            log.error("Error while updating identityManagement Properties of Resident Idp.", e);
        } catch (OrganizationManagementException e) {
            throw new IdentityGovernanceException(String.format("Error while checking if tenant %s is an organization.",
                    tenantDomain), e);
        }
    }

    @Override
    public Property[] getConfiguration(String tenantDomain) throws IdentityGovernanceException {

        IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
        IdentityProvider residentIdp = null;
        try {
            residentIdp = identityProviderManager.getResidentIdP(tenantDomain);
        } catch (IdentityProviderManagementException e) {
            String errorMsg = String.format("Error while retrieving resident Idp for %s tenant.", tenantDomain);
            throw new IdentityGovernanceException(errorMsg, e);
        }
        IdentityProviderProperty[] identityMgtProperties = residentIdp.getIdpProperties();
        Property[] configMap = new Property[identityMgtProperties.length];
        int index = 0;
        for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
            if (IdentityEventConstants.PropertyConfig.ALREADY_WRITTEN_PROPERTY_KEY
                    .equals(identityMgtProperty.getName())) {
                continue;
            }
            Property property = new Property();
            property.setName(identityMgtProperty.getName());
            property.setValue(identityMgtProperty.getValue());
            configMap[index] = property;
            index++;
        }

        return configMap;
    }

    @Override
    public Property[] getConfiguration(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        List<Property> requestedProperties = new ArrayList<>();
        Property[] allProperties = getConfiguration(tenantDomain);
        for (String propertyName : propertyNames) {
            for (int i = 0; i < allProperties.length; i++) {
                if (propertyName.equals(allProperties[i].getName())) {
                    requestedProperties.add(allProperties[i]);
                    break;
                }
            }

        }
        return requestedProperties.toArray(new Property[requestedProperties.size()]);

    }

    /**
     * Delete the configurations of an organization from cache and database.
     *
     * @param propertyNames property names to be deleted.
     * @param tenantDomain  tenant domain of the organization.
     * @throws IdentityGovernanceException if an error occurs while deleting the configurations.
     */
    @Override
    public void deleteConfiguration(List<String> propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        try {
            IdpManager identityProviderManager = IdentityMgtServiceDataHolder.getInstance().getIdpManager();
            identityProviderManager.deleteResidentIdpProperties(propertyNames, tenantDomain);
        } catch (IdentityProviderManagementException e) {
            throw new IdentityGovernanceException(e.getMessage(), e);
        }
    }

    public List<IdentityConnectorConfig> getConnectorList() throws IdentityGovernanceException {

        return IdentityMgtServiceDataHolder.getInstance().getIdentityGovernanceConnectorList();
    }

    public List<ConnectorConfig> getConnectorListWithConfigs(String tenantDomain) throws IdentityGovernanceException {

        List<IdentityConnectorConfig> list = IdentityMgtServiceDataHolder.getInstance()
                .getIdentityGovernanceConnectorList();
        Property[] properties = this.getConfiguration(tenantDomain);
        List<ConnectorConfig> configs = new ArrayList<>(list.size());
        String[] connectorProperties;
        String connectorName;
        for (int i = 0; i < list.size(); i++) {
            ConnectorConfig config = new ConnectorConfig();
            Map<String, String> propertyFriendlyNames = list.get(i).getPropertyNameMapping();
            Map<String, String> propertyDescriptions = list.get(i).getPropertyDescriptionMapping();
            Map<String, Property> metaData = list.get(i).getMetaData();
            List<String> confidentialProperties = list.get(i).getConfidentialPropertyValues(tenantDomain);
            config.setFriendlyName(list.get(i).getFriendlyName());
            config.setName(list.get(i).getName());
            config.setCategory(list.get(i).getCategory());
            config.setSubCategory(list.get(i).getSubCategory());
            config.setOrder(list.get(i).getOrder());
            connectorProperties = list.get(i).getPropertyNames();
            connectorName = list.get(i).getName();
            List<Property> configProperties = new ArrayList<>();
            Set<String> addedProperties = new HashSet<>();

            for (String connectorProperty : connectorProperties) {
                for (Property property : properties) {
                    if (StringUtils.isBlank(property.getName()) || addedProperties.contains(property.getName())) {
                        continue;
                    }
                    if (connectorProperty.equals(property.getName()) ||
                            (StringUtils.isNotBlank(connectorName) && property.getName().startsWith(connectorName))) {
                        Property configProperty = new Property();
                        configProperty.setName(property.getName());
                        configProperty.setValue(property.getValue());
                        configProperty.setDisplayName(propertyFriendlyNames.get(property.getName()));
                        configProperty.setDescription(propertyDescriptions.get(property.getName()));

                        if (metaData != null && metaData.containsKey(property.getName())) {
                            configProperty.setType(metaData.get(property.getName()).getType());
                            configProperty.setRegex(metaData.get(property.getName()).getRegex());
                            configProperty.setGroupId(metaData.get(property.getName()).getGroupId());
                        }
                        if (confidentialProperties != null &&
                                confidentialProperties.contains(configProperty.getName())) {
                            configProperty.setConfidential(true);
                        }
                        configProperties.add(configProperty);
                        addedProperties.add(property.getName());
                    }
                }
            }
            config.setProperties(configProperties.toArray(new Property[0]));
            configs.add(i, config);
        }

        try {
            if (OrganizationManagementUtil.isOrganization(tenantDomain)) {
                return getMaskedConnectorList(configs);
            }

            return configs;
        } catch (OrganizationManagementException e) {
            String errorMsg = String.format("Error while checking if tenant %s is an organization", tenantDomain);
            throw new IdentityGovernanceException(errorMsg, e);
        }
    }

    private List<ConnectorConfig> getMaskedConnectorList(List<ConnectorConfig> connectorList) {

        for (ConnectorConfig config : connectorList) {
            Optional<IdentityMgtConstants.INHERITED_MASKED_CONNECTORS> matchingMaskedConnector =
                    IdentityMgtConstants.INHERITED_MASKED_CONNECTORS.findByName(config.getName());
            if (matchingMaskedConnector.isPresent()) {
                List<String> maskableProperties = matchingMaskedConnector.get().getMaskableProperties();
                Property[] maskedProperties = Arrays.stream(config.getProperties()).map(
                        property -> {
                            if (maskableProperties.contains(property.getName())) {
                                Property maskedProperty = new Property();
                                maskedProperty.setName(property.getName());
                                maskedProperty.setValue(property.getValue() != null ?
                                        "*".repeat(property.getValue().length()) : null);
                                maskedProperty.setDisplayName(property.getDisplayName());
                                maskedProperty.setDescription(property.getDescription());
                                return maskedProperty;
                            }
                            return property;
                        }).toArray(Property[]::new);
                config.setProperties(maskedProperties);
            }
        }
        return connectorList;
    }

    public Map<String, List<ConnectorConfig>> getCategorizedConnectorListWithConfigs(String tenantDomain)
            throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        Map<String, List<ConnectorConfig>> categorizedConnectorListWithConfigs = new HashMap<>();

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            String category = connectorConfig.getCategory();
            if (categorizedConnectorListWithConfigs.get(category) == null) {
                List<ConnectorConfig> categorizedConnectors = new ArrayList<>();
                categorizedConnectors.add(connectorConfig);
                categorizedConnectorListWithConfigs.put(category, categorizedConnectors);
            } else {
                categorizedConnectorListWithConfigs.get(category).add(connectorConfig);
            }
        }

        return categorizedConnectorListWithConfigs;
    }

    public List<ConnectorConfig> getConnectorListWithConfigsByCategory(String tenantDomain,
                                                                       String category)
            throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        List<ConnectorConfig> categorizedConnectorListWithConfigs = new ArrayList<>();

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            if (connectorConfig.getCategory().equals(category)) {
                categorizedConnectorListWithConfigs.add(connectorConfig);
            }
        }

        return categorizedConnectorListWithConfigs;
    }

    public ConnectorConfig getConnectorWithConfigs(String tenantDomain,
                                                   String connectorName) throws IdentityGovernanceException {

        List<ConnectorConfig> connectorListWithConfigs = this.getConnectorListWithConfigs(tenantDomain);

        for (ConnectorConfig connectorConfig : connectorListWithConfigs) {
            if (connectorConfig.getName().equals(connectorName)) {
                // Should remove this logic eventually.
                if (isEmailOTPConnector(connectorName, connectorConfig)) {
                    readEmailOTPAlphanumericPropertyValue(connectorConfig);
                }
                return connectorConfig;
            }
        }
        return null;
    }

    /**
     * This method writes value of alphanumeric property to numeric property for email OTP connector.
     *
     * @param configurationDetails Configuration details of the email OTP connector.
     */
    private void updateEmailOTPNumericPropertyValue(Map<String, String> configurationDetails) {

        if (configurationDetails.containsKey(EMAIL_OTP_USE_ALPHANUMERIC_CHARS)) {
            // Gets the value of alphanumeric property value and negates it.
            if (StringUtils.isNotBlank(configurationDetails.get(EMAIL_OTP_USE_ALPHANUMERIC_CHARS))) {
                boolean useNumericChars = !Boolean.parseBoolean(configurationDetails.get(EMAIL_OTP_USE_ALPHANUMERIC_CHARS));
                configurationDetails.put(EMAIL_OTP_USE_NUMERIC_CHARS, String.valueOf(useNumericChars));
            } else {
                configurationDetails.put(EMAIL_OTP_USE_NUMERIC_CHARS, "true");
                configurationDetails.put(EMAIL_OTP_USE_ALPHANUMERIC_CHARS, "false");
            }
        }
    }

    /**
     * This method is used to convert the property value useNumericCharacters to useAlphanumericCharacters for email OTP connector.
     *
     * @param connectorConfig Connector configuration.
     */
    private void readEmailOTPAlphanumericPropertyValue(ConnectorConfig connectorConfig) {

        int alphanumericPropertyIndex = -1;
        int numericPropertyIndex = -1;
        String numericPropertyValue = null;
        for (int i = 0; i < connectorConfig.getProperties().length; i++) {
            if (EMAIL_OTP_USE_ALPHANUMERIC_CHARS.equals(connectorConfig.getProperties()[i].getName())) {
                alphanumericPropertyIndex = i;
            } else if (EMAIL_OTP_USE_NUMERIC_CHARS.equals(connectorConfig.getProperties()[i].getName())) {
                numericPropertyIndex = i;
                numericPropertyValue = connectorConfig.getProperties()[i].getValue();
            }
        }
        
        if (alphanumericPropertyIndex == -1 || numericPropertyIndex == -1) {
            log.debug("The connector: email OTP doesn't have both old and new email OTP type related " +
                    "configs.");
            return;
        }

        if (StringUtils.isNotBlank(numericPropertyValue)) {
            // Extract the value of the useNumericCharacters property.
            boolean useAlphanumericChars = !Boolean.parseBoolean(numericPropertyValue);
            // Assign the value to the alphanumeric property.
            connectorConfig.getProperties()[alphanumericPropertyIndex].setValue(
                    String.valueOf(useAlphanumericChars));
        } else {
            // Default value of alphanumeric property should be false.
            connectorConfig.getProperties()[numericPropertyIndex].setValue("true");
            connectorConfig.getProperties()[alphanumericPropertyIndex].setValue("false");
        }
    }

    /**
     * This method is used to check whether the connector is email OTP connector or not.
     *
     * @param connectorName   Name of the connector.
     * @param connectorConfig Connector configuration.
     *
     * @return True if the connector is email OTP connector and have both old and new email OTP type related configs.
     */
    private boolean isEmailOTPConnector(String connectorName, ConnectorConfig connectorConfig) {

        /*
        If the new config is also added, the length of the properties will be 5 with both old
        and new OTP type properties. Here the purpose of adding 'at least check' is to allow
        developers to safely add new configs.
         */
        return EMAIL_OTP_AUTHENTICATOR.equals(connectorName);
    }

    /**
     * This method updates the password recovery property values based on the new configurations.
     *
     * @param configurationDetails    Updating configuration details of the resident identity provider.
     * @param identityMgtProperties   Identity management properties of the resident identity provider.
     */
    private void updatePasswordRecoveryPropertyValues(Map<String, String> configurationDetails,
                                                      IdentityProviderProperty[] identityMgtProperties) {

        if (configurationDetails.containsKey(RECOVERY_NOTIFICATION_PASSWORD_PROPERTY) ||
                configurationDetails.containsKey(EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY) ||
                configurationDetails.containsKey(EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY) ||
                configurationDetails.containsKey(SMS_OTP_PASSWORD_RECOVERY_PROPERTY)) {
            // Perform process only if notification based password recovery connector or options are updated.
            String recoveryNotificationPasswordProp = configurationDetails.get(RECOVERY_NOTIFICATION_PASSWORD_PROPERTY);
            String emailLinkForPasswordRecoveryProp = configurationDetails.get(EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY);
            String emailOtpForPasswordRecoveryProp = configurationDetails.get(EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY);
            String smsOtpForPasswordRecoveryProp = configurationDetails.get(SMS_OTP_PASSWORD_RECOVERY_PROPERTY);

            boolean recoveryNotificationPasswordProperty = Boolean.parseBoolean(recoveryNotificationPasswordProp);
            boolean smsOtpPasswordRecoveryProperty = Boolean.parseBoolean(smsOtpForPasswordRecoveryProp);
            boolean emailLinkPasswordRecoveryProperty = Boolean.parseBoolean(emailLinkForPasswordRecoveryProp);
            boolean emailOtpPasswordRecoveryProperty = Boolean.parseBoolean(emailOtpForPasswordRecoveryProp);

            if (recoveryNotificationPasswordProperty) {
                // Connector is enabled and if channels are not explicitly enabled/disabled, email link and sms otp
                // will be enabled in default.
                configurationDetails.put(EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(emailLinkPasswordRecoveryProperty ||
                                StringUtils.isBlank(emailLinkForPasswordRecoveryProp)));
                configurationDetails.put(EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(emailOtpPasswordRecoveryProperty));
                configurationDetails.put(SMS_OTP_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(smsOtpPasswordRecoveryProperty ||
                                StringUtils.isBlank(smsOtpForPasswordRecoveryProp)));
            } else if (StringUtils.isBlank(recoveryNotificationPasswordProp)) {
                // Connector is not explicitly enabled or disabled. The connector state is derived from new and existing
                // configurations.
                boolean isEmailLinkCurrentlyEnabled = false;
                boolean isEmailOtpCurrentlyEnabled = false;
                boolean isSmsOtpCurrentlyEnabled = false;

                for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
                    if (EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY.equals(identityMgtProperty.getName())) {
                        isEmailLinkCurrentlyEnabled = Boolean.parseBoolean(identityMgtProperty.getValue());
                    } else if (EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY.equals(identityMgtProperty.getName())) {
                        isEmailOtpCurrentlyEnabled = Boolean.parseBoolean(identityMgtProperty.getValue());
                    } else if (SMS_OTP_PASSWORD_RECOVERY_PROPERTY.equals(identityMgtProperty.getName())) {
                        isSmsOtpCurrentlyEnabled = Boolean.parseBoolean(identityMgtProperty.getValue());
                    }
                }

                boolean enableEmailLinkPasswordRecovery = emailLinkPasswordRecoveryProperty ||
                        ( StringUtils.isBlank(emailLinkForPasswordRecoveryProp) &&
                                isEmailLinkCurrentlyEnabled );
                boolean enableEmailOtpPasswordRecovery = emailOtpPasswordRecoveryProperty ||
                        ( StringUtils.isBlank(emailOtpForPasswordRecoveryProp) &&
                                isEmailOtpCurrentlyEnabled);
                boolean enableSmsOtpPasswordRecovery = smsOtpPasswordRecoveryProperty ||
                        ( StringUtils.isBlank(smsOtpForPasswordRecoveryProp) &&
                                isSmsOtpCurrentlyEnabled );

                configurationDetails.put(EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(enableEmailLinkPasswordRecovery));
                configurationDetails.put(EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(enableEmailOtpPasswordRecovery));
                configurationDetails.put(SMS_OTP_PASSWORD_RECOVERY_PROPERTY,
                        String.valueOf(enableSmsOtpPasswordRecovery));
                configurationDetails.put(RECOVERY_NOTIFICATION_PASSWORD_PROPERTY,
                        String.valueOf(enableEmailLinkPasswordRecovery || enableEmailOtpPasswordRecovery ||
                                enableSmsOtpPasswordRecovery));
            } else {
                // This is the scenario where the RECOVERY_NOTIFICATION_PASSWORD_PROPERTY is being disabled.
                configurationDetails.put(EMAIL_LINK_PASSWORD_RECOVERY_PROPERTY, FALSE_STRING);
                configurationDetails.put(EMAIL_OTP_PASSWORD_RECOVERY_PROPERTY, FALSE_STRING);
                configurationDetails.put(SMS_OTP_PASSWORD_RECOVERY_PROPERTY, FALSE_STRING);
            }
        }
    }

    /**
     * This method updates the username recovery property values based on the new configurations.
     *
     * @param configurationDetails    Updating configuration details of the resident identity provider.
     * @param identityMgtProperties   Identity management properties of the resident identity provider.
     */
    private void updateUsernameRecoveryPropertyValues(Map<String, String> configurationDetails,
                                                      IdentityProviderProperty[] identityMgtProperties) {

        if (configurationDetails.containsKey(USERNAME_RECOVERY_ENABLE) ||
                configurationDetails.containsKey(USERNAME_RECOVERY_EMAIL_ENABLE) ||
                configurationDetails.containsKey(USERNAME_RECOVERY_SMS_ENABLE)) {

            String usernameRecoveryProp = configurationDetails.get(USERNAME_RECOVERY_ENABLE);
            String usernameRecoveryEmailProp = configurationDetails.get(USERNAME_RECOVERY_EMAIL_ENABLE);
            String usernameRecoverySmsProp = configurationDetails.get(USERNAME_RECOVERY_SMS_ENABLE);

            boolean usernameRecoveryProperty = Boolean.parseBoolean(usernameRecoveryProp);
            boolean usernameRecoveryEmailProperty = Boolean.parseBoolean(usernameRecoveryEmailProp);
            boolean usernameRecoverySmsProperty = Boolean.parseBoolean(usernameRecoverySmsProp);

            if(usernameRecoveryProperty) {
                configurationDetails.put(USERNAME_RECOVERY_EMAIL_ENABLE,
                        String.valueOf(usernameRecoveryEmailProperty ||
                                StringUtils.isBlank(usernameRecoveryEmailProp)));
                configurationDetails.put(USERNAME_RECOVERY_SMS_ENABLE,
                        String.valueOf(usernameRecoverySmsProperty ||
                                StringUtils.isBlank(usernameRecoverySmsProp)));
            } else if (StringUtils.isBlank(usernameRecoveryProp)) {
                // Connector is not explicitly enabled or disabled. The connector state is derived from new and existing
                // configurations.
                boolean isUsernameEmailRecoveryCurrentlyEnabled = false;
                boolean isUsernameSmsRecoveryCurrentlyEnabled = false;
                for (IdentityProviderProperty identityMgtProperty : identityMgtProperties) {
                    if (USERNAME_RECOVERY_EMAIL_ENABLE.equals(identityMgtProperty.getName())) {
                        isUsernameEmailRecoveryCurrentlyEnabled = Boolean.parseBoolean(identityMgtProperty.getValue());
                    } else if (USERNAME_RECOVERY_SMS_ENABLE.equals(identityMgtProperty.getName())) {
                        isUsernameSmsRecoveryCurrentlyEnabled = Boolean.parseBoolean(identityMgtProperty.getValue());
                    }
                }
                boolean enableUsernameEmailRecovery = usernameRecoveryEmailProperty ||
                        ( StringUtils.isBlank(usernameRecoveryEmailProp) &&
                                isUsernameEmailRecoveryCurrentlyEnabled );
                boolean enableUsernameSmsRecovery = usernameRecoverySmsProperty ||
                        ( StringUtils.isBlank(usernameRecoverySmsProp) &&
                                isUsernameSmsRecoveryCurrentlyEnabled );
                configurationDetails.put(USERNAME_RECOVERY_EMAIL_ENABLE,
                        String.valueOf(enableUsernameEmailRecovery));
                configurationDetails.put(USERNAME_RECOVERY_SMS_ENABLE,
                        String.valueOf(enableUsernameSmsRecovery));
                configurationDetails.put(USERNAME_RECOVERY_ENABLE,
                        String.valueOf(enableUsernameEmailRecovery || enableUsernameSmsRecovery));
            } else {
                configurationDetails.put(USERNAME_RECOVERY_EMAIL_ENABLE, FALSE_STRING);
                configurationDetails.put(USERNAME_RECOVERY_SMS_ENABLE, FALSE_STRING);
            }
        }
    }
}
