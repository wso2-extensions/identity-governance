package org.wso2.carbon.identity.recovery.connector;

import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.application.common.model.Property;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.IdentityMgtConstants;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;


import static org.wso2.carbon.identity.governance.IdentityGovernanceUtil.getPropertyObject;

public class OrganizationConfigImp implements IdentityConnectorConfig {


    public static final String ONBOARD_ADMIN_TO_SUB_ORGANIZATION = "Organization.SelfService.OnboardAdminToSubOrg";
    public static final String ENABLE_ADMIN_EMAIL_VERIFICATION = "Organization.SelfService.AdminEmailVerification";
    public static final String ENABLE_SELF_SERVICE = "Organization.SelfService.Enable";
    public static final String DEFAULT_SUB_ORGANIZATION_TIER = "Organization.SelfService.DefaultSubOrgTier";

    private static final String connectorName = "organization-self-service";

    private static final String CATEGORY = "User Onboarding";
    private static final String FRIENDLY_NAME = "Sub Organization Self Service";


    @Override
    public String getName() {

        return connectorName;
    }

    @Override
    public String getFriendlyName() {

        return FRIENDLY_NAME;
    }

    @Override
    public String getCategory() {

        return CATEGORY;
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
        nameMapping.put(ENABLE_SELF_SERVICE, "Enable sub organization self service");
        nameMapping.put(ENABLE_ADMIN_EMAIL_VERIFICATION, "Enable admin email verification");
        nameMapping.put(ONBOARD_ADMIN_TO_SUB_ORGANIZATION, "Onboard admin to root organization");
        nameMapping.put(DEFAULT_SUB_ORGANIZATION_TIER, "Default sub organization tier");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(ENABLE_SELF_SERVICE,
                "Allow user's to self service sub organization to the system.");
        descriptionMapping.put(ENABLE_ADMIN_EMAIL_VERIFICATION,
                "User gets email verification before proceeding with sub organization creation.");
        descriptionMapping.put(ONBOARD_ADMIN_TO_SUB_ORGANIZATION,
                "User gets onboard as admin in sub organization");
        descriptionMapping.put(DEFAULT_SUB_ORGANIZATION_TIER,
                "Default tier of the sub organization created in the system");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(ENABLE_SELF_SERVICE);
        properties.add(ENABLE_ADMIN_EMAIL_VERIFICATION);
        properties.add(ONBOARD_ADMIN_TO_SUB_ORGANIZATION);
        properties.add(DEFAULT_SUB_ORGANIZATION_TIER);

        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) throws IdentityGovernanceException {

        String enableSelfService = "false";
        String enableAdminEmailVerification = "false";
        String onboardAdminToSubOrganization = "false";
        String defaultSubOrganizationTier = "free";

        String enableOrganizationSelfServiceProperty = IdentityUtil.getProperty(ENABLE_SELF_SERVICE);
        String enableAdminEmailVerificationProperty = IdentityUtil.getProperty(ENABLE_ADMIN_EMAIL_VERIFICATION);
        String onboardAdminToRootOrganizationProperty = IdentityUtil.getProperty(ONBOARD_ADMIN_TO_SUB_ORGANIZATION);
        String defaultSubOrganizationTierProperty = IdentityUtil.getProperty(DEFAULT_SUB_ORGANIZATION_TIER);

        if (StringUtils.isNotEmpty(enableOrganizationSelfServiceProperty)) {
            enableSelfService = enableOrganizationSelfServiceProperty;
        }
        if (StringUtils.isNotEmpty(enableAdminEmailVerificationProperty)) {
            enableAdminEmailVerification = enableAdminEmailVerificationProperty;
        }
        if (StringUtils.isNotEmpty(onboardAdminToRootOrganizationProperty)) {
            onboardAdminToSubOrganization = onboardAdminToRootOrganizationProperty;
        }
        if (StringUtils.isNotBlank(defaultSubOrganizationTierProperty)) {
            defaultSubOrganizationTier = defaultSubOrganizationTierProperty;
        }

        Map<String, String> defaultProperties = new HashMap<>();
        defaultProperties.put(ENABLE_SELF_SERVICE, enableSelfService);
        defaultProperties.put(ENABLE_ADMIN_EMAIL_VERIFICATION, enableAdminEmailVerification);
        defaultProperties.put(ONBOARD_ADMIN_TO_SUB_ORGANIZATION, onboardAdminToSubOrganization);
        defaultProperties.put(DEFAULT_SUB_ORGANIZATION_TIER, defaultSubOrganizationTier);

        Properties properties = new Properties();
        properties.putAll(defaultProperties);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain)
            throws IdentityGovernanceException {

        return null;
    }

    @Override
    public Map<String, Property> getMetaData() {

        Map<String, Property> meta = new HashMap<>();
        meta.put(ENABLE_SELF_SERVICE, getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        meta.put(ENABLE_ADMIN_EMAIL_VERIFICATION, getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        meta.put(ONBOARD_ADMIN_TO_SUB_ORGANIZATION,
                getPropertyObject(IdentityMgtConstants.DataTypes.BOOLEAN.getValue()));
        meta.put(DEFAULT_SUB_ORGANIZATION_TIER, getPropertyObject(IdentityMgtConstants.DataTypes.STRING.getValue()));
        return meta;
    }

}
