/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations und
 */
package org.wso2.carbon.identity.recovery.connector;

import org.apache.axiom.om.OMElement;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.wso2.carbon.identity.core.util.IdentityConfigParser;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.governance.common.IdentityConnectorConfig;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.namespace.QName;

/**
 * Class which contains user claim update configs.
 */
public class UserClaimUpdateConfigImpl implements IdentityConnectorConfig {

    private static final String CONNECTOR_NAME = "user-claim-update";
    private static final String CATEGORY = "Other Settings";
    private static final String FRIENDLY_NAME = "User Claim Update";
    private static final String SUB_CATEGORY = "DEFAULT";
    private static final String DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_CODE_EXPIRY_TIME = "1440";
    private static final String DEFAULT_ENABLE_VALUE_FOR_EMAIL_VERIFICATION_ON_UPDATE = "false";
    private static final String DEFAULT_MOBILE_NUM_VERIFICATION_ON_UPDATE_SMS_OTP_EXPIRY_TIME = "15";
    private static final String DEFAULT_ENABLE_VALUE_FOR_MOBILE_NUMBER_VERIFICATION_ON_UPDATE = "false";
    private static final String USER_CLAIM_UPDATE_ELEMENT = "UserClaimUpdate";
    private static final String ENABLE_ELEMENT = "Enable";
    private static final String CLAIM_ELEMENT = "Claim";
    private static final String CLAIM_URI = "uri";
    private static final String VERIFICATION_CODE_ELEMENT = "VerificationCode";
    private static final String EXPIRY_TIME_ELEMENT = "ExpiryTime";
    private static final String VERIFICATION_ON_UPDATE_ELEMENT = "VerificationOnUpdate";
    private static String enableEmailVerificationOnUpdateProperty = null;
    private static String emailVerificationOnUpdateCodeExpiryProperty = null;
    private static String enableMobileNumVerificationOnUpdateProperty = null;
    private static String mobileNumVerificationOnUpdateCodeExpiryProperty = null;

    @Override
    public String getName() {

        return CONNECTOR_NAME;
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

        return SUB_CATEGORY;
    }

    @Override
    public int getOrder() {

        return 0;
    }

    @Override
    public Map<String, String> getPropertyNameMapping() {

        Map<String, String> nameMapping = new HashMap<>();
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Enable user email verification on update");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Email verification on update link expiry time");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Enable user mobile number verification on update");
        nameMapping.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Mobile number verification on update SMS OTP expiry time");
        return nameMapping;
    }

    @Override
    public Map<String, String> getPropertyDescriptionMapping() {

        Map<String, String> descriptionMapping = new HashMap<>();
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Trigger a verification notification when user's email address is updated.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Validity time of the email confirmation link in minutes.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Trigger a verification SMS OTP when user's mobile number is updated.");
        descriptionMapping.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Validity time of the mobile number confirmation OTP in minutes.");
        return descriptionMapping;
    }

    @Override
    public String[] getPropertyNames() {

        List<String> properties = new ArrayList<>();
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE);
        properties.add(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        return properties.toArray(new String[0]);
    }

    @Override
    public Properties getDefaultPropertyValues(String tenantDomain) {

        String enableEmailVerificationOnUpdate = DEFAULT_ENABLE_VALUE_FOR_EMAIL_VERIFICATION_ON_UPDATE;
        String emailVerificationOnUpdateCodeExpiry = DEFAULT_EMAIL_VERIFICATION_ON_UPDATE_CODE_EXPIRY_TIME;
        String enableMobileNumVerificationOnUpdate = DEFAULT_ENABLE_VALUE_FOR_MOBILE_NUMBER_VERIFICATION_ON_UPDATE;
        String mobileNumVerificationOnUpdateCodeExpiry = DEFAULT_MOBILE_NUM_VERIFICATION_ON_UPDATE_SMS_OTP_EXPIRY_TIME;

        loadConfigurations();

        if (StringUtils.isNotBlank(enableEmailVerificationOnUpdateProperty)) {
            enableEmailVerificationOnUpdate = enableEmailVerificationOnUpdateProperty;
        }
        if (StringUtils.isNotBlank(emailVerificationOnUpdateCodeExpiryProperty)) {
            emailVerificationOnUpdateCodeExpiry = emailVerificationOnUpdateCodeExpiryProperty;
        }
        if (StringUtils.isNotBlank(enableMobileNumVerificationOnUpdateProperty)) {
            enableMobileNumVerificationOnUpdate = enableMobileNumVerificationOnUpdateProperty;
        }
        if (StringUtils.isNotBlank(mobileNumVerificationOnUpdateCodeExpiryProperty)) {
            mobileNumVerificationOnUpdateCodeExpiry = mobileNumVerificationOnUpdateCodeExpiryProperty;
        }

        Properties properties = new Properties();
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                enableEmailVerificationOnUpdate);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                emailVerificationOnUpdateCodeExpiry);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                enableMobileNumVerificationOnUpdate);
        properties.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                mobileNumVerificationOnUpdateCodeExpiry);
        return properties;
    }

    @Override
    public Map<String, String> getDefaultPropertyValues(String[] propertyNames, String tenantDomain) throws
            IdentityGovernanceException {

        Properties properties = getDefaultPropertyValues(tenantDomain);
        Map<String, String> filteredDefaultProperties = new HashMap<>();
        if (ArrayUtils.isNotEmpty(propertyNames)) {
            for (String propertyName : propertyNames) {
                if (properties.containsKey(propertyName)) {
                    filteredDefaultProperties.put(propertyName, properties.getProperty(propertyName));
                }
            }
        }
        return filteredDefaultProperties;
    }

    /**
     * Read configuration values defined in identity.xml.
     */
    private void loadConfigurations() {

        // Read configuration values defined in identity.xml.
        OMElement userClaimUpdate = IdentityConfigParser.getInstance().getConfigElement(USER_CLAIM_UPDATE_ELEMENT);
        Iterator claims = null;
        if (userClaimUpdate != null) {
            claims = userClaimUpdate.getChildrenWithName(new QName(IdentityCoreConstants
                    .IDENTITY_DEFAULT_NAMESPACE, CLAIM_ELEMENT));
        }

        if (claims != null) {
            while (claims.hasNext()) {
                OMElement claim = (OMElement) claims.next();
                String claimURI = claim.getAttributeValue(new QName(CLAIM_URI));
                /* Currently claim verification on update support is there only for http://wso2.org/claims/emailaddress
                and http://wso2.org/claims/mobile. */
                if (IdentityRecoveryConstants.EMAIL_ADDRESS_CLAIM.equals(claimURI)) {
                    OMElement verificationOnUpdate = claim.getFirstChildWithName(new QName(IdentityCoreConstants
                            .IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_ON_UPDATE_ELEMENT));
                    if (verificationOnUpdate != null) {
                        enableEmailVerificationOnUpdateProperty = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, ENABLE_ELEMENT)).getText();
                        OMElement verificationCode = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_CODE_ELEMENT));
                        if (verificationCode != null) {
                            emailVerificationOnUpdateCodeExpiryProperty = verificationCode.getFirstChildWithName(new
                                    QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, EXPIRY_TIME_ELEMENT))
                                    .getText();
                        }
                    }
                } else if (IdentityRecoveryConstants.MOBILE_NUMBER_CLAIM.equals(claimURI)) {
                    OMElement verificationOnUpdate = claim.getFirstChildWithName(new QName(IdentityCoreConstants
                            .IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_ON_UPDATE_ELEMENT));
                    if (verificationOnUpdate != null) {
                        enableMobileNumVerificationOnUpdateProperty = verificationOnUpdate.getFirstChildWithName(new
                                QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, ENABLE_ELEMENT)).getText();
                        OMElement verificationCode = verificationOnUpdate.getFirstChildWithName(new QName
                                (IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_CODE_ELEMENT));
                        if (verificationCode != null) {
                            mobileNumVerificationOnUpdateCodeExpiryProperty = verificationCode.getFirstChildWithName(
                                    new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE, EXPIRY_TIME_ELEMENT))
                                    .getText();
                        }
                    }
                }
            }
        }
    }
}
