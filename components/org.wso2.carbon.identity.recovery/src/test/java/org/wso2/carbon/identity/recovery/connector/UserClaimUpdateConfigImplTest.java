/*
 * Copyright (c) 2020, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
 *
 * WSO2 Inc. licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.wso2.carbon.identity.recovery.connector;

import org.apache.axiom.om.OMElement;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.core.util.IdentityConfigParser;
import org.wso2.carbon.identity.core.util.IdentityCoreConstants;
import org.wso2.carbon.identity.governance.IdentityGovernanceException;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.namespace.QName;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * This class does unit test coverage for UserClaimUpdateConfigImpl class.
 */
public class UserClaimUpdateConfigImplTest {

    private UserClaimUpdateConfigImpl userClaimUpdateConfig;
    private static final String CONNECTOR_NAME = "user-claim-update";
    private static final String CATEGORY = "Other Settings";
    private static final String FRIENDLY_NAME = "User Claim Update";
    private static final String SUB_CATEGORY = "DEFAULT";
    private static final String TENANT_DOMAIN = "carbon.super";
    private static final String USER_CLAIM_UPDATE_ELEMENT = "UserClaimUpdate";
    private static final String ENABLE_ELEMENT = "Enable";
    private static final String CLAIM_ELEMENT = "Claim";
    private static final String CLAIM_URI = "uri";
    private static final String VERIFICATION_CODE_ELEMENT = "VerificationCode";
    private static final String EXPIRY_TIME_ELEMENT = "ExpiryTime";
    private static final String VERIFICATION_ON_UPDATE_ELEMENT = "VerificationOnUpdate";
    private MockedStatic<IdentityConfigParser> mockedIdentityConfigParser;

    @BeforeTest
    public void init() {

        userClaimUpdateConfig = new UserClaimUpdateConfigImpl();
    }

    @BeforeMethod
    public void setUp() {

        mockedIdentityConfigParser = Mockito.mockStatic(IdentityConfigParser.class);
    }

    @AfterMethod
    public void tearDown() {

        mockedIdentityConfigParser.close();
    }

    @Test
    public void testGetName() {

        assertEquals(userClaimUpdateConfig.getName(), CONNECTOR_NAME);
    }

    @Test
    public void testGetFriendlyName() {

        assertEquals(userClaimUpdateConfig.getFriendlyName(), FRIENDLY_NAME);
    }

    @Test
    public void testGetCategory() {

        assertEquals(userClaimUpdateConfig.getCategory(), CATEGORY);
    }

    @Test
    public void testGetSubCategory() {

        assertEquals(userClaimUpdateConfig.getSubCategory(), SUB_CATEGORY);
    }

    @Test
    public void testGetOrder() {

        assertEquals(userClaimUpdateConfig.getOrder(), 0);
    }

    @Test
    public void testGetPropertyNameMapping() {

        Map<String, String> nameMappingExpected = new HashMap<>();
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Enable user email verification on update");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Email verification on update link expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                "Enable user email notification on update");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Enable user mobile number verification on update");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Mobile number verification on update SMS OTP expiry time");
        nameMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                "Enable mobile number verification by privileged users");
        Map<String, String> nameMapping = userClaimUpdateConfig.getPropertyNameMapping();
        assertEquals(nameMapping, nameMappingExpected, "Maps are not equal.");
    }

    @Test
    public void testGetPropertyDescriptionMapping() {

        Map<String, String> descriptionMappingExpected = new HashMap<>();
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE,
                "Trigger a verification notification when user's email address is updated.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME, "Validity time of the email confirmation link in " +
                "minutes.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE,
                "Trigger a notification to the existing email address when the user attempts to update the existing " +
                        "email address.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE,
                "Trigger a verification SMS OTP when user's mobile number is updated.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,
                "Validity time of the mobile number confirmation OTP in minutes.");
        descriptionMappingExpected.put(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_VERIFICATION_BY_PRIVILEGED_USER,
                "Allow privileged users to initiate mobile number verification on update.");
        Map<String, String> descriptionMapping = userClaimUpdateConfig.getPropertyDescriptionMapping();
        assertEquals(descriptionMapping, descriptionMappingExpected, "Maps are not equal.");
    }

    @Test
    public void testGetPropertyNames() {

        List<String> propertiesExpected = new ArrayList<>();
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_EMAIL_VERIFICATION_ON_UPDATE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_NOTIFICATION_ON_EMAIL_UPDATE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE);
        propertiesExpected.add(IdentityRecoveryConstants.ConnectorConfig.MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME);
        String[] propertiesArrayExpected = propertiesExpected.toArray(new String[0]);

        String[] properties = userClaimUpdateConfig.getPropertyNames();

        for (int i = 0; i < propertiesArrayExpected.length; i++) {
            assertEquals(properties[i], propertiesArrayExpected[i], "Properties are not equal.");
        }
    }

    @Test
    public void testGetDefaultPropertyValues() throws IdentityGovernanceException {

        IdentityConfigParser mockConfigParser = Mockito.mock(IdentityConfigParser.class);
        mockedIdentityConfigParser.when(IdentityConfigParser::getInstance).thenReturn(mockConfigParser);
        OMElement mockOMElement = mock(OMElement.class);
        when(mockConfigParser.getConfigElement(USER_CLAIM_UPDATE_ELEMENT)).thenReturn(mockOMElement);
        ArrayList<OMElement> claimsList = new ArrayList<>();
        claimsList.add(mockOMElement);
        Iterator claims = claimsList.iterator();
        when(mockOMElement.getChildrenWithName(new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE,
                CLAIM_ELEMENT))).thenReturn(claims);
        when(mockOMElement.getAttributeValue(new QName(CLAIM_URI))).thenReturn(IdentityRecoveryConstants
                .EMAIL_ADDRESS_CLAIM);
        when(mockOMElement.getFirstChildWithName(new QName(IdentityCoreConstants
                .IDENTITY_DEFAULT_NAMESPACE, VERIFICATION_ON_UPDATE_ELEMENT))).thenReturn(mockOMElement);
        when(mockOMElement.getFirstChildWithName(new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE,
                ENABLE_ELEMENT))).thenReturn(mockOMElement);
        when(mockOMElement.getFirstChildWithName(new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE,
                VERIFICATION_CODE_ELEMENT))).thenReturn(mockOMElement);
        when(mockOMElement.getFirstChildWithName(new QName(IdentityCoreConstants.IDENTITY_DEFAULT_NAMESPACE,
                EXPIRY_TIME_ELEMENT))).thenReturn(mockOMElement);

        Properties defaultPropertyValues = userClaimUpdateConfig.getDefaultPropertyValues(TENANT_DOMAIN);
        assertNotNull(defaultPropertyValues.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_EMAIL_VERIFICATION_ON_UPDATE));
        assertNotNull(defaultPropertyValues.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME));
        assertNotNull(defaultPropertyValues.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE));
        assertNotNull(defaultPropertyValues.getProperty(IdentityRecoveryConstants.ConnectorConfig
                .MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME));
    }

    @Test
    public void testGetDefaultProperties() throws IdentityGovernanceException {

        String[] propertyNames = new String[]{IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_EMAIL_VERIFICATION_ON_UPDATE, IdentityRecoveryConstants.ConnectorConfig
                .EMAIL_VERIFICATION_ON_UPDATE_EXPIRY_TIME, IdentityRecoveryConstants.ConnectorConfig
                .ENABLE_MOBILE_NUM_VERIFICATION_ON_UPDATE, IdentityRecoveryConstants.ConnectorConfig
                .MOBILE_NUM_VERIFICATION_ON_UPDATE_EXPIRY_TIME,"testproperty"};

        IdentityConfigParser mockConfigParser = mock(IdentityConfigParser.class);
        mockedIdentityConfigParser.when(IdentityConfigParser::getInstance).thenReturn(mockConfigParser);
        Map<String, String> defaultPropertyValues = userClaimUpdateConfig.getDefaultPropertyValues(propertyNames,
                TENANT_DOMAIN);
        assertEquals(defaultPropertyValues.size(), propertyNames.length - 1, "Maps are not equal as" +
                " their size differs.");
    }
}
