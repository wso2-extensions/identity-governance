
/*
 * Copyright (c) 2018, WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

package org.wso2.carbon.identity.recovery.signup;

import org.junit.Assert;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.testng.PowerMockTestCase;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;
import org.wso2.carbon.consent.mgt.core.ConsentManager;
import org.wso2.carbon.consent.mgt.core.ConsentManagerImpl;
import org.wso2.carbon.consent.mgt.core.exception.ConsentManagementException;
import org.wso2.carbon.consent.mgt.core.model.AddReceiptResponse;
import org.wso2.carbon.consent.mgt.core.model.ConsentManagerConfigurationHolder;
import org.wso2.carbon.consent.mgt.core.model.ReceiptInput;
import org.wso2.carbon.identity.application.common.model.IdentityProvider;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.core.util.IdentityTenantUtil;
import org.wso2.carbon.identity.core.util.IdentityUtil;
import org.wso2.carbon.identity.recovery.IdentityRecoveryConstants;
import org.wso2.carbon.identity.recovery.internal.IdentityRecoveryServiceDataHolder;
import org.wso2.carbon.identity.recovery.util.Utils;
import org.wso2.carbon.idp.mgt.IdentityProviderManager;

@PrepareForTest({IdentityUtil.class, Utils.class, IdentityTenantUtil.class, IdentityProviderManager.class})
public class UserSelfRegistrationManagerTest extends PowerMockTestCase {

    UserSelfRegistrationManager userSelfRegistrationManager = UserSelfRegistrationManager.getInstance();
    ReceiptInput resultReceipt;
    @Mock
    IdentityProviderManager identityProviderManager;

    @BeforeTest
    void setup() {

        this.resultReceipt = null;
    }

    String consentData =
            "{\"jurisdiction\":\"someJurisdiction\",\"collectionMethod\":\"Web Form - Self Registration\"," +
                    "\"language\":\"en\",\"piiPrincipalId\":\"DOMAIN/testuser\",\"services\":" +
                    "[{\"tenantDomain\":\"wso2.com\",\"serviceDisplayName\":\"Resident IDP\"," +
                    "\"serviceDescription\":\"Resident IDP\",\"purposes\":[{\"purposeId\":3,\"purposeCategoryId\":[1]," +
                    "\"consentType\":\"EXPLICIT\",\"piiCategory\":[{\"piiCategoryId\":1," +
                    "\"validity\":\"DATE_UNTIL:INDEFINITE\"}],\"primaryPurpose\":true," +
                    "\"termination\":\"DATE_UNTIL:INDEFINITE\",\"thirdPartyDisclosure\":false}],\"tenantId\":1}]," +
                    "\"policyURL\":\"somePolicyUrl\",\"tenantId\":1,\"properties\":{}}";


    @Test
    public void testAddConsent() throws Exception {

        PowerMockito.mockStatic(IdentityProviderManager.class);
        IdentityProvider identityProvider = new IdentityProvider();
        PowerMockito.when(identityProviderManager.getInstance()).thenReturn(identityProviderManager);
        PowerMockito.when(identityProviderManager.getResidentIdP(Matchers.anyString())).thenReturn(identityProvider);
        ConsentManager consentManager = new MyConsentManager(new ConsentManagerConfigurationHolder());
        IdentityRecoveryServiceDataHolder.getInstance().setConsentManager(consentManager);
        userSelfRegistrationManager.addUserConsent(consentData, "wso2.com");
        Assert.assertEquals(IdentityRecoveryConstants.Consent.COLLECTION_METHOD_SELF_REGISTRATION,
                resultReceipt.getCollectionMethod());
        Assert.assertEquals("someJurisdiction", resultReceipt.getJurisdiction());
        Assert.assertEquals("en", resultReceipt.getLanguage());
        Assert.assertNotNull(resultReceipt.getServices());
        Assert.assertEquals(1, resultReceipt.getServices().size());
        Assert.assertNotNull(resultReceipt.getServices().get(0).getPurposes());
        Assert.assertEquals(1, resultReceipt.getServices().get(0).getPurposes().size());
        Assert.assertEquals(new Integer(3), resultReceipt.getServices().get(0).getPurposes().get(0).getPurposeId());
        Assert.assertEquals(IdentityRecoveryConstants.Consent.EXPLICIT_CONSENT_TYPE,
                resultReceipt.getServices().get(0).getPurposes().get(0).getConsentType());
        Assert.assertEquals(IdentityRecoveryConstants.Consent.INFINITE_TERMINATION,
                resultReceipt.getServices().get(0).getPurposes().get(0).getTermination());
        Assert.assertEquals(new Integer(3),
                resultReceipt.getServices().get(0).getPurposes().get(0).getPurposeId());
    }

    /**
     * Sample consent manager class.
     */
    class MyConsentManager extends ConsentManagerImpl {

        public MyConsentManager(ConsentManagerConfigurationHolder configHolder) {

            super(configHolder);
        }

        public AddReceiptResponse addConsent(ReceiptInput receiptInput) throws ConsentManagementException {

            resultReceipt = receiptInput;
            return null;
        }
    }
}