/*
 * Copyright (c) 2023, WSO2 LLC. (http://www.wso2.com).
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

package org.wso2.carbon.identity.user.export.core.service.impl;

import org.mockito.MockitoAnnotations;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import org.wso2.carbon.identity.application.authentication.framework.model.AuthenticatedUser;
import org.wso2.carbon.identity.application.common.model.User;
import org.wso2.carbon.identity.user.export.core.dto.UserInformationDTO;
import org.wso2.carbon.identity.user.export.core.internal.UserProfileExportDataHolder;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.FederatedAssociationManager;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.model.AssociatedIdentityProvider;
import org.wso2.carbon.identity.user.profile.mgt.association.federation.model.FederatedAssociation;
import org.wso2.carbon.user.api.UserStoreException;
import org.wso2.carbon.user.core.service.RealmService;
import org.wso2.carbon.user.core.tenant.TenantManager;

import java.util.ArrayList;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class LinkedAccountsProviderTest {

    private static final String USERNAME_CLAIM_VALUE = "tom";
    private static final String USERSTORE_NAME = "primary";
    private static final String TENANT_NAME = "carbon.super";
    private static final int TENANT_ID = -1234;

    private LinkedAccountsProvider linkedAccountsProvider;
    private FederatedAssociationManager federatedAssociationManager;
    private RealmService realmService;
    private TenantManager tenantManager;

    @BeforeTest
    public void beforeTest() {

        federatedAssociationManager = mock(FederatedAssociationManager.class);
        realmService = mock(RealmService.class);
        tenantManager = mock(TenantManager.class);
        UserProfileExportDataHolder.setFederatedAssociationManager(federatedAssociationManager);
        UserProfileExportDataHolder.setRealmService(realmService);
    }

    @AfterTest
    public void afterTest() {

        federatedAssociationManager = null;
        realmService = null;
        tenantManager = null;
        UserProfileExportDataHolder.setFederatedAssociationManager(null);
        UserProfileExportDataHolder.setRealmService(null);
    }

    @BeforeMethod
    public void setUp() {

        MockitoAnnotations.openMocks(this);
        linkedAccountsProvider = new LinkedAccountsProvider();
        UserProfileExportDataHolder.getUserInformationProviders().add(linkedAccountsProvider);
    }

    @AfterMethod
    public void tearDown() {

    }

    @DataProvider
    public static Object[][] dataProviderWithValidSocialAccounts() {

        User user = new AuthenticatedUser();
        user.setUserName(USERNAME_CLAIM_VALUE);
        user.setUserStoreDomain(USERSTORE_NAME);
        user.setTenantDomain(TENANT_NAME);

        AssociatedIdentityProvider googleIdp = new AssociatedIdentityProvider();
        googleIdp.setName("google");
        googleIdp.setDisplayName("google");
        googleIdp.setId("uuid-1234");

        AssociatedIdentityProvider microsoftIdp = new AssociatedIdentityProvider();
        microsoftIdp.setName("microsoft");
        microsoftIdp.setDisplayName("microsoft");
        microsoftIdp.setId("uuid-123456");

        FederatedAssociation[] federatedAssociations1 = new FederatedAssociation[2];
        federatedAssociations1[0] = new FederatedAssociation("1234", googleIdp, "AAA2345");
        federatedAssociations1[1] = new FederatedAssociation("123456", microsoftIdp, "AAA2345");

        FederatedAssociation[] federatedAssociations2 = new FederatedAssociation[1];
        federatedAssociations2[0] = new FederatedAssociation("1234", googleIdp, "AAA2345");

        return new Object[][]{
                {federatedAssociations1, 2},
                {federatedAssociations2, 1}
        };
    }

    @Test(dataProvider = "dataProviderWithValidSocialAccounts")
    public void testGetLinkedAccounts(FederatedAssociation[] federatedAssociations, int expectedAssociations)
            throws Exception {

        mockObject();
        when(federatedAssociationManager.getFederatedAssociationsOfUser(any())).thenReturn(federatedAssociations);

        UserInformationDTO userAttributesObj = linkedAccountsProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                USERSTORE_NAME, TENANT_ID);

        if (!userAttributesObj.isInformationAvailable()) {
            Assert.fail();
        } else {
            Assert.assertTrue(userAttributesObj.getData() instanceof ArrayList);
            if (userAttributesObj.getData() instanceof ArrayList) {
                ArrayList<Object> userInformation = (ArrayList<Object>) userAttributesObj.getData();
                Assert.assertEquals(userInformation.size(), expectedAssociations);
            }
        }
    }

    @DataProvider
    public static Object[][] dataProviderWithZeroSocialAccounts() {

        User user = new AuthenticatedUser();
        user.setUserName(USERNAME_CLAIM_VALUE);
        user.setUserStoreDomain(USERSTORE_NAME);
        user.setTenantDomain(TENANT_NAME);

        FederatedAssociation[] federatedAssociations = {};

        return new Object[][]{
                {federatedAssociations}
        };
    }

    @Test(dataProvider = "dataProviderWithZeroSocialAccounts")
    public void testGetLinkedAccountsWithEmptyAccounts(FederatedAssociation[] federatedAssociations) throws Exception {

        mockObject();
        when(federatedAssociationManager.getFederatedAssociationsOfUser(any())).thenReturn(federatedAssociations);

        UserInformationDTO userAttributesObj = linkedAccountsProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                USERSTORE_NAME, TENANT_ID);

        if (userAttributesObj.isInformationAvailable()) {
            Assert.fail();
        }
    }

    @DataProvider
    public static Object[][] dataProviderWithInvalidSocialAccounts() {

        User user = new AuthenticatedUser();
        user.setUserName(USERNAME_CLAIM_VALUE);
        user.setUserStoreDomain(USERSTORE_NAME);
        user.setTenantDomain(TENANT_NAME);

        FederatedAssociation[] federatedAssociations = null;

        return new Object[][]{
                {federatedAssociations}
        };
    }

    @Test(dataProvider = "dataProviderWithInvalidSocialAccounts")
    public void testGetLinkedAccountsWithInvalidIdP(FederatedAssociation[] federatedAssociations) throws Exception {

        mockObject();
        when(federatedAssociationManager.getFederatedAssociationsOfUser(any())).thenReturn(federatedAssociations);

        UserInformationDTO userAttributesObj = linkedAccountsProvider.getRetainedUserInformation(USERNAME_CLAIM_VALUE,
                USERSTORE_NAME, TENANT_ID);

        if (userAttributesObj.isInformationAvailable()) {
            Assert.fail();
        }
    }

    private void mockObject() throws UserStoreException {

        when(realmService.getTenantManager()).thenReturn(tenantManager);
        when(tenantManager.getDomain(anyInt())).thenReturn(TENANT_NAME);
    }
}
